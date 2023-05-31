library(tidyverse)
library(tidymodels)
library(corrplot)
library(naniar)
library(simputation)
library(lubridate)

#load all austin bikeshare data, remove unneeded columns, create negate function
  setwd("~/M_departure_2023/portfolio/austin_bikeshare/austin_bikeshare/data")
  
  aus_stations <- read_csv("austin_bikeshare_stations.csv", col_names = T)
  
  "%!in%" <- Negate("%in%")

  read_trips_data <- data.frame(filenames = dir()) %>% 
    filter(filenames %!in% c("austin_bikeshare_stations.csv", "sub_types.csv"))
  
  read_trips_data <- read_trips_data %>% 
    mutate(data = map(.x = filenames, 
               .f = ~read_csv(.x,
                              col_names = T,
                              col_types = cols(col_double(),
                                               col_character(),
                                               col_double(),
                                               col_character(),
                                               col_character(),
                                               col_character(),
                                               col_character(),
                                               col_character(),
                                               col_double(),
                                               col_double(),
                                               col_double()))))
  
  aus_trips <- do.call("rbind", read_trips_data$data)
  
  aus_trips <- aus_trips %>% 
    select(-c(10,11))
  
  rm(read_trips_data)
  
#create fiscal calendar
  fiscal <- data.frame(date = seq(from = as_date("2013-12-21"), 
                to = as_date("2023-12-31"),
                by = "days")) %>% 
    mutate(year = year(date),
           month = month(date),
           week = week(date),
           dow = weekdays(date),
           week_join_field = paste0(year, "_", week),
           month_join_field = paste0(year, "_", month))
  
  fiscal_wk_min_join <- fiscal %>% 
    group_by(year, week) %>% 
    summarise(week_start = min(date)) %>% 
    mutate(week_join_field = paste0(year, "_", week)) %>% 
    ungroup() %>% 
    select(week_join_field, week_start)
    
  fiscal_month_min_join <- fiscal %>% 
    group_by(year, month) %>% 
    summarise(month_start = min(date)) %>% 
    mutate(month_join_field = paste0(year, "_", month)) %>% 
    ungroup() %>% 
    select(month_join_field, month_start)
  
  fiscal <- left_join(fiscal, fiscal_wk_min_join, by = "week_join_field")
  fiscal <- left_join(fiscal, fiscal_month_min_join, by = "month_join_field")
  
  fiscal <- fiscal %>% 
    select(-contains("field"))
  
#join fiscal to aus_trips
  fiscal <- fiscal %>% 
    select(date, week_start, month_start)
  
  aus_trips <- left_join(x = aus_trips, y = fiscal, by = "date")

#check missing values
  gg_miss_var(aus_trips, show_pct = T)
  
#removing the NA values for now, 93.5% of original data remaining
  aus_trips <- aus_trips %>% 
    na.omit()

#clean up the date field, add in year, month, day
  aus_trips <- aus_trips %>%
    separate(start_time, 
             into = c("date", "time", "tz"), 
             sep = " ",
             remove = T)
  
  aus_trips <- aus_trips %>% 
    mutate(date = as_date(date, tz = NULL),
           year = year(date),
           month = month(date),
           week = week(date),
           dow = weekdays(date))

  
#explore duration of trips by subscriber ----
  
  #distribution of minutes per ride by type of user
    ggplot(data = aus_trips, aes(x = duration_minutes)) + 
      geom_histogram()
  
    #found that there's outliers here, probably people forgetting to return, or stolen
    #add in an indicator variable for trips greater than 180 min
    #found these are ~1.43% of trips
      aus_trips <- aus_trips %>% 
        mutate(forgot_to_return_flag = ifelse(duration_minutes >= 180, 1, 0))
      
      table(aus_trips$forgot_to_return_flag)
      
      25892 / nrow(aus_trips) * 100
      
    #distribution of minutes per ride by type of user, removing outliers
      ggplot(data = aus_trips %>% filter(forgot_to_return_flag == 0), aes(x = duration_minutes)) + 
        geom_histogram(color = "black", fill = "skyblue2") + 
        facet_wrap(~subscriber_type)
      
    #you need to clean up the subscriber type field
      sub_types <- aus_trips %>% count(subscriber_type)
      
      sub_types <- sub_types %>% 
        arrange(desc(n)) %>% 
        mutate(perc_of_trips = round((n / sum(sub_types$n)) * 100, 2),
               cume_perc_of_trips = cumsum(perc_of_trips))
    
    #write sub type data to excel, will be easier to work with, then join it back in
      #write_csv(sub_types, "~/M_departure_2023/portfolio/austin_bikeshare_prep/data/sub_types.csv")
      
    #read sub type aggregation back in
      sub_types <- read_csv("sub_types.csv", col_names = T)
      
    #aggregate super sub types
      super_sub_types <- sub_types %>% 
        group_by(super_subscriber_type) %>% 
        summarise(sum_trips = sum(n)) %>% 
        arrange(desc(sum_trips)) %>% 
        mutate(perc_of_trips = round((sum_trips / sum(sub_types$n)) * 100, 2),
               cume_perc_of_trips = cumsum(perc_of_trips),
               keep_sub_flag = ifelse(cume_perc_of_trips <= 99, 1, 0))
  
    #join back to sub types, and aus trips, keep 99% of all trips
      super_sub_join <- super_sub_types %>% 
        ungroup() %>% 
        select(super_subscriber_type, keep_sub_flag) %>% 
        filter(keep_sub_flag == 1)
      
      sub_types <- inner_join(sub_types, super_sub_join, by = "super_subscriber_type")
      
      sub_types_join <- sub_types %>% 
        select(subscriber_type, super_subscriber_type)
      
      aus_trips <- inner_join(aus_trips, sub_types_join, by = "subscriber_type")
      
      
  #distribution of minutes per ride by type of super sub user, removing outliers
    ggplot(data = aus_trips %>% filter(forgot_to_return_flag == 0), aes(x = duration_minutes)) + 
      geom_density(color = "black", fill = "skyblue2") + 
      facet_wrap(~super_subscriber_type)
    
    ggplot(data = aus_trips %>% filter(forgot_to_return_flag == 0), 
           aes(x = duration_minutes, color = super_subscriber_type)) + 
      stat_ecdf(geom = "line", size = 2)
    
    ggplot(data = aus_trips %>% filter(forgot_to_return_flag == 0), 
           aes(x = super_subscriber_type, y = duration_minutes, fill = super_subscriber_type)) + 
      geom_boxplot(outlier.alpha = 0.25)
    
  #thoughts
    #the more higher duration rides, the less overall trips you can do...
    #keep in mind for forecasting
    
#explore station imbalance ----
  
  #aggregate up starting station, ending station figures by day, join
    start_station <- aus_trips %>% 
      group_by(start_station_id, date) %>% 
      summarise(nbr_trips_from = n_distinct(trip_id)) %>% 
      ungroup() %>% 
      mutate(join_field = paste0(start_station_id, "_", date))
    
    end_station <- aus_trips %>% 
      group_by(end_station_id, date) %>% 
      summarise(nbr_trips_to = n_distinct(trip_id)) %>% 
      ungroup() %>% 
      mutate(join_field = paste0(end_station_id, "_", date))
    
    station_from_to <- full_join(x = start_station, y = end_station, by = "join_field")
    
  #clean up joined data
    station_from_to <- station_from_to %>% 
      mutate(station_id = coalesce(start_station_id, end_station_id), 
             date = coalesce(date.x, date.y)) %>% 
      select(station_id, date, nbr_trips_from, nbr_trips_to) %>% 
      mutate(nbr_trips_from = ifelse(is.na(nbr_trips_from), 0, nbr_trips_from),
             nbr_trips_to = ifelse(is.na(nbr_trips_to), 0, nbr_trips_to),
             daily_imbalance = nbr_trips_to - nbr_trips_from)
    
#explore total volume over time ----
  
  #aggregate up to monthly figures: total
    monthly_aus_trips_super <- aus_trips %>% 
      group_by(year, month, month_start, super_subscriber_type) %>% 
      summarise(nbr_trips = n_distinct(trip_id),
                nbr_start_stations = n_distinct(start_station_id),
                nbr_end_stations = n_distinct(end_station_id))
    
  #plot monthly over time: total
    ggplot(data = monthly_aus_trips_super, aes(x = month_start, y = nbr_trips)) + 
      geom_line() + 
      geom_point() + 
      facet_wrap(~super_subscriber_type)
    
  #aggregate up to weekly figures: total
    weekly_aus_trips_super <- aus_trips %>% 
      group_by(year, month, week, week_start, super_subscriber_type) %>% 
      summarise(nbr_trips = n_distinct(trip_id),
                nbr_start_stations = n_distinct(start_station_id),
                nbr_end_stations = n_distinct(end_station_id))
    
  #plot weekly over time: total
    ggplot(data = weekly_aus_trips_super, aes(x = week_start, y = nbr_trips)) + 
      geom_line() + 
      geom_point() + 
      facet_wrap(~super_subscriber_type)
    
  #aggregate up to daily figures: total
    daily_aus_trips_super <- aus_trips %>% 
      group_by(year, month, week, date, super_subscriber_type) %>% 
      summarise(nbr_trips = n_distinct(trip_id),
                nbr_start_stations = n_distinct(start_station_id),
                nbr_end_stations = n_distinct(end_station_id))
    
#explore super sub volume over time ----
    
  #plot monthly over time: by super sub
    ggplot(data = monthly_aus_trips_super, aes(x = month_start, y = nbr_trips, color = super_subscriber_type)) + 
      geom_line() + 
      geom_point() + 
      facet_wrap(~super_subscriber_type)
    
  #plot weekly over time: by super sub
    ggplot(data = weekly_aus_trips_super, aes(x = week_start, y = nbr_trips, color = super_subscriber_type)) + 
      geom_line() + 
      geom_point() + 
      facet_wrap(~super_subscriber_type)
    
  #plot daily over time: by super sub
    ggplot(data = daily_aus_trips_super, aes(x = date, y = nbr_trips, color = super_subscriber_type)) + 
      geom_line() + 
      geom_point() + 
      facet_wrap(~super_subscriber_type)
    
#explore super sub volume over time, PERCENT BASED ----
  
  #monthly pivot wider
    monthly_aus_trips_super_wide <- monthly_aus_trips_super %>% 
      ungroup() %>% 
      select(-c(nbr_start_stations, nbr_end_stations)) %>% 
      pivot_wider(names_from = super_subscriber_type,
                  values_from = nbr_trips)
    
  
  