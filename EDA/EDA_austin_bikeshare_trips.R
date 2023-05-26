library(tidyverse)
library(tidymodels)
library(corrplot)
library(naniar)
library(simputation)
library(lubridate)

#load all austin bikeshare data, remove unneeded columns
  setwd("~/M_departure_2023/portfolio/austin_bikeshare/austin_bikeshare/data")
  
  aus_stations <- read_csv("austin_bikeshare_stations.csv", col_names = T)
  
  read_trips_data <- data.frame(filenames = dir()) %>% 
    filter(filenames != "austin_bikeshare_stations.csv")
  
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
           dow = weekdays(date))

  
#explore the data ----
  
  #distribution of minutes per ride by type of user
    ggplot(data = aus_trips, aes(x = duration_minutes)) + 
      geom_histogram()
  
    #found that there's outliers here, probably people forgetting to return
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
    
    #
  
    
    
  