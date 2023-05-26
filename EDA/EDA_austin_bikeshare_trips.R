library(tidyverse)
library(tidymodels)
library(corrplot)
library(naniar)
library(simputation)

#load austin bikeshare data
  setwd("~/M_departure_2023/portfolio/austin_bikeshare/data")
  aus_trips <- read_csv("austin_bikeshare_trips.csv", col_names = T)
  aus_stations <- read_csv("austin_bikeshare_stations.csv", col_names = T)

