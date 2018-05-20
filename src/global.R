library(tidyr)
library(dplyr)
library(feather)
library(readr)
library(rgdal)
library(forcats)
library(purrr)
library(shiny)
library(shinyjs)
library(DT)
library(plotly)
library(leaflet)
library(stringr)
library(V8)
#library(shinydashboard)


crime <- read_feather("crime.feather")

crime <- crime %>% mutate(violent_crime = as.numeric(violent_crime),
                          agg_ass_sum = as.numeric(agg_ass_sum),
                          rape_sum = as.numeric(rape_sum),
                          rob_sum = as.numeric(rob_sum),
                          homs_sum = as.numeric(homs_sum))

geocode <- read_csv("geocode.csv")

leaf_data <- readOGR("shc/cb_2016_us_county_20m.shp",
                    layer = "cb_2016_us_county_20m", GDAL1_integer64_policy = TRUE)

#geodata <- leaf_data@data

leafdata <- leaf_data@data %>% mutate(GEOID = as.character(GEOID)) %>% mutate(GEOID = as.numeric(GEOID))

first <- left_join(leafdata, geocode, by = c("GEOID" = "X2"))

df <- first %>% mutate(NAME = stringi::stri_trans_tolower(NAME))

state_list <- crime$State %>% unique()

names(state_list) <- NULL

year_list <- crime$Year %>% unique()

state_data <- readOGR("shp/cb_2016_us_state_20m.shp",
                         layer = "cb_2016_us_state_20m", GDAL1_integer64_policy = TRUE)

states_inter <- state_data@data %>% mutate(GEOID = as.numeric(as.character(GEOID)))

crime_inter <- crime %>% group_by(code) %>% summarise(violent_crime = sum(violent_crime, na.rm = TRUE), 
                                       homs_sum = sum(homs_sum, na.rm = TRUE), 
                                       rob_sum = sum(rob_sum, na.rm = TRUE), 
                                       agg_ass_sum = sum(agg_ass_sum, na.rm = TRUE), 
                                       rape_sum = sum(rape_sum, na.rm = TRUE)) #%>% View()

states_join <- left_join(states_inter, crime_inter, by = c("STUSPS" = "code")) #%>% View()
