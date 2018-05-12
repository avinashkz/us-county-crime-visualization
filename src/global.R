library(tidyr)
library(dplyr)
library(forcats)
library(purrr)
library(feather)
library(rgdal)
library(readr)
library(shiny)
library(leaflet)

crime <- read_feather("../results/crime.feather")

crime <- crime %>% mutate(violent_crime = as.numeric(violent_crime),
                          agg_ass_sum = as.numeric(agg_ass_sum),
                          rape_sum = as.numeric(rape_sum),
                          rob_sum = as.numeric(rob_sum),
                          homs_sum = as.numeric(homs_sum))

geocode <- read_csv("../data/geocode.csv")

leaf_data <- readOGR("../data/shc/cb_2016_us_county_20m.shp",
                    layer = "cb_2016_us_county_20m", GDAL1_integer64_policy = TRUE)

geodata <- leaf_data@data

leafdata <- geodata %>% mutate(GEOID = as.character(GEOID)) %>% mutate(GEOID = as.numeric(GEOID))


first <- left_join(leafdata, geocode, by = c("GEOID" = "X2"))

df <- first %>% mutate(NAME = stringi::stri_trans_tolower(NAME))



state_list <- crime$State %>% unique()

names(state_list) <- NULL
#county_list <-

year_list <- crime$Year %>% unique()

