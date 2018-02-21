library(tidyr)
library(dplyr)
library(forcats)
library(purrr)
library(feather)
#library(shiny)

crime <- read_feather("../results/crime.feather")

crime <- crime %>% mutate(violent_crime = as.numeric(violent_crime),
                          agg_ass_sum = as.numeric(agg_ass_sum),
                          rape_sum = as.numeric(rape_sum),
                          rob_sum = as.numeric(rob_sum),
                          homs_sum = as.numeric(homs_sum))



state_list <- crime$State %>% unique()

names(state_list) <- NULL
#county_list <-

year_list <- crime$Year %>% unique()

