
setwd("C:/Users/pelat/OneDrive/Documents/GitHub/DataAnalytics_TermProject/data/clean")

library(tidyverse)
library(tigris)
library(sf)
library(tmap)

Florida_Race_and_Income_tract = read_csv("Florida_Race_and_Income_tract.csv")

broward_Race_and_Income = Florida_Race_and_Income_tract |>
  filter(COUNTY=="Broward County")


tracts = tracts(cb = TRUE)

broward_tracts = tracts |>
  filter(NAMELSADCO=="Broward County")

names(broward_tracts)[names(broward_tracts)=="TRACTCE"] = "TRACTA"


joined_broward_tracts = left_join(broward_Race_and_Income, broward_tracts, by="TRACTA")

joined_broward_tracts = st_as_sf(joined_broward_tracts)


joined_broward_tracts_noempty = joined_broward_tracts |>
  filter(TRACTA != 990000)

tm_shape(joined_broward_tracts_noempty) + tm_polygons("median_household_income" , 
                                                      style = "cont",
                                                      breaks = c(14000, 50000, 100000, 150000, 200000),
                                                      palette = "Blues")



