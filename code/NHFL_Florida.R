library(sf)
library(readr)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(tmap)

#### LOAD IN DATA ##### -------------------------------------------------------------
# load in flood hazard data layer for Broward County, FL
broward_hazzard <- st_read("data_raw/12011C_20221102/S_FLD_HAZ_AR.shp") # CRS NAD83
broward_hazzard <- st_make_valid(broward_hazzard)

# load in clean Florida demographics (tract income and race data)
florida_pop <- read_csv("data_clean/Florida_Race_and_Income_tract")

# load in US county geometry and florida tracts
counties <- counties(cb = TRUE)
tracts <- tracts(cb = TRUE)%>%
  filter(STUSPS == "FL")

#### CLEAN #### ---------------------------------------------------------------

# subset demographics to only Broward County
broward_pop <- florida_pop%>%
  filter(COUNTY == "Broward County")

# subset counties data to create Broward County outline
florida_geometry <- counties%>%
  filter(STUSPS == "FL")%>%
  rename("COUNTY" = "NAMELSAD")

broward_outline <- florida_geometry%>%
  filter(COUNTY == "Broward County")


# subset tracts data to Create tract level geometry for Broward County
broward_geo <- tracts%>%
  filter(NAMELSADCO == "Broward County")

# rename so boward_geo and demograpics can be joined
broward_geo1 <- broward_geo%>%
  rename("TRACTA" = "TRACTCE")

broward_pop1 <- broward_pop%>%
  filter(TRACTA == 990000)


#### JOINS / INTERSECTIONS #### -----------------------------------------------------------------
# join broward demographics with broward tract geometry
broward_pop_geo <- left_join(broward_pop1, broward_geo1, by = "TRACTA")
broward_pop_geo <-st_as_sf(broward_pop_geo)

# spatial join with population and the hazzard layer
broward_pop_hazzard_join <- st_join(broward_hazzard, broward_pop_geo)

# intersects with population and the hazzard layer
broward_pop_hazzard_intersects <- st_intersects(broward_hazzard, broward_pop_geo)
broward_pop_hazzard_intersects1 <- st_intersects(broward_pop_geo, broward_hazzard) #OUTPUT LIST OF 1 :(

# current issue with st_join and st_intersects is the y dataset comes up as NA

# intersections
broward_pop_hazzard_intersection <- st_intersection(broward_hazzard, broward_pop_geo) # ouput 0 obs :(
