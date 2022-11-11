# set working directory to shared OneDrive

library(sf)
library(readr)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(tmap)
library(car)

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
  filter(TRACTA != 990000)

# create numeric variable that ranks hazzard layer 
broward_hazzard1 <- broward_hazzard%>%
  mutate(risk = recode(broward_hazzard$FLD_ZONE,
                       recodes = "'VE' = 5; 'AE' = 4; 'AH'= 3; 'AO' = 2; 'X' = 1; 'D' = 0; 'AREA NOT INCLUDED' = 0;"))


#### INTERSECTION HAZARD LAYER AND DEMOGRAPHICS #### -----------------------------------------------------------------
# join broward demographics with broward tract geometry
broward_pop_geo <- left_join(broward_pop1, broward_geo1, by = "TRACTA")
broward_pop_geo <-st_as_sf(broward_pop_geo)

# intersections
broward_pop_hazzard_intersection <- st_intersection(broward_pop_geo, broward_hazzard1) # ouput 0 obs :(

# reproject in to crs that preserves area
broward_pop_hazzard_intersection1 <- st_transform(broward_pop_hazzard_intersection, 2163)

# calculate area
broward_pop_hazzard_intersection2 <- broward_pop_hazzard_intersection1 %>%
  cbind(area = units::set_units(st_area(broward_pop_hazzard_intersection1), km^2))

st_write(broward_pop_hazzard_intersection2, "data_temp/pop_hazzard_intersection_area.shp")

######  CLEAN/CREATE TIDY DATA FRAME #########################################
#read in flood hazzard intersection
broward_pop_hazzard_intersection2 <- st_read("data_temp/pop_hazzard_intersection_area.shp")

# Make key columns easier to read in shapefile
colnames(broward_pop_hazzard_intersection2)[13:17] = c("nhsTot", "nhsWhit", "nhsBlck", "nhsNatv", "nhsAsia")
colnames(broward_pop_hazzard_intersection2)[23] = c("hispTot")
colnames(broward_pop_hazzard_intersection2)[33] = c("mIncome")

# create final data frame were each tract is repeated as many hazzard zones *types*  there are in the tract.
# there should not be more than 2,912 obsrvations. there are 7 different FLD_ZONE values and 416*7 = 2,912
# likely this will be less than 2912 bc I don't think every tract has each type of zone. 
# I have calculated each area and we want the sum of the area of each zone type for each tract
# becareful to not sum the tract population data but to make sure it stays the same as the original dataset.
broward_pop_hazzard_intersection3 <- broward_pop_hazzard_intersection2 %>%
  group_by(FLD_ZON, TRACTA) %>%
  summarise(fldArea = sum(area))

# reproject broward_pop_geo into crs that preserves area
broward_pop_geo_proj = st_transform(broward_pop_geo, 2163)

# calculate tract areas
broward_pop_geo_withtractAreas = broward_pop_geo_proj |>
  cbind(trtArea = units::set_units(st_area(broward_pop_geo_proj), km^2))

# Get rid of geometry in one df to make left join possible
broward_pop_withtractAreas = st_set_geometry(broward_pop_geo_withtractAreas, NULL)

# Add back race/income data via left join
broward_pop_hazzard_intersection4 = left_join(broward_pop_hazzard_intersection3, broward_pop_withtractAreas, by="TRACTA")

# Resort so geometry is last column
broward_pop_hazzard_intersection5 = broward_pop_hazzard_intersection4 |>
  select(FLD_ZON:fldArea, GISJOIN:trtArea, geometry) |>
  select(-c(BLCK_GRPA))

st_write(broward_pop_hazzard_intersection5, "data_clean/broward_master_tidy.shp")
