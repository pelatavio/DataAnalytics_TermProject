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


#### JOINS / INTERSECTIONS #### -----------------------------------------------------------------
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

# create final data frame were each tract is repeated as many hazzard zones *types*  there are in the tract.
# there should not be more than 2,912 obsrvations. there are 7 different FLD_ZONE values and 416*7 = 2,912
# likely this will be less than 2912 bc I don't think every tract has each type of zone. 
# I have calculated each area and we want the sum of the area of each zone type for each tract
# becareful to not sum the tract population data but to make sure it stays the same as the original dataset.
broward_pop_hazzard_intersection3 <- broward_pop_hazzard_intersection2 %>%
  group_by(FLD_ZONE, TRACTA) %>%
  summarise(area = sum(area))
  

#intersection -> transform into area/or no projections? -> calculate area -> group_by fld_zone summarise sum(area)

#### MAPPING ####--------------------------------------------------------------------------------

income <- tm_shape(broward_pop_geo) + tm_polygons("median_household_income" , 
                                                              style = "cont",
                                                              breaks = c(14000, 50000, 100000, 150000, 200000),
                                                              colorNA = "light grey",
                                                              title = "Median Household Income",
                                                              palette = "YlGn")
income <- income + tm_shape(broward_geo1) + tm_borders(lwd = 0.5, col = "black")
income

hazzard <- tm_shape(broward_hazzard1) + tm_polygons("risk",
                                                   style = "cont",
                                                   breaks = c(0, 1, 2, 3, 4, 5),
                                                   border.alpha = 0,
                                                   title = "Flood Hazzard Zones",
                                                   palette = "PuRd")
hazzard <- hazzard + tm_shape(broward_geo1) + tm_borders(lwd = 0.5, col = "black")
hazzard

panel <- tmap_arrange(income, hazzard, ncol = 2)
panel

# change working directory here to save to github
tmap_save(income, "output/maps/broward_median_income.png")
tmap_save(hazzard, "output/maps/broward_ranked_fld_hzd.png")
tmap_save(panel, "output/maps/broward_panel_income_fldhzd.png")
