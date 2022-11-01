#I know hard coding working directories is very bad but until we can ge larger files on github I want to remember were our data is organized. i am sorry.

#pacman::p_load(sf, readr, raster,tidyr,tidyverse, dplyr, lubridate,haven, stringr, readxl, tigris,ggplot2, tmap)
#pacman::p_load(sf, readr,tidyverse, dplyr, haven, tigris, ggplot2, tmap)
library(sf)
library(readr)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)


#load in florida flood hazard data layer
setwd("/Users/m28t112/OneDrive - Montana State University/Documents")
NFHL = "NFHL_12_20220922.gdb"

st_layers(NFHL)

hazard<-st_read(dsn = NFHL, layer = "S_FLD_HAZ_AR") 

setwd("/Users/m28t112/OneDrive - Montana State University/Documents/GitHub/DataAnalytics_TermProject")
#load in clean florida tract income and race data
florida_tract <- read_csv("/data/clean/Florida_Race_and_Income_tract")

counties<-counties(cb=TRUE)

florida_geometry<-counties%>%
  filter(STUSPS == "FL")%>%
  rename("COUNTY" = "NAMELSAD")

#add geometry to florida census tract data
merge<- left_join(florida_tract, florida_geometry, by = "COUNTY")

merge<-st_as_sf(merge)


#join tract data and hazard layer
sf::sf_use_s2(FALSE)
florida_hazard_tract<-st_join(hazard, merge)
write_csv(florida_hazard_tract, "/data/clean/fl_hazard_tract_merge.csv")
st_write(florida_hazard_tract, "/data/clean/fl_hazard_tract_merge.shp")

florida_hazard_tract_intersects<-st_intersects(hazard, merge)

