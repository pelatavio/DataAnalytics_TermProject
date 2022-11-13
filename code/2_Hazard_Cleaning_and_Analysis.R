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
broward_pop_geo <- st_as_sf(broward_pop_geo)

# intersections
broward_pop_hazzard_intersection <- st_intersection(broward_pop_geo, broward_hazzard1) # ouput 0 obs :(

# reproject in to crs that preserves area
broward_pop_hazzard_intersection1 <- st_transform(broward_pop_hazzard_intersection, 2163)

# calculate area
broward_pop_hazzard_intersection2 <- broward_pop_hazzard_intersection1 %>%
  cbind(area = units::set_units(st_area(broward_pop_hazzard_intersection1), km^2))

st_write(broward_pop_hazzard_intersection2, "data_temp/pop_hazzard_intersection_area.shp")

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
  

#intersection -> transform into area/or no projections? -> calculate area -> group_by fld_zone summarise sum(area)

#### Analysis of Risk by Race -------------------------------------------------------

# reproject broward_pop_geo into crs that preserves area
broward_pop_geo_proj = st_transform(broward_pop_geo, 2163)

# calculate tract areas
broward_pop_geo_withtractAreas = broward_pop_geo_proj |>
  cbind(trtArea = units::set_units(st_area(broward_pop_geo_proj), km^2))

# Get rid of geometry in one df to make left join possible
broward_pop_withtractAreas = st_set_geometry(broward_pop_geo_withtractAreas, NULL)

# Add back race/income data via left join
broward_pop_hazzard_intersection4 = left_join(broward_pop_hazzard_intersection3, broward_pop_withtractAreas, by="TRACTA")

# Resort so geometry is last column, save
broward_pop_hazzard_intersection4 |>
  select(FLD_ZON:fldArea, GISJOIN:trtArea, geometry) |>
  select(-c(BLCK_GRPA)) |>
  st_write("data_clean/broward_master_tidy.shp")


#### Estimate number of individuals by race in high-risk areas ####

# Make a simplifying assumption that *within* tracts, population is dispersed evenly and there are no racial disparities in risk-exposure
# Effect of assumption will likely be to bias us against finding different effects

# Get proportion of tract's total area covered by its respective kind of flood zone, then
# Estimate people affected by the flood zone using our simplifying assumption
broward_pop_hazzard_intersection5 = broward_pop_hazzard_intersection4 |>
  ungroup() |>
  mutate(cvdArea = fldArea/trtArea,
         whitEst = cvdArea*nonhisp_white,
         blckEst = cvdArea*nonhisp_black,
         natvEst = cvdArea*nonhisp_native,
         asiaEst = cvdArea*nonhisp_asian,
         hispEst = cvdArea*hisp_total,
         othrEst = cvdArea*(totPop-(nonhisp_white + nonhisp_black + nonhisp_native + nonhisp_asian + hisp_total)),
         totEst = cvdArea*totPop)

# Create dummy to identify high-risk zones
broward_pop_hazzard_intersection5$highRsk = ifelse(broward_pop_hazzard_intersection5$FLD_ZON == "VE", 1, ifelse(broward_pop_hazzard_intersection5$FLD_ZON == "AE", 1, 0))

# Separate high-risk zone data
broward_highrisk = broward_pop_hazzard_intersection5 |>
  filter(highRsk==1)

# Use estimated ppl affected to get demographics affected (proportion)
broward_highrisk_affected = broward_highrisk |>
  summarize(pctWhit = sum(whitEst)/sum(totEst) * 100,
            pctBlck = sum(blckEst)/sum(totEst) * 100,
            pctNatv = sum(natvEst)/sum(totEst) * 100,
            pctAsia = sum(asiaEst)/sum(totEst) * 100,
            pctHisp = sum(hispEst)/sum(totEst) * 100,
            pctOthr = sum(othrEst)/sum(totEst) * 100)

# Reformat the data and graph it in a bar plot

broward_highrisk_affected = st_set_geometry(broward_highrisk_affected, NULL)

colnames(broward_highrisk_affected) = c("White", "Black", "Native", "Asian", "Hispanic", "Other")

broward_highrisk_affected$White = as.numeric(broward_highrisk_affected$White)
broward_highrisk_affected$Black = as.numeric(broward_highrisk_affected$Black)
broward_highrisk_affected$Native = as.numeric(broward_highrisk_affected$Native)
broward_highrisk_affected$Asian = as.numeric(broward_highrisk_affected$Asian)
broward_highrisk_affected$Hispanic = as.numeric(broward_highrisk_affected$Hispanic)
broward_highrisk_affected$Other = as.numeric(broward_highrisk_affected$Other)

forbarplot = broward_highrisk_affected |> 
  pivot_longer(cols=White:Other, names_to="Ethnicity", values_to="Percentage") |>
  arrange(Percentage)

highrisk_ethnic_composition = ggplot(forbarplot, (aes(x=Ethnicity, y=Percentage))) +
  geom_bar(stat="identity", width=0.5, fill="steelblue") +
  geom_text(aes(label=round(Percentage, 2)), vjust=1.6, color="white", size=3.5) +
  labs(title="Demographics of Highest Risk Pop (Broward County, FL in 2020)",
       y = "Percentage of Population") +
  scale_x_discrete(limits=c("White", "Hispanic", "Black","Asian","Other"))+
  coord_cartesian(ylim = c(0,50))

highrisk_ethnic_composition

# Save output

# change working directory here to save to github
## e.g., setwd("C:/Users/pelat/OneDrive/Documents/GitHub/DataAnalytics_TermProject")
ggsave(plot = highrisk_ethnic_composition, filename = "output/charts/highrisk_ethnic_composition.pdf")


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
                                                   title = "Flood Hazard Zones",
                                                   palette = "PuRd")
hazzard <- hazzard + tm_shape(broward_geo1) + tm_borders(lwd = 0.5, col = "black")
hazzard

panel <- tmap_arrange(income, hazzard, ncol = 2)
panel

# change working directory here to save to github
tmap_save(income, "output/maps/broward_median_income.png")
tmap_save(hazzard, "output/maps/broward_ranked_fld_hzd.png")
tmap_save(panel, "output/maps/broward_panel_income_fldhzd.png")
ggsave(plot = highrisk_ethnic_composition, filename = "output/charts/highrisk_ethnic_composition.png")

