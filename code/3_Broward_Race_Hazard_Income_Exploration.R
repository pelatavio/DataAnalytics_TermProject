
# Set wd to Github
## e.g., setwd("C:/Users/pelat/OneDrive/Documents/GitHub/DataAnalytics_TermProject")

library(tidyverse)
library(tigris)
library(sf)
library(tmap)
library(ggplot2)
library(gridExtra)

# Load Florida Race and Income data and isolate Broward County

Florida_Race_and_Income_tract = read_csv("data/clean/Florida_Race_and_Income_tract.csv")

broward_Race_and_Income = Florida_Race_and_Income_tract |>
  filter(COUNTY=="Broward County")

# Merge race/income data with spatial geometry so it can be mapped

tracts = tracts(cb = TRUE)

broward_tracts = tracts |>
  filter(NAMELSADCO=="Broward County")

names(broward_tracts)[names(broward_tracts)=="TRACTCE"] = "TRACTA"


joined_broward_tracts = left_join(broward_Race_and_Income, broward_tracts, by="TRACTA")

joined_broward_tracts = st_as_sf(joined_broward_tracts)


joined_broward_tracts_noempty = joined_broward_tracts |>
  filter(TRACTA != 990000)


# Exploration of Median Household Income ----------------------------------------------------------------

income_map = tm_shape(joined_broward_tracts_noempty) + tm_polygons("median_household_income" , 
                                                                   style = "cont",
                                                                   breaks = c(14000, 50000, 100000, 150000, 200000, 250000),
                                                                   palette = "YlGn",
                                                                   title = "Median Household Income") +
  tm_layout(main.title = "Tract-Level Median Household Income in Broward County, FL (2020)",
            main.title.position = "center",
            legend.bg.color = "white",
            legend.frame = "black")

# Viewing distribution of median household income
summary(broward_Race_and_Income$median_household_income)

test <- broward_Race_and_Income%>%
  filter(median_household_income <= 52000)

test2 <- broward_Race_and_Income%>%
  filter(median_household_income >= 87000)

# Exploration of Racial Demographics ----------------------------------------------------------------

#### Bar Plot of Ethnicities as Percentage of Total County Population ####

# Calculate percentage of County population each ethnicity makes up

broward_overall_race_proportions = broward_Race_and_Income |>
  mutate(totalpop = sum(totPop),
         totalwhite = sum(nonhisp_white),
         totalblack = sum(nonhisp_black),
         totalasian = sum(nonhisp_asian),
         totalhisp = sum(hisp_total),
         White = totalwhite/totalpop * 100,
         Black = totalblack/totalpop * 100,
         Asian = totalasian/totalpop * 100,
         Hispanic = totalhisp/totalpop * 100,
         Other = 100 - (White + Black + Asian + Hispanic)
  ) |>
  select(White:Other)

# Reformat the data and graph it in a bar plot

broward_overall_race_proportions_clean = broward_overall_race_proportions[1,]
broward_overall_race_proportions_clean

forbarplot = broward_overall_race_proportions_clean |> 
  pivot_longer(cols=White:Other, names_to="Ethnicity", values_to="Percentage") |>
  arrange(Percentage)

broward_ethnic_composition = ggplot(forbarplot, (aes(x=Ethnicity, y=Percentage))) +
  geom_bar(stat="identity", width=0.5, fill="steelblue") +
  geom_text(aes(label=round(Percentage, 2)), vjust=1.6, color="white", size=3.5) +
  labs(title="Demographics of Broward County, FL (2020)",
       y = "Percentage of Population") +
  scale_x_discrete(limits=c("White", "Hispanic", "Black","Asian","Other")) + 
  coord_cartesian(ylim = c(0,50))

broward_ethnic_composition

# NEED to make this run in one file

panel2 <- arrangeGrob(broward_ethnic_composition, highrisk_ethnic_composition, ncol = 2)

#### Map of Tracts by largest nonwhite group ####


# Create vector to store largest nonwhite racial group in given tract
largest_nonwhite_grp = vector("character",length = nrow(joined_broward_tracts_noempty))

# Populate the vector
for (i in 1:nrow(joined_broward_tracts_noempty)) {
  
  amount_in_largest_nonwhite_grp = pmax(joined_broward_tracts_noempty$nonhisp_black, joined_broward_tracts_noempty$nonhisp_native, joined_broward_tracts_noempty$nonhisp_asian, joined_broward_tracts_noempty$hisp_total)[[i]]
  
  if (joined_broward_tracts_noempty$nonhisp_black[[i]]==amount_in_largest_nonwhite_grp) {
    
    largest_nonwhite_grp[[i]] = "black"
    
  } else if (joined_broward_tracts_noempty$nonhisp_native[[i]]==amount_in_largest_nonwhite_grp) {
    
    largest_nonwhite_grp[[i]] = "native"
    
  } else if (joined_broward_tracts_noempty$nonhisp_asian[[i]]==amount_in_largest_nonwhite_grp) {
    
    largest_nonwhite_grp[[i]] = "asian"
    
  } else if (joined_broward_tracts_noempty$hisp_total[[i]]==amount_in_largest_nonwhite_grp) {
    
    largest_nonwhite_grp[[i]] = "hispanic"
    
  }
  
}


# Join the vector with the original df

largest_nonwhite_grp = as_tibble(largest_nonwhite_grp)
colnames(largest_nonwhite_grp) = "largest_nonwhite_grp"

jbt_demographics1 = cbind(joined_broward_tracts_noempty, largest_nonwhite_grp)


# Subset the df based on largest nonwhite group; calculate percentage of pop on tract-level

jbt_demographics1 |>
  count(largest_nonwhite_grp)

jbt_demographics1_black = jbt_demographics1 |>
  filter(largest_nonwhite_grp=="black") |>
  mutate(percent_black = (nonhisp_black/totPop)*100)

jbt_demographics1_hispanic = jbt_demographics1 |>
  filter(largest_nonwhite_grp=="hispanic") |>
  mutate(percent_hispanic = (hisp_total/totPop)*100)

# Map

largestnonwhite_map = tm_shape(jbt_demographics1_black) + tm_polygons("percent_black" , 
                                                                      style = "cont",
                                                                      palette = "Blues",
                                                                      breaks = c(9.999999, 50, 100),
                                                                      labels = c("< 10% People of Color","10% - 50% People of Color","Majority People of Color"),
                                                                      title = "Largest Non-white Group is Black") +
  tm_shape(jbt_demographics1_hispanic) + tm_polygons("percent_hispanic" , 
                                                     style = "cont",
                                                     palette = "YlGn",
                                                     breaks = c(9.999999, 50, 100),
                                                     labels = c("< 10% People of Color","10% - 50% People of Color","Majority People of Color"),
                                                     title = "Largest Non-white Group is Hispanic") +
  tm_layout(main.title = "Broward County Floridians of Color in 2020",
            main.title.position = "center",
            legend.bg.color = "white",
            legend.frame = "black")




#### Map of Tracts by largest ethnic group ####


# Create vector to store largest racial group in given tract

largest_grp = vector("character",length = nrow(joined_broward_tracts_noempty))

# Populate the vector

for (i in 1:nrow(joined_broward_tracts_noempty)) {
  
  amount_in_largest_grp = pmax(joined_broward_tracts_noempty$nonhisp_black, joined_broward_tracts_noempty$nonhisp_native, joined_broward_tracts_noempty$nonhisp_asian, joined_broward_tracts_noempty$hisp_total, joined_broward_tracts_noempty$nonhisp_white)[[i]]
  
  if (joined_broward_tracts_noempty$nonhisp_black[[i]]==amount_in_largest_grp) {
    
    largest_grp[[i]] = "black"
    
  } else if (joined_broward_tracts_noempty$nonhisp_native[[i]]==amount_in_largest_grp) {
    
    largest_grp[[i]] = "native"
    
  } else if (joined_broward_tracts_noempty$nonhisp_asian[[i]]==amount_in_largest_grp) {
    
    largest_grp[[i]] = "asian"
    
  } else if (joined_broward_tracts_noempty$hisp_total[[i]]==amount_in_largest_grp) {
    
    largest_grp[[i]] = "hispanic"
    
  } else if (joined_broward_tracts_noempty$nonhisp_white[[i]]==amount_in_largest_grp) {
    
    largest_grp[[i]] = "white"
    
  } 
  
}

# Join the vector with the original df

largest_grp = as_tibble(largest_grp)
colnames(largest_grp) = "largest_grp"

jbt_demographics2 = cbind(joined_broward_tracts_noempty, largest_grp)


# Subset the df based on largest ethnic group; calculate percentage of pop on tract-level

jbt_demographics2 |>
  count(largest_grp)

jbt_demographics2_black = jbt_demographics2 |>
  filter(largest_grp=="black") |>
  mutate(percent_black = (nonhisp_black/totPop)*100)

jbt_demographics2_hispanic = jbt_demographics2 |>
  filter(largest_grp=="hispanic") |>
  mutate(percent_hispanic = (hisp_total/totPop)*100)

jbt_demographics2_white = jbt_demographics2 |>
  filter(largest_grp=="white") |>
  mutate(percent_white = (nonhisp_white/totPop)*100)

# Map

broward_ethnic_map = tm_shape(jbt_demographics2_black) + tm_polygons("percent_black", 
                                                                     style = "cont",
                                                                     palette = "Blues",
                                                                     title = "Largest Ethnic Group is Black") +
  tm_shape(jbt_demographics2_hispanic) + tm_polygons("percent_hispanic", 
                                                     style = "cont",
                                                     palette = "YlGn",
                                                     title = "Largest Ethnic Group is Hispanic") +
  tm_shape(jbt_demographics2_white) + tm_polygons("percent_white", 
                                                  style = "cont",
                                                  palette = "Reds",
                                                  title = "Largest Ethnic Group is White") +
  tm_layout(main.title = " Tract-Level Percent Ethnic Composition of Broward County, FL (2020)",
            main.title.position = "center",
            legend.bg.color = "white",
            legend.frame = "black")


# Save Output ---------------------------------------------------------------------------------------

# change working directory here to save to github
## e.g., setwd("C:/Users/pelat/OneDrive/Documents/GitHub/DataAnalytics_TermProject")

tmap_save(income_map, "output/maps/broward_median_income.png")
tmap_save(broward_ethnic_map, "output/maps/broward_ethnic_map.png")
tmap_save(hazzard, "output/maps/broward_ranked_fld_hzd.png")
ggsave(plot = broward_ethnic_composition, filename = "output/charts/broward_ethnic_composition.png")
ggsave(plot = panel2, filename = "output/charts/bargraph_panel.png")

