
# Set Working Directory to project OneDrive
# E.g., setwd("C:/Users/pelat/OneDrive - Montana State University/Data Analytics Project")

library(tidyverse)
library(tigris)
library(sf)
library(tmap)


broward_master_tidy = st_read("data/clean/broward_master_tidy.shp")

# Get rid of unneeded geometry
broward_master_tidy = st_set_geometry(broward_master_tidy, NULL)

# Create dummy to identify high-risk zones
broward_master_tidy$highRsk = ifelse(broward_master_tidy$FLD_ZON == "VE", 1, ifelse(broward_master_tidy$FLD_ZON == "AE", 1, 0))

# Separate high-risk zone data
broward_highrisk = broward_master_tidy |>
  filter(highRsk==1)

# Find percent area in tracts that is high risk and percent white/Black pop
tract_risk_distribution1 = broward_highrisk |>
  mutate(pctRisk = fldArea/trtArea * 100,
         pctWhit = nnhsp_w/totPop * 100,
         pctBlck = nnhsp_b/totPop * 100) |>
  group_by(TRACTA) |>
  mutate(pctHRsk = sum(pctRisk)) |>
  select(TRACTA, mdn_hs_, pctHRsk, pctWhit, pctBlck)

# Eliminate duplicates
tract_risk_distribution2 = distinct(tract_risk_distribution1)


# Separate non high-risk zone data
broward_nohighrisk = broward_master_tidy |>
  filter(highRsk==0)

# Note that there is zero percent high risk and percent white/black pop
tract_norisk_distribution1 = broward_nohighrisk |>
  mutate(pctHRsk = 0,
         pctWhit = nnhsp_w/totPop * 100,
         pctBlck = nnhsp_b/totPop * 100) |>
  select(TRACTA, mdn_hs_, pctHRsk, pctWhit, pctBlck)

# Eliminate duplicates
tract_norisk_distribution2 = distinct(tract_norisk_distribution1)

# Put highrisk and norisk together
pctHRsk_by_X_data_pre = rbind(tract_risk_distribution2, tract_norisk_distribution2)


# If tract shows up more than once, as having some percent high risk and as having zero percent high risk, drop the observation saying it has zero percent
pctHRsk_by_X_data_pre = pctHRsk_by_X_data_pre |>
  ungroup() |>
  arrange(desc(pctHRsk))

pctHRsk_by_X_data_pre$ID = seq.int(nrow(pctHRsk_by_X_data_pre))

pctHRsk_by_X_data_post = pctHRsk_by_X_data_pre |>
  group_by(TRACTA) |>
  filter(ID == min(ID))


# Create scatter plots

pctHRsk_by_income_plot = ggplot(data = pctHRsk_by_X_data_post, aes(x = mdn_hs_, y = pctHRsk)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(title="Flood Risk versus Income by Tract (Broward County, FL, 2020)",
       x="Median Household Income ($2020)", y = "Percent of Tract Area at High Flood Risk") + 
  theme_light()


pctHRsk_by_pctWhit_plot = ggplot(data = pctHRsk_by_X_data_post, aes(x = pctWhit, y = pctHRsk)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(title="Flood Risk versus Percent White by Tract (Broward County, FL, 2020)",
       x="Percent of Tract Population that is White", y = "Percent of Tract Area at High Flood Risk") + 
  theme_light()

pctHRsk_by_pctBlck_plot = ggplot(data = pctHRsk_by_X_data_post, aes(x = pctBlck, y = pctHRsk)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(title="Flood Risk versus Percent Black by Tract (Broward County, FL, 2020)",
       x="Percent of Tract Population that is Black", y = "Percent of Tract Area at High Flood Risk") + 
  theme_light()


# Save Output ---------------------------------------------------------------------------

ggsave(plot = pctHRsk_by_income_plot, filename = "output/charts/pctHRsk_by_income.png")

ggsave(plot = pctHRsk_by_pctWhit_plot, filename = "output/charts/pctHRsk_by_pctWhit.png")

ggsave(plot = pctHRsk_by_pctBlck_plot, filename = "output/charts/pctHRsk_by_pctBlck.png")





