# Set Working Directory to project OneDrive
# E.g., setwd("C:/Users/pelat/OneDrive - Montana State University/Data Analytics Project")

pacman::p_load(tidyverse, tigris, sf, tmap, estimatr, broom, summarytools, fixest, binsreg, modelsummary)


broward_master_tidy = st_read("data/clean/broward_master_tidy.shp")

# Get rid of unneeded geometry
broward_master_tidy = st_set_geometry(broward_master_tidy, NULL)

# Create dummy to identify high-risk zones
broward_master_tidy$highRsk = ifelse(broward_master_tidy$FLD_ZON == "VE", 1, ifelse(broward_master_tidy$FLD_ZON == "AE", 1, 0))

broward_master_tidy <- broward_master_tidy %>%
  mutate(cvdArea = fldArea/trtArea)

# Separate high-risk zone data
broward_highrisk = broward_master_tidy |>
  filter(highRsk==1)

# Find percent area in tracts that is high risk and percent white/black/hispanic/native/asian pop
tract_risk_distribution1 = broward_highrisk |>
  mutate(pctRisk = fldArea/trtArea * 100,
         pctWhit = nnhsp_w/totPop * 100,
         pctBlck = nnhsp_b/totPop * 100,
         pctHisp = hsp_ttl/totPop * 100,
         pctNatv = nnhsp_n/totPop * 100,
         pctAsia = nnhsp_sn/totPop * 100) |>
  group_by(TRACTA) |>
  mutate(pctHRsk = sum(pctRisk)) |>
  select(TRACTA, mdn_hs_, pctHRsk, pctWhit, pctBlck, pctHisp, pctNatv, pctAsia, cvdArea)

tract_risk_distribution2 <- tract_risk_distribution1 %>%
  group_by(TRACTA) %>%
  mutate(cvdArea1 = sum(cvdArea))


tract_risk_distribution3 <- tract_risk_distribution2 %>%
  mutate(hhigh = ifelse(cvdArea1 >= "0.1238471", 1, 0))


# Eliminate duplicates
tract_risk_distribution4 = distinct(tract_risk_distribution3)


# Separate non high-risk zone data
broward_nohighrisk = broward_master_tidy |>
  filter(highRsk==0)

# Note that there is zero percent high risk and percent white/black/hispanic/native/asian pop
tract_norisk_distribution1 = broward_nohighrisk |>
  mutate(pctHRsk = 0,
         pctWhit = nnhsp_w/totPop * 100,
         pctBlck = nnhsp_b/totPop * 100,
         pctHisp = hsp_ttl/totPop * 100,
         pctNatv = nnhsp_n/totPop * 100,
         pctAsia = nnhsp_sn/totPop * 100,
         hhigh = 0) |>
  select(TRACTA, mdn_hs_, pctHRsk, pctWhit, pctBlck, pctHisp, pctNatv, pctAsia, hhigh, cvdArea)

# Eliminate duplicates
tract_norisk_distribution2 = distinct(tract_norisk_distribution1)

# Put highrisk and norisk together
pctHRsk_by_X_data_pre = rbind(tract_risk_distribution4, tract_norisk_distribution2)


# If tract shows up more than once, as having some percent high risk and as having zero percent high risk, drop the observation saying it has zero percent
pctHRsk_by_X_data_pre = pctHRsk_by_X_data_pre |>
  ungroup() |>
  arrange(desc(pctHRsk))

pctHRsk_by_X_data_pre$ID = seq.int(nrow(pctHRsk_by_X_data_pre))

pctHRsk_by_X_data_post = pctHRsk_by_X_data_pre |>
  group_by(TRACTA) |>
  filter(ID == min(ID))


# Regress

lm_robust(pctHRsk ~ pctWhit + pctBlck + pctHisp + pctAsia + mdn_hs_, data = pctHRsk_by_X_data_post)


# LPM using pctHRsk_by_X_data_pre
lpm5 = lm_robust(hhigh ~ pctWhit + pctBlck + pctHisp + pctAsia + mdn_hs_, data = pctHRsk_by_X_data_post)
lpm1 = lm_robust(hhigh ~ pctWhit + mdn_hs_, data = pctHRsk_by_X_data_post)
lpm2 = lm_robust(hhigh ~ pctBlck + mdn_hs_, data = pctHRsk_by_X_data_post)
lpm3 = lm_robust(hhigh ~ pctHisp + mdn_hs_, data = pctHRsk_by_X_data_post)
lpm4 = lm_robust(hhigh ~ pctAsia + mdn_hs_, data = pctHRsk_by_X_data_post)
lpm5 = lm_robust(hhigh ~ pctWhit + pctBlck + pctHisp + pctAsia + mdn_hs_, data = pctHRsk_by_X_data_post)

table <- modelsummary(
  list(
    "Model 1" = lpm1,
    "Model 2" = lpm2,
    "Model 3" = lpm3,
    "Model 4" = lpm4, 
    "Model 5" = lpm5
  ), 
  fmt = 2,
  escape = TRUE,
  output = "output/regressions/lmp.tex"
) 

table

