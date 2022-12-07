# Set Working Directory to project OneDrive
# E.g., setwd("C:/Users/pelat/OneDrive - Montana State University/Data Analytics Project")

pacman::p_load(tidyverse, tigris, sf, tmap, estimatr, broom, summarytools, fixest, binsreg, modelsummary, texreg)


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
  mutate(hhigh = ifelse(cvdArea1 >= "0.1238471", 1, 0),
         hhhigh = ifelse(cvdArea1 >= "0.2397875", 1, 0))


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
         hhigh = 0,
         hhhigh = 0) |>
  select(TRACTA, mdn_hs_, pctHRsk, pctWhit, pctBlck, pctHisp, pctNatv, pctAsia, hhigh, hhhigh, cvdArea)

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


##### SAM REGRESSION #####

# LPM using pctHRsk_by_X_data_pre
lpm1 = lm_robust(hhigh ~ pctWhit + mdn_hs_, data = pctHRsk_by_X_data_post)

lpm2 = lm_robust(hhigh ~ pctBlck + mdn_hs_, data = pctHRsk_by_X_data_post)

lpm3 = lm_robust(hhigh ~ pctHisp + mdn_hs_, data = pctHRsk_by_X_data_post)

lpm4 = lm_robust(hhigh ~ pctAsia + mdn_hs_, data = pctHRsk_by_X_data_post)

# Prepare regressions for LaTeX
texreg(list(lpm1, lpm2, lpm3, lpm4), stars=c(0.01, 0.05, 0.1), caption = "Linear Probaility Model")

# LPM different hhhigh
hlpm1 = lm_robust(hhhigh ~ pctWhit + mdn_hs_, data = pctHRsk_by_X_data_post)

hlpm2 = lm_robust(hhhigh ~ pctBlck + mdn_hs_, data = pctHRsk_by_X_data_post)

hlpm3 = lm_robust(hhhigh ~ pctHisp + mdn_hs_, data = pctHRsk_by_X_data_post)

hlpm4 = lm_robust(hhhigh ~ pctAsia + mdn_hs_, data = pctHRsk_by_X_data_post)

# Prepare regressions for LaTeX
texreg(list(hlpm1, hlpm2, hlpm3, hlpm4), stars=c(0.01, 0.05, 0.1), caption = "Linear Probaility Model")


##### TAV REGRESSION ######

# Finalize regression data and regress

regression_data = pctHRsk_by_X_data_post |>
  mutate(ln_median_household_income = log(mdn_hs_))

regression1 = lm_robust(pctHRsk ~ pctWhit + ln_median_household_income, data = regression_data)

regression2 = lm_robust(pctHRsk ~ pctBlck + ln_median_household_income, data = regression_data)

regression3 = lm_robust(pctHRsk ~ pctHisp + ln_median_household_income, data = regression_data)

regression4 = lm_robust(pctHRsk ~ pctAsia + ln_median_household_income, data = regression_data)

# Prepare regressions for LaTeX

texreg(list(regression1, regression2, regression3, regression4), stars=c(0.01, 0.05, 0.1), caption = "Percentage of Tract Area at High Flood Risk")
