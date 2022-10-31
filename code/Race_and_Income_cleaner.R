# Purpose: Clean Race and Income data for use in Data Analytics Project

## setwd("C:/Users/pelat/OneDrive/Documents/Graduate School (MSU)/Data Analytics")
dir("data")


library(tidyverse)
library(readxl)
library(skimr)

# Clean blockgroup data -----------------------------------------------------------------

Race_and_Income_blockgrp_raw = read_csv("data/Race_and_Income_blockgrp_raw.csv")


no_useless_columns1 = Race_and_Income_blockgrp_raw |>
  select(GISJOIN:STUSAB, STATE:COUNTYA, TRACTA:BLCK_GRPA, GEOID, NAME_E:AMR8M001)

colnames(no_useless_columns1)[12:20] = c("totPop", "nonhisp_total", "nonhisp_white", "nonhisp_black", "nonhisp_native", "nonhisp_asian", "nonhisp_islander", "nonhisp_other", "nonhisp_multiracial")
colnames(no_useless_columns1)[23:30] = c("hisp_total", "hisp_white", "hisp_black", "hisp_native", "hisp_asian", "hisp_islander", "hisp_other", "hisp_multiracial")
colnames(no_useless_columns1)[33] = "median_household_income"

skim(no_useless_columns1)

## WARNING: There are missing values in our median_household_income variable

write_csv(no_useless_columns1, "Race_and_Income_blockgrp")


# Clean tract data -----------------------------------------------------------------


Race_and_Income_tract_raw = read_csv("data/Race_and_Income_tract_raw.csv")

no_useless_columns2 = Race_and_Income_tract_raw |>
  select(GISJOIN:STUSAB, STATE:COUNTYA, TRACTA:BLCK_GRPA, GEOID, NAME_E:AMR8M001)

colnames(no_useless_columns2)[12:20] = c("totPop", "nonhisp_total", "nonhisp_white", "nonhisp_black", "nonhisp_native", "nonhisp_asian", "nonhisp_islander", "nonhisp_other", "nonhisp_multiracial")
colnames(no_useless_columns2)[23:30] = c("hisp_total", "hisp_white", "hisp_black", "hisp_native", "hisp_asian", "hisp_islander", "hisp_other", "hisp_multiracial")
colnames(no_useless_columns2)[33] = "median_household_income"

skim(no_useless_columns2)

## WARNING: There are missing values in our median_household_income variable

write_csv(no_useless_columns2, "Race_and_Income_tract")


# Clean county data -----------------------------------------------------------------

Race_and_Income_county_raw = read_csv("data/Race_and_Income_county_raw.csv")

no_useless_columns3 = Race_and_Income_county_raw |>
  select(GISJOIN:STUSAB, STATE:COUNTYA, TRACTA:BLCK_GRPA, GEOID, NAME_E:AMR8M001)

colnames(no_useless_columns3)[12:20] = c("totPop", "nonhisp_total", "nonhisp_white", "nonhisp_black", "nonhisp_native", "nonhisp_asian", "nonhisp_islander", "nonhisp_other", "nonhisp_multiracial")
colnames(no_useless_columns3)[23:30] = c("hisp_total", "hisp_white", "hisp_black", "hisp_native", "hisp_asian", "hisp_islander", "hisp_other", "hisp_multiracial")
colnames(no_useless_columns3)[33] = "median_household_income"

skim(no_useless_columns3)

## WARNING: There is one missing value in our median_household_income variable

write_csv(no_useless_columns3, "Race_and_Income_county")

# Look at NA results to figure out what data we're missing ----------------------------------------

Race_and_Income_blockgrp = read_csv("Race_and_Income_blockgrp")
Race_and_Income_tract = read_csv("Race_and_Income_tract")
Race_and_Income_county = read_csv("Race_and_Income_county")

checkingNAs_blockgrp = Race_and_Income_blockgrp |>
  filter(is.na(median_household_income)==TRUE)

checkingNAs_tract = Race_and_Income_tract |>
  filter(is.na(median_household_income)==TRUE)

checkingNAs_county = Race_and_Income_county |>
  filter(is.na(median_household_income)==TRUE)

##### Check county-level data #####

checkingNAs_county |>
  count(STATE)
# Result: we're missing one county from Texas, but nothing else


##### Check tract-level data #####

checkingNAs_tract |>
  count(STATE) |>
  arrange(n)
# Result: we're missing the least data from Vermont, West VA, and Wyoming

## We're interested in looking at Florida, so we'll check how much tract-level data we're missing for it
checkingNAs_tract |>
  count(STATE) |>
  filter(STATE=="Florida")
## Florida is missing 127 tracts.

# Check how significant that result is by looking at how many tracts we have
Race_and_Income_tract |>
  count(STATE) |>
  filter(STATE=="Florida")

## So we're missing 127 out of 5160, or about 2.46%, of the tracts.


##### Check blockgroup-level data #####

checkingNAs_blockgrp |>
  count(STATE)|>
  arrange(n)
# Result: we're missing the least data from Alaska and South Dakota


## We're interested in looking at Florida, so we'll check how much blockgroup data we're missing for it
checkingNAs_blockgrp |>
  count(STATE) |>
  filter(STATE=="Florida")
## Florida is missing 927 blockgroups

# Check how significant that result is by looking at how many blockgroups we have
Race_and_Income_blockgrp |>
  count(STATE) |>
  filter(STATE=="Florida")

## So we're missing 927 out of 13388, or about 6.92%, of the blockgroups

# Isolate tract-level Florida Data and save ------------------------------------------------

Florida_Race_and_Income_tract = Race_and_Income_tract = read_csv("Race_and_Income_tract") |>
  select(GISJOIN:median_household_income) |>
  filter(STATE=="Florida")

write_csv(Florida_Race_and_Income_tract, "Florida_Race_and_Income_tract.csv")



