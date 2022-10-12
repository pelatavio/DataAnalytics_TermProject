# DataAnalytics_TermProject

Team members: Samantha J. Poteet and Tavio A. Pela

## Topic:
Risk and effects of flooding in the US

## Why it Matters:
Risk of flooding puts communities in danger of numerous short- and long-term consequences.  Entire communities can be uprooted as their homes are destroyed.  Flooding can cause fatalities, property damage, and disease by carrying sewage and toxic runoff, polluting drinking water supplies, and leaving bacteria and mold that contribute to respiratory illnesses in their wake.  Flood risks are expected to continue to increase as global climate change contributes to heavier precipitation, more frequent hurricanes, and higher sea levels.

## Potential Research Questions:
- Where within the US are floods most severe, and what demographics (race, income) are most affected?

- Of the highest-risk zones/populations, which are best prepared to deal with floods (via access to flood insurance, investment in infrastructure, etc.)?

## Description of Datasets

### Race_and_Income

The Race_and_Income datasets contain information about both racial composition and median household income for 2020 at the county, census tract, and block group level.

These datasets were made by combining sections of two tables from the IPUMS National Historical Geographic Information System (IPUMS NHGIS, University of Minnesota, www.nhgis.org).  We make use of Table B03002 and Table B19013.  Both use data from the 2020 American Community Survey available at the county, census tract, and block group level.

Table B03002 displays estimates of population demographics categorized via Hispanic or Latino origin by race (example categories include Not Hispanic or Latino: White alone, Not Hispanic or Latino: Two or more races, Hispanic or Latino: White alone, etc.).

Table B19013 displays median household income in the past 12 months (relative to the 2020 survey period) in 2020 inflation-adjusted dollars.

### NFHL

Access to the National Flood Hazard Layer (NFHL) is provided by the Federal Emergency Management Agency (FEMA) (FEMA Map Service Center - National Flood Hazard Layer (NFHL) Database Search). In this dataset there are 32 layers, some include: 
flood hazard zones and labels,
flood insurance rate map, 
hydraulic and flood control structures,
coastal barrier resource systems and otherwise protected areas,
and Limit of Moderate Wave Action (LiMWA). 

For our purposes we are concerned with the zoning layers and the flood control structures. The data set is the most current and updated in 2022. This dataset spatially covers over 90 percent of the United States.

### How are they Related?

The NFHL observations contain identifying codes that can be used to merge it with the Race_and_Income data by census tract so that we can investigate flood hazard zone demographics.
