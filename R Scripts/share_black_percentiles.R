library(readr)
library(dplyr)
library(ggplot2)
library(ipumsr)
library(sjlabelled)
library(USAboundaries) 
library(sf)
library(lwgeom)
library(tidyverse)
library(knitr)
library(kableExtra)
library(haven)

data <- "/Users/brunokomel/Library/CloudStorage/Dropbox/Reconstruction Crimes Proejct/Data/Original Data/"

IPUMS_Extract_1860_1880_Census <- read_csv(paste0(data,"IPUMS Extract - 1860-1880 Census.csv"))

census_file <- paste0(data,"IPUMS Extract - 1860-1880 Census.csv")
ddi_file <- paste0(data,"IPUMS Extract - 1860-1880 Census.xml")
ddi <- read_ipums_ddi(ddi_file, lower_vars =TRUE)
census <-read_ipums_micro(ddi, data_file = census_file, verbose = FALSE)

# census <-  read_dta(file=paste0(data,"IPUMS Extract - 1860-1900 Census.dta"))

census <- census %>% 
  filter(year > 1860)

IPUMS_Extract_1860_1880_Census <- IPUMS_Extract_1860_1880_Census %>% 
  filter(YEAR > 1860)

rm(IPUMS_Extract_1860_1880_Census)

census <- census %>% 
  mutate(state = haven::as_factor(statefip),
         race_lab = haven::as_factor(race),
         occ_lab = haven::as_factor(occ1950)) %>% 
  mutate(state = droplevels(state),
         race_lab = droplevels(race_lab),
         occ_lab = droplevels(occ_lab))

census <- census %>% 
  mutate(county_code = as_character(substr(countyicp, 1 , nchar(countyicp) - 1)),
         county_str = sprintf("%03s", county_code),
         state_str = sprintf("%02d", statefip),
         fips = paste0(state_str,county_str ))
  
census_share_black <- census %>% 
  filter(race == 1 | race == 2) %>%  ## Selecting only whites and african americans
  group_by(year, fips ) %>%  ## collapsing by year, county code 
  summarize(share_black = sum(race == 2)/n()) 

# create a variable with each fips code's percentile of share black for each year
census_share_black <- census_share_black %>% 
  group_by(year) %>% 
  mutate(percentile = rank(share_black)/(n()+1))

## Merging crosswalk
crosswalk <- readxl::read_excel(paste0(data,"Working Data/ipums_usa_1970_county_group_composition.xls"))

crosswalk <- crosswalk %>% 
  select(-c(starts_with("*"))) %>% 
  mutate(fips_county = paste0(`State FIPS Code (STATEFIP)`, `County FIPS Code* (COUNTYFIP)`)) %>% 
  select(`NHGIS GISJOIN`, fips_county) %>% 
  rename(gisjoin = `NHGIS GISJOIN`)

census_share_black <- left_join(census_share_black, crosswalk, by = c('fips' = 'fips_county'))


write_csv(census_share_black, paste0(data,"Working Data/census_share_black.csv"))
