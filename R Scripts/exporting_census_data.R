## Export data for stata analysis

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
# 
# IPUMS_Extract_1860_1880_Census <- read_csv(paste0(data,"IPUMS Extract - 1860-1880 Census.csv"))
# 
# census_file <- paste0(data,"IPUMS Extract - 1860-1880 Census.csv")
# ddi_file <- paste0(data,"IPUMS Extract - 1860-1880 Census.xml")
# ddi <- read_ipums_ddi(ddi_file, lower_vars =TRUE)
# census <-read_ipums_micro(ddi, data_file = census_file, verbose = FALSE)

census <-  read_dta(file=paste0(data,"IPUMS Extract - 1860-1900 Census.dta"))

census <- census %>% 
  filter(year > 1860)

IPUMS_Extract_1860_1880_Census <- IPUMS_Extract_1860_1880_Census %>% 
  filter(YEAR > 1860)

ipums_data <- IPUMS_Extract_1860_1880_Census

rm(IPUMS_Extract_1860_1880_Census)

names(ipums_data) <- tolower(names(ipums_data))

ipums_data_occ <- ipums_data %>% 
  filter(race == 1 | race == 2, 
         year > 1860) %>%  ## Selecting only whites and african americans
  group_by(year, occ1950,race) %>%  ## collapsing by year, occupation, and race
  summarise(n = n()) %>% 
  ungroup()


census <- census %>% 
  mutate(state = haven::as_factor(statefip),
         race_lab = haven::as_factor(race),
         occ_lab = haven::as_factor(occ1950)) %>% 
  mutate(state = droplevels(state),
         race_lab = droplevels(race_lab),
         occ_lab = droplevels(occ_lab))

counties_1880 <- us_counties("1880-12-31", states = levels(census$state), resolution = "high") 
plot(st_geometry(counties_1880))
title("U.S. state boundaries on December 31, 1880")


counties_1870 <- us_counties("1870-12-31", states = levels(census$state), resolution = "high") 
plot(st_geometry(counties_1870))
title("U.S. state boundaries on December 31, 1870")

census <- census %>% 
  mutate(county_code = as_character(substr(countyicp, 1 , nchar(countyicp) - 1)),
         county_str = sprintf("%03s", county_code),
         state_str = sprintf("%02d", statefip),
         fips = paste0(state_str,county_str )
  )

counties_1880 <- counties_1880 %>% 
  mutate(year = 1880) 

counties_1870 <- counties_1870 %>%
  mutate(year = 1870)

counties_70_80 <- rbind(counties_1870, counties_1880)

census <- left_join(census , counties_70_80, by = c("fips" = "fips", "year" = "year"), relationship = "many-to-one")

census %>% group_by(year) %>% summarize(na = sum(is.na(geometry)))

# census2 <- left_join(census , counties_1880, by = c("fips" = "fips"), relationship = "many-to-one") ## This version ignores the 1870 county boundaries
# 
# census2 %>% group_by(year) %>% summarize(na = sum(is.na(geometry)))

census_county1 <- census %>% 
  group_by(fips,year) %>% 
  mutate(n_pop = n()) %>% 
  ungroup() %>% 
  group_by(race, fips,year) %>% ## this is going to create variables for each race group in each county in each year
  mutate(n_race = n()) %>% 
  ungroup() %>% 
  group_by(occ1950, fips,year) %>% 
  mutate(n_occ = n()) %>% 
  ungroup() %>% 
  group_by(fips,race,race_lab,year,occ1950, occ_lab) %>% 
  summarize(n = n(),
            n_race = max(n_race),
            n_occ = max(n_occ),
            n_pop = max(n_pop)) %>% 
  ungroup() %>% 
  mutate(n_by_pop = n/n_race,
         n_by_occ = n/n_occ,
         share_race = n_race/n_pop)  %>% 
  mutate(share_black = case_when(race == 2 ~ share_race,
                                 TRUE ~ 0)) %>% 
  group_by(fips,year) %>% 
  mutate(share_black = max(share_black)) %>% 
  ungroup()

cops_etc <- c("55", "773", "782")

census_county2 <- census_county1 %>% 
  filter(occ1950 %in% cops_etc) %>%
  group_by(year, fips) %>% 
  mutate(n_occ = sum(n)) %>%
  filter(race == 2) %>% 
  ungroup() %>% 
  group_by(year, fips) %>% 
  summarize(n = sum(n),
            n_occ = sum(n_occ),
            n_race = mean(n_race),
            n_pop = mean(n_pop),
  ) %>% 
  mutate(perc_cops = n/n_occ,
         perc_black = n_race/n_pop,
         ind_cops = 1)


### To export
census_county_moransi <- census %>% 
  group_by(fips,year, geometry) %>% 
  summarize(share_black = mean(race == 2)) 

census_county_moransi <- left_join(census_county_moransi, census_county2, by = c("fips" = "fips", "year" = "year"), relationship = "one-to-one")

census_county_moransi <- census_county_moransi %>% 
  mutate(n = case_when(is.na(n) ~ 0,
                       TRUE ~ n),
         n_occ = case_when(is.na(n_occ) ~ 0,
                           TRUE ~ n_occ),
         n_race = case_when(is.na(n_race) ~ 0,
                            TRUE ~ n_race),
         n_pop = case_when(is.na(n_pop) ~ 0,
                           TRUE ~ n_pop),
         perc_cops = case_when(is.na(perc_cops) ~ 0,
                               TRUE ~ perc_cops),
         perc_black = case_when(is.na(perc_black) ~ 0,
                                TRUE ~ perc_black),
         ind_cops = case_when(is.na(ind_cops) ~ 0,
                              TRUE ~ ind_cops))

# centroids_70 <- st_centroid(census_county_moransi$geometry)

census_county_moransi$centroids <- st_transform(census_county_moransi$geometry, 29101) %>% 
  st_centroid() %>% 
  # this is the crs from d, which has no EPSG code:
  st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
  # since you want the centroids in a second geometry col:
  st_geometry()

plot(st_geometry(census_county_moransi$geometry))
plot(census_county_moransi[, 'centroids'], add = T, col = 'red', pch = 19)

# Extract latitude and longitude from census_county_moransi$centroids
census_county_moransi$lon <- st_coordinates(census_county_moransi$centroids)[, 1]
census_county_moransi$lat <- st_coordinates(census_county_moransi$centroids)[, 2]

#drop geometry and centroids
census_county_moransi_final <- census_county_moransi %>% select(-c("geometry", "centroids"))


# save census_county_moransi as a csv file
path <- "/Users/brunokomel/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Crimes Against Blacks - Reconstruction Project/Data/Working Data/"
filename <- paste0(path,"census_county_moransi.csv")
write.csv(census_county_moransi_final, filename, row.names = FALSE)


