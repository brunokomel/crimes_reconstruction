library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(spdep)
library(sp)
library(ipumsr)
library(USAboundaries) 
library(haven)
library(sjlabelled)

data <- "/Users/brunokomel/Library/CloudStorage/Dropbox/Reconstruction Crimes Proejct/Data/"

working_data <- paste0(data, "Working Data/")

# Load the data

counties <- read_csv(paste0(working_data,"neighbor_list.csv") ,
                     col_types = cols(fips = col_character()))

counties_st <- us_counties("1880-12-31", states = unique(counties$state_abbr), resolution = "high") %>% 
  filter(!is.na(fips)) ## This is NA in the USAboundaries package
plot(st_geometry(counties_st))
title("U.S. state boundaries on December 31, 1880")

neighb_list <- st_intersects(counties_st, counties_st, sparse = FALSE)

# Make sure geometries are valid
counties_st <- st_make_valid(counties_st)

# If not already an sf object, convert it to one
if (!inherits(counties_st, "sf")) {
  counties_st <- st_as_sf(counties_st)
}

# Use st_intersects with sparse = TRUE to get a list of indices
neighb <- st_intersects(counties_st, counties_st, sparse = TRUE)

########
## Data for Treatment (occupation troops)
########

# open excel file
crosswalk <- readxl::read_excel(paste0(data,"Working Data/ipums_usa_1970_county_group_composition.xls"))

crosswalk <- crosswalk %>% 
  select(-c(starts_with("*"))) %>% 
  mutate(fips_county = paste0(`State FIPS Code (STATEFIP)`, `County FIPS Code* (COUNTYFIP)`)) %>% 
  select(`NHGIS GISJOIN`, fips_county) %>% 
  rename(gisjoin = `NHGIS GISJOIN`)

counties <- left_join(counties, crosswalk, by = c('fips' = 'fips_county'))

troops_1870 <- read_dta(paste0(data,"occupation_troops_1870.dta"))

median_troops_conditional <- troops_1870$cum_troops_1870[troops_1870$cum_troops_1870>0] %>% 
  median(, na.rm = TRUE)

## Defining treated variable

df <- troops_1870 %>% 
  mutate(any_troops_1870 = ifelse(is.na(any_troops_1870), 0, any_troops_1870),
         treated = case_when(cum_troops_1870 > median_troops_conditional ~ 1, # Defining treatment as above median troops
                             TRUE ~ 0))

df_treated <- df %>% 
  select(decade, gisjoin , any_troops_1870, cum_troops_1870, treated) %>% 
  filter(treated == 1)

df_treated <- left_join(df_treated, crosswalk, by = c('gisjoin' = 'gisjoin')) %>% 
  rename(fips = fips_county)

# troops_1870 <- troops_1870 %>% 
#   filter(any_troops_1870 == 1) ## This is our treatment variable

counties <- left_join(counties, df, by = c('gisjoin' = 'gisjoin'))

table(counties$any_troops_1870) ## Losing 7 obeservations

counties_troops <- counties %>% 
  select(fips, any_troops_1870)  %>% 
  filter(any_troops_1870 == 1) ## This is our treatment variable

neighb_cod <- as.list(rep(NA, nrow(counties)))

counties$neighb <- neighb

for(i in 1:nrow(counties)){
  neighb_cod[[i]] <- counties[[5]][unlist(counties['neighb'][[1]][i])] ## This selects the column 5 of the counties dataframe, which corresponds to FIPS code
}

c(unlist(neighb_cod[[1]]))
## Test: ID 11 (Chilton, had a neighbor who was treated)
neighb_cod[[11]] ## The line id for neighbors of Chilton
max(neighb_cod[[11]] %in% counties_troops$fips) # This should be 1

## 48011 (Dallam, TX) did not have neighbors with troops in 1870
neighb_cod[[819]]
max(neighb_cod[[819]] %in% counties_troops$fips) # This should be 0 

counties_geom <- counties_st %>% 
  select(fips, geometry)

counties <- left_join(counties, counties_geom, by = c('fips' = 'fips'), relationship = "one-to-one")


###### Finding neighbors of treated

neigh_treated <- c()

for(i in 1:nrow(counties)){
  neigh_treated[i] <- case_when( max(neighb_cod[[i]] %in% df_treated$fips) == 0 ~ 0, 
                                 TRUE ~ max(neighb_cod[[i]] %in% df_treated$fips))
}

counties_70 <- counties

counties_70['neighbor_treated'] <- neigh_treated

counties_70 <- counties_70 %>% 
  select(fips, neighbor_treated) %>% 
  st_drop_geometry() %>% 
  filter(!is.na(fips))

counties.1 <- left_join(counties, counties_70, by = c('fips' = 'fips')) %>% 
  mutate(treat_or_neigh_treat = case_when(neighbor_treated == 1 ~ 1,
                                          treated == 1 ~ 1,
                                          TRUE ~ 0))


### Plotting all the counties that were treated (had Union troops)
counties %>% 
  filter(#race == 2,
    #occ1950 %in% cops_etc,
    #!is.na(diff),
    #diff != 0
  ) %>%
  mutate(any_troops_1870 = ifelse(is.na(any_troops_1870), 0, any_troops_1870),
         # treated = case_when(cum_troops_1870 > median_troops_conditional ~ 1,
         #                     TRUE ~ 0)
  ) %>% 
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = treated)) +
  #scale_fill_gradient2(low = "navy", mid = "green", high = "red", midpoint = 0, limit = c(min(census_county_full$diff, na.rm = TRUE), max(census_county_full$diff, na.rm = TRUE)), space = "Lab", name = "Difference 1880-1870") +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs( #title = "Change in number of Black Policemen, etc. (1880 - 1870)",
    fill = "Troops" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts



# Plotting all the counties that were treated, or that had a neighbor who was treated

counties.1 %>% 
  filter(#race == 2,
    #occ1950 %in% cops_etc,
    #!is.na(diff),
    #diff != 0
  ) %>%
  # mutate(any_troops_1870 = ifelse(is.na(any_troops_1870), 0, any_troops_1870)) %>% 
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = treat_or_neigh_treat)) +
  #scale_fill_gradient2(low = "navy", mid = "green", high = "red", midpoint = 0, limit = c(min(census_county_full$diff, na.rm = TRUE), max(census_county_full$diff, na.rm = TRUE)), space = "Lab", name = "Difference 1880-1870") +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs( #title = "Change in number of Black Policemen, etc. (1880 - 1870)",
    fill = "Troops" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

counties.1 <- counties.1 %>% 
  st_drop_geometry() %>% 
  # select(-all_of(c("neighb", "geometry"))) %>% 
  select(all_of(c("fips", "gisjoin", names(troops_1870),"treated", "treat_or_neigh_treat")))

filename <-paste0(working_data,'neighbor_troops.csv')
write.csv(counties.1, filename, row.names = FALSE)

neighbors_data <- read_csv(filename)




