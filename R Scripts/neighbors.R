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

IPUMS_Extract_1860_1880_Census <- read_csv(paste0(data,"IPUMS Extract - 1860-1880 Census.csv"))

census_file <- paste0(data,"IPUMS Extract - 1860-1880 Census.csv")
ddi_file <- paste0(data,"IPUMS Extract - 1860-1880 Census.xml")
ddi <- read_ipums_ddi(ddi_file, lower_vars =TRUE)
census <-read_ipums_micro(ddi, data_file = census_file, verbose = FALSE)

census <- census %>%
  filter(year > 1860)

IPUMS_Extract_1860_1880_Census <- IPUMS_Extract_1860_1880_Census %>%
  filter(YEAR > 1860)

ipums_data <- IPUMS_Extract_1860_1880_Census

rm(IPUMS_Extract_1860_1880_Census)

names(ipums_data) <- tolower(names(ipums_data))


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
         fips = paste0(state_str,county_str )
  )

sf_use_s2(FALSE)

counties <- us_counties("1880-12-31", states = levels(census$state), resolution = "high") %>% 
  filter(!is.na(fips)) ## Removes NA in the USAboundaries package
plot(st_geometry(counties))
title("U.S. state boundaries on December 31, 1880")

drop(census)

neighb_list <- st_intersects(counties, counties, sparse = FALSE)

# Make sure geometries are valid
counties <- st_make_valid(counties)

# If not already an sf object, convert it to one
if (!inherits(counties, "sf")) {
  counties <- st_as_sf(counties)
}

# Use st_intersects with sparse = TRUE to get a list of indices
neighb <- st_intersects(counties, counties, sparse = TRUE)

# Convert the sparse list to a data frame of neighbor IDs for each municipality
# Excluding the municipality itself from its list of neighbors
neighb_df <- lapply(seq_len(length(neighb)), function(i) {
  setdiff(neighb[[i]], i)
})

# Bind the neighbor IDs as a new column
counties$neighb <- neighb_df

# Print the first few rows of the updated counties data frame with the neighb column
print(head(counties))


unlist(counties['neighb'][[1]][1])

counties[[1]][51]

neighb_cod <- as.list(rep(NA, nrow(counties)))

for(i in 1:nrow(counties)){
  neighb_cod[[i]] <- counties[[5]][unlist(counties['neighb'][[1]][i])] ## This selects the column 5 of the counties dataframe, which corresponds to FIPS code
}

c(unlist(neighb_cod[[1]]))

## Creating dummies for each treated county's neighbors
counties$neighb_cod <- neighb_cod

counties2 <- counties
st_geometry(counties2) <- NULL

for(n in  counties$fips){
  
  for(i in 1:nrow(counties2)){
    
    var_name <- paste("neigh", n, sep = "_" )
    
    counties2[[var_name]][i] <- case_when(counties2$fips[i] %in% counties2$fips[unlist(counties2['neighb'][[1]][counties2$fips == n])] ~ TRUE,
                                      counties2$fips[i] == n  ~ TRUE,  
                                      TRUE ~ FALSE) 
  }
  print(which(n == counties$fips))
}

counties3 <- counties2 %>% 
  select(-c("neighb", "neighb_cod"))

filename <-paste(data,'Working Data/neighbor_list.csv',sep = '/')
write.csv(counties3,filename,row.names = FALSE)

# Show the updated dataframe
counties3.1 <- counties3 %>% 
  select(fips, neigh_01001,neigh_01001, neigh_01047) #, neigh_01047_only)

counties3.2 <- counties3 %>% 
  select(fips, starts_with("neigh_01")) %>% 
  filter(fips == "01047") ## ideally, this is only == 1 for 01047



# 
# ########
# ## Data for occupation_troops
# ########
# 
# # open excel file
# crosswalk <- readxl::read_excel(paste0(data,"Working Data/ipums_usa_1970_county_group_composition.xls"))
# 
# crosswalk <- crosswalk %>% 
#   select(-c(starts_with("*"))) %>% 
#   mutate(fips_county = paste0(`State FIPS Code (STATEFIP)`, `County FIPS Code* (COUNTYFIP)`)) %>% 
#   select(`NHGIS GISJOIN`, fips_county) %>% 
#   rename(gisjoin = `NHGIS GISJOIN`)
# 
# counties3 <- left_join(counties3, crosswalk, by = c('fips' = 'fips_county'))
# 
# troops_1870 <- read_dta(paste0(data,"occupation_troops_1870.dta"))
# 
# troops_1870 <- troops_1870 %>% 
#   filter(any_troops_1870 == 1)
# 
# counties3 <- left_join(counties3, troops_1870, by = c('gisjoin' = 'gisjoin'))
# 
# table(counties$any_troops_1870) ## Losing 7 obeservations
# 
# counties_troops <- counties3 %>% 
#   select(fips, any_troops_1870)  %>% 
#   filter(any_troops_1870 == 1)
# 
# ## Test: ID 11 (Chilton, had a neighbor who was treated)
# neighb_cod[[11]] ## The line id for neighbors of Chilton
# max(neighb_cod[[11]] %in% counties_troops$fips) # This should be 1
# 
# ## 48011 (Dallam, TX) did not have neighbors with troops in 1870
# neighb_cod[[870]]
# max(neighb_cod[[870]] %in% counties_troops$fips) # This should be 0 
# 
# ### Plotting all the counties that were treated (had Union troops)
# counties %>% 
#   filter(#race == 2,
#     #occ1950 %in% cops_etc,
#     #!is.na(diff),
#     #diff != 0
#   ) %>%
#   ggplot() + 
#   aes(geometry = geometry) +
#   geom_sf(data = counties,  color = "#B4B6B9", linewidth = 0.15) +
#   geom_sf(aes(fill = any_troops_1870)) +
#   #scale_fill_gradient2(low = "navy", mid = "green", high = "red", midpoint = 0, limit = c(min(census_county_full$diff, na.rm = TRUE), max(census_county_full$diff, na.rm = TRUE)), space = "Lab", name = "Difference 1880-1870") +
#   #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
#   #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
#   #labs(subtitle = "Municipalities", size = 5)+
#   labs( #title = "Change in number of Black Policemen, etc. (1880 - 1870)",
#        fill = "Troops" ) + 
#   theme_minimal() +
#   theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
#         axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
#         plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
#         legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
#         legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts
# 
# ######  Neighbor Treated
# 
# treated <- troops_1870 %>% 
#   select(decade, gisjoin , any_troops_1870)
# 
# treated <- left_join(treated, crosswalk, by = c('gisjoin' = 'gisjoin')) %>% 
#   rename(fips = fips_county)
# 
# neigh_treated <- c()
# 
# for(i in 1:nrow(counties)){
#   neigh_treated[i] <- case_when( max(neighb_cod[[i]] %in% treated$fips) < 0 ~ 0, 
#                                        TRUE ~ max(neighb_cod[[i]] %in% treated$fips))
# }
# 
# counties_70 <- counties
# 
# counties_70['neighbor_treated'] <- neigh_treated
# 
# counties_70 <- counties_70 %>% 
#   select(fips, neighbor_treated) %>% 
#   st_drop_geometry() %>% 
#   filter(!is.na(fips))
# 
# counties.1 <- left_join(counties, counties_70, by = c('fips' = 'fips')) %>% 
#   mutate(treat_or_neigh_treat = case_when(neighbor_treated == 1 ~ 1,
#                              any_troops_1870 == 1 ~ 1,
#                              TRUE ~ 0))
# 
# # Plotting all the counties that were treated, or that had a neighbor who was treated
# 
# counties.1 %>% 
#   filter(#race == 2,
#     #occ1950 %in% cops_etc,
#     #!is.na(diff),
#     #diff != 0
#   ) %>%
#   ggplot() + 
#   aes(geometry = geometry) +
#   geom_sf(data = counties,  color = "#B4B6B9", linewidth = 0.15) +
#   geom_sf(aes(fill = treat_or_neigh_treat)) +
#   #scale_fill_gradient2(low = "navy", mid = "green", high = "red", midpoint = 0, limit = c(min(census_county_full$diff, na.rm = TRUE), max(census_county_full$diff, na.rm = TRUE)), space = "Lab", name = "Difference 1880-1870") +
#   #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
#   #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
#   #labs(subtitle = "Municipalities", size = 5)+
#   labs( #title = "Change in number of Black Policemen, etc. (1880 - 1870)",
#     fill = "Troops" ) + 
#   theme_minimal() +
#   theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
#         axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
#         plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
#         legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
#         legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts
# 
# counties.1 <- counties.1 %>% 
#   st_drop_geometry() %>% 
#   select(-c(neighb, neighb_cod))
# 
# filename <-paste0(data,'neighbor_troops.csv')
# write.csv(counties.1, filename, row.names = FALSE)
# 
# neighbors_data <- read.csv(filename)
# 
# 
# 
# 
