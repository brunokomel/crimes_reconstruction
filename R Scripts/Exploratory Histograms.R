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

#################
# Histograms
#################

cops_etc <- c("55", "773", "782")

ipums_data %>% 
  filter(#year == 1870,
    #occ1950 >= 0 & occ1950 < 100 ,
    race == 1 | race == 2 , ## Selecting only whites and african americans
    #race == 2,
    occ1950 %in% cops_etc #c("055", "773", "782")
  ) %>% 
  ggplot(aes(x = as.factor(occ1950), fill = as.factor(year) ) ) + # occ1950 on the x-axis, fill by race
  geom_histogram( stat = "count", position = "dodge", alpha = 0.5, binwidth = 1) + # Adjust alpha for transparency
  scale_fill_brewer(palette = "Set1") + # Optional: Use a color palette for differentiation
  labs(title = "Distribution of Occupations by Race", x = "Occupation Code (occ1950)", y = "Count") + 
  theme_minimal() + # Use a minimal theme for a cleaner look
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts
  facet_wrap(~ statefip + race , scales = "fixed")

census <- census %>% 
  mutate(state = haven::as_factor(statefip),
         race_lab = haven::as_factor(race),
         occ_lab = haven::as_factor(occ1950)) %>% 
  mutate(state = droplevels(state),
         race_lab = droplevels(race_lab),
         occ_lab = droplevels(occ_lab))

### STATES
census %>% 
  filter(#year == 1870,
    #occ1950 >= 0 & occ1950 < 100 ,
    race == 1 | race == 2 , ## Selecting only whites and african americans
    #race == 2,
    occ1950 %in% cops_etc
  ) %>% 
  ggplot(aes(x = as.factor(occ_lab), fill = as.factor(year) ) ) + # occ1950 on the x-axis, fill by race
  geom_histogram( stat = "count", position = "dodge", alpha = 0.5, binwidth = 1) + # Adjust alpha for transparency
  scale_fill_brewer(palette = "Set1") + # Optional: Use a color palette for differentiation
  labs(title = "Distribution of Occupations by Race", x = "Occupation Code (occ1950)", y = "Count") + 
  theme_minimal() + # Use a minimal theme for a cleaner look
  facet_wrap( state ~ race_lab  , scales = "free_y") +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12), # Bold and bigger legend texts
        plot.margin = margin(1, 1, 1, 1, "cm")) 

results_figures <- "/Users/brunokomel/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Crimes Against Blacks - Reconstruction Project/Ultra Preliminary Results/"
ggsave(paste0(results_figures, "states_south_histogram.pdf"), dpi = "retina",  width = 40, height = 10,units = "in" , device = "pdf") 


#### SOUTH
census %>% 
  filter(#year == 1870,
    #occ1950 >= 0 & occ1950 < 100 ,
    race == 1 | race == 2 , ## Selecting only whites and african americans
    race == 2,
    occ1950 %in% cops_etc
  ) %>% 
  ggplot(aes(x = as.factor(occ_lab), fill = as.factor(year) ) ) + # occ1950 on the x-axis, fill by race
  geom_histogram( stat = "count", position = "dodge", alpha = 0.5, binwidth = 1) + # Adjust alpha for transparency
  #geom_text(stat = 'count', aes(label = as.factor(occ_lab)), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  scale_fill_brewer(palette = "Set1") + # Optional: Use a color palette for differentiation
  labs(title = "Distribution of Occupations by Race", x = "Occupation Code (occ1950)", y = "Count", fill = "Census Year") + 
  theme_minimal() + # Use a minimal theme for a cleaner look
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) + # Bold and bigger legend texts +
  facet_wrap(~ race_lab,  scales = "free_y")

ggsave(paste0(results_figures, "south_histogram.pdf"), dpi = "retina",  width = 21, height = 7,units = "in" , device = "pdf") 


####################
### Tables
####################


tab_70 <- census %>% 
  filter(year == 1870,
         occ1950 %in% cops_etc,
         race == 2 | race == 1) %>%
  group_by(year, race_lab, occ_lab) %>% 
  summarize(n = n()) %>% 
  ungroup()

tab_70 <- tab_70 %>% 
  rename(n70 = n)


tab_80 <- census %>% 
  filter(year == 1880,
         occ1950 %in% cops_etc,
         race == 2 | race == 1) %>%
  group_by(year, race_lab, occ_lab) %>% 
  summarize(n = n()) %>% 
  ungroup()

tab_80 <- tab_80 %>% select(n) %>% rename(n80 = n)

#tab_80 <- tab_80[,3]

tab_comb <- bind_cols(tab_70, tab_80) 

tab_comb <- tab_comb %>% 
  select(-c("year"))


# Assuming `my_tibble` is your tibble
kable(tab_comb, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

####################
### Maps
####################



## Plotting map:
counties_1880 <- us_counties("1880-12-31", states = levels(census$state), resolution = "high") 
plot(st_geometry(counties_1880))
title("U.S. state boundaries on December 31, 1880")


## calculating the number of people of each race in each occupation for each county

census <- census %>% 
  mutate(county_code = as_character(substr(countyicp, 1 , nchar(countyicp) - 1)),
         county_str = sprintf("%03s", county_code),
         state_str = sprintf("%02d", statefip),
         fips = paste0(state_str,county_str )
  )


census <- left_join(census , counties_1880, by = c("fips" = "fips"), relationship = "many-to-one")

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

census_state <- census %>% 
  group_by(race, state,year) %>% ## this is going to create variables for each race group in each state in each year
  mutate(n_race = n()) %>% 
  ungroup() %>%  
  group_by(occ1950, state,year) %>% 
  mutate(n_occ = n()) %>% 
  ungroup() %>% 
  group_by(state,race,race_lab, year,occ1950, occ_lab) %>% 
  summarize(n = n(),
            n_race = max(n_race),
            n_occ = max(n_occ)) %>% 
  ungroup() %>% 
  mutate(n_by_pop = n/n_race,
         n_by_occ = n/n_occ)

census_south <- census %>% 
  group_by(race, year) %>%   ## this is going to create variables for each race group for the entire south in each year
  mutate(n_race = n()) %>% 
  ungroup() %>%  
  group_by(occ1950, year) %>% 
  mutate(n_occ = n()) %>% 
  ungroup() %>% 
  group_by(race, race_lab,year,occ1950, occ_lab) %>% 
  summarize(n = n(),
            n_race = max(n_race),
            n_occ = max(n_occ)) %>% 
  ungroup() %>% 
  mutate(n_by_pop = n/n_race,
         n_by_occ = n/n_occ)

census_south_cops <- census_south %>% 
  filter(occ1950 %in% cops_etc) 


census_south %>% 
  filter(occ1950 %in% cops_etc, 
         race == 2
  )  %>%
  ggplot(aes(x = occ_lab, y = n_by_pop, fill = as.factor(year))) +
  geom_bar(stat = "identity", fun = "mean", position = "dodge") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) + # Bold and bigger legend texts
  labs(title = "Bar Plot by Category",
       x = "Category",
       y = "Density") 



census_south %>% 
  filter(occ1950 %in% cops_etc) %>% 
  group_by(occ_lab, race, year) %>% 
  summarise(n = mean(n),
            n_race = mean(n_race),
            n_by_pop = mean(n_by_pop))

#### Considering the % of black and white cops etc.

census_state %>% 
  filter(occ1950 %in% cops_etc, 
         race == 1 | race == 2)  %>%
  ggplot(aes(x = occ_lab, y = n_by_pop, fill = interaction(as.factor(year), race_lab) )  ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) + # Bold and bigger legend texts
  labs(title = "Bar Plot by Category",
       x = "Category",
       y = "Density",
       fill = "Census Year x Race") +
  facet_wrap(~ state + race_lab , scales = "fixed") +
  scale_fill_manual(
    values = c('#33CC33','#006633','steelblue2','steelblue4'),
    #labels = c('Female All Employees','Female New Hires','Male All Employees','Male New Hires')
  )

ggsave(paste0(results_figures, "states_south_histogram_perc_pop.pdf"), dpi = "retina",  width = 40, height = 10,units = "in" , device = "pdf") 


census_state %>% 
  filter(occ1950 %in% cops_etc, 
         race == 1 | race == 2)  %>%
  ggplot(aes(x = occ_lab, y = n_by_pop, fill = interaction(as.factor(year), race_lab) )  ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) + # Bold and bigger legend texts
  labs(title = "Bar Plot by Category",
       x = "Category",
       y = "Density",
       fill = "Census Year x Race") +
  facet_wrap(~ state + race_lab , scales = "free") +
  scale_fill_manual(
    values = c('#33CC33','#006633','steelblue2','steelblue4'),
    #labels = c('Female All Employees','Female New Hires','Male All Employees','Male New Hires')
  )

ggsave(paste0(results_figures, "states_south_histogram_perc_pop_free.pdf"), dpi = "retina",  width = 40, height = 10,units = "in" , device = "pdf") 

#### Considering the % of black and white cops etc.

census_state %>% 
  filter(occ1950 %in% cops_etc, 
         race == 1 | race == 2)  %>%
  ggplot(aes(x = occ_lab, y = n_by_occ, fill = interaction(as.factor(year), race_lab) )  ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) + # Bold and bigger legend texts
  labs(title = "Bar Plot by Category",
       x = "Category",
       y = "Density",
       fill = "Census Year x Race") +
  facet_wrap(~ state + race_lab , scales = "fixed") +
  scale_fill_manual(
    values = c('#33CC33','#006633','steelblue2','steelblue4'),
    #labels = c('Female All Employees','Female New Hires','Male All Employees','Male New Hires')
  )

ggsave(paste0(results_figures, "states_south_histogram_perc_occ.pdf"), dpi = "retina",  width = 40, height = 10,units = "in" , device = "pdf") 


census_state %>% 
  filter(occ1950 %in% cops_etc, 
         race == 1 | race == 2)  %>%
  ggplot(aes(x = occ_lab, y = n_by_occ, fill = interaction(as.factor(year), race_lab) )  ) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) + # Bold and bigger legend texts
  labs(title = "Bar Plot by Category",
       x = "Category",
       y = "Density",
       fill = "Census Year x Race") +
  facet_wrap(~ state + race_lab , scales = "free") +
  scale_fill_manual(
    values = c('#33CC33','#006633','steelblue2','steelblue4'),
    #labels = c('Female All Employees','Female New Hires','Male All Employees','Male New Hires')
  )

ggsave(paste0(results_figures, "states_south_histogram_perc_occ_free.pdf"), dpi = "retina",  width = 40, height = 10,units = "in" , device = "pdf") 





## Now merging the data to take the difference

census_county_1870 <- census_county1 %>% filter(year == 1870)
census_county_1880 <- census_county1 %>% filter(year == 1880)


census_county <- left_join(census_county_1880, census_county_1870, by = c( "fips" = "fips", "race" = "race", "occ1950" = "occ1950")) ## preliminary left_join

census_county_full <- full_join(census_county_1880, census_county_1870, by = c( "fips" = "fips", "race" = "race", "occ1950" = "occ1950")) ## this will bring in the observations that have values in 1870 without a match from 1880 as well

census_county <- census_county %>% 
  mutate(n.x = case_when(is.na(n.x) ~ 0, 
                         TRUE ~ n.x),
         n.y = case_when(is.na(n.y) ~ 0, 
                         TRUE ~ n.y)
  ) %>% 
  mutate(diff = n.x - n.y) %>% 
  filter(race == 2,
         occ1950 %in% cops_etc) %>% 
  group_by(fips) %>% 
  summarise(diff = sum(diff, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(diff_cat = case_when(diff < 0 ~ "<0",
                              diff == 0 ~ "=0",
                              diff > 0 ~ ">0"))

census_county_full <- census_county_full %>% 
  mutate(n.x = case_when(is.na(n.x) ~ 0, 
                         TRUE ~ n.x),
         n.y = case_when(is.na(n.y) ~ 0, 
                         TRUE ~ n.y)
  ) %>% 
  mutate(diff = n.x - n.y) %>% 
  filter(race == 2,
         occ1950 %in% cops_etc) %>% 
  group_by(fips) %>% 
  summarise(diff = sum(diff, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(diff_cat = case_when(diff < 0 ~ "<0",
                              diff == 0 ~ "=0",
                              diff > 0 ~ ">0"))

census_county <- left_join(census_county , counties_1880, by = c("fips" = "fips"), relationship = "many-to-one")

census_county_full <- left_join(census_county_full , counties_1880, by = c("fips" = "fips"), relationship = "many-to-one")

## now we plot

census_county %>% 
  # filter(race == 2,
  #        occ1950 %in% cops_etc,
  #        !is.na(diff)
  #        ) %>% 
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties_1880,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = diff)) +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts


### plotting differences

census_county_full %>% 
  filter(#race == 2,
    #occ1950 %in% cops_etc,
    #!is.na(diff),
    #diff != 0
  ) %>%
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties_1880,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = diff)) +
  #scale_fill_gradient2(low = "navy", mid = "green", high = "red", midpoint = 0, limit = c(min(census_county_full$diff, na.rm = TRUE), max(census_county_full$diff, na.rm = TRUE)), space = "Lab", name = "Difference 1880-1870") +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs(title = "Change in number of Black Policemen, etc. (1880 - 1870)",
       fill = "Difference 1880-1870" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "south_map.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 

census_county_full %>% 
  filter(#race == 2,
    #occ1950 %in% cops_etc,
    #!is.na(diff),
    #diff != 0
  ) %>%
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties_1880,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = diff_cat)) +
  #scale_fill_gradient2(low = "navy", mid = "green", high = "red", midpoint = 0, limit = c(min(census_county_full$diff, na.rm = TRUE), max(census_county_full$diff, na.rm = TRUE)), space = "Lab", name = "Difference 1880-1870") +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs(title = "Change in number of Black Policemen, etc. (1880 - 1870)",
       fill = "Difference 1880-1870" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "south_map_diff_cat.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 

### Excluding counties where there was no change
census_county_full %>% 
  filter(#race == 2,
    #occ1950 %in% cops_etc,
    !is.na(diff),
    diff != 0
  ) %>%
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties_1880,  color = "#B4B6B9", linewidth = 0.15) +
  #geom_sf(data = filter(counties_1880, id_num == '15427'), color = "red", size = 0.5, show.legend = FALSE) +
  geom_sf(aes(fill = diff)) +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs(title = "Change in number of Black Policemen etc. (1880 - 1870, non-zero)",
       fill = "Difference 1880-1870" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "south_map_neq_0.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 


### Now plotting maps of each census year

census_county_1880 <- left_join(census_county_1880 , counties_1880, by = c("fips" = "fips"), relationship = "many-to-one")

census_county_1880 %>% 
  filter(race == 2,
         occ1950 %in% cops_etc,
         #!is.na(diff),
         #diff != 0
  ) %>%
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties_1880,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = n_by_pop)) +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs(title = "Number of Black Policemen, etc. (1880)",
       fill = "N. 1880" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "map_1880.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 


census_county_1880 %>% 
  filter(race == 2,
         occ1950 %in% cops_etc,
         #!is.na(diff),
         #diff != 0
  ) %>% 
  mutate(n_ind = case_when(n > 0 ~ 1, 
                           TRUE ~ 0)
  ) %>%
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties_1880,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = n_ind)) +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs(title = "Number of Black Policemen, etc. (1880)",
       fill = "N. 1880" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "map_1880_ind.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 



census_county_1870 <- left_join(census_county_1870 , counties_1880, by = c("fips" = "fips"), relationship = "many-to-one")

census_county_1870 %>% 
  filter(race == 2,
         occ1950 %in% cops_etc,
         #!is.na(diff),
         #diff != 0
  ) %>% 
  mutate(n_ind = case_when(n > 0 ~ 1, 
                           TRUE ~ 0)
  ) %>%
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties_1880,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = n)) +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs(title = "Number of Black Policemen, etc. (1870)",
       fill = "N. 1870" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "map_1870.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 



census_county_1870 %>% 
  filter(race == 2,
         occ1950 %in% cops_etc,
         #!is.na(diff),
         #diff != 0
  ) %>% 
  mutate(n_ind = case_when(n > 0 ~ 1, 
                           TRUE ~ 0)
  ) %>%
  ggplot() + 
  aes(geometry = geometry) +
  geom_sf(data = counties_1880,  color = "#B4B6B9", linewidth = 0.15) +
  geom_sf(aes(fill = n_ind)) +
  #geom_sf(data= state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15, show.legend = FALSE) +
  #geom_sf(data = state_capitals_data, fill = "#0099FF", color = "#B4B6B9", linewidth = 0.15) +
  #labs(subtitle = "Municipalities", size = 5)+
  labs(title = "Number of Black Policemen, etc. (1870)",
       fill = "N. 1870" ) + 
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "map_1870_ind.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 

#####################
# Lynching analysis #
#####################

lynching_data_orig <- read_dta("/Users/brunokomel/Library/CloudStorage/OneDrive-UniversityofPittsburgh/Crimes Against Blacks - Reconstruction Project/Data/lynching_data.dta")



lynching_data_orig %>% 
  filter(state %in% unique(census$state_abbr),
         race == "Black"
         ) %>% 
  group_by(full_fips) %>% 
  summarize(n = n())  ## So there should be 597 lynchings

lynching_data <- lynching_data_orig %>% 
  filter(race == "Black") %>% 
  mutate(full_fips = as.character(full_fips)) %>% 
  group_by(full_fips) %>% 
  summarise(n_lynch = n()) %>% 
  ungroup()

#census_county <- left_join(census_county_1880, census_county_1870, by = c( "fips" = "fips", "race" = "race", "occ1950" = "occ1950")) ## preliminary left_join

# census_county_full <- full_join(census_county_1880, census_county_1870, by = c( "fips" = "fips", "race" = "race", "occ1950" = "occ1950")) ## this will bring in the observations that have values in 1870 without a match from 1880 as well

census_county_1880_police_etc <- census_county_1880 %>% 
  filter(occ1950 %in% cops_etc) %>% 
  group_by(fips) %>% 
  mutate(n_occ = sum(n)) %>% 
  ungroup() %>% 
  group_by(fips, race) %>% 
  summarize(n_pol_etc = sum(n),
            n_occ = mean(n_occ),
            perc = n_pol_etc/n_occ,
            share_race = max(share_race),
            share_black = max(share_black)) %>% 
  ungroup() %>% 
  filter(race == 1) %>% 
  mutate(perc_black_pol_etc =  1- perc)

# census_county_1880_police_etc <- census_county_1880 %>% 
#   filter(occ1950 %in% cops_etc) %>% 
#   group_by(fips) %>% 
#   mutate(n_occ = sum(n)) %>% 
#   ungroup() %>% 
#   group_by(fips, race) %>% 
#   summarize(n_pol_etc = sum(n),
#             n_occ = mean(n_occ),
#             perc = n_pol_etc/n_occ,
#             share_race = max(share_race),
#             share_black = max(share_black)) %>% 
#   ungroup() %>% 
#   filter(race == 1) %>% 
#   mutate(perc_black_pol_etc =  1- perc)


census_county_police_etc_lynch <- left_join(census_county_1880_police_etc , lynching_data, by = c("fips" = "full_fips"), relationship = "one-to-one")

sum(census_county_police_etc_lynch$n_lynch, na.rm = TRUE) ## So I only have 551 in this final data. I'm losing about 46 lynchings
## The 551 figure is irrelevant now that I filtered black only


census_county_1870_police_etc <- census_county_1870 %>% 
  filter(occ1950 %in% cops_etc) %>% 
  group_by(fips) %>% 
  mutate(n_occ = sum(n)) %>% 
  ungroup() %>% 
  group_by(fips, race) %>% 
  summarize(n_pol_etc = sum(n),
            n_occ = mean(n_occ),
            perc = n_pol_etc/n_occ,
            share_race = max(share_race),
            share_black = max(share_black)) %>% 
  ungroup() %>% 
  filter(race == 1) %>% 
  mutate(perc_black_pol_etc =  1- perc)

# census_county_1880_police_etc <- census_county_1880 %>% 
#   filter(occ1950 %in% cops_etc) %>% 
#   group_by(fips) %>% 
#   mutate(n_occ = sum(n)) %>% 
#   ungroup() %>% 
#   group_by(fips, race) %>% 
#   summarize(n_pol_etc = sum(n),
#             n_occ = mean(n_occ),
#             perc = n_pol_etc/n_occ,
#             share_race = max(share_race),
#             share_black = max(share_black)) %>% 
#   ungroup() %>% 
#   filter(race == 1) %>% 
#   mutate(perc_black_pol_etc =  1- perc)


census_county_police_etc_lynch_1870 <- left_join(census_county_1870_police_etc , lynching_data, by = c("fips" = "full_fips"), relationship = "one-to-one")

sum(census_county_police_etc_lynch_1870$n_lynch, na.rm = TRUE) ## So I only have 551 in this final data. I'm losing about 46 lynchings


##########################
########################## NEED TO FIGURE OUT WHAT IS HAPPENING WITH THE 46 LYNCHINGS (13 of them come from the weird fips in the lynching data)
##########################

census_county_police_etc_lynch %>% 
  filter(!is.na(n_lynch),
         perc_black_pol_etc > 0) %>% 
  summarize(n = n())
  


census_county_police_etc_lynch %>% 
  filter(!is.na(n_lynch)) %>% 
  ggplot(aes(x = perc_black_pol_etc, y = n_lynch )) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Scatter Plot",
       x = "% Policemen, etc. who are Black in 1880",
       y = "Number of Lynchings") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "scatter_lynch.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 



census_county_police_etc_lynch %>% 
  filter(perc_black_pol_etc > 0,
    !is.na(n_lynch)) %>% 
  ggplot(aes(x = perc_black_pol_etc, y = n_lynch )) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Scatter Plot",
       x = "% Policemen, etc. who are Black in 1880",
       y = "Number of Lynchings (1883-1936)") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "scatter_lynch_neq_0.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 



census_county_police_etc_lynch_1870 %>% 
  filter(!is.na(n_lynch),
         #perc_black_pol_etc < 0.3,
         ) %>% 
  ggplot(aes(x = perc_black_pol_etc, y = n_lynch )) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Scatter Plot",
       x = "% Policemen, etc. who are Black in 1870",
       y = "Number of Lynchings") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts
 
ggsave(paste0(results_figures, "scatter_lynch_1870.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 



census_county_police_etc_lynch_1870 %>% 
  filter(perc_black_pol_etc > 0, 
         #perc_black_pol_etc < 0.3,
         !is.na(n_lynch)) %>% 
  ggplot(aes(x = perc_black_pol_etc, y = n_lynch )) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Scatter Plot",
       x = "% Policemen, etc. who are Black in 1870",
       y = "Number of Lynchings (1883-1936)") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "scatter_lynch_neq_0_1870.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 


census_county_police_etc_lynch_1870 %>% 
  filter(perc_black_pol_etc > 0, 
         perc_black_pol_etc < 0.3,
         !is.na(n_lynch)) %>% 
  ggplot(aes(x = perc_black_pol_etc, y = n_lynch )) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Scatter Plot",
       x = "% Policemen, etc. who are Black in 1870",
       y = "Number of Lynchings (1883-1936)") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "scatter_lynch_neq_0_1870_no_outliers.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 



#### Share of black population

census_county_police_etc_lynch %>% 
  filter(#perc_black_pol_etc > 0,
         !is.na(n_lynch),
         ) %>% 
  ggplot(aes(x = share_black, y = n_lynch )) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Scatter Plot",
       x = "Share Black population 1880",
       y = "Number of Lynchings (1883-1936)") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts


ggsave(paste0(results_figures, "scatter_lynch_share_black.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 


## remove outlier
census_county_police_etc_lynch %>% 
  filter(#perc_black_pol_etc > 0,
    !is.na(n_lynch),
    n_lynch < 16
  ) %>% 
  ggplot(aes(x = share_black, y = n_lynch )) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") + 
  labs(title = "Scatter Plot",
       x = "Share Black population 1880",
       y = "Number of Lynchings (1883-1936)") +
  theme_minimal() +
  theme(axis.title = element_text(face = "bold", size = 14), # Bold and bigger axis titles
        axis.text = element_text(face = "bold", size = 12), # Bold and bigger axis text
        plot.title = element_text(face = "bold", size = 16), # Bold and bigger plot title
        legend.title = element_text(face = "bold", size = 12), # Bold and bigger legend titles
        legend.text = element_text(face = "bold", size = 12)) # Bold and bigger legend texts

ggsave(paste0(results_figures, "scatter_lynch_share_black_no_outlier.pdf"), dpi = "retina",  width = 12, height = 9,units = "in" , device = "pdf") 

