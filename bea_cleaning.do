/*------------------------------------------------------------------------------
  Project: 		Crimes Project 
  Start:		08/15/2024
  
  This file:	Cleans Personal Income Data

  Sub-do's:		
				

------------------------------------------------------------------------------*/

// Got BEA data from https://apps.bea.gov/itable/?ReqID=70&step=1&_gl=1*1oytem7*_ga*MTk4MDk1NDI3Ny4xNzE1NTUyMzEy*_ga_J4698JNNFT*MTcyNDI2NDY2OS4zLjEuMTcyNDI2NzA4My4zNi4wLjA.#eyJhcHBpZCI6NzAsInN0ZXBzIjpbMSwyOSwyNSwzMSwyNiwyNywzMF0sImRhdGEiOltbIlRhYmxlSWQiLCIyMCJdLFsiTWFqb3JfQXJlYSIsIjQiXSxbIlN0YXRlIixbIlhYIl1dLFsiQXJlYSIsWyJYWCJdXSxbIlN0YXRpc3RpYyIsWyIxIl1dLFsiVW5pdF9vZl9tZWFzdXJlIiwiTGV2ZWxzIl0sWyJZZWFyIixbIjIwMDAiXV0sWyJZZWFyQmVnaW4iLCItMSJdLFsiWWVhcl9FbmQiLCItMSJdXX0=

clear all
set more off


* automatically set working directory
#delimit ;
foreach i in "C:/Users/Economics/Desktop/test" 
			 "/Users/brunokomel/Library/CloudStorage/Dropbox/Reconstruction Crimes Proejct" 
			 "C:/Users/anf137/Dropbox/projects/Reconstruction Crimes Project/" {;
			 global path "`i'";
			 confirmdir "$path";
			 if `r(confirmdir)'==0 continue, break;
};
#delimit cr

global data "$path/Data"

global od "$path/Data/Original Data"

global wd "$path/Data/Working Data"

global git "$path/Do Files + R Scripts"

import delimited "$od/BEA/personal_income_per_capita_2000.csv", clear

rename v1 fips
recast str fips

rename v2 county_name
rename v3 personal_income_pc_2000

drop if _n < 5 // Removing the header
drop if _n >= 3141

save "$wd/personal_income_per_capita_2000.dta", replace

// Now population for 2020

import delimited "$od/BEA/population_2020.csv", clear

rename v1 fips
recast str fips

rename v2 county_name
rename v3 population_2020

drop if _n < 5 // Removing the header
drop if _n >= 3144

save "$wd/population_2020.dta", replace

// Now real GDP (2020)

import delimited "$od/BEA/real_gdp_2020.csv", clear

rename v1 fips
recast str fips

rename v2 county_name
rename v3 real_gdp_2020

drop if _n < 5 // Removing the header
drop if _n >= 3119

save "$wd/real_gdp_2020.dta", replace



merge 1:1 fips using "$wd/personal_income_per_capita_2000.dta"

drop _merge

merge 1:1 fips using "$wd/population_2020.dta"

format %5s fips
drop _merge 



replace personal_income_pc_2000 = "" if personal_income_pc_2000 == "(NA)"
destring personal_income_pc_2000, replace
replace population_2020 = "" if population_2020 == "(NA)"
destring population_2020, replace
replace real_gdp_2020 = "" if real_gdp_2020 ==  "(NA)"
destring real_gdp_2020, replace

gen real_gdp_pc_2020 = real_gdp_2020*1000 / population_2020

keep fips real_gdp_pc_2020 personal_income_pc_2000

gen state_fips = substr(fips, 1, 2)
gen county_fips = substr(fips, 3, 5)

gen gisjoin = "G" + state_fips + "0" + county_fips + "0"

save "$wd/bea_data.dta", replace
