/*------------------------------------------------------------------------------
  Project: 		Crimes Project 
  Start:		08/15/2024
  
  This file:	Runs preliminary percentile rank regressions

  Sub-do's:		
				

------------------------------------------------------------------------------*/
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


use "$wd/census_cops_troop_lynch_vote.dta", clear

// merge m:1 gisjoin using "$wd/neighbor_troops.dta"

// // have to deal with the 37 lost counties
//
// // Running regressions
// keep if _merge == 3
// drop _merge

// Generating indicators
gen cops1870 = (year == 1870 & n_black_cops >0)

egen ind_cops1870 = max(cops1870), by(gisjoin)

gen cops1880 = (year == 1880 & n_black_cops >0)

egen ind_cops1880 = max(cops1880), by(gisjoin)

gen troops1870 = (year == 1870 & any_troops ==1)

egen ind_troops1870 = max(troops1870), by(gisjoin)

gen troops1880 = (year == 1880 & any_troops ==1)

egen ind_troops1880 = max(troops1880), by(gisjoin)

replace n_lynch = 0 if n_lynch == .

gen post = (year == 1880)

//
//
// gen treat = 0
// replace treat = 1 if treated == "1"
//
// gen interaction = post*treat
//
// gen interaction_neigh = post*treat_or_neigh_treat
//
// reghdfe percentile interaction,  abs(gisjoin year)
//
// reghdfe percentile interaction_neigh,  abs(gisjoin year)

******************
*   Cops, etc.   *
******************

reghdfe pctle_share_black ind_cops1870##post,  abs(gisjoin year)

******************
*  Union Troops  *
******************

reghdfe pctle_share_black ind_troops1870##post,  abs(gisjoin year)

reghdfe pctle_share_black ind_troops1870##ind_cops1870##post,  abs(gisjoin year) // Places that had union troops and black cops in 1870 attracted blacks

sum pctle_share_black, detail

gen dummy = (pctle_share_black > .89)

gen dummy_2 = (pctle_share_black < .1)

reghdfe  dummy ind_troops1870##post,  abs(gisjoin year)

reghdfe  dummy_2 ind_troops1870##post,  abs(gisjoin year)

reghdfe pctle_share_black ind_troops1870##ind_cops1880##post,  abs(gisjoin year) // But it doesn't seem that blakcs necessarily went to places where there were black cops in 1880

gen cum_troops_1870 = cum_troops 
replace cum_troops_1870 = 0 if year == 1880

egen cum_troops_1870_2 = max(cum_troops_1870), by(gisjoin)

gen log_troops = log(cum_troops_1870_2 + 1)

reghdfe pctle_share_black c.log_troops##post,  abs(gisjoin year) // More troops, bigger decrease in share blacks

qui sum cum_troops_1870_2 , detail
gen above_mean_troops = cum_troops_1870_2 >= `r(mean)'

reghdfe pctle_share_black above_mean_troops##post,  abs(gisjoin year) // same thing here

**************************
*  Democrat Vote Shares  *
**************************

capture drop high_dem
gen high_dem = 0
local election_year =  "dem_vote_share1868"
local cutoff = 80
replace high_dem = 1 if `election_year' >= `cutoff'

reghdfe pctle_share_black high_dem##post ,  abs(gisjoin year)

capture drop high_dem
gen high_dem = 0
local election_year =  "dem_vote_share1876"
local cutoff = 80
replace high_dem = 1 if `election_year' >= `cutoff'

reghdfe pctle_share_black high_dem##post ,  abs(gisjoin year)


