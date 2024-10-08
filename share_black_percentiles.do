/*------------------------------------------------------------------------------
  Project: 		Crimes Project 
  Start:		08/15/2024
  
  This file:	Extracts share black for each from Census and 
				ranks counties by percentiles

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

use "$od/IPUMS Extract - 1860-1900 Census.dta", clear

* Filter the data for years greater than 1860 and less than 1900
keep if year > 1860 & year < 1900

* Remove empty levels
drop if missing(statefip)
drop if missing(race)
drop if missing(occ1950)

* Create county_code, county_str, state_str, and fips
// // gen county_code = substr(string(countyicp, "%06.0f"), 1, length(string(countyicp, "%06.0f")) - 1)
// gen county_str = string(countyicp, "%04.0f")
// gen state_str = string(statefip, "%02.0f")
// // gen fips = state_str + county_str
// gen countynhg_str =  state_str + "0" + county_str

gen str_countynhg = string(countynhg, "%12.0f") 

* Concatenate the prefix "G0" with the numeric string
gen gisjoin = cond(strlen(str_countynhg) < 7, "G0" + str_countynhg, "G" + str_countynhg)


* Filter and summarize the data by race and fips
// label list RACE
gen black = (race == 2)
// keep if race == 1 | race == 2
collapse (sum) num_black = black (count) total_pop = race , by (year gisjoin)
gen share_black = num_black/total_pop


* Calculate the percentile for share_black by year
bysort year: egen share_rank = rank(share_black)
bysort year: gen n_obs = _N
bysort year: gen pctle_share_black = share_rank/(n_obs + 1)

keep year gisjoin pctle_share_black share_black num_black total_pop

foreach y in 1870 1880 {

preserve

 keep if year == `y'

 rename gisjoin gisjoin_`y'

 merge 1:1 gisjoin_`y' using "$od/County Crosswalks/crosswalks/County-CD-centroid-lat-lon/lat_lon_coordinates_county_stata/Counties_`y'_xy.dta"

 keep if _merge == 3
 drop _merge decade
 
 rename gisjoin_`y' gisjoin
 
* Save the final dataset
save "$wd/census_share_black_`y'.dta", replace


restore
}


* Save the final dataset
save "$wd/census_share_black.dta", replace

// calculating black growth by county
use  "$wd/census_share_black_1870.dta", clear

foreach var in  num_black total_pop share_black pctle_share_black {
	rename `var' `var'_1870
}

merge 1:1 gisjoin using   "$wd/census_share_black_1880.dta"

foreach var in  num_black total_pop share_black pctle_share_black{
	rename `var' `var'_1880
}

drop _merge

gen change_black_pop = num_black_1880 - num_black_1870
gen pct_change_black_pop = change_black_pop/num_black_1870
gen change_share_black = share_black_1880 - share_black_1870
gen change_pop = total_pop_1880 - total_pop_1870
gen change_black_pop_by_change_pop = change_black_pop/change_pop

label var pct_change_black_pop "Percent change in black pop (black_pop_1880 - black_pop_1870)/black_pop_1870"
label var change_share_black   "Change in share black pop (share_black_1880 - share_black_1870)"
label var change_black_pop_by_change "Change in black pop divided by change in pop"


save "$wd/changes_in_black_pop.dta", replace
