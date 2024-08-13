** Crimes Project - Share Black Percentiles

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

// * Convert statefip, race, and occ1950 to labeled variables
// encode statefip, gen(state)
// encode race, gen(race_lab)
// encode occ1950, gen(occ_lab)

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
keep if race == 1 | race == 2
collapse (sum) num_black = black (count) count_var = race , by (year gisjoin)
gen share_black = num_black/count_var


* Calculate the percentile for share_black by year
bysort year: egen share_rank = rank(share_black)
bysort year: gen n_obs = _N
bysort year: gen pctle_share_black = share_rank/(n_obs + 1)

keep year gisjoin pctle_share_black

foreach y in 1870 1880 {

preserve

 keep if year == `y'

 rename gisjoin gisjoin_`y'

 merge 1:1 gisjoin_`y' using "$od/County Crosswalks/crosswalks/County-CD-centroid-lat-lon/lat_lon_coordinates_county_stata/Counties_`y'_xy.dta"

 keep if _merge == 3
 drop _merge decade
 
* Save the final dataset
save "$wd/census_share_black_`y'.dta", replace


restore
}


* Save the final dataset
save "$wd/census_share_black.dta", replace
