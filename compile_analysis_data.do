** Crimes Project - Bringing all the data together

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

foreach y in 1870 1880 {
	
use "$wd/census_share_black_`y'.dta", clear

rename gisjoin_`y' gisjoin

* Bringing in cops data

merge 1:1 gisjoin using "$wd/black_cops_`y'.dta"

drop _merge 

* Replacing missing values with 0's
foreach var in n_black_cops total_pop n_black_pop total_occ perc_cops_who_are_black {
	replace `var' = 0 if `var' == .
}


* Bringing in Union troops data

merge 1:1 gisjoin using "$wd/occupation_troops_`y'.dta"

global southern_fips "01 05 10 12 13 21 22 24 28 37 40 45 47 48 51 54"

gen state_fips = substr(gisjoin, 2, 2)

* Create a new variable that indicates if state_fips is in the global list
gen is_southern = 0  // Initialize with 0 (not in list)

foreach val in $southern_fips {
	di "`val'"
    replace is_southern = 1 if state_fips == "`val'"
}

keep if is_southern == 1

drop if _merge == 2 // only 17 counties, all in Texas
drop _merge

* bringing in lynching data
merge 1:1 gisjoin using "$wd/lynching_by_county.dta"

summarize n_lynch 
 di `r(sum)'
 
// There are 53 counties in 1870 and 39 in 1880 in the lynching data that don't exist in the census
drop if _merge == 2
drop _merge 
 
* bringing in dem_vote_share data

merge 1:1 gisjoin using "$wd/dem_vote_share.dta"

** This is where it's weird. There are 470 counties in 1870, and 353 in 1880, for which I have dem_vote_share data but which are not in the census
drop if _merge == 2
drop _merge 
 
save "$wd/census_cops_troop_lynch_vote_`y'.dta", replace
}
