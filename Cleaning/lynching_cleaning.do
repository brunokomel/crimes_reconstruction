** Crimes Project - Cleaning Lynching 

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

use "$od/lynching_data.dta", clear

// Going from fips to gisjoin
gen str_county_fips = string(county_fips, "%12.0f") 
replace str_county_fips = str_county_fips + "0"

gen county_fips2 = cond(strlen(str_county_fips) < 4, "0" + str_county_fips , str_county_fips )
replace county_fips2 = cond(strlen(county_fips2) < 4, "0" + county_fips2 , county_fips2 )

gen str_state_fips = string(state_fips, "%12.0f") 

gen state_fips2 = cond(strlen(str_state_fips) < 2, "0" + str_state_fips , str_state_fips )

gen gisjoin = "G" + state_fips2 +  "0" + county_fips2 

keep if race == "Black"

gen n_lynch = 1

global southern_fips "01 05 10 12 13 21 22 24 28 37 40 45 47 48 51 54"

* Create a new variable that indicates if state_fips is in the global list
gen is_southern = 0  // Initialize with 0 (not in list)

foreach val in $southern_fips {
	di "`val'"
    replace is_southern = 1 if state_fips2 == "`val'"
}

keep if is_southern == 1

collapse (sum) n_lynch , by(gisjoin)

save "$wd/lynching_by_county.dta", replace
