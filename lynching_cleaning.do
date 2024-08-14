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

collapse (sum) n_lynch , by(gisjoin)

save "$wd/lynching_by_county.dta", replace
