** Crimes Project - Preliminary Spatial Analysis

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

global wd "$path/Data/Working Data"

// Converting files to .dta
import delimited "$wd/census_share_black.csv", clear

save "$wd/census_share_black.dta", replace

import delimited "$wd/neighbor_troops.csv", clear

drop if gisjoin == "NA"

save "$wd/neighbor_troops.dta", replace


/// Merging the files
use "$wd/census_share_black.dta", clear

merge m:1 gisjoin using "$wd/neighbor_troops.dta"

// have to deal with the 37 lost counties

// Running regressions
keep if _merge == 3
drop _merge

gen post = (year == 1880)

gen treat = 0
replace treat = 1 if treated == "1"

gen interaction = post*treat

gen interaction_neigh = post*treat_or_neigh_treat

reghdfe percentile interaction,  abs(gisjoin year)

reghdfe percentile interaction_neigh,  abs(gisjoin year)
