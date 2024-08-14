** Crimes Project - Cleaning Union Soldiers

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

foreach y in 1870 1880 {
	use "$od/occupation_troops_`y'.dta", clear
	
	drop decade placeid places_of_occupation*
	
	save "$wd/occupation_troops_`y'.dta", replace
}

use "$od/occupation_troops_pre1870.dta", clear

drop year

save "$wd/occupation_troops_pre1870.dta", replace
