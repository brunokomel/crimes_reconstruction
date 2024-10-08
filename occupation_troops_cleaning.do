/*------------------------------------------------------------------------------
  Project: 		Crimes Project 
  Start:		08/15/2024
  
  This file:	Cleans Union Soldiers files

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

foreach y in 1870 1880 {
	use "$od/occupation_troops_`y'.dta", clear
	
	drop decade placeid places_of_occupation*
	
	rename cum_troops_`y' cum_troops
	rename any_troops_`y' any_troops
	
	save "$wd/occupation_troops_`y'.dta", replace
}

use "$od/occupation_troops_pre1870.dta", clear

drop year

save "$wd/occupation_troops_pre1870.dta", replace
