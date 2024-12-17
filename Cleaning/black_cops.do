/*------------------------------------------------------------------------------
  Project: 		Crimes Project 
  Start:		08/15/2024
  
  This file:	Extracts Black Policemen, etc. from Census

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



* Set the data path

* Load the dataset
use "$od/IPUMS Extract - 1860-1900 Census.dta", clear

* Filter data for years greater than 1860 and less than 1900
keep if year > 1860 & year < 1900

* Filter for whites and African Americans and collapse by year, occupation, and race
// preserve
// keep if race == 1 | race == 2
collapse (count) count = hhwt, by(year occ1950 race countynhg)
// restore

gen str_countynhg = string(countynhg, "%12.0f") 

* Concatenate the prefix "G0" with the numeric string
gen gisjoin = cond(strlen(str_countynhg) < 7, "G0" + str_countynhg, "G" + str_countynhg)



* Generate population and race-specific counts
egen n_pop = sum(count), by(gisjoin year)
egen n_race = sum(count), by(gisjoin year race)
egen n_occ = sum(count), by(gisjoin year occ1950)
egen n = sum(count), by(gisjoin year occ1950 race)

bysort gisjoin race  year occ1950 : gen n_by_pop = n / n_race
bysort gisjoin race  year occ1950 : gen n_by_occ = n / n_occ

* Filter and summarize specific occupations
local cops_etc "55 773 782"
// gen is_cop = inlist(occ1950, `cops_etc')
gen is_cop2 = (occ1950 == 55 |  occ1950 == 773 | occ1950 == 782)
bysort gisjoin year is_cop2 : gen n_occ2 = sum(n)

bysort gisjoin year : gen has_black_cop = race == 2
egen has_black_cop2 = max(has_black_cop), by(gisjoin year)

bysort gisjoin year : gen n_black = n_race*(race==2)
egen n_black_pop = max(n_black), by(gisjoin year)
rename n_pop total_pop

keep if  is_cop2 == 1 

egen total_occ = max(n_occ2), by(gisjoin year)

keep if race == 2

rename n  n_black_cops 

collapse (sum) n_black_cops (mean) total_pop n_black_pop (max) total_occ , by(gisjoin year)

gen perc_cops_who_are_black = n_black_cops / total_occ

sort year gisjoin

* Save the final dataset
save "$wd/black_cops_1870_1880.dta", replace



foreach y in 1870 1880{
	preserve 
	
	keep if year == `y'
	
	save "$wd/black_cops_`y'.dta", replace
	
	restore
}




