/*------------------------------------------------------------------------------
  Project: 		Crimes Project 
  Start:		08/15/2024
  
  This file:	Cleans Democratic Vote Share Data

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

// Cleanup state icpsr to fips
import delimited "$od/icpsrcnt.txt", clear 

collapse (first) state, by(stateicp statefips)

gen str_stateicp = string(stateicp, "%12.0f") 

gen str_statefips = string(statefips, "%12.0f") 
			 
gen stateicp2 = cond(strlen(str_stateicp) < 2, "0" + str_stateicp,  str_stateicp)
gen statefips2 = cond(strlen(str_statefips) < 2, "0" + str_statefips,  str_statefips)

keep *2
rename stateicp2 stateicp
rename statefips2 statefips

save "$wd/state_icp_to_fips.dta", replace

use "$od/Election Data (ICPSR)/DS0001/08611-0001-Data.dta", clear

// Cleanup from the authors:
do "$path/Do Files + R Scripts/08611-0001-Supplemental_syntax.do"

drop if V3 == 9999 | V3 == . // Dropping non-counties

drop V690-V759

* Loop over variables, generating rename commands
foreach var of varlist _all {
    local label : variable label `var'
    * Create a new variable name from the label
    local newname = substr(`"`label'"', 1, 17)
    local newname = subinstr(`"`newname'"', " ", "", .)
    local newname = subinstr(`"`newname'"', ".", "", .)
	local newname = subinstr(`"`newname'"', "/", "_", .)
	local newname = subinstr(`"`newname'"', "-", "_", .)

    * Initialize local macro for numeric prefix
    local numeric_prefix ""
    local rest_name "`newname'"

    * Check first four characters
    forvalues i = 1/4 {
        if inrange(substr("`rest_name'", 1, 1), "0", "9") {
            local numeric_prefix = "`numeric_prefix'" + substr("`rest_name'", 1, 1)
            local rest_name = substr("`rest_name'", 2, .)
        }
    }

    * Concatenate rest of the name with the numeric prefix at the end
    if "`numeric_prefix'" != "" {
        local newname = "`rest_name'" + "`numeric_prefix'"
    }

    * Ensure the new variable name is valid and not empty
    if "`newname'" != "" & "`newname'" != "`var'" {
        display "Renaming `var' to `newname'"
        rename `var' `newname'
    }

}

foreach var of varlist _all {
    local lowername = lower("`var'")
    rename `var' `lowername'
}

keep icpsrstatecode countyidentifica countyname pres* 

// Selecting the relevant years

foreach y in 60 64 68 72 76 80 84 88 92 {
    * Construct variable names
    local presdem_var = "presdem18`y'"
    local presdem_eq_var = "presdem_eq18`y'"
    local dem_vote_share_var = "dem_vote_share18`y'"

    * Check if presdem_eq18`y' exists
    capture describe `presdem_eq_var'
    if _rc == 0 {
        * If presdem_eq18`y' exists, sum presdem18`y' + presdem_eq18`y'
        gen `dem_vote_share_var' = `presdem_var' + `presdem_eq_var'
    }
    else {
        * If presdem_eq18`y' does not exist, use only presdem18`y'
        gen `dem_vote_share_var' = `presdem_var'
    }
}

gen str_icpsrstatecode = string(icpsrstatecode, "%12.0f") 

gen str_countyid = string(countyidentifica, "%12.0f") 
			 
gen icpsrst = cond(strlen(str_icpsrstatecode) < 2, "0" + str_icpsrstatecode,  str_icpsrstatecode)
gen icpsrcty = cond(strlen(str_countyid) < 4, "0" + str_countyid,  str_countyid)
replace icpsrcty = cond(strlen(icpsrcty) < 4, "0" + icpsrcty,  icpsrcty)

gen stateicp = cond(strlen(icpsrst) < 2, "0" + icpsrst,  icpsrst)


drop str*

gen icpsrcode = icpsrst + icpsrcty

keep countyidentifica countyname dem_vote_share18* icpsr* stateicp


// Fixing missing values for counties that did not have voting data/did not exist at the time
foreach var of varlist dem_vote_share* {
	replace `var' = . if `var' > 100
}


 merge m:1 stateicp using "$wd/state_icp_to_fips.dta"
 
 drop if _merge == 2
 drop _merge
 
 gen gisjoin = "G" + statefips + "0" + icpsrcty
 
 drop icpsrst icpsrcty stateicp icpsrcode statefips
 
 // Keeping only Southern States:
 keep if icpsrstatecode == 41 | icpsrstatecode == 42 | icpsrstatecode == 11 | icpsrstatecode == 43 | icpsrstatecode == 44 | icpsrstatecode == 51  | icpsrstatecode == 45 | icpsrstatecode == 52 | icpsrstatecode == 46 | icpsrstatecode == 47 | icpsrstatecode == 53 | icpsrstatecode == 48 | icpsrstatecode == 54 | icpsrstatecode == 49 | icpsrstatecode == 40 | icpsrstatecode == 56
 
 gen all_na_vote_share = (dem_vote_share1860 == . & dem_vote_share1864 == . & dem_vote_share1868 == . & dem_vote_share1872 == . & dem_vote_share1876 == . & dem_vote_share1880 == . & dem_vote_share1884 == . & dem_vote_share1888 == . & dem_vote_share1892 == .)
 
//  drop if all_na_vote_share == 1
//  drop all_na_vote_share
 
* Save the final dataset
save "$wd/dem_vote_share.dta", replace

