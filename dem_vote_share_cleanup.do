** Crimes Project - Cleaning Democratic Vote Share Data

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
 
* Save the final dataset
save "$wd/dem_vote_share.dta", replace


use "$od/County Crosswalks/crosswalks/CountyToCounty/1860/1860_stata/Identifiers_1860.dta", clear

drop if icpsrcty == .

gen str_icpsrstatecode = string(icpsrst, "%12.0f") 

gen str_countyid = string(icpsrcty, "%12.0f") 
			 
gen icpsrst2 = cond(strlen(str_icpsrstatecode) < 2, "0" + str_icpsrstatecode,  str_icpsrstatecode)
gen icpsrcty2 = cond(strlen(str_countyid) < 4, "0" + str_countyid,  str_countyid)
replace icpsrcty2 = cond(strlen(icpsrcty2) < 4, "0" + icpsrcty2,  icpsrcty2)

drop str*

gen icpsrcode = icpsrst2 + icpsrcty2


* Bringing in ICPSR to GISJOIN

* Sort the dataset if necessary
sort icpsrcode

* Create the new variable
gen same_as_above = (icpsrcode== icpsrcode[_n-1])

* removing icpsrcodes that match to multiple giscodes 

drop if gisjoin == "G2001350" & icpsrcode == "321370"
drop if gisjoin == "G1301570" & icpsrcode == "441510"

keep icpsrcode gisjoin

save "$wd/icpsr_to_gisjoin_1860.dta", replace




use "$wd/dem_vote_share.dta", clear

gen str_icpsrst = string(icpsrst, "%12.0f") 
gen stateicp = cond(strlen(str_icpsrst) < 2, "0" + str_icpsrst,  str_icpsrst)

 merge m:1 stateicp using "$wd/state_icp_to_fips.dta"
 
 drop if _merge == 2
 drop _merge



keep icpsrstatecode countyname countyidentifica icpsrst icpsrcty icpsrcode dem_vote_share186*

gen all_na = (dem_vote_share1860 == . & dem_vote_share1864== . & dem_vote_share1868 == .)

drop if all_na == 1
dorp all_na

// drop if dem_vote_share1860 == .

 merge 1:1 icpsrcode using "$wd/icpsr_to_gisjoin_1860.dta"
 
 drop if _merge == 2
