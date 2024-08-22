
/*------------------------------------------------------------------------------
  Project: 		Crimes Project 
  Start:		08/15/2024
  
  This file:	Runs analysis on long run GDP outcomes

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



********************************
*                              *
*         GDP Analysis         *
*                              *
********************************

use "$wd/changes_in_black_pop.dta", clear 

merge 1:1 gisjoin using "$wd/bea_data.dta"

keep if _merge == 3
drop _merge

foreach var in personal_income_pc_2000 real_gdp_pc_2020{
	gen ln_`var' = ln(`var')
}

rename change_black_pop_by_change_pop change_black_pop_by_change

foreach var in pct_change_black_pop change_share_black change_black_pop_by_change{
	qui sum `var', detail
	gen high_`var'= (`var' >= `r(p75)')
	replace high_`var' = . if `var' == .
	gen low_`var'= (`var' <= `r(p25)')
	replace low_`var' = . if `var' == .
}




// Percent change in black pop (black_pop_1880 - black_pop_1870)/black_pop_1870 on the RHS

reghdfe personal_income_pc_2000 pct_change_black_pop
reghdfe personal_income_pc_2000 pct_change_black_pop, abs(state_fips)

reghdfe ln_personal_income_pc_2000 pct_change_black_pop
reghdfe ln_personal_income_pc_2000 pct_change_black_pop, abs(state_fips)

reghdfe real_gdp_pc_2020 pct_change_black_pop
reghdfe real_gdp_pc_2020 pct_change_black_pop, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 pct_change_black_pop
reghdfe ln_real_gdp_pc_2020 pct_change_black_pop, abs(state_fips)


// Change in share black pop (share_black_1880 - share_black_1870) on the RHS

reghdfe personal_income_pc_2000 change_share_black
reghdfe personal_income_pc_2000 change_share_black, abs(state_fips)

reghdfe ln_personal_income_pc_2000 change_share_black
reghdfe ln_personal_income_pc_2000 change_share_black, abs(state_fips)

reghdfe real_gdp_pc_2020 change_share_black
reghdfe real_gdp_pc_2020 change_share_black, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 change_share_black
reghdfe ln_real_gdp_pc_2020 change_share_black, abs(state_fips)

// Change in black pop divided by change in pop on the RHS

reghdfe personal_income_pc_2000 change_black_pop_by_change
reghdfe personal_income_pc_2000 change_black_pop_by_change, abs(state_fips)

reghdfe ln_personal_income_pc_2000 change_black_pop_by_change
reghdfe ln_personal_income_pc_2000 change_black_pop_by_change, abs(state_fips)

reghdfe real_gdp_pc_2020 change_black_pop_by_change
reghdfe real_gdp_pc_2020 change_black_pop_by_change, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 change_black_pop_by_change
reghdfe ln_real_gdp_pc_2020 change_black_pop_by_change, abs(state_fips)

**************************
* High change indicators *
**************************

// Percent change in black pop
reghdfe personal_income_pc_2000 high_pct_change_black_pop
reghdfe personal_income_pc_2000 high_pct_change_black_pop, abs(state_fips)

reghdfe ln_personal_income_pc_2000 high_pct_change_black_pop
reghdfe ln_personal_income_pc_2000 high_pct_change_black_pop, abs(state_fips)

reghdfe real_gdp_pc_2020 high_pct_change_black_pop
reghdfe real_gdp_pc_2020 high_pct_change_black_pop, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 high_pct_change_black_pop
reghdfe ln_real_gdp_pc_2020 high_pct_change_black_pop, abs(state_fips)


// Change in share black pop

reghdfe personal_income_pc_2000 high_change_share_black
reghdfe personal_income_pc_2000 high_change_share_black, abs(state_fips)

reghdfe ln_personal_income_pc_2000 high_change_share_black
reghdfe ln_personal_income_pc_2000 high_change_share_black, abs(state_fips)

reghdfe real_gdp_pc_2020 high_change_share_black
reghdfe real_gdp_pc_2020 high_change_share_black, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 high_change_share_black
reghdfe ln_real_gdp_pc_2020 high_change_share_black, abs(state_fips)

// Change in black pop divided by change in pop on the RHS

reghdfe personal_income_pc_2000 high_change_black_pop_by_change
reghdfe personal_income_pc_2000 high_change_black_pop_by_change, abs(state_fips)

reghdfe ln_personal_income_pc_2000 high_change_black_pop_by_change
reghdfe ln_personal_income_pc_2000 high_change_black_pop_by_change, abs(state_fips)

reghdfe real_gdp_pc_2020 high_change_black_pop_by_change
reghdfe real_gdp_pc_2020 high_change_black_pop_by_change, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 high_change_black_pop_by_change
reghdfe ln_real_gdp_pc_2020 high_change_black_pop_by_change, abs(state_fips)

**************************
* Low change indicators *
**************************

// basically negative change

// Percent change in black pop
reghdfe personal_income_pc_2000 low_pct_change_black_pop
reghdfe personal_income_pc_2000 low_pct_change_black_pop, abs(state_fips)

reghdfe ln_personal_income_pc_2000 low_pct_change_black_pop
reghdfe ln_personal_income_pc_2000 low_pct_change_black_pop, abs(state_fips)

reghdfe real_gdp_pc_2020 low_pct_change_black_pop
reghdfe real_gdp_pc_2020 low_pct_change_black_pop, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 low_pct_change_black_pop
reghdfe ln_real_gdp_pc_2020 low_pct_change_black_pop, abs(state_fips)


// Change in share black pop

reghdfe personal_income_pc_2000 low_change_share_black
reghdfe personal_income_pc_2000 low_change_share_black, abs(state_fips)

reghdfe ln_personal_income_pc_2000 low_change_share_black
reghdfe ln_personal_income_pc_2000 low_change_share_black, abs(state_fips)

reghdfe real_gdp_pc_2020 low_change_share_black
reghdfe real_gdp_pc_2020 low_change_share_black, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 low_change_share_black
reghdfe ln_real_gdp_pc_2020 low_change_share_black, abs(state_fips)

// Change in black pop divided by change in pop on the RHS

reghdfe personal_income_pc_2000 low_change_black_pop_by_change
reghdfe personal_income_pc_2000 low_change_black_pop_by_change, abs(state_fips)

reghdfe ln_personal_income_pc_2000 low_change_black_pop_by_change
reghdfe ln_personal_income_pc_2000 low_change_black_pop_by_change, abs(state_fips)

reghdfe real_gdp_pc_2020 low_change_black_pop_by_change
reghdfe real_gdp_pc_2020 low_change_black_pop_by_change, abs(state_fips)

reghdfe ln_real_gdp_pc_2020 low_change_black_pop_by_change
reghdfe ln_real_gdp_pc_2020 low_change_black_pop_by_change, abs(state_fips)
