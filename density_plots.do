** Crimes Project - Preliminary Analysis

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


use "$wd/census_cops_troop_lynch_vote.dta", clear

kdensity share_black

twoway kdensity share_black if year==1870 || kdensity share_black if year==1880, legend(label(1 "% Black, 1870") label(2 "% Black, 1880")) ytitle("Density") xtitle("% Black population")


gen cops1870 = (year == 1870 & n_black_cops >0)

egen ind_cops1870 = max(cops1870), by(gisjoin)

gen cops1880 = (year == 1880 & n_black_cops >0)

egen ind_cops1880 = max(cops1880), by(gisjoin)

// Cops 1870

twoway kdensity share_black if year==1870 & ind_cops1870==0 || kdensity share_black if year==1880 & ind_cops1870==0 || kdensity share_black if year==1870 & ind_cops1870==1 || kdensity share_black if year==1880 & ind_cops1870==1 , ///
legend(label(1 "% Black, 1870, No Cops 1870") label(2 "% Black, 1880, No Cops 1870")) legend(label(3 "% Black, 1870, With Cops 1870") label(4 "% Black, 1880, With Cops 1870")) ytitle("Density") xtitle("% Black population")

// Cops 1880

twoway kdensity share_black if year==1870 & ind_cops1880==0 || kdensity share_black if year==1880 & ind_cops1880==0 || kdensity share_black if year==1870 & ind_cops1880==1 || kdensity share_black if year==1880 & ind_cops1880==1 , ///
legend(label(1 "% Black, 1870, No Cops 1880") label(2 "% Black, 1880, No Cops 1880")) legend(label(3 "% Black, 1880, With Cops 1880") label(4 "% Black, 1880, With Cops 1880")) ytitle("Density") xtitle("% Black population")


// Democrat Vote Share

local high_dem = 80
local election_year = "dem_vote_share1868"

twoway kdensity share_black if year==1870 & `election_year' <= `high_dem' || kdensity share_black if year==1880 & `election_year' <= `high_dem' || kdensity share_black if year==1870 & `election_year' > `high_dem' || kdensity share_black if year==1880 & `election_year' > `high_dem' , ///
legend(label(1 "% Black, 1870, Low Dem") label(2 "% Black, 1880, Low Dem")) legend(label(3 "% Black, 1870, High Dem") label(4 "% Black, 1880, High Dem")) ytitle("Density") xtitle("% Black population")


local high_dem = 80
local election_year = "dem_vote_share1872"

twoway kdensity share_black if year==1870 & `election_year' <= `high_dem' || kdensity share_black if year==1880 & `election_year' <= `high_dem' || kdensity share_black if year==1870 & `election_year' > `high_dem' || kdensity share_black if year==1880 & `election_year' > `high_dem' , ///
legend(label(1 "% Black, 1870, Low Dem") label(2 "% Black, 1880, Low Dem")) legend(label(3 "% Black, 1870, High Dem") label(4 "% Black, 1880, High Dem")) ytitle("Density") xtitle("% Black population")


// The two above are VERY different.

// Troops
twoway kdensity share_black if year==1870 & any_troops==0 || kdensity share_black if year==1880 & any_troops==0 || kdensity share_black if year==1870 & any_troops==1 || kdensity share_black if year==1880 & any_troops==1 , ///
legend(label(1 "% Black, 1870, No Troops") label(2 "% Black, 1880, No Troops")) legend(label(3 "% Black, 1870, With Troops") label(4 "% Black, 1880, With Troops")) ytitle("Density") xtitle("% Black population")


gen troops1870 = (year == 1870 & any_troops ==1)

egen ind_troops1870 = max(troops1870), by(gisjoin)

gen troops1880 = (year == 1870 & any_troops ==1)

egen ind_troops1880 = max(troops1880), by(gisjoin)

// Troops 1870

twoway kdensity share_black if year==1870 & ind_troops1870==0 || kdensity share_black if year==1880 & ind_troops1870==0 || kdensity share_black if year==1870 & ind_troops1870==1 || kdensity share_black if year==1880 & ind_troops1870==1 , ///
legend(label(1 "% Black, 1870, No Troops 1870") label(2 "% Black, 1880, No Troops 1870")) legend(label(3 "% Black, 1870, With Troops 1870") label(4 "% Black, 1880, With Troops 1870")) ytitle("Density") xtitle("% Black population")

// Troops 1880

twoway kdensity share_black if year==1870 & ind_troops1880==0 || kdensity share_black if year==1880 & ind_troops1880==0 || kdensity share_black if year==1870 & ind_troops1880==1 || kdensity share_black if year==1880 & ind_troops1880==1 , ///
legend(label(1 "% Black, 1870, No Troops 1880") label(2 "% Black, 1880, No Troops 1880")) legend(label(3 "% Black, 1880, With Troops 1880") label(4 "% Black, 1880, With Troops 1880")) ytitle("Density") xtitle("% Black population")


// Lynchings

replace n_lynch = 0 if n_lynch == .

twoway kdensity share_black if year==1870 & n_lynch == 0 || kdensity share_black if year==1880 & n_lynch == 0|| kdensity share_black if year==1870 & n_lynch > 0 || kdensity share_black if year==1880 & n_lynch > 0, ///
legend(label(1 "% Black, 1870, No Lynchings") label(2 "% Black, 1880, No Lynchings")) legend(label(3 "% Black, 1870, With Lynchings") label(4 "% Black, 1880, With Lynchings")) ytitle("Density") xtitle("% Black population")

