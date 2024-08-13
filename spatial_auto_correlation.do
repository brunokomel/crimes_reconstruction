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


import delimited "$wd/census_county_moransi.csv"

kdensity share_black

twoway kdensity share_black if year==1870 || kdensity share_black if year==1880, legend(label(1 "% Black, 1870") label(2 "% Black, 1880")) ytitle("Density") xtitle("% Black population")

gen cops1870 = (year == 1870 & ind_cops ==1)

twoway kdensity share_black if year==1870 & cops1870==0 || kdensity share_black if year==1880 & cops1870==0 || kdensity share_black if year==1870 & cops1870==1 || kdensity share_black if year==1880 & cops1870==1


*ssc install moransi

drop if lon == "NA" // Drops 4 obs.

destring lon, replace
destring lat, replace

// Share black

preserve 

keep if year == 1870

moransi share_black, lat(lat) lon(lon)  swm(exp 0.9)  dist(200) dunit(mi)

restore

preserve 

keep if year == 1880

moransi share_black, lat(lat) lon(lon) swm(exp 0.9)  dist(200) dunit(mi)

restore

// Percentage of black cops, etc.

preserve 

keep if year == 1870

moransi perc_cops, lat(lat) lon(lon) swm(bin) dist(200) dunit(mi)

restore

preserve 

keep if year == 1880

moransi perc_cops, lat(lat) lon(lon) swm(bin) dist(200) dunit(mi)

restore

// Number of black cops, etc.

preserve 

keep if year == 1870

moransi n_occ, lat(lat) lon(lon) swm(bin) dist(200) dunit(mi)

restore

preserve 

keep if year == 1880

moransi n_occ, lat(lat) lon(lon) swm(bin) dist(200) dunit(mi)

restore



// Indicator of black cops, etc.

preserve 

keep if year == 1870

moransi ind_cops, lat(lat) lon(lon) swm(bin) dist(200) dunit(mi)

restore

preserve 

keep if year == 1880

moransi ind_cops, lat(lat) lon(lon) swm(bin) dist(200) dunit(mi)

restore

