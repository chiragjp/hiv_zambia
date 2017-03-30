set more off
graph drop _all
*use "/Users/ebd/Desktop/desktop junk/adultswHIV.dta", clear
use "/Users/ebd/Dropbox/HIV xwas work/data/allageswHIV.dta", clear
foreach x of varlist v1990- v2014 {
gen `x'par=strpos(`x',"[")
gen `x'hiv=substr(`x',1,`x'par-2)
replace `x'hiv=subinstr(`x'hiv," ","",.)
replace `x'par=strpos(`x'hiv,"<")
replace `x'hiv=substr(`x'hiv,2,.) if `x'par>0
destring `x'hiv, replace
}

foreach x of varlist v1990- v2014 {
drop `x' `x'par
}

replace country="C™te d'Ivoire" if country=="CÌ«te d'Ivoire"
tempfile whiv
save "`whiv'", replace

use "/Users/ebd/Dropbox/HIV xwas work/data/allagesonART.dta", clear
foreach x of varlist v2010-v2014 {
rename `x' `x'art
replace `x'art=subinstr(`x'art," ","",.)
destring `x'art, replace
}
merge 1:1 country using "`whiv'"

keep if _merge==3
drop _merge

forval x=2010/2014 {
gen v`x'cov=v`x'art/v`x'hiv
}

sort v2014cov
hist v2014cov, kdensity xlabel(0(0.2)1) name("h14")
hist v2013cov, kdensity  xlabel(0(0.2)1) name("h13")
hist v2012cov, kdensity  xlabel(0(0.2)1) name("h12")
hist v2011cov, kdensity  xlabel(0(0.2)1) name("h11")
hist v2010cov, kdensity  xlabel(0(0.2)1) name("h10")
