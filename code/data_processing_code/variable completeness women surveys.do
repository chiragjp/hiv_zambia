********************************************************************************

********************************************************************************
* File cycling through all DHS surveys and checking for completeness
*
********************************************************************************
 
clear all
set more off
pause on

********************* Local macros
local project_directory="~/Dropbox/HIV xwas work/output"
local filesDirectory = "~/Dropbox/Mapping DHS/DHS files"
*local filesDirectory = "~/Desktop/desktop junk/trial"
file open myfile using "`project_directory'/completeness ir files.txt", write replace
file write myfile  "variable" _tab "filename" _tab "var label" _tab "observations" _tab "completeness" _n

*********************

cd "`filesDirectory'"

local directories: dir . dirs "*"
foreach directory of local directories {
	cd "`filesDirectory'/`directory'"
	local irfiles: dir . files "*"	
	foreach irfileyn of local irfiles {
		if ((strpos("`irfileyn'","ir") | strpos("`irfileyn'","IR")) & (strpos("`irfileyn'",".dta") | strpos("`irfileyn'",".DTA")) & !strpos("`irfileyn'", "_") ///
		& "`directory'"!="09PeruContinuous"){
			use "`irfileyn'", clear
			gen start=1
			gen end=1
			order start
			order end, last
			quietly gen xx=.
			foreach x of varlist start-end {
			local x=subinstr("`x'", char(34),"",.)
			file write myfile "`x'" _tab "`irfileyn'"
			capture local labelr : var label `x'
			if _rc==0 {
			capture local labelr=subinstr("`labelr'", char(34),"",.)
			capture file write myfile _tab "`labelr'"
			capture replace xx=(`x'!=.)
			if _rc!=0 {
			capture replace xx=(`x'!="")
			}
			quietly sum xx 
			file write myfile _tab %9.4g (r(N)) _tab %7.3f (r(mean))
			}
			file write myfile _n
		}
		}
	}
}

file close myfile
local project_directory="~/Dropbox/HIV xwas work/output"
insheet using "`project_directory'/completeness ir files.txt", clear
duplicates tag variable, gen (n_svys)
quietly replace n_svys=n_svys+1
drop if variable=="start" | variable=="end"
gsort -n_svys -comple variable filename

save "`project_directory'/completeness ir files", replace
replace  varlabel="." if varlabel==""
egen var_label_grp=group( variable varlabel)
collapse (mean) completeness n_svys (count) n_in_group=var_label_grp, by(variable varlabel)
bys variable: egen max_n=max(n_svys)
gsort -max variable -comple
save "`project_directory'/completeness ir files collapsed", replace
