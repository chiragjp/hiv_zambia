
********************************************************************************
  
clear all
set more off
set matsize 11000
pause on

********************* Macros and setup *******************
*local project_directory="~/Dropbox/HIV xwas work/output/tables_07-13"
local project_directory="~/Desktop/desktop junk"
local files_directory = "~/Dropbox/HIV xwas work/data/"

file open myfile using "`project_directory'/variables labels.txt", write replace
file write myfile  "variable" _tab "var_label" _tab "level" _tab "value_label" _n
local year=13

**********************************************************

run "~/Dropbox/HIV xwas work/code/data processing code/generating labels step 1.do"
run "~/Dropbox/HIV xwas work/code/data processing code/generating labels step 2.do"
run "~/Dropbox/HIV xwas work/code/data processing code/generating labels step 3.do"
