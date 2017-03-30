*local filename="super_adjusted" "ors"
local filename="xwas_baseline"
local directory="~/Dropbox/HIV xwas work/output/results output old/"

cd "`directory'"
import delimited "`filename'.csv", varnames(1) asdouble clear

gen parse1=strpos(varlabel_long,"(v")
replace parse1=strpos(varlabel_long,"(s") if strpos(varlabel_long,"(s")>0
replace parse1=strpos(varlabel_long,"(m") if strpos(varlabel_long,"(m")>0
gen parse2=strpos(varlabel_long,")")
gen flag1=parse2<parse1
keep if validate_unadj==1

gen var_label_1=substr(varlabel_long,1,parse1-1) 
gen variable=substr(varlabel_long,parse1+1,parse2-parse1-1) if flag1==0
replace variable=substr(varlabel_long,parse1+1,4) if flag1==1
replace variable=substr(varlabel_long,parse1+1,7) if (flag1==1 & strpos(varlabel_long,"(ml")>0)
replace variable=substr(varlabel_long,parse1+1,6) if (flag1==1 & strpos(varlabel_long,"(v513")>0)


replace parse1=strpos(conf_int_string_unadj,"[")
replace parse2=strpos(conf_int_string_unadj,"]")
gen effect_size=substr(conf_int_string_unadj,1,parse1-2)
gen ci_str=substr(conf_int_string_unadj,parse1+1,parse2-parse1-1)
replace parse1=strpos(ci_str,",")
gen ci_low=substr(ci_str,1,parse1-1)
gen ci_hi=substr(ci_str,parse1+1,.)

destring effect_size ci_low ci_hi, replace
format effect_size ci_low ci_hi %4.2f

gen effect_ci=string(effect_size, "%4.2f")+" ["+string(ci_low, "%4.2f")+", "+string(ci_hi, "%4.2f")+"]"
gen p_value_unadj=-log10(pvalue_unadj)
gen p_value=string(p_value_unadj, "%4.1f")

merge m:1 variable using "~/Dropbox/HIV xwas work/data/variable legends and keys.dta"

keep if _merge==3
 
*gsort -pval -or
gsort -p_value_unadj

keep manuscript_label effect_ci p_value p_value_unadj effect_size

order manuscript_label effect_ci p_value
* pause here for unadjusted findings for Table 1
gen order=_n
keep manuscript_label order
tempfile table1unadj
save `table1unadj', replace

************************************************************

*local filename="super_adjusted" "ors"
local filename="xwas_adjusted"
local directory="~/Dropbox/HIV xwas work/code/result"

cd "`directory'"
import delimited "`filename'.csv", varnames(1) asdouble clear

gen parse1=strpos(varlabel_long,"(v")
replace parse1=strpos(varlabel_long,"(s") if strpos(varlabel_long,"(s")>0
replace parse1=strpos(varlabel_long,"(m") if strpos(varlabel_long,"(m")>0
gen parse2=strpos(varlabel_long,")")
gen flag1=parse2<parse1
replace validate_adj =2 if conf_int_string_adj=="NA"
keep if validate_adj >0

gen var_label_1=substr(varlabel_long,1,parse1-1) 
gen variable=substr(varlabel_long,parse1+1,parse2-parse1-1) if flag1==0
replace variable=substr(varlabel_long,parse1+1,4) if flag1==1
replace variable=substr(varlabel_long,parse1+1,7) if (flag1==1 & strpos(varlabel_long,"(ml")>0)
replace variable=substr(varlabel_long,parse1+1,6) if (flag1==1 & strpos(varlabel_long,"(v513")>0)


replace parse1=strpos(conf_int_string_adj,"[")
replace parse2=strpos(conf_int_string_adj,"]")
gen effect_size=substr(conf_int_string_adj,1,parse1-2)
gen ci_str=substr(conf_int_string_adj,parse1+1,parse2-parse1-1)
replace parse1=strpos(ci_str,",")
gen ci_low=substr(ci_str,1,parse1-1)
gen ci_hi=substr(ci_str,parse1+1,.)

destring effect_size ci_low ci_hi, replace
format effect_size ci_low ci_hi %4.2f

gen effect_ci=string(effect_size, "%4.2f")+" ["+string(ci_low, "%4.2f")+", "+string(ci_hi, "%4.2f")+"]"
destring pvalue_adj, gen(pval_numeric) force
gen p_value_adj=-log10(pval_numeric)
gen p_value=string(p_value_adj, "%4.1f")
replace p_value="Adjustor" if p_value=="."

merge m:1 variable using "~/Dropbox/HIV xwas work/data/variable legends and keys.dta"
keep if _merge==3
 
*gsort -pval -or

keep manuscript_label effect_ci p_value p_value_adj effect_size

*drop p_value_adj
order manuscript_label effect_ci p_value

merge 1:1 manuscript_label using `table1unadj'
gen original=_merge>1
gsort -original order -p_value_adj


************************************************************

local filename="xwas_super_adjusted"
*local filename="xwas_adjusted" "ors"
local directory="~/Dropbox/HIV xwas work/code/result"

cd "`directory'"
import delimited "`filename'.csv", varnames(1) asdouble clear

gen parse1=strpos(varlabel_long,"(v")
replace parse1=strpos(varlabel_long,"(s") if strpos(varlabel_long,"(s")>0
replace parse1=strpos(varlabel_long,"(m") if strpos(varlabel_long,"(m")>0
gen parse2=strpos(varlabel_long,")")
gen flag1=parse2<parse1
replace validate_super_adj =2 if conf_int_string_super_adj=="NA"
keep if validate_super_adj >0

gen var_label_1=substr(varlabel_long,1,parse1-1) 
gen variable=substr(varlabel_long,parse1+1,parse2-parse1-1) if flag1==0
replace variable=substr(varlabel_long,parse1+1,4) if flag1==1
replace variable=substr(varlabel_long,parse1+1,7) if (flag1==1 & strpos(varlabel_long,"(ml")>0)
replace variable=substr(varlabel_long,parse1+1,6) if (flag1==1 & strpos(varlabel_long,"(v513")>0)


replace parse1=strpos(conf_int_string_super_adj,"[")
replace parse2=strpos(conf_int_string_super_adj,"]")
gen effect_size=substr(conf_int_string_super_adj,1,parse1-2)
gen ci_str=substr(conf_int_string_super_adj,parse1+1,parse2-parse1-1)
replace parse1=strpos(ci_str,",")
gen ci_low=substr(ci_str,1,parse1-1)
gen ci_hi=substr(ci_str,parse1+1,.)

destring effect_size ci_low ci_hi, replace
format effect_size ci_low ci_hi %4.2f

gen effect_ci=string(effect_size, "%4.2f")+" ["+string(ci_low, "%4.2f")+", "+string(ci_hi, "%4.2f")+"]"
destring pvalue_super_adj, gen(pval_numeric) force
gen p_value_super_adj=-log10(pval_numeric)
gen p_value=string(p_value_super_adj, "%4.1f")
replace p_value="Adjustor" if p_value=="."
replace effect_ci="Adjustor" if effect_ci==". [., .]"

merge m:1 variable using "~/Dropbox/HIV xwas work/data/variable legends and keys.dta"
keep if _merge==3
 
*gsort -pval -or

keep manuscript_label effect_ci p_value p_value_super_adj effect_size

*drop p_value_adj
order manuscript_label effect_ci p_value

merge 1:1 manuscript_label using `table1unadj'
gen original=_merge>1
gsort -original order -p_value_super_adj


