
/********************************************************************************************************/
/* DESCRIPTIVE STATISTICS */
/********************************************************************************************************/

//---------------------------------------------------------------------------------//
//----------------------------- Table 1 (desc_sum) --------------------------------// 
//---------------------------------------------------------------------------------//

global covar_so l12age  l12noneng l12eduuni l12pinc l12jobsec l12casual l12ndepch 
global covar_co l12age  l12noneng l12eduuni l12pinc l12jobsec l12casual l12ndepch l12pearnhh  
global covar_cp l12age_sp l12noneng_sp l12pinc_sp l12wstat_spc1 l12wstat_spc4 

use "hl_di_main.dta", clear
gen mshock2 = 1 - shock2		// so that difference is on treated - control, not control - treated
eststo clear

tabulate l12wstat_sp, generate(l12wstat_spc)
la var l12wstat_spc1 "Unemployed"
la var l12wstat_spc2 "Employed casually"
la var l12wstat_spc3 "Employed part time"
la var l12wstat_spc4 "Employed full time"

local vars1 "l12age l12female l12noneng l12eduuni l12pinc l12jobsec l12casual l12ndepch l12pearnhh"
local labels1 `" "Age (in years)" "Female$^\dagger$" "Non-English$^\dagger$" "University degree$^\dagger$" "Labor earnings (in 10000 AUD)" "Job security (0-20)" "Casual worker$^\dagger$" "Number of dependent children" "Household income share""' 
local vars2 "l12age_sp l12noneng_sp l12pinc_sp l12wstat_spc1 l12wstat_spc4" 
local labels2 `" "Age (in years)" "Non-English$^\dagger$" "Labor earnings (in 10000 AUD)" "Unemployed$^\dagger$" "Employed full-time$^\dagger$""' 

local vars `"`vars1' `vars2'"'
local labels `"`labels1' `labels2'"'
local n: word count `vars'
forval i = 1/`n'{
	local a: word `i' of `vars'
	local b: word `i' of `labels'
	la var `a' "`b'"
}
save "hl_di_desc_sum.dta", replace


**************************************************************
** separate statistics for males and females **

use "hl_di_desc_sum.dta", clear

cap erase "table_desc_sum.tex"
eststo clear
* 1- adult households
local n = 0
foreach var of varlist $covar_so {
local n = `n' + 1
	if inlist("`var'","l12female","l12noneng","l12eduuni","l12casual") {
		replace `var'=`var'*100
	}
	estpost ttest `var' if singlef == 1 & female == 0, by(mshock2)
	eststo test_m
	estpost ttest `var' if singlef == 1 & female == 1, by(mshock2)
	eststo test_f	
	if `n' == 1 {
	estout test_m test_f using "table_desc_sum.tex", cell("mu_1(fmt(1)) mu_2(fmt(1)) mu_1(fmt(1)) b(star fmt(1))") style(tex) starlevels(* 0.1 ** 0.05 *** 0.01)  collabels(none) mlabels(none) label prehead(\midrule \multicolumn{9}{l}{\textbf{1-adult households}} \\) append
	}
	else {
	estout using "table_desc_sum.tex", cell("mu_1(fmt(1)) mu_2(fmt(1)) mu_1(fmt(1)) b(star fmt(1))") style(tex) starlevels(* 0.1 ** 0.05 *** 0.01)  collabels(none) mlabels(none) label append 	
	}
	if inlist("`var'","l12female","l12noneng","l12eduuni","l12casual")  {
		replace `var'=`var'/100
	}
}

* 1-adult households, Ntreat
count if shock2 == 0 & singlef == 1 & female == 0
local n0_m = r(N)
count if shock2 == 1 & singlef == 1 & female == 0 
local n1_m = r(N)		
count if shock2 == 0 & singlef == 1 & female == 1
local n0_f = r(N)
count if shock2 == 1 & singlef == 1 & female == 1 
local n1_f = r(N)
estout using "table_desc_sum.tex", cell(none) postfoot(\addlinespace N & `n1_m' & `n0_m' & `n0_m' & --- & `n1_f' & `n0_f' & `n0_f' & --- \\ ) style(tex) starlevels(* 0.1 ** 0.05 *** 0.01)  collabels(none) mlabels(none) label append 


* 2-adult households, own characteristics
local n = 0
foreach var of varlist $covar_co {
local n = `n' + 1
	if inlist("`var'","l12female","l12noneng","l12eduuni","l12casual") {
		replace `var'=`var'*100
	}
	estpost ttest `var' if couplef == 1 & female == 0 , by(mshock2)
	eststo test_m
	estpost ttest `var' if couplef == 1 & female == 1 , by(mshock2)	
	eststo test_f
	if `n' == 1 {
	estout test_m test_f using "table_desc_sum.tex", cell("mu_1(fmt(1)) mu_2(fmt(1)) mu_1(fmt(1)) b(star fmt(1))") style(tex) starlevels(* 0.1 ** 0.05 *** 0.01)  collabels(none) mlabels(none) label prehead(\midrule \multicolumn{9}{l}{\textbf{2-adult households, own characteristics}} \\) append
	}
	else {
	estout test_m test_f using "table_desc_sum.tex", cell("mu_1(fmt(1)) mu_2(fmt(1)) mu_1(fmt(1)) b(star fmt(1))") style(tex) starlevels(* 0.1 ** 0.05 *** 0.01)  collabels(none) mlabels(none) label append 	
	}
	if inlist("`var'","l12female","l12noneng","l12eduuni","l12casual")  {
		replace `var'=`var'/100
	}
}


* 2-adult households, partner's characteristics
local n = 0
foreach var of varlist $covar_cp {
local n = `n' + 1
	if inlist("`var'","l12noneng_sp","l12wstat_spc1","l12wstat_spc4") {
		replace `var'=`var'*100
	}
	estpost ttest `var' if couplef == 1 & female == 0 , by(mshock2)
	eststo test_m
	estpost ttest `var' if couplef == 1 & female == 1 , by(mshock2)	
	eststo test_f
	if `n' == 1 {
	estout test_m test_f using "table_desc_sum.tex", cell("mu_1(fmt(1)) mu_2(fmt(1)) mu_1(fmt(1)) b(star fmt(1))") style(tex) starlevels(* 0.1 ** 0.05 *** 0.01)  collabels(none) mlabels(none) label prehead(\addlinespace \multicolumn{9}{l}{\textbf{2-adult households, partner's characteristics}} \\) append
	}
	else {
	estout test_m test_f using "table_desc_sum.tex", cell("mu_1(fmt(1)) mu_2(fmt(1)) mu_1(fmt(1)) b(star fmt(1))") style(tex) starlevels(* 0.1 ** 0.05 *** 0.01)  collabels(none) mlabels(none) label append 	
	}
	if inlist("`var'","l12noneng_sp","l12wstat_spc1","l12wstat_spc4")  {
		replace `var'=`var'/100
	}
}


* 2-adult households, Ntreat
count if shock2 == 0 & couplef == 1 & female == 0
local n0_m = r(N)
count if shock2 == 1 & couplef == 1 & female == 0 
local n1_m = r(N)		
count if shock2 == 0 & couplef == 1 & female == 1
local n0_f = r(N)
count if shock2 == 1 & couplef == 1 & female == 1 
local n1_f = r(N)	
estout using "table_desc_sum.tex", cell(none) postfoot(\addlinespace N & `n1_m' & `n0_m' & `n0_m' & --- & `n1_f' & `n0_f' & `n0_f' & --- \\ ) style(tex) starlevels(* 0.1 ** 0.05 *** 0.01)  collabels(none) mlabels(none) append 



//---------------------------------------------------------------------------------//
//---------------------------- Table B.2 (desc_lei) -------------------------------// 
//---------------------------------------------------------------------------------//

set more off 

use "hl_di_main.dta", clear

local vars1 "lemar leprg lebth leins"
local labels1 `" "Got married & 50" "Pregnancy & 40" "Birth/adoption of new child & 39" "Serious personal injury/illness & 53""'

local vars2 "leinf ledrl ledfr levio lepcm"
local labels2 `" "Serious injury/illness to family member & 44" "Death of close relative/family member & 63" "Death of a close friend & 37" "Victim of physical violence & 53" "Victim of a property crime & 44""'

local vars3 "lejls lejlf lefni lefnw lemvd"
local labels3 `""Detained in jail & 63" "Close family member detained in jail & 50" "Major improvement in finances & 38" "Major worsening in finances & 58" "Changed residence & 20""'

local vars `"`vars1' `vars2' `vars3'"'
local labels `"`labels1' `labels2' `labels3'"'
local n: word count `vars'
forval i = 1/`n'{
	local a: word `i' of `vars'
	local b: word `i' of `labels'
	la var `a' "`b'"
}

save "hl_di_desc_lei.dta", replace


eststo clear          
cap erase "table_desc_lei.tex"
eststo le: estpost sum lemar leprg lebth leins leinf ledrl ledfr levio lepcm lejls lejlf lefni lefnw lemvd 
estadd matrix pctm = e(mean)*100
estout le using "table_desc_lei.tex", cells("sum(fmt(0)) pctm(fmt(1)) sd(fmt(2))") collabels(none) mlabels(none) eqlabels(none) style(tex) label append		// sum(fmt(%9.3gc)): comma for thousand when >= 5 digits


eststo slei: estpost tabstat slei, listwise statistics(min max mean sd) columns(statistics)
estout slei using "table_desc_lei.tex", cells("min(fmt(0)) max(fmt(0)) mean(fmt(1)) sd(fmt(2))") style(tex) collabels(none) mlabels(none) eqlabels(none) varlabels(slei "Standardized index") prehead(\midrule) append							



//---------------------------------------------------------------------------------//
//----------------------------- stats in the paper --------------------------------// 
//---------------------------------------------------------------------------------//

* prevalence including double displacement (worker and partner)
use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if cp == 1 & hasparintv == 1 & pairs != 0 & l12empl == 1 & (uempl == 1 | lejob2 == 0)  // removed shock2_sp == 0 to calculate this stat
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess) & !mi(d2skess_sp)		
drop if samesex == 1

sum shock2 if female == 0 
sum shock2 if female == 1

sum shock2 if female == 0 & shock2_sp == 0
sum shock2 if female == 1 & shock2_sp == 0

* direct difference if not dividing the sample by gender
use "hl_di_main.dta", clear

reg d2skess shock2 $covar_single if singlef == 1 [weight = _webal]
eststo s_o, refresh

reg d2skess shock2 $covar_couple if couplef == 1 [weight = _webal]
eststo c_o, refresh

suest s_o c_o, vce(cluster hhid)
test [s_o_mean]shock2 = [c_o_mean]shock2	// p-value = 0.0412

* number of singles with dependent children
use "hl_di_main.dta", clear
count if idepch == 1 & singlef == 1 & shock2 == 1 & female == 0
local n1 = r(N)
count if singlef == 1 & shock2 == 1 & female == 0
local n0 = r(N)
local prop_depch = `n1' / `n0'
di "The percentage of treated 1-adult males with dependent children is `prop_depch'"

count if idepch == 1 & singlef == 1 & shock2 == 1 & female == 1
local n1 = r(N)
count if singlef == 1 & shock2 == 1 & female == 1
local n0 = r(N)
local prop_depch = `n1' / `n0'
di "The percentage of treated 1-adult females with dependent children is `prop_depch'"





//-------------------------------------------------------------------------------------//
//----------------------------- change in family structure ----------------------------// 
//-------------------------------------------------------------------------------------//

* balancing
use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if l12empl == 1 & (uempl == 1 | lejob2 == 0) 
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess)
save "hl_di_unmatched_fam.dta", replace 	

foreach yesno in 0 1 {
	use "hl_di_unmatched_fam.dta", clear	
	keep if female == `yesno'
	sort random 	
	ebalance shock $covar_allhh, targets(1) maxiter(100) gen(_webal)			// usings shock (not shock2) for contemporanous effects
	keep index wave _webal  
	save hl_di_f`yesno'_fam.dta, replace 
}

use "hl_di_f0_fam.dta", clear
append using "hl_di_f1_fam.dta" 

sort index wave
save "hl_di_matched_fam.dta", replace
use "hl_di_unmatched_fam.dta", clear
merge 1:1 index wave using "hl_di_matched_fam.dta", keep(master match) nogen
save "hl_di_fam.dta", replace 

erase hl_di_f0_fam.dta
erase hl_di_f1_fam.dta
erase hl_di_matched_fam.dta
erase hl_di_unmatched_fam.dta


* descriptive difference 
use "hl_di_fam.dta", clear
foreach var in mschgdv mschgsp mschgmr mschgrs lebth ndepch {
	di `"`: var label `var''"' 
	ttest `var', by(shock)									
}

foreach var in mschgdv mschgsp mschgmr mschgrs lebth {
	forvalues i = 1 / 5 {									
	    di `"`: var label `var'', year `i'"' 
	    ttest f`i'`var' if f`i'uempl == 1, by(shock)		
	}    
}



* regression
foreach var in mschgdv mschgsp mschgmr mschgrs lebth {
   	di `"`: var label `var''"'  
    reg `var' shock $covar_allhh [weight = _webal], vce(cluster hhid)			
}

foreach var in lebth {
   	di `"`: var label `var''"'  
	forvalues i = 1 / 5 {
		reg f`i'`var' shock $covar_allhh [weight = _webal] if f`i'uempl == 1, vce(cluster hhid)	   
	}
}


