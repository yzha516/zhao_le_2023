
/********************************************************************************************************/
/* MAIN RESULTS */
/********************************************************************************************************/
  
*******************************************************************
** global **
 
global covar_couple l12age l12female l12noneng l12ghbp l12ghgh l12ghpf l12ghre l12ghrp l12ghsf l12ghvt l12pinc l12nevuemp l12yrpaid i.l12csize l12jobsec i.l12occ1da i.l12ind1da l12slf1 l12slf2 l12slf3 l12hrswk l12casual l12pearnhh l12iedusec l12eduuni l12eduvoc l12ndepchc1 l12ndepchc2 l12ndepchc3 l12ndepchcg3 l12avgurp i.l12remar i.l12state l12slei l12snbh l12decdis l12dececr l12decedo l12cohab btqia btqia_sp ib9.wave l12age_sp l12noneng_sp l12ghbp_sp l12ghgh_sp l12ghpf_sp l12ghre_sp l12ghrp_sp l12ghsf_sp l12ghvt_sp l12pinc_sp l12nevuemp_sp ib1.l12wstat_sp l12iedusec_sp l12eduuni_sp l12eduvoc_sp l12age2 l12ghbp2 l12ghgh2  l12ghpf2 l12ghre2 l12ghrp2 l12ghsf2 l12ghvt2 l12pinc2 l12jobsec2 l12hrswk2 l12pearnhh2 l12avgurp2 l12slei2 l12snbh2 l12decdis2 l12dececr2 l12decedo2 l12age_sp2 l12ghbp_sp2 l12ghgh_sp2  l12ghpf_sp2 l12ghre_sp2 l12ghrp_sp2 l12ghsf_sp2 l12ghvt_sp2 l12pinc_sp2  

global covar_single l12age l12female l12noneng l12ghbp l12ghgh l12ghpf l12ghre l12ghrp l12ghsf l12ghvt l12pinc l12nevuemp l12yrpaid i.l12csize l12jobsec i.l12occ1da i.l12ind1da l12slf1 l12slf2 l12slf3 l12hrswk l12casual l12iedusec l12eduuni l12eduvoc l12ndepchc1 l12ndepchc2 l12ndepchc3 l12ndepchcg3 l12avgurp i.l12remar i.l12state l12slei l12snbh l12decdis l12dececr l12decedo lg btqia ib9.wave l12age2 l12ghbp2 l12ghgh2 l12ghpf2 l12ghre2 l12ghrp2 l12ghsf2 l12ghvt2 l12pinc2 l12jobsec2 l12hrswk2 l12avgurp2 l12slei2 l12snbh2 l12decdis2 l12dececr2 l12decedo2	 

global covar_allhh l12age l12female l12noneng l12ghbp l12ghgh l12ghpf l12ghre l12ghrp l12ghsf l12ghvt l12pinc l12nevuemp l12yrpaid i.l12csize l12jobsec i.l12occ1da i.l12ind1da l12slf1 l12slf2 l12slf3 l12hrswk l12casual l12pearnhh l12iedusec l12eduuni l12eduvoc l12ndepchc1 l12ndepchc2 l12ndepchc3 l12ndepchcg3 l12avgurp i.l12remar i.l12state l12slei l12snbh l12decdis l12dececr l12decedo l12cohab  lg btqia ib9.wave l12age2 l12ghbp2 l12ghgh2  l12ghpf2 l12ghre2 l12ghrp2 l12ghsf2 l12ghvt2 l12pinc2 l12jobsec2 l12hrswk2 l12pearnhh2 l12avgurp2 l12slei2 l12snbh2 l12decdis2 l12dececr2 l12decedo2 

global ifcd_scmf_o "(female == 0 | female == 1)"
global ifcd_scm_o "female == 0"
global ifcd_scf_o "female == 1"
global ifcd_smf_o "singlef == 1"
global ifcd_sm_o "(singlef == 1 & female == 0)"
global ifcd_sf_o "(singlef == 1 & female == 1)"
global ifcd_cmf_o "couplef == 1"
global ifcd_cm_o "(couplef == 1 & female == 0)"
global ifcd_cf_o "(couplef == 1 & female == 1)"
global ifcd_cmf_p "couplef == 1"
global ifcd_cm_p "(couplef == 1 & female == 0)"
global ifcd_cf_p "(couplef == 1 & female == 1)"



*******************************************************************
** program **

* this program conducts Wald tests on parametres within a specification
* it is repeatedly called in subsequent do files

capture program drop ws_pval 
program define ws_pval

* direct difference
test [smf_o_mean]shock2 = [cmf_o_mean]shock2													
estadd scalar pval_dir = r(p): `13' 
test [sm_o_mean]shock2 - [cm_o_mean]shock2 = 0
estadd scalar pval_dir_m = r(p): `13' 
test [sf_o_mean]shock2 - [cf_o_mean]shock2 = 0
estadd scalar pval_dir_f = r(p): `13' 


* average household difference using average prevalence  
test `1'*[smf_o_mean]shock2 = `1'*[cmf_o_mean]shock2 + `10'*[cmf_p_mean]shock2 		
estadd scalar pval_ahh = r(p): `13'
test `2'*[sm_o_mean]shock2 - `2'*[cm_o_mean]shock2 - `12'*[cf_p_mean]shock2 = 0
estadd scalar pval_ahh_m = r(p): `13'
test `3'*[sf_o_mean]shock2 - `3'*[cf_o_mean]shock2 - `11'*[cm_p_mean]shock2 = 0
estadd scalar pval_ahh_f = r(p): `13'


* overall household difference (prevalence-weighted, prevalence effect + burden-sharing effect) 
test `4'*[smf_o_mean]shock2 = `7'*[cmf_o_mean]shock2 + `10'*[cmf_p_mean]shock2		
estadd scalar pval_ohh = r(p): `13'
test `5'*[sm_o_mean]shock2 - `8'*[cm_o_mean]shock2 - `12'*[cf_p_mean]shock2 = 0
estadd scalar pval_ohh_m = r(p): `13'
test `6'*[sf_o_mean]shock2 - `9'*[cf_o_mean]shock2 - `11'*[cm_p_mean]shock2 = 0
estadd scalar pval_ohh_f = r(p): `13'


* gender difference
test [scm_o_mean]shock2 = [scf_o_mean]shock2													
estadd scalar pval_gdr = r(p): `13'
test [sm_o_mean]shock2 - [sf_o_mean]shock2 = 0
estadd scalar pval_gdr_s = r(p): `13' 
test [cm_o_mean]shock2 - [cf_o_mean]shock2 = 0
estadd scalar pval_gdr_c = r(p): `13' 


* role difference
test [cmf_o_mean]shock2 = [cmf_p_mean]shock2												
estadd scalar pval_role = r(p): `13'
test [cm_o_mean]shock2 - [cf_p_mean]shock2 = 0
estadd scalar pval_role_m = r(p): `13' 
test [cf_o_mean]shock2 - [cm_p_mean]shock2 = 0
estadd scalar pval_role_f = r(p): `13' 

end



*******************************************************************
** [1] main specification **

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if cp == 1 & hasparintv == 1 & shock2_sp == 0 & pairs != 0 & l12empl == 1 & (uempl == 1 | lejob2 == 0) 
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess) & !mi(d2skess_sp)		
drop if samesex == 1
gen couplef = 1	
la var couplef "Couple file indicator"
save "hl_di_unmatched_couple.dta", replace

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if lp == 1 & noparintv == 1 & l12empl == 1 & (uempl == 1 | lejob2 == 0)
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess)
gen singlef = 1
la var singlef "Single file indicator"
save "hl_di_unmatched_single.dta", replace

use "hl_di_unmatched_couple.dta", clear
append using "hl_di_unmatched_single.dta"
save "hl_di_unmatched.dta", replace

* separate matching by household type and gender
set sortseed 2020
foreach yesno in 0 1 {
********************************************
** couple **

	use "hl_di_unmatched_couple.dta", clear
	keep if female == `yesno'
	sort random 		
	egen l12pinc_rk = rank(l12pinc), by(wave) unique
	egen l12pinc_qt = xtile(l12pinc_rk), by(wave) nq(4)    		// for heterogeneity analysis 
	egen l12pearnhh_rk = rank(l12pearnhh), by(wave) unique
	egen l12pearnhh_qt = xtile(l12pearnhh_rk), by(wave) nq(4) 	// for heterogeneity analysis 
	ebalance shock2 $covar_couple, targets(1) maxiter(100) gen(_webal)
	keep index wave _webal l12pinc_qt l12pearnhh_qt 
	save hl_di_f`yesno'_couple.dta, replace 
	
********************************************
** single ** 	
	
	use "hl_di_unmatched_single.dta", clear
	keep if female == `yesno'
	sort random	
	egen l12pinc_rk = rank(l12pinc), by(wave) unique
	egen l12pinc_qt = xtile(l12pinc_rk), by(wave) nq(4)
	egen l12pearnhh_rk = rank(l12pearnhh), by(wave) unique
	egen l12pearnhh_qt = xtile(l12pearnhh_rk), by(wave) nq(4) 	
	ebalance shock2 $covar_single, targets(1) maxiter(100) gen(_webal)	
	keep index wave _webal l12pinc_qt l12pearnhh_qt
	save hl_di_f`yesno'_single.dta, replace
}

use "hl_di_f0_couple.dta", clear
append using "hl_di_f1_couple.dta" "hl_di_f0_single.dta" "hl_di_f1_single.dta"

sort index wave
save "hl_di_matched.dta", replace
use "hl_di_unmatched.dta", clear

merge 1:1 index wave using "hl_di_matched.dta", keep(master match) nogen
gen smpl_main = 1
save "hl_di_main.dta", replace 

erase hl_di_f0_couple.dta 
erase hl_di_f1_couple.dta
erase hl_di_f0_single.dta
erase hl_di_f1_single.dta
erase hl_di_matched.dta
erase hl_di_unmatched.dta 
erase hl_di_unmatched_couple.dta
erase hl_di_unmatched_single.dta

keep if shock2 == 0
replace smpl_main = .
drop _webal
save "hl_di_ctrl.dta", replace 		// use same controls for all other specifications




*******************************************************************
** [2] re-employed **

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if cp == 1 & hasparintv == 1 & shock2_sp == 0 & pairs != 0 & l12empl == 1 & uempl == 0 & lejob2 != 0
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess) & !mi(d2skess_sp)
drop if samesex == 1
gen couplef = 1	
la var couplef "Couple file indicator"
keep if shock2 == 1 				// using treatment from new group
append using "hl_di_ctrl.dta"		// appending controls from the main specification
keep if couplef == 1				// keep 2-adult households only
save "hl_di_unmatched_couple_reemp.dta", replace

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if lp == 1 & noparintv == 1 & l12empl == 1 & uempl == 0 & lejob2 != 0 
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess)
gen singlef = 1
la var singlef "Single file indicator"
keep if shock2 == 1 				
append using "hl_di_ctrl.dta"		
keep if singlef == 1	
save "hl_di_unmatched_single_reemp.dta", replace

use "hl_di_unmatched_couple_reemp.dta", clear
append using "hl_di_unmatched_single_reemp.dta"
save "hl_di_unmatched_reemp.dta", replace


foreach yesno in 0 1 {
********************************************
** couple **

	use "hl_di_unmatched_couple_reemp.dta", clear
	keep if female == `yesno'
	sort random 	
	ebalance shock2 $covar_couple, targets(1) maxiter(100) gen(_webal)
	keep index wave _webal 
	save hl_di_f`yesno'_couple_reemp.dta, replace 
	
********************************************
** single ** 	
	
	use "hl_di_unmatched_single_reemp.dta", clear
	keep if female == `yesno'
	sort random
	ebalance shock2 $covar_single, targets(1) maxiter(100) gen(_webal)	
	keep index wave _webal 
	save hl_di_f`yesno'_single_reemp.dta, replace
}

use "hl_di_f0_couple_reemp.dta", clear
append using "hl_di_f1_couple_reemp.dta" "hl_di_f0_single_reemp.dta" "hl_di_f1_single_reemp.dta"

sort index wave
save "hl_di_matched_reemp.dta", replace
use "hl_di_unmatched_reemp.dta", clear
merge 1:1 index wave using "hl_di_matched_reemp.dta", keep(master match) nogen
gen smpl_reemp = 1
save "hl_di_reemp.dta", replace 

erase hl_di_f0_couple_reemp.dta 
erase hl_di_f1_couple_reemp.dta
erase hl_di_f0_single_reemp.dta
erase hl_di_f1_single_reemp.dta
erase hl_di_matched_reemp.dta
erase hl_di_unmatched_reemp.dta 
erase hl_di_unmatched_couple_reemp.dta
erase hl_di_unmatched_single_reemp.dta



*******************************************************************
** [3] unemployed & re-employed **

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if cp == 1 & hasparintv == 1 & shock2_sp == 0 & pairs != 0 & l12empl == 1 & (uempl == 1 | uempl == 0 | lejob2 == 0)
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess) & !mi(d2skess_sp)
drop if samesex == 1
gen couplef = 1	
la var couplef "Couple file indicator"
keep if shock2 == 1 				
append using "hl_di_ctrl.dta"		
keep if couplef == 1				
save "hl_di_unmatched_couple_unreemp.dta", replace

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if lp == 1 & noparintv == 1 & l12empl == 1 & (uempl == 1 | uempl == 0 | lejob2 == 0)
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess)
gen singlef = 1
la var singlef "Single file indicator"
keep if shock2 == 1 				
append using "hl_di_ctrl.dta"		
keep if singlef == 1			
save "hl_di_unmatched_single_unreemp.dta", replace

use "hl_di_unmatched_couple_unreemp.dta", clear
append using "hl_di_unmatched_single_unreemp.dta"
save "hl_di_unmatched_unreemp.dta", replace


foreach yesno in 0 1 {
********************************************
** couple **

	use "hl_di_unmatched_couple_unreemp.dta", clear
	keep if female == `yesno'
	sort random 	
	ebalance shock2 $covar_couple, targets(1) maxiter(100) gen(_webal)
	keep index wave _webal 
	save hl_di_f`yesno'_couple_unreemp.dta, replace 
	
********************************************
** single ** 	
	
	use "hl_di_unmatched_single_unreemp.dta", clear
	keep if female == `yesno'
	sort random
	ebalance shock2 $covar_single, targets(1) maxiter(100) gen(_webal)	
	keep index wave _webal 
	save hl_di_f`yesno'_single_unreemp.dta, replace
}

use "hl_di_f0_couple_unreemp.dta", clear
append using "hl_di_f1_couple_unreemp.dta" "hl_di_f0_single_unreemp.dta" "hl_di_f1_single_unreemp.dta"

sort index wave
save "hl_di_matched_unreemp.dta", replace
use "hl_di_unmatched_unreemp.dta", clear
merge 1:1 index wave using "hl_di_matched_unreemp.dta", keep(master match) nogen
gen smpl_unreemp = 1
save "hl_di_unreemp.dta", replace 

erase hl_di_f0_couple_unreemp.dta 
erase hl_di_f1_couple_unreemp.dta
erase hl_di_f0_single_unreemp.dta
erase hl_di_f1_single_unreemp.dta
erase hl_di_matched_unreemp.dta
erase hl_di_unmatched_unreemp.dta 
erase hl_di_unmatched_couple_unreemp.dta
erase hl_di_unmatched_single_unreemp.dta


*******************************************************************
** merge files **

use "hl_di_main.dta", clear
append using "hl_di_reemp.dta" "hl_di_unreemp.dta"
save "hl_di_emp.dta", replace

erase hl_di_reemp.dta
erase hl_di_unreemp.dta 





*********************************************************
**** regressions **** 

set more off
eststo clear

use "hl_di_emp.dta", clear

foreach spec in main reemp unreemp { 

local n = 0
foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
	local n = `n' + 1
	
	if inlist("`eq'", "cmf_p", "cm_p", "cf_p") {
	    local yvar "d2skess_sp"
	}
	else {
	    local yvar "d2skess"
	}
	
	if inlist("`eq'", "scmf_o", "scm_o", "scf_o") {
	    local xvar $covar_allhh
	}
	else if inlist("`eq'", "smf_o", "sm_o", "sf_o") {
	    local xvar $covar_single
	}
	else {
	    local xvar $covar_couple
	}
	 

	local cmif "smpl_`spec' == 1"		
	local treat "shock2"
	local weight "[weight = _webal]"
	
	* prevalence
	sum shock2 if ${ifcd_`eq'} & `cmif' 
	local rho`n' = r(mean)
	
	* separate regression 
	reg `yvar' `treat' `xvar' if ${ifcd_`eq'} & `cmif' `weight'
	eststo `eq'
	eststo `eq'_`spec'_pw, refresh
	count if `treat' == 1 & e(sample)
	scalar n_`eq' = r(N) 
}



* joint regression 
suest scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p, vce(cluster hhid)
eststo scmf_`spec'_n, refresh			// for number of observations
eststo scmf_`spec'_rho, refresh			// for prevalence
eststo scmf_`spec'_pw, refresh 			// without additional stats for column-wise tests and pairwise tests later
eststo scmf_`spec', refresh				// for tables (with only one layer of suest, similar to main results)
ws_pval `rho1' `rho2' `rho3' `rho4' `rho5' `rho6' `rho7' `rho8' `rho9' `rho10' `rho11' `rho12' scmf_`spec'
estadd scalar n_allt = n_scmf_o, replace: scmf_`spec'		// all treated (for later tables)
est save scmf_`spec', replace	



* number of observations 
mat define n_treat = (n_scmf_o, n_scm_o, n_scf_o, n_smf_o, n_sm_o, n_sf_o, n_cmf_o, n_cm_o, n_cf_o, n_cmf_p, n_cm_p, n_cf_p) 
mat colnames n_treat = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix n_treat = n_treat, replace: scmf_`spec'_n


* prevalence
mat define rho = (`rho1', `rho2', `rho3', `rho4', `rho5', `rho6', `rho7', `rho8', `rho9', `rho10', `rho11', `rho12')
mat colnames rho = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix rho = rho, replace: scmf_`spec'_rho
	
if "`spec'" == "main" {	
	mat colnames rho = rho1 rho2 rho3 rho4 rho5 rho6 rho7 rho8 rho9 rho10 rho11 rho12
	putexcel set table_emp_rho, replace
	putexcel A1 = matrix(rho), colnames
}
}


* cross-specification difference tests 
foreach spec in reemp unreemp {

	suest 								///
	scmf_o_main_pw scmf_o_`spec'_pw 	///
	scm_o_main_pw scm_o_`spec'_pw		///
	scf_o_main_pw scf_o_`spec'_pw		///
	smf_o_main_pw smf_o_`spec'_pw		///
	sm_o_main_pw sm_o_`spec'_pw			///
	sf_o_main_pw sf_o_`spec'_pw			///
	cmf_o_main_pw cmf_o_`spec'_pw		///
	cm_o_main_pw cm_o_`spec'_pw			///
	cf_o_main_pw cf_o_`spec'_pw			///
	cmf_p_main_pw cmf_p_`spec'_pw		///
	cm_p_main_pw cm_p_`spec'_pw			///
	cf_p_main_pw cf_p_`spec'_pw		

	foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
		test [`eq'_main_pw_mean]shock2 = [`eq'_`spec'_pw_mean]shock2
		scalar pval_`eq' = r(p)
	}

	test 															///
	([scmf_o_main_pw_mean]shock2 = [scmf_o_`spec'_pw_mean]shock2) 	/// 
	([scm_o_main_pw_mean]shock2 = [scm_o_`spec'_pw_mean]shock2) 	/// 
	([scf_o_main_pw_mean]shock2 = [scf_o_`spec'_pw_mean]shock2) 	/// 
	([smf_o_main_pw_mean]shock2 = [smf_o_`spec'_pw_mean]shock2) 	/// 
	([sm_o_main_pw_mean]shock2 = [sm_o_`spec'_pw_mean]shock2) 		/// 
	([sf_o_main_pw_mean]shock2 = [sf_o_`spec'_pw_mean]shock2) 		/// 
	([cmf_o_main_pw_mean]shock2 = [cmf_o_`spec'_pw_mean]shock2) 	///
	([cm_o_main_pw_mean]shock2 = [cm_o_`spec'_pw_mean]shock2) 		///
	([cf_o_main_pw_mean]shock2 = [cf_o_`spec'_pw_mean]shock2) 		///
	([cmf_p_main_pw_mean]shock2 = [cmf_p_`spec'_pw_mean]shock2) 	///
	([cm_p_main_pw_mean]shock2 = [cm_p_`spec'_pw_mean]shock2) 		///
	([cf_p_main_pw_mean]shock2 = [cf_p_`spec'_pw_mean]shock2) 	
	scalar pval_joint = r(p)

est restore scmf_`spec'_pw
mat define pval = (pval_scmf_o, pval_scm_o, pval_scf_o, pval_smf_o, pval_sm_o, pval_sf_o, pval_cmf_o, pval_cm_o, pval_cf_o, pval_cmf_p, pval_cm_p, pval_cf_p)
mat colnames pval = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix pval = pval, replace
estadd scalar pval_joint = pval_joint, replace
eststo scmf_`spec'_pw
est save scmf_`spec'_pw, replace
}






//---------------------------------------------------------------------------------//
//------------------------------ Table 2 (ebal_emp) -------------------------------// 
//--------------------------------- Main Results ----------------------------------//
//---------------------------------------------------------------------------------//

global tb "table_emp.tex"
global out "scmf_main scmf_main_n scmf_reemp scmf_reemp_n scmf_reemp_pw scmf_unreemp scmf_unreemp_n scmf_unreemp_pw"
global cell "cells(`"b(star pattern(1 0 1 0 0 1 0 0) fmt(2)) se(par pattern(1 0 1 0 0 1 0 0) fmt(2)) n_treat(pattern(0 1 0 1 0 0 1 0) fmt(0)) pval(pattern(0 0 0 0 1 0 0 1) fmt(2))"')"  
global ncol = 12
global reg_opt "collabels(none) mlabels(none) eqlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) style(tex) append"
global pval_opt "drop(*) extracols(3 6 9) collabels(none) mlabels(none) eqlabels(none) style(tex) append"


cap erase $tb

* all households 
estout $out using $tb, $cell keep(scmf_o_mean:shock2) varlabels(scmf_o_mean:shock2 "All displacement for workers (\$\hat{\gamma}_1$)") posthead(\multicolumn{$ncol}{l}{\textbf{All households}} \\) $reg_opt
estout $out using $tb, $cell keep(scm_o_mean:shock2) varlabels(scm_o_mean:shock2 "Male displacement for workers (\$\hat{\gamma}_2$)") $reg_opt 
estout $out using $tb, $cell keep(scf_o_mean:shock2) varlabels(scf_o_mean:shock2 "Female displacement for workers (\$\hat{\gamma}_3$)") $reg_opt
 

* 1-adult households
estout $out using $tb, $cell keep(smf_o_mean:shock2) varlabels(smf_o_mean:shock2 "All displacement for workers (\$\hat{\gamma}_4$)") posthead(\multicolumn{$ncol}{l}{\textbf{1-adult households}} \\) $reg_opt
estout $out using $tb, $cell keep(sm_o_mean:shock2) varlabels(sm_o_mean:shock2 "Male displacement for workers (\$\hat{\gamma}_5$)") $reg_opt
estout $out using $tb, $cell keep(sf_o_mean:shock2) varlabels(sf_o_mean:shock2 "Female displacement for workers (\$\hat{\gamma}_6$)") $reg_opt


* 2-adult households, workers
estout $out using $tb, $cell keep(cmf_o_mean:shock2) varlabels(cmf_o_mean:shock2 "All displacement for workers (\$\hat{\gamma}_7$)") posthead(\multicolumn{$ncol}{l}{\textbf{2-adult households}} \\) $reg_opt
estout $out using $tb, $cell keep(cm_o_mean:shock2) varlabels(cm_o_mean:shock2 "Male displacement for workers (\$\hat{\gamma}_8$)") $reg_opt
estout $out using $tb, $cell keep(cf_o_mean:shock2) varlabels(cf_o_mean:shock2 "Female displacement for workers (\$\hat{\gamma}_9$)") $reg_opt

* 2-adult households, partners 
estout $out using $tb, $cell keep(cmf_p_mean:shock2) varlabels(cmf_p_mean:shock2 "All displacement for partners (\$\hat{\gamma}_{10}$)") $reg_opt
estout $out using $tb, $cell keep(cm_p_mean:shock2) varlabels(cm_p_mean:shock2 "Male displacement for partners (\$\hat{\gamma}_{11}$)") $reg_opt
estout $out using $tb, $cell keep(cf_p_mean:shock2) varlabels(cf_p_mean:shock2 "Female displacement for partners (\$\hat{\gamma}_{12}$)") postfoot(\addlinespace \midrule) stat(pval_joint, fmt(2) labels("\addlinespace \$p$-value of joint difference across specifications")) $reg_opt


** p-value of difference **

* direct
estout $out using $tb, stat(pval_dir pval_dir_m pval_dir_f, fmt(2) labels("All, \$H_0: \gamma_4 = \gamma_7$" "Males, \$H_0: \gamma_5 = \gamma_8$" "Females, \$H_0: \gamma_6 = \gamma_9$")) prehead(\multicolumn{$ncol}{l}{\textbf{\$p$-value of difference within specification}} \\ \multicolumn{$ncol}{l}{\textit{Direct difference}} \\) $pval_opt

* average household
estout $out using $tb, stat(pval_ahh pval_ahh_m pval_ahh_f, fmt(2) labels("All, \$H_0: \rho_1 \gamma_4 = \rho_1 \gamma_7 + \rho_{10} \gamma_{10}$" "Males, \$H_0: \rho_2 \gamma_5 = \rho_2 \gamma_8 + \rho_{12} \gamma_{12}$" "Females, \$H_0: \rho_3 \gamma_6 = \rho_3 \gamma_9 + \rho_{11} \gamma_{11}$")) prehead(\addlinespace \multicolumn{$ncol}{l}{\textit{Household difference}} \\) $pval_opt
 
* overall household
estout $out using $tb, stat(pval_ohh pval_ohh_m pval_ohh_f, fmt(2) labels("All, \$H_0: \rho_4 \gamma_4 = \rho_7 \gamma_7 + \rho_{10} \gamma_{10}$" "Males, \$H_0: \rho_5 \gamma_5 = \rho_8 \gamma_8 + \rho_{12} \gamma_{12}$" "Females, \$H_0: \rho_6 \gamma_6 = \rho_9 \gamma_9 + \rho_{11} \gamma_{11}$")) prehead(\addlinespace \multicolumn{$ncol}{l}{\textit{Overall household difference including prevalence}} \\) $pval_opt

* gender
estout $out using $tb, stat(pval_gdr pval_gdr_s pval_gdr_c, fmt(2) labels("All, \$H_0: \gamma_2 = \gamma_3$"  "1-adult households, \$H_0: \gamma_5 = \gamma_6$" "2-adult households, \$H_0: \gamma_8 = \gamma_9$")) prehead(\addlinespace \multicolumn{$ncol}{l}{\textit{Gender difference}} \\) $pval_opt

* role
estout $out using $tb, stat(pval_role pval_role_m pval_role_f, fmt(2) labels("All, \$H_0: \gamma_7 = \gamma_{10}$"  "Males, \$H_0: \gamma_8 = \gamma_{12}$" "Females, \$H_0: \gamma_9 = \gamma_{11}$")) prehead(\addlinespace \multicolumn{$ncol}{l}{\textit{Role difference}} \\) $pval_opt


	

	
	
//-----------------------------------------------------------------------------------//
//------------------------------ Table C.1 in appendix ------------------------------// 
//-----------------------------------------------------------------------------------//



global tb "table_decomp.tex"
global out "scmf_main scmf_main_rho scmf_reemp scmf_reemp_rho scmf_unreemp scmf_unreemp_rho"
global cell "cells(`"b(star pattern(1 0 1 0 1 0) fmt(2)) se(par pattern(1 0 1 0 1 0) fmt(2)) rho(pattern(0 1 0 1 0 1) fmt(3))"')"  
global ncol = 10
global reg_opt "collabels(none) mlabels(none) eqlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) style(tex) append"
global pval_opt "drop(*) extracols(3 5 7) collabels(none) mlabels(none) eqlabels(none) style(tex) append"



cap erase $tb

* all households 
estout $out using $tb, $cell keep(scmf_o_mean:shock2) varlabels(scmf_o_mean:shock2 "All displacement for workers (\$\hat{\gamma}_1$)") posthead(\multicolumn{$ncol}{l}{\textbf{All households}} \\) $reg_opt
estout $out using $tb, $cell keep(scm_o_mean:shock2) varlabels(scm_o_mean:shock2 "Male displacement for workers (\$\hat{\gamma}_2$)") $reg_opt 
estout $out using $tb, $cell keep(scf_o_mean:shock2) varlabels(scf_o_mean:shock2 "Female displacement for workers (\$\hat{\gamma}_3$)") $reg_opt
 

* 1-adult households
estout $out using $tb, $cell keep(smf_o_mean:shock2) varlabels(smf_o_mean:shock2 "All displacement for workers (\$\hat{\gamma}_4$)") posthead(\multicolumn{$ncol}{l}{\textbf{1-adult households}} \\) $reg_opt
estout $out using $tb, $cell keep(sm_o_mean:shock2) varlabels(sm_o_mean:shock2 "Male displacement for workers (\$\hat{\gamma}_5$)") $reg_opt
estout $out using $tb, $cell keep(sf_o_mean:shock2) varlabels(sf_o_mean:shock2 "Female displacement for workers (\$\hat{\gamma}_6$)") $reg_opt


* 2-adult households, workers
estout $out using $tb, $cell keep(cmf_o_mean:shock2) varlabels(cmf_o_mean:shock2 "All displacement for workers (\$\hat{\gamma}_7$)") posthead(\multicolumn{$ncol}{l}{\textbf{2-adult households}} \\) $reg_opt
estout $out using $tb, $cell keep(cm_o_mean:shock2) varlabels(cm_o_mean:shock2 "Male displacement for workers (\$\hat{\gamma}_8$)") $reg_opt
estout $out using $tb, $cell keep(cf_o_mean:shock2) varlabels(cf_o_mean:shock2 "Female displacement for workers (\$\hat{\gamma}_9$)") $reg_opt


* 2-adult households, partners 
estout $out using $tb, $cell keep(cmf_p_mean:shock2) varlabels(cmf_p_mean:shock2 "All displacement for partners (\$\hat{\gamma}_{10}$)") $reg_opt
estout $out using $tb, $cell keep(cm_p_mean:shock2) varlabels(cm_p_mean:shock2 "Male displacement for partners (\$\hat{\gamma}_{11}$)") $reg_opt
estout $out using $tb, $cell keep(cf_p_mean:shock2) varlabels(cf_p_mean:shock2 "Female displacement for partners (\$\hat{\gamma}_{12}$)") postfoot(\addlinespace \midrule) $reg_opt


** p-value of difference **

* average household
estout $out using $tb, stat(pval_ahh pval_ahh_m pval_ahh_f, fmt(2) labels("All, \$H_0: \rho_1 \gamma_4 = \rho_1 \gamma_7 + \rho_{10} \gamma_{10}$" "Males, \$H_0: \rho_2 \gamma_5 = \rho_2 \gamma_8 + \rho_{12} \gamma_{12}$" "Females, \$H_0: \rho_3 \gamma_6 = \rho_3 \gamma_9 + \rho_{11} \gamma_{11}$")) prehead(\multicolumn{$ncol}{l}{\textbf{\$p$-value of difference within specification}} \\ \multicolumn{$ncol}{l}{\textit{Household difference in burden and risk sharing}} \\) $pval_opt
 
* overall household
estout $out using $tb, stat(pval_ohh pval_ohh_m pval_ohh_f, fmt(2) labels("All, \$H_0: \rho_4 \gamma_4 = \rho_7 \gamma_7 + \rho_{10} \gamma_{10}$" "Males, \$H_0: \rho_5 \gamma_5 = \rho_8 \gamma_8 + \rho_{12} \gamma_{12}$" "Females, \$H_0: \rho_6 \gamma_6 = \rho_9 \gamma_9 + \rho_{11} \gamma_{11}$")) prehead(\addlinespace \multicolumn{$ncol}{l}{\textit{Overall household difference including prevalence}} \\) posthead(\addlinespace) $pval_opt






** decomposition **

foreach spec in main reemp unreemp {
	est use scmf_`spec'
	local n = 0
	foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
		local n = `n' + 1	
		sum shock2 if ${ifcd_`eq'} & smpl_`spec' == 1
		local rho`n' = r(mean) 
		local gamma`n' = [`eq'_mean]shock2		
}	
	local burd_mf = `rho7'*`gamma4' - `rho7'*`gamma7' - `rho10'*`gamma10'
	local prev_mf = `rho4'*`gamma4' - `rho7'*`gamma4'	
	local sburd_mf = `burd_mf' / (`burd_mf' + `prev_mf')
	local sprev_mf = `prev_mf' / (`burd_mf' + `prev_mf')
	
	local burd_m = `rho8'*`gamma5' - `rho8'*`gamma8' - `rho12'*`gamma12'				
	local prev_m = `rho5'*`gamma5' - `rho8'*`gamma5'								
	local sburd_m = `burd_m' / (`burd_m' + `prev_m')
	local sprev_m = `prev_m' / (`burd_m' + `prev_m')

	local burd_f = `rho9'*`gamma6'- `rho9'*`gamma9' - `rho11'*`gamma11' 
	local prev_f = `rho6'*`gamma6' - `rho9'*`gamma6'
	local sburd_f = `burd_f' / (`burd_f' + `prev_f')
	local sprev_f = `prev_f' / (`burd_f' + `prev_f')
	
	mat define decomp_`spec' = (`sburd_mf' \ `sburd_m' \ `sburd_f' \ `sprev_mf' \ `sprev_m' \ `sprev_f')
}


mat define burdprev = (decomp_main, decomp_reemp, decomp_unreemp)
estout matrix(burdprev, fmt(2)) using "table_decomp_burdprev.tex", varlabels(r1 "All, burden- and risk-sharing effect" r2 "Males, burden- and risk-sharing effect" r3 "Females, burden- and risk-sharing effect" r4 "All, prevalence effect"  r5 "Males, prevalence effect"  r6 "Females, prevalence effect") extracols(2 2 3 3 5 5) collabels(none) mlabels(none) eqlabels(none) style(tex) replace  // append to table_decomp




