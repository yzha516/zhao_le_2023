
/********************************************************************************************************/
/* TYPES OF EMPLOYMENT EVENTS */
/********************************************************************************************************/

***********************************************************
** [4] anticipated unemployment (not shock or voluntary) **

use "hl_di_full.dta", clear
keep if age >= 18 & age <= 65
keep if cp == 1 & hasparintv == 1 & atcue2_sp == 0 & pairs != 0 & l12empl == 1 & (uempl == 1 | lejob2 == 0) 
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess) & !mi(d2skess_sp)
drop if samesex == 1
gen couplef = 1	
save "hl_di_unmatched_couple_atc.dta", replace

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if lp == 1 & noparintv == 1 & l12empl == 1 & (uempl == 1 | lejob2 == 0)
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess)
gen singlef = 1
save "hl_di_unmatched_single_atc.dta", replace

use "hl_di_unmatched_couple_atc.dta", clear
append using "hl_di_unmatched_single_atc.dta"
save "hl_di_unmatched_atc.dta", replace


foreach yesno in 0 1 {
********************************************
** couple **

	use "hl_di_unmatched_couple_atc.dta", clear
	keep if female == `yesno'
	sort random 	
	ebalance atcue2 $covar_couple, targets(1) maxiter(100)  gen(_webal) 
	keep index wave _webal 
	save hl_di_f`yesno'_couple_atc.dta, replace 
	
********************************************
** single ** 	
	
	use "hl_di_unmatched_single_atc.dta", clear
	keep if female == `yesno'
	sort random
	ebalance atcue2 $covar_single, targets(1) maxiter(100) gen(_webal) 	
	keep index wave _webal 
	save hl_di_f`yesno'_single_atc.dta, replace
}

use "hl_di_f0_couple_atc.dta", clear
append using "hl_di_f1_couple_atc.dta" "hl_di_f0_single_atc.dta" "hl_di_f1_single_atc.dta"

sort index wave
save "hl_di_matched_atc.dta", replace
use "hl_di_unmatched_atc.dta", clear
merge 1:1 index wave using "hl_di_matched_atc.dta", keep(master match) nogen

qui gen smpl_atcue2 = 1
la var smpl_atcue2 "Anticipated unemployment sample indicator"
save "hl_di_atc.dta", replace

erase hl_di_f0_couple_atc.dta 
erase hl_di_f1_couple_atc.dta
erase hl_di_f0_single_atc.dta
erase hl_di_f1_single_atc.dta
erase hl_di_matched_atc.dta
erase hl_di_unmatched_atc.dta 
erase hl_di_unmatched_couple_atc.dta
erase hl_di_unmatched_single_atc.dta	


*********************************************************
** [5] voluntary unemployment **

use "hl_di_full.dta", clear 

keep if age >= 18 & age <= 65
keep if cp == 1 & hasparintv == 1 & volue2_sp == 0 & pairs != 0 & l12empl == 1 & (uempl == 1 | lejob2 == 0) 
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess) & !mi(d2skess_sp)
drop if samesex == 1
gen couplef = 1
save "hl_di_unmatched_couple_vol.dta", replace

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if lp == 1 & noparintv == 1 & l12empl == 1 & (uempl == 1 | lejob2 == 0)
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess)
gen singlef = 1
save "hl_di_unmatched_single_vol.dta", replace

use "hl_di_unmatched_couple_vol.dta", clear
append using "hl_di_unmatched_single_vol.dta"
save "hl_di_unmatched_vol.dta", replace

		
foreach yesno in 0 1 {
********************************************
** couple **

	use "hl_di_unmatched_couple_vol.dta", clear
	keep if female == `yesno'
	sort random 	
	ebalance volue2 $covar_couple, targets(1) maxiter(100) gen(_webal) 
	keep index wave _webal 
	save hl_di_f`yesno'_couple_vol.dta, replace 
	
********************************************
** single ** 	
	
	use "hl_di_unmatched_single_vol.dta", clear
	keep if female == `yesno'
	sort random
	ebalance volue2 $covar_single, targets(1) maxiter(100) gen(_webal) 
	keep index wave _webal 
	save hl_di_f`yesno'_single_vol.dta, replace
}


use "hl_di_f0_couple_vol.dta", clear
append using "hl_di_f1_couple_vol.dta" "hl_di_f0_single_vol.dta" "hl_di_f1_single_vol.dta"

sort index wave
save "hl_di_matched_vol.dta", replace

use "hl_di_unmatched_vol.dta", clear
merge 1:1 index wave using "hl_di_matched_vol.dta", keep(master match) nogen

qui gen smpl_volue2 = 1
la var smpl_volue2 "Voluntary unemployment sample indicator"
save "hl_di_vol.dta", replace

erase hl_di_f0_couple_vol.dta 
erase hl_di_f1_couple_vol.dta
erase hl_di_f0_single_vol.dta
erase hl_di_f1_single_vol.dta
erase hl_di_matched_vol.dta
erase hl_di_unmatched_vol.dta 
erase hl_di_unmatched_couple_vol.dta
erase hl_di_unmatched_single_vol.dta	



*********************************************************
** [6] all unemployment **

* merge the types 
use "hl_di_main.dta", clear
qui gen smpl_shock2 = 1
la var smpl_shock2 "Job displacement file indicator"
merge 1:1 index index_sp wave using "hl_di_atc.dta", gen(merge_atc)
merge 1:1 index index_sp wave using "hl_di_vol.dta", gen(merge_vol)
gen uempl2 = 0
replace uempl2 = 1 if (shock2 == 1 & smpl_shock2 == 1) | (volue2 == 1 & smpl_volue2 == 1) | (atcue2 == 1 & smpl_atcue2 == 1) 			
la var uempl2 "All types of unemployment indicator"
drop _webal
save "hl_di_ue.dta", replace

use "hl_di_ue.dta", clear
keep if couplef == 1
save "hl_di_unmatched_couple_ue.dta", replace

use "hl_di_ue.dta", clear 
keep if singlef == 1
save "hl_di_unmatched_single_ue.dta", replace

use "hl_di_unmatched_couple_ue.dta", clear
append using "hl_di_unmatched_single_ue.dta"
save "hl_di_unmatched_ue.dta", replace

foreach yesno in 0 1 {
********************************************
** couple **

	use "hl_di_unmatched_couple_ue.dta", clear
	keep if female == `yesno'
	sort random 	
	ebalance uempl2 $covar_couple, targets(1) maxiter(100) gen(_webal) 
	keep index wave _webal 
	save hl_di_f`yesno'_couple_ue.dta, replace 
	
********************************************
** single ** 	
	
	use "hl_di_unmatched_single_ue.dta", clear
	keep if female == `yesno'
	sort random
	ebalance uempl2 $covar_single, targets(1) maxiter(100) gen(_webal) 
	keep index wave _webal 
	save hl_di_f`yesno'_single_ue.dta, replace
}

use "hl_di_f0_couple_ue.dta", clear
append using "hl_di_f1_couple_ue.dta" "hl_di_f0_single_ue.dta" "hl_di_f1_single_ue.dta"

sort index wave
save "hl_di_matched_ue.dta", replace
use "hl_di_unmatched_ue.dta", clear
merge 1:1 index wave using "hl_di_matched_ue.dta", keep(master match) nogen

qui gen smpl_uempl2 = 1
la var smpl_uempl2 "All types of unemployment sample indicator"
count if uempl2 == 1
count if (shock2 == 1 & smpl_shock2 == 1) | (volue2 == 1 & smpl_volue2 == 1) | (atcue2 == 1 & smpl_atcue2 == 1)		// sum check
replace smpl_shock2 = 0
replace smpl_atcue2 = 0
replace smpl_volue2 = 0
save "hl_di_ue.dta", replace

erase hl_di_f0_couple_ue.dta 
erase hl_di_f1_couple_ue.dta
erase hl_di_f0_single_ue.dta
erase hl_di_f1_single_ue.dta
erase hl_di_matched_ue.dta
erase hl_di_unmatched_ue.dta 
erase hl_di_unmatched_couple_ue.dta
erase hl_di_unmatched_single_ue.dta
	




*********************************************************
** [7] job change **

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if cp == 1 & hasparintv == 1 & lejob2_sp == 0 & pairs != 0 & l12empl == 1 & (uempl == 1 | shock2 == 0) 
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess) & !mi(d2skess_sp)
drop if samesex == 1
gen couplef = 1
replace lejob2 = 0 if shock2 == 1 | atcue2 == 1 | volue2 == 1 
save "hl_di_unmatched_couple_chg.dta", replace

use "hl_di_full.dta", clear 
keep if age >= 18 & age <= 65
keep if lp == 1 & noparintv == 1 & l12empl == 1 & (uempl == 1 | shock2 == 0)
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess)
gen singlef = 1
replace lejob2 = 0 if shock2 == 1 | atcue2 == 1 | volue2 == 1 
save "hl_di_unmatched_single_chg.dta", replace

use "hl_di_unmatched_couple_chg.dta", clear
append using "hl_di_unmatched_single_chg.dta"
save "hl_di_unmatched_chg.dta", replace


foreach yesno in 0 1 {
********************************************
** couple **

	use "hl_di_unmatched_couple_chg.dta", clear
	keep if female == `yesno'
	sort random 	
	ebalance lejob2 $covar_couple, targets(1) maxiter(100) gen(_webal) 
	keep index wave _webal 
	save hl_di_f`yesno'_couple_chg.dta, replace 
	
********************************************
** single ** 	
	
	use "hl_di_unmatched_single_chg.dta", clear
	keep if female == `yesno'
	sort random
	ebalance lejob2 $covar_single, targets(1) maxiter(100) gen(_webal) 
	keep index wave _webal 
	save hl_di_f`yesno'_single_chg.dta, replace
}

use "hl_di_f0_couple_chg.dta", clear
append using "hl_di_f1_couple_chg.dta" "hl_di_f0_single_chg.dta" "hl_di_f1_single_chg.dta"

sort index wave
save "hl_di_matched_chg.dta", replace
use "hl_di_unmatched_chg.dta", clear
merge 1:1 index wave using "hl_di_matched_chg.dta", keep(master match) nogen

qui gen smpl_lejob2 = 1
la var smpl_lejob2 "Job change sample indicator"
save "hl_di_chg.dta", replace

erase hl_di_f0_couple_chg.dta 
erase hl_di_f1_couple_chg.dta
erase hl_di_f0_single_chg.dta
erase hl_di_f1_single_chg.dta
erase hl_di_matched_chg.dta
erase hl_di_unmatched_chg.dta 
erase hl_di_unmatched_couple_chg.dta
erase hl_di_unmatched_single_chg.dta


*********************************************************
** merge unemployment with job change **

use "hl_di_main.dta", clear
gen smpl_shock2 = 1
append using "hl_di_atc.dta" "hl_di_vol.dta" "hl_di_ue.dta" "hl_di_chg.dta"
gen treat2 = 0
foreach event in shock2 atcue2 volue2 uempl2 lejob2 {
	replace treat2 = 1 if `event' == 1 & smpl_`event' == 1
}	
drop shock2
rename treat2 shock2
save "hl_di_type.dta", replace

erase hl_di_atc.dta
erase hl_di_vol.dta
erase hl_di_ue.dta
erase hl_di_chg.dta

 


*********************************************************
**** regressions **** 


set more off 
eststo clear 
foreach save in scmf_main {
	est use `save'
	eststo `save'
}

use "hl_di_type.dta", clear


foreach event in shock2 atcue2 volue2 uempl2 lejob2 {   // shock2 reestimated because of change of dataset, o/w cannot suest [1] and alternative specifications
   
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
	 

	local cmif "smpl_`event' == 1"		
	local treat "shock2"
	local weight "[weight = _webal]"
	
	* prevalence
	sum shock2 if ${ifcd_`eq'} & `cmif' 
	local rho`n' = r(mean)		
	
	* separate regression 
	reg `yvar' `treat' `xvar' if ${ifcd_`eq'} & `cmif' `weight'
	eststo `eq', refresh
	eststo `eq'_`event'_pw, refresh
	count if `treat' == 1 & e(sample)
	scalar n_`eq' = r(N) 
}


* joint regression 
suest scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p, vce(cluster hhid)
eststo scmf_`event'_n, refresh				// for number of observations
eststo scmf_`event'_pw, refresh 			// without additional stats for column-wise tests and pairwise tests later
eststo scmf_`event', refresh				// for tables (with only one layer of suest, similar to main results)
ws_pval `rho1' `rho2' `rho3' `rho4' `rho5' `rho6' `rho7' `rho8' `rho9' `rho10' `rho11' `rho12' scmf_`event'
estadd scalar n_allt = n_scmf_o, replace: scmf_`event'		// all treated (for smaller table)


* number of observations 
mat define n_treat = (n_scmf_o, n_scm_o, n_scf_o, n_smf_o, n_sm_o, n_sf_o, n_cmf_o, n_cm_o, n_cf_o, n_cmf_p, n_cm_p, n_cf_p) 
mat colnames n_treat = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix n_treat = n_treat, replace: scmf_`event'_n

}




* cross-specification difference tests 
foreach event in atcue2 volue2 uempl2 lejob2 {

	suest 									///
	scmf_o_shock2_pw scmf_o_`event'_pw 		///
	scm_o_shock2_pw scm_o_`event'_pw		///
	scf_o_shock2_pw scf_o_`event'_pw		///
	smf_o_shock2_pw smf_o_`event'_pw		///
	sm_o_shock2_pw sm_o_`event'_pw			///
	sf_o_shock2_pw sf_o_`event'_pw			///
	cmf_o_shock2_pw cmf_o_`event'_pw		///
	cm_o_shock2_pw cm_o_`event'_pw			///
	cf_o_shock2_pw cf_o_`event'_pw			///
	cmf_p_shock2_pw cmf_p_`event'_pw		///
	cm_p_shock2_pw cm_p_`event'_pw			///
	cf_p_shock2_pw cf_p_`event'_pw		

	foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
		test [`eq'_shock2_pw_mean]shock2 = [`eq'_`event'_pw_mean]shock2
		scalar pval_`eq' = r(p)
	}

	test 																///
	([scmf_o_shock2_pw_mean]shock2 = [scmf_o_`event'_pw_mean]shock2) 	/// 
	([scm_o_shock2_pw_mean]shock2 = [scm_o_`event'_pw_mean]shock2) 		/// 
	([scf_o_shock2_pw_mean]shock2 = [scf_o_`event'_pw_mean]shock2) 		/// 
	([smf_o_shock2_pw_mean]shock2 = [smf_o_`event'_pw_mean]shock2) 		/// 
	([sm_o_shock2_pw_mean]shock2 = [sm_o_`event'_pw_mean]shock2) 		/// 
	([sf_o_shock2_pw_mean]shock2 = [sf_o_`event'_pw_mean]shock2) 		/// 
	([cmf_o_shock2_pw_mean]shock2 = [cmf_o_`event'_pw_mean]shock2) 		///
	([cm_o_shock2_pw_mean]shock2 = [cm_o_`event'_pw_mean]shock2) 		///
	([cf_o_shock2_pw_mean]shock2 = [cf_o_`event'_pw_mean]shock2) 		///
	([cmf_p_shock2_pw_mean]shock2 = [cmf_p_`event'_pw_mean]shock2) 		///
	([cm_p_shock2_pw_mean]shock2 = [cm_p_`event'_pw_mean]shock2) 		///
	([cf_p_shock2_pw_mean]shock2 = [cf_p_`event'_pw_mean]shock2) 	
	scalar pval_joint = r(p)

est restore scmf_`event'_pw
mat define pval = (pval_scmf_o, pval_scm_o, pval_scf_o, pval_smf_o, pval_sm_o, pval_sf_o, pval_cmf_o, pval_cm_o, pval_cf_o, pval_cmf_p, pval_cm_p, pval_cf_p)
mat colnames pval = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix pval = pval, replace
estadd scalar pval_joint = pval_joint, replace
eststo scmf_`event'_pw
est save scmf_`event'_pw, replace
}






//---------------------------------------------------------------------------------//
//---------------------------- Table 3 (table_type) -------------------------------// 
//---------------------------------------------------------------------------------//

global tb "table_type.tex"		
global out "scmf_main scmf_atcue2 scmf_atcue2_pw scmf_volue2 scmf_volue2_pw scmf_uempl2 scmf_uempl2_pw scmf_lejob2 scmf_lejob2_pw"
global cell "cells(`"b(star pattern(1 1 0 1 0 1 0 1 0) fmt(2)) se(par pattern(1 1 0 1 0 1 0 1 0) fmt(2)) pval(pattern(0 0 1 0 1 0 1 0 1) fmt(2))"')"  
global ncol = 15
global reg_opt "collabels(none) mlabels(none) eqlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) style(tex) append"
global pval_opt "drop(*) extracols(2 3 5 7 9) collabels(none) mlabels(none) eqlabels(none) style(tex) append"


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
estout $out using $tb, $cell keep(cf_p_mean:shock2) varlabels(cf_p_mean:shock2 "Female displacement for partners (\$\hat{\gamma}_{12}$)") postfoot(\addlinespace \midrule) stat(n_allt pval_joint, fmt(0 2) labels("\addlinespace $\mathrm{N_{Treated}}$" "\$p$-value of joint difference across specifications")) $reg_opt


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


 

** figure (not presented) ** 

set scheme Cleanplots
coefplot (scmf_main, label(Main specification) msymbol(O) mcolor(gs0) mfcolor(gs16) msize(small) ciopts(lcolor(gs0)) offset(1/2)) (scmf_atcue2, label(Anticipated displacement) msymbol(D) mcolor(gs9) msize(small) ciopts(lcolor(gs9)) offset(1/4)) (scmf_volue2, label(Voluntary unemployment) msymbol(S) mcolor(gs5) mfcolor(gs16) msize(small) ciopts(lcolor(gs5)) offset(0)) (scmf_uempl2, label(All unemployment) msymbol(T) mcolor(gs11) msize(small) ciopts(lcolor(gs11)) offset(-1/4)) (scmf_lejob2, label(Job change) msymbol(X) mcolor(gs4) msize(medium) ciopts(lcolor(gs4)) offset(-1/2)), keep(sm_o_mean:shock2 sf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2) coeflabels(sm_o_mean:shock2 = "Male displacement for workers" sf_o_mean:shock2 = "Female displacement for workers" cm_o_mean:shock2 = "Male displacement for workers" cf_o_mean:shock2 = "Female displacement for workers" cm_p_mean:shock2 = "Male displacement for partners"  cf_p_mean:shock2 = "Female displacement for partners", wrap(20)) xline(0) headings(sm_o_mean:shock2 = "{bf:1-adult households}" cm_o_mean:shock2 = "{bf:2-adult households}", nogap) noeqlabels plotregion(margin(vsmall)) xsize(8) eqlabels(,gap(1)) levels(90) yscale(r(0.5 13.75)) 
 
graph export "figure_type.eps", replace