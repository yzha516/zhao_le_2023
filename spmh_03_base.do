
/********************************************************************************************************/
/* BASELINE ESTIMATES */
/********************************************************************************************************/


**********************************************************************
** [6] placebo regression **

use "hl_di_full.dta", clear 

replace l2d2skess = l2d2ssfmh if l2d2skess == . & l2d2ssfmh != .	// fill missing with the other MH measure in the survey (o/w will encounter convergence issues)
replace l2d2skess_sp = l2d2ssfmh_sp if l2d2skess_sp == . & l2d2ssfmh_sp != .
replace csize = 6 if csize == 7										// for convergence, company size now censored at 5000 instead of 20000 

save "hl_di_full_pcbo.dta", replace 

global covar_couple_pcbo age female noneng ghbp ghgh ghpf ghre ghrp ghsf ghvt pinc nevuemp yrpaid i.csize jobsec i.occ1da i.ind1da slf1 slf2 slf3 hrswk casual pearnhh iedusec eduuni eduvoc ndepchc1 ndepchc2 ndepchc3 ndepchcg3 avgurp i.remar i.state slei snbh decdis dececr decedo cohab  btqia btqia_sp ib9.wave age_sp noneng_sp ghbp_sp ghgh_sp ghpf_sp ghre_sp ghrp_sp ghsf_sp ghvt_sp pinc_sp nevuemp_sp ib1.wstat_sp iedusec_sp eduuni_sp eduvoc_sp age2 ghbp2 ghgh2  ghpf2 ghre2 ghrp2 ghsf2 ghvt2 jobsec2 hrswk2 pearnhh2 avgurp2 slei2 snbh2 decdis2 dececr2 decedo2 age_sp2 ghbp_sp2 ghgh_sp2  ghpf_sp2 ghre_sp2 ghrp_sp2 ghsf_sp2 ghvt_sp2 pinc_sp2  // took out pinc2 as did not converge, ib9.wave because main regressions have base wave 9 (for suest)

global covar_single_pcbo age female noneng ghbp ghgh ghpf ghre ghrp ghsf ghvt pinc nevuemp yrpaid i.csize jobsec i.occ1da i.ind1da slf1 slf2 slf3 hrswk casual iedusec eduuni eduvoc ndepchc1 ndepchc2 ndepchc3 ndepchcg3 avgurp i.remar i.state slei snbh decdis dececr decedo lg btqia ib9.wave age2 ghbp2 ghgh2 ghpf2 ghre2 ghrp2 ghsf2 ghvt2 pinc2 jobsec2 hrswk2 avgurp2 slei2 snbh2 decdis2 dececr2 decedo2	

global covar_allhh_pcbo age female noneng ghbp ghgh ghpf ghre ghrp ghsf ghvt pinc nevuemp yrpaid i.csize jobsec i.occ1da i.ind1da slf1 slf2 slf3 hrswk casual pearnhh iedusec eduuni eduvoc ndepchc1 ndepchc2 ndepchc3 ndepchcg3 avgurp i.remar i.state slei snbh decdis dececr decedo cohab lg btqia ib9.wave age2 ghbp2 ghgh2  ghpf2 ghre2 ghrp2 ghsf2 ghvt2 pinc2 jobsec2 hrswk2 pearnhh2 avgurp2 slei2 snbh2 decdis2 dececr2 decedo2   


use "hl_di_full_pcbo.dta", clear 
keep if age >= 18 & age <= 65
keep if cp == 1 & hasparintv == 1 & shock2_sp == 0 & pairs != 0 & l12empl == 1 & (uempl == 1 | lejob2 == 0) 
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess) & !mi(d2skess_sp)
drop if samesex == 1		
gen couplef = 1	
la var couplef "Couple file indicator"
save "hl_di_unmatched_couple_pcbo.dta", replace

use "hl_di_full_pcbo.dta", clear 
keep if age >= 18 & age <= 65
keep if lp == 1 & noparintv == 1 & l12empl == 1 & (uempl == 1 | lejob2 == 0)
keep if inlist(wave, 9,11,13,15,17,19) & !mi(d2skess)
gen singlef = 1
la var singlef "Single file indicator"
save "hl_di_unmatched_single_pcbo.dta", replace

use "hl_di_unmatched_couple_pcbo.dta", clear
append using "hl_di_unmatched_single_pcbo.dta"
save "hl_di_unmatched_pcbo.dta", replace


foreach yesno in 0 1 {
********************************************
** couple **

	use "hl_di_unmatched_couple_pcbo.dta", clear
	keep if female == `yesno'		
	sort random 
	ebalance shock2 $covar_couple_pcbo, targets(1) maxiter(100) 	
	keep index wave _webal 
	save hl_di_f`yesno'_couple_pcbo.dta, replace
	
	
********************************************
** single ** 	
	
	use "hl_di_unmatched_single_pcbo.dta", clear
	keep if female == `yesno'
	sort random
	ebalance shock2 $covar_single_pcbo, targets(1) maxiter(100)
	keep index wave _webal 
	save hl_di_f`yesno'_single_pcbo.dta, replace
}

use "hl_di_f0_couple_pcbo.dta", clear
append using "hl_di_f1_couple_pcbo.dta" "hl_di_f0_single_pcbo.dta" "hl_di_f1_single_pcbo.dta"
sort index wave
save "hl_di_matched_pcbo.dta", replace

use "hl_di_unmatched_pcbo.dta", clear
merge 1:1 index wave using "hl_di_matched_pcbo.dta", keep(master match) nogen
merge 1:1 index wave using "hl_di_main.dta", keep(match) keepusing(index wave) nogen	// keep obs matched with main spec
gen smpl_pcbo = 1																		// to append later
save "hl_di_pcbo.dta", replace 

erase hl_di_f0_couple_pcbo.dta 
erase hl_di_f1_couple_pcbo.dta
erase hl_di_f0_single_pcbo.dta
erase hl_di_f1_single_pcbo.dta
erase hl_di_matched_pcbo.dta
erase hl_di_unmatched_pcbo.dta 
erase hl_di_unmatched_couple_pcbo.dta
erase hl_di_unmatched_single_pcbo.dta



*******************************************************************
** merge files **
use "hl_di_main.dta", clear
append using "hl_di_pcbo.dta"
save "hl_di_base.dta", replace

erase hl_di_pcbo.dta 


*********************************************************
**** regressions **** 

set more off 
eststo clear
use "hl_di_base.dta", clear


forvalues i = 1 / 6 {

local n = 0
foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
	local n = `n' + 1

	if inlist("`eq'", "cmf_p", "cm_p", "cf_p") {
	    if `i' == 2 {
		    local yvar "skess_sp"
		}
		else if `i' == 6 {
		    local yvar "l2d2skess_sp"
		}
		else {
		    local yvar "d2skess_sp"
		}
	}
	else {
	    if `i' == 2 {
		    local yvar "skess"
		}
		else if `i' == 6 {
		    local yvar "l2d2skess"
		}
		else {
		    local yvar "d2skess"
		}	    
	}
	
	if inlist("`eq'", "scmf_o", "scm_o", "scf_o") {
	    if `i' == 6 {
			local xvar $covar_allhh_pcbo	
		}
		else {
		    local xvar $covar_allhh
		}
	}
	else if inlist("`eq'", "smf_o", "sm_o", "sf_o") {
	    if `i' == 6 {
			local xvar $covar_single_pcbo	
		}
		else {
		    local xvar $covar_single
		}
	}
	else {
	    if `i' == 6 {
			local xvar $covar_couple_pcbo		
		}
		else {
		    local xvar $covar_couple
		}
	}
	
	if `i' == 6 {
	    local cmif "smpl_pcbo == 1"
	}
	else {
	    local cmif "smpl_main == 1"
	}
	local treat "shock2"
	local weight "[weight = _webal]"
	
	
	* prevalence
	sum shock2 if ${ifcd_`eq'} & `cmif' 
	local rho`n' = r(mean)
	
	
	* separate regression 
	if `i' == 1 | `i' == 6 {  
		reg `yvar' `treat' `xvar' if ${ifcd_`eq'} & `cmif' `weight'	   
	}
	else if `i' == 2 {
	    reg `yvar' `treat' if ${ifcd_`eq'} & `cmif'
	}
	else if `i' == 3 {
	    reg `yvar' `treat' if ${ifcd_`eq'} & `cmif'						// yvar is different here
	}
	else if `i' == 4 {
	    reg `yvar' `treat' `xvar' if ${ifcd_`eq'} & `cmif'
	}
	else {
	    reg `yvar' `treat' if ${ifcd_`eq'} & `cmif' `weight'
	}

	eststo `eq'
	eststo `eq'_`i'_pw, refresh
	count if `treat' == 1 & e(sample)
	scalar n_`eq' = r(N) 
}



* joint regression 
suest scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p, vce(cluster hhid)
eststo scmf_`i'_pw, refresh 			// without additional stats for column-wise tests and pairwise tests later
eststo scmf_`i', refresh				// for tables (with only one layer of suest, similar to main results)
ws_pval `rho1' `rho2' `rho3' `rho4' `rho5' `rho6' `rho7' `rho8' `rho9' `rho10' `rho11' `rho12' scmf_`i'
estadd scalar n_allt = n_scmf_o, replace: scmf_`i'		// all treated (for later tables)

}



* cross-specification difference tests 
foreach i in 6 {
   
	suest 						///
	scmf_o_1_pw scmf_o_`i'_pw 	///
	scm_o_1_pw scm_o_`i'_pw		///
	scf_o_1_pw scf_o_`i'_pw		///
	smf_o_1_pw smf_o_`i'_pw		///
	sm_o_1_pw sm_o_`i'_pw		///
	sf_o_1_pw sf_o_`i'_pw		///
	cmf_o_1_pw cmf_o_`i'_pw		///
	cm_o_1_pw cm_o_`i'_pw		///
	cf_o_1_pw cf_o_`i'_pw		///
	cmf_p_1_pw cmf_p_`i'_pw		///
	cm_p_1_pw cm_p_`i'_pw		///
	cf_p_1_pw cf_p_`i'_pw		

	foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
		test [`eq'_1_pw_mean]shock2 = [`eq'_`i'_pw_mean]shock2
		scalar pval_`eq' = r(p)
	}

	test 														///
	([scmf_o_1_pw_mean]shock2 = [scmf_o_`i'_pw_mean]shock2) 	/// 
	([scm_o_1_pw_mean]shock2 = [scm_o_`i'_pw_mean]shock2) 		/// 
	([scf_o_1_pw_mean]shock2 = [scf_o_`i'_pw_mean]shock2) 		/// 
	([smf_o_1_pw_mean]shock2 = [smf_o_`i'_pw_mean]shock2) 		/// 
	([sm_o_1_pw_mean]shock2 = [sm_o_`i'_pw_mean]shock2) 		/// 
	([sf_o_1_pw_mean]shock2 = [sf_o_`i'_pw_mean]shock2) 		/// 
	([cmf_o_1_pw_mean]shock2 = [cmf_o_`i'_pw_mean]shock2) 		///
	([cm_o_1_pw_mean]shock2 = [cm_o_`i'_pw_mean]shock2) 		///
	([cf_o_1_pw_mean]shock2 = [cf_o_`i'_pw_mean]shock2) 		///
	([cmf_p_1_pw_mean]shock2 = [cmf_p_`i'_pw_mean]shock2) 		///
	([cm_p_1_pw_mean]shock2 = [cm_p_`i'_pw_mean]shock2) 		///
	([cf_p_1_pw_mean]shock2 = [cf_p_`i'_pw_mean]shock2) 	
	scalar pval_joint = r(p)

est restore scmf_`i'_pw
mat define pval = (pval_scmf_o, pval_scm_o, pval_scf_o, pval_smf_o, pval_sm_o, pval_sf_o, pval_cmf_o, pval_cm_o, pval_cf_o, pval_cmf_p, pval_cm_p, pval_cf_p)
mat colnames pval = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix pval = pval, replace
estadd scalar pval_joint = pval_joint, replace
eststo scmf_`i'_pw
est save scmf_`i'_pw, replace	    

}




//------------------------------------------------------------------------------------//
//------------------------------ Table D.1 (table_base) ------------------------------// 
//--------------------------------- Baseline Results ---------------------------------//
//------------------------------------------------------------------------------------//

global tb "table_base.tex"
global out "scmf_1 scmf_2 scmf_3 scmf_4 scmf_5 scmf_6 scmf_6_pw"
global cell "cells(`"b(star pattern(1 1 1 1 1 1 0) fmt(2)) se(par pattern(1 1 1 1 1 1 0) fmt(2)) pval(pattern(0 0 0 0 0 0 1) fmt(2))"')"  
global ncol = 14
global reg_opt "collabels(none) mlabels(none) eqlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) style(tex) append"
global pval_opt "drop(*) extracols(2 3 4 5 6 7) collabels(none) mlabels(none) eqlabels(none) style(tex) append"


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
