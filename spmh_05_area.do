
/********************************************************************************************************/
/* DOMAINS OF WELL-BEING */
/********************************************************************************************************/

* renamed from "areas of mental health"

set more off 
eststo clear 
est use scmf_main
eststo scmf_main
use "hl_di_main.dta", clear


foreach y in skess sdepmd smotag sfatig sworth sanxty { 

local n = 0
foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
	local n = `n' + 1
	
	if inlist("`eq'", "cmf_p", "cm_p", "cf_p") {
	    local yvar "d2`y'_sp"
	}
	else {
	    local yvar "d2`y'"
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
	
	local treat "shock2"
	local weight "[weight = _webal]"
	
	* prevalence
	sum shock2 if ${ifcd_`eq'}   
	local rho`n' = r(mean)
	
	* separate regression 
	reg `yvar' `treat' `xvar' if ${ifcd_`eq'}  `weight' 
	eststo `eq'
	eststo `eq'_`y'_pw, refresh
	count if `treat' == 1 & e(sample)
	scalar n_`eq' = r(N) 
}



* joint regression 
suest scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p, vce(cluster hhid)
eststo scmf_`y'_pw, refresh 							// without additional stats for column-wise tests and pairwise tests later
eststo scmf_`y', refresh								// for tables (with only one layer of suest, similar to main results)
ws_pval `rho1' `rho2' `rho3' `rho4' `rho5' `rho6' `rho7' `rho8' `rho9' `rho10' `rho11' `rho12' scmf_`y'
estadd scalar n_allt = n_scmf_o, replace: scmf_`y'		// all treated (for later tables)
}


* cross-specification difference tests 
foreach y in sdepmd smotag sfatig sworth sanxty {

	suest 								///
	scmf_o_skess_pw scmf_o_`y'_pw 		///
	scm_o_skess_pw scm_o_`y'_pw			///
	scf_o_skess_pw scf_o_`y'_pw			///
	smf_o_skess_pw smf_o_`y'_pw			///
	sm_o_skess_pw sm_o_`y'_pw			///
	sf_o_skess_pw sf_o_`y'_pw			///
	cmf_o_skess_pw cmf_o_`y'_pw			///
	cm_o_skess_pw cm_o_`y'_pw			///
	cf_o_skess_pw cf_o_`y'_pw			///
	cmf_p_skess_pw cmf_p_`y'_pw			///
	cm_p_skess_pw cm_p_`y'_pw			///
	cf_p_skess_pw cf_p_`y'_pw		

	foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
		test [`eq'_skess_pw_mean]shock2 = [`eq'_`y'_pw_mean]shock2
		scalar pval_`eq' = r(p)
	}

	test 															///
	([scmf_o_skess_pw_mean]shock2 = [scmf_o_`y'_pw_mean]shock2) 	/// 
	([scm_o_skess_pw_mean]shock2 = [scm_o_`y'_pw_mean]shock2) 		/// 
	([scf_o_skess_pw_mean]shock2 = [scf_o_`y'_pw_mean]shock2) 		/// 
	([smf_o_skess_pw_mean]shock2 = [smf_o_`y'_pw_mean]shock2) 		/// 
	([sm_o_skess_pw_mean]shock2 = [sm_o_`y'_pw_mean]shock2) 		/// 
	([sf_o_skess_pw_mean]shock2 = [sf_o_`y'_pw_mean]shock2) 		/// 
	([cmf_o_skess_pw_mean]shock2 = [cmf_o_`y'_pw_mean]shock2) 		///
	([cm_o_skess_pw_mean]shock2 = [cm_o_`y'_pw_mean]shock2) 		///
	([cf_o_skess_pw_mean]shock2 = [cf_o_`y'_pw_mean]shock2) 		///
	([cmf_p_skess_pw_mean]shock2 = [cmf_p_`y'_pw_mean]shock2) 		///
	([cm_p_skess_pw_mean]shock2 = [cm_p_`y'_pw_mean]shock2) 		///
	([cf_p_skess_pw_mean]shock2 = [cf_p_`y'_pw_mean]shock2) 	
	scalar pval_joint = r(p)

est restore scmf_`y'_pw
mat define pval = (pval_scmf_o, pval_scm_o, pval_scf_o, pval_smf_o, pval_sm_o, pval_sf_o, pval_cmf_o, pval_cm_o, pval_cf_o, pval_cmf_p, pval_cm_p, pval_cf_p)
mat colnames pval = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix pval = pval, replace
estadd scalar pval_joint = pval_joint, replace
eststo scmf_`y'_pw
est save scmf_`y'_pw, replace
}



*********************************************************
**** output **** 

** table (not reported) **

global tb "table_area.tex"
global out "scmf_sdepmd scmf_sdepmd_pw scmf_smotag scmf_smotag_pw scmf_sfatig scmf_sfatig_pw scmf_sworth scmf_sworth_pw scmf_sanxty scmf_sanxty_pw"
global cell "cells(`"b(star pattern(1 0 1 0 1 0 1 0 1 0) fmt(2)) se(par pattern(1 0 1 0 1 0 1 0 1 0) fmt(2)) pval(pattern(0 1 0 1 0 1 0 1 0 1) fmt(2))"')"   
global ncol = 16
global reg_opt "collabels(none) mlabels(none) eqlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) style(tex) append"
global pval_opt "drop(*) extracols(2 4 6 8 10) collabels(none) mlabels(none) eqlabels(none) style(tex) append"


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




//---------------------------------------------------------------------------------//
//---------------------------- Figure 2 (figure_area) -----------------------------// 
//---------------------------------------------------------------------------------//

graph set window fontface "Times New Roman"

coefplot (scmf_main, label(Main specification) msymbol(O) mcolor(gs0) mfcolor(gs16) msize(small) ciopts(lcolor(gs0)) offset(5/10)) (scmf_sdepmd, label(Depressed mood) msymbol(D) mcolor(gs9) msize(small) ciopts(lcolor(gs9)) offset(3/10)) (scmf_smotag, label(Motor agitation) msymbol(S) mcolor(gs5) mfcolor(gs16) msize(small) ciopts(lcolor(gs5)) offset(1/10)) (scmf_sfatig, label(Fatigue) msymbol(T) mcolor(gs11) msize(small) ciopts(lcolor(gs11)) offset(-1/10)) (scmf_sworth, label(Worthless guilt) msymbol(X) mcolor(gs4) msize(medium) ciopts(lcolor(gs4)) offset(-3/10)) (scmf_sanxty, label(Anxiety) msymbol(|) mcolor(gs9) msize(medium) ciopts(lcolor(gs9)) offset(-5/10)), keep(sm_o_mean:shock2 sf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2) coeflabels(sm_o_mean:shock2 = "Male displacement for workers" sf_o_mean:shock2 = "Female displacement for workers" cm_o_mean:shock2 = "Male displacement for workers" cf_o_mean:shock2 = "Female displacement for workers" cm_p_mean:shock2 = "Male displacement for partners" cf_p_mean:shock2 = "Female displacement for partners", wrap(20)) xline(0) headings(sm_o_mean:shock2 = "{bf:1-adult households}" cm_o_mean:shock2 = "{bf:2-adult households}", nogap) noeqlabels plotregion(margin(vsmall)) xscale(r(-3 12)) xlabel(-3(3)12) xsize(8) eqlabels(,gap(1)) levels(90) yscale(r(0.5 13.75)) 
graph export "figure_area.eps", replace

graph set window fontface default

