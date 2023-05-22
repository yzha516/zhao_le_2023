
/********************************************************************************************************/
/* HETEROGENEITY ANALYSIS */
/********************************************************************************************************/


set more off 
eststo clear

use "hl_di_main.dta", clear


*********************************************************
**** regressions **** 

** covariates **

foreach het in eduhigh remar { 
	
	** covariates ** 
	if "`het'" == "eduhigh" {
		global minus l12eduuni l12iedusec					// variables to remove
		local cd0 "l12eduuni == 0 & l12iedusec == 0"		// high school or lower
		local cd1 "l12eduuni == 1 | l12iedusec == 1"		// diploma or above
	} 	
	else {
		global minus l12remar
		local cd0 "l12remar == 0"							// remote area
		local cd1 "l12remar != 0"
	}
	foreach hh in allhh single couple {
		global covar_`hh'_het: list global(covar_`hh') - global(minus)		// new lists
	}
	
	
	** regressions **

	* all households
	local cat 0 1
	
	foreach i of local cat {
		
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
				local xvar $covar_allhh_het
			}
			else if inlist("`eq'", "smf_o", "sm_o", "sf_o") {
				local xvar $covar_single_het
			}
			else {
				local xvar $covar_couple_het
			}
			
			local cmif "`cd`i''"
			local treat "shock2"
			local weight "[weight = _webal]"
			
			* prevalence
			sum shock2 if ${ifcd_`eq'} & `cmif'
			local rho`n' = r(mean)
			
			* separate regression 
			reg `yvar' `treat' `xvar' if ${ifcd_`eq'} & `cmif' `weight' 
			eststo `eq'
			eststo `eq'_`het'`i'_pw, refresh
			count if `treat' == 1 & e(sample)
			scalar n_`eq' = r(N) 
			}		

		* joint regression 
		suest scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p, vce(cluster hhid)	
		eststo scmf_`het'`i'_pw, refresh 							// without additional stats for column-wise tests and pairwise tests later
		eststo scmf_`het'`i', refresh								// for tables (with only one layer of suest, similar to main results)
		ws_pval `rho1' `rho2' `rho3' `rho4' `rho5' `rho6' `rho7' `rho8' `rho9' `rho10' `rho11' `rho12' scmf_`het'`i'
		estadd scalar n_allt = n_scmf_o, replace: scmf_`het'`i'
		
	}
			
}



* cross-specification difference tests 
foreach het in eduhigh remar {

	suest 									///
	scmf_o_`het'0_pw scmf_o_`het'1_pw 		///
	scm_o_`het'0_pw scm_o_`het'1_pw			///
	scf_o_`het'0_pw scf_o_`het'1_pw			///
	smf_o_`het'0_pw smf_o_`het'1_pw			///
	sm_o_`het'0_pw sm_o_`het'1_pw			///
	sf_o_`het'0_pw sf_o_`het'1_pw			///
	cmf_o_`het'0_pw cmf_o_`het'1_pw			///
	cm_o_`het'0_pw cm_o_`het'1_pw			///
	cf_o_`het'0_pw cf_o_`het'1_pw			///
	cmf_p_`het'0_pw cmf_p_`het'1_pw			///
	cm_p_`het'0_pw cm_p_`het'1_pw			///
	cf_p_`het'0_pw cf_p_`het'1_pw		
	

	foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
		test [`eq'_`het'0_pw_mean]shock2 = [`eq'_`het'1_pw_mean]shock2
		scalar pval_`eq' = r(p)
	}

	test 																///
	([scmf_o_`het'0_pw_mean]shock2 = [scmf_o_`het'1_pw_mean]shock2) 	/// 
	([scm_o_`het'0_pw_mean]shock2 = [scm_o_`het'1_pw_mean]shock2) 		/// 
	([scf_o_`het'0_pw_mean]shock2 = [scf_o_`het'1_pw_mean]shock2) 		/// 
	([smf_o_`het'0_pw_mean]shock2 = [smf_o_`het'1_pw_mean]shock2) 		/// 
	([sm_o_`het'0_pw_mean]shock2 = [sm_o_`het'1_pw_mean]shock2) 		/// 
	([sf_o_`het'0_pw_mean]shock2 = [sf_o_`het'1_pw_mean]shock2) 		/// 
	([cmf_o_`het'0_pw_mean]shock2 = [cmf_o_`het'1_pw_mean]shock2) 		///
	([cm_o_`het'0_pw_mean]shock2 = [cm_o_`het'1_pw_mean]shock2) 		///
	([cf_o_`het'0_pw_mean]shock2 = [cf_o_`het'1_pw_mean]shock2) 		///
	([cmf_p_`het'0_pw_mean]shock2 = [cmf_p_`het'1_pw_mean]shock2) 		///
	([cm_p_`het'0_pw_mean]shock2 = [cm_p_`het'1_pw_mean]shock2) 		///
	([cf_p_`het'0_pw_mean]shock2 = [cf_p_`het'1_pw_mean]shock2) 	
	scalar pval_joint = r(p)

est restore scmf_`het'1_pw
mat define pval = (pval_scmf_o, pval_scm_o, pval_scf_o, pval_smf_o, pval_sm_o, pval_sf_o, pval_cmf_o, pval_cm_o, pval_cf_o, pval_cmf_p, pval_cm_p, pval_cf_p)
mat colnames pval = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix pval = pval, replace
estadd scalar pval_joint = pval_joint, replace
eststo scmf_`het'1_pw
est save scmf_`het'1_pw, replace
}




//---------------------------------------------------------------------------------//
//------------------------------ Table 4 (table_het) ------------------------------// 
//---------------------------------------------------------------------------------//

global tb "table_het.tex"
global out "scmf_eduhigh0 scmf_eduhigh1 scmf_eduhigh1_pw scmf_remar0 scmf_remar1 scmf_remar1_pw"
global cell "cells(`"b(star pattern(1 1 0 1 1 0) fmt(2)) se(par pattern(1 1 0 1 1 0) fmt(2)) pval(pattern(0 0 1 0 0 1) fmt(2))"')"
global ncol = 11
global reg_opt "collabels(none) mlabels(none) eqlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) style(tex) append"
global pval_opt "drop(*) extracols(2 4 5 7) collabels(none) mlabels(none) eqlabels(none) style(tex) append"


** mental health outcomes ** 

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





