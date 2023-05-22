
/********************************************************************************************************/
/* HETEROGENEITY BY EARNINGS */
/********************************************************************************************************/


set more off 
eststo clear

use "hl_di_main.dta", clear


***********************************************************
**** regressions ****

** covariates **

local n = 0
foreach earn in pinc pearnhh { 
	local n = `n' + 1
	
	** covariates ** 
	if "`earn'" == "pinc" {
		global minus l12pinc l12pinc2
		local cd1 "l12pinc_qt == 1"							// quartile of labor earnings, Q1 = lowest
		local cd2 "l12pinc_qt == 2"
		local cd3 "l12pinc_qt == 3"
		local cd4 "l12pinc_qt == 4"		
	}
	else {
		global minus l12pearnhh
		local cd1 "l12pearnhh_qt == 1"						// quartile of % earned in HH, Q1 = lowest
		local cd2 "l12pearnhh_qt == 2"
		local cd3 "l12pearnhh_qt == 3"
		local cd4 "l12pearnhh_qt == 4"			
	}

	foreach hh in allhh couple single {
		global covar_`hh'_earn: list global(covar_`hh') - global(minus)		// new lists
	}
	
	
	** regressions **

	* all households
	local cat 1 2 3 4
	
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
				local xvar $covar_allhh_earn
			}
			else if inlist("`eq'", "smf_o", "sm_o", "sf_o") {
				local xvar $covar_single_earn
			}
			else {
				local xvar $covar_couple_earn
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
			eststo `eq'_`earn'`i'_pw, refresh
			count if `treat' == 1 & e(sample)
			scalar n_`eq' = r(N) 
			}		

		* joint regression 
		suest scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p, vce(cluster hhid)	
		eststo scmf_`earn'`i'1_pw, refresh 							// without additional stats for column-wise tests and pairwise tests later
		eststo scmf_`earn'`i'2_pw, refresh 							// for pairwise tests; not all saved results will be recalled
		eststo scmf_`earn'`i'3_pw, refresh 		
		eststo scmf_`earn'`i'4_pw, refresh 			
		eststo scmf_`earn'`i', refresh								// for tables (with only one layer of suest, similar to main results)
		ws_pval `rho1' `rho2' `rho3' `rho4' `rho5' `rho6' `rho7' `rho8' `rho9' `rho10' `rho11' `rho12' scmf_`earn'`i'
		estadd scalar n_allt = n_scmf_o, replace: scmf_`earn'`i'
		

		
		* save results
		foreach eqs in sm_o sf_o cm_o cf_o cm_p cf_p {				// equations to save
			scalar `eqs'_`earn'_qt`i' = [`eqs'_mean]shock2
			scalar `eqs'_`earn'_qt`i'_sel = [`eqs'_mean]shock2 - invnormal(0.95) * [`eqs'_mean]_se[shock2]
			scalar `eqs'_`earn'_qt`i'_seu = [`eqs'_mean]shock2 + invnormal(0.95) * [`eqs'_mean]_se[shock2]
			
			if `i' == 1 {
				mat `eqs'_`earn' = `eqs'_`earn'_qt`i'
				mat `eqs'_`earn'_sel = `eqs'_`earn'_qt`i'_sel
				mat `eqs'_`earn'_seu = `eqs'_`earn'_qt`i'_seu				
			} 
			else {
				mat `eqs'_`earn' = (`eqs'_`earn', `eqs'_`earn'_qt`i')
				mat `eqs'_`earn'_sel = (`eqs'_`earn'_sel, `eqs'_`earn'_qt`i'_sel)
				mat `eqs'_`earn'_seu = (`eqs'_`earn'_seu, `eqs'_`earn'_qt`i'_seu)						
			}
		}		
	}
}





* cross-specification difference tests 
foreach earn in pinc pearnhh {
	
	local max = 4 
	forval i = 1/ `=`max'-1' {
		forval j = `=`i' + 1'/`max' { // loop through combinations
		
	suest 											///
	scmf_o_`earn'`i'_pw scmf_o_`earn'`j'_pw 		///
	scm_o_`earn'`i'_pw scm_o_`earn'`j'_pw			///
	scf_o_`earn'`i'_pw scf_o_`earn'`j'_pw			///
	smf_o_`earn'`i'_pw smf_o_`earn'`j'_pw			///
	sm_o_`earn'`i'_pw sm_o_`earn'`j'_pw				///
	sf_o_`earn'`i'_pw sf_o_`earn'`j'_pw				///
	cmf_o_`earn'`i'_pw cmf_o_`earn'`j'_pw			///
	cm_o_`earn'`i'_pw cm_o_`earn'`j'_pw				///
	cf_o_`earn'`i'_pw cf_o_`earn'`j'_pw				///
	cmf_p_`earn'`i'_pw cmf_p_`earn'`j'_pw			///
	cm_p_`earn'`i'_pw cm_p_`earn'`j'_pw				///
	cf_p_`earn'`i'_pw cf_p_`earn'`j'_pw		
	

	foreach eq in scmf_o scm_o scf_o smf_o sm_o sf_o cmf_o cm_o cf_o cmf_p cm_p cf_p {
		test [`eq'_`earn'`i'_pw_mean]shock2 = [`eq'_`earn'`j'_pw_mean]shock2
		scalar pval_`eq' = r(p)
	}

	test 																		///
	([scmf_o_`earn'`i'_pw_mean]shock2 = [scmf_o_`earn'`j'_pw_mean]shock2) 		/// 
	([scm_o_`earn'`i'_pw_mean]shock2 = [scm_o_`earn'`j'_pw_mean]shock2) 		/// 
	([scf_o_`earn'`i'_pw_mean]shock2 = [scf_o_`earn'`j'_pw_mean]shock2) 		/// 
	([smf_o_`earn'`i'_pw_mean]shock2 = [smf_o_`earn'`j'_pw_mean]shock2) 		/// 
	([sm_o_`earn'`i'_pw_mean]shock2 = [sm_o_`earn'`j'_pw_mean]shock2) 			/// 
	([sf_o_`earn'`i'_pw_mean]shock2 = [sf_o_`earn'`j'_pw_mean]shock2) 			/// 
	([cmf_o_`earn'`i'_pw_mean]shock2 = [cmf_o_`earn'`j'_pw_mean]shock2) 		///
	([cm_o_`earn'`i'_pw_mean]shock2 = [cm_o_`earn'`j'_pw_mean]shock2) 			///
	([cf_o_`earn'`i'_pw_mean]shock2 = [cf_o_`earn'`j'_pw_mean]shock2) 			///
	([cmf_p_`earn'`i'_pw_mean]shock2 = [cmf_p_`earn'`j'_pw_mean]shock2) 		///
	([cm_p_`earn'`i'_pw_mean]shock2 = [cm_p_`earn'`j'_pw_mean]shock2) 			///
	([cf_p_`earn'`i'_pw_mean]shock2 = [cf_p_`earn'`j'_pw_mean]shock2) 	
	scalar pval_joint = r(p)


est restore scmf_`earn'`i'`j'_pw
mat define pval = (pval_scmf_o, pval_scm_o, pval_scf_o, pval_smf_o, pval_sm_o, pval_sf_o, pval_cmf_o, pval_cm_o, pval_cf_o, pval_cmf_p, pval_cm_p, pval_cf_p)
mat colnames pval = scmf_o_mean:shock2 scm_o_mean:shock2 scf_o_mean:shock2 smf_o_mean:shock2 sm_o_mean:shock2 sf_o_mean:shock2 cmf_o_mean:shock2 cm_o_mean:shock2 cf_o_mean:shock2 cmf_p_mean:shock2 cm_p_mean:shock2 cf_p_mean:shock2
estadd matrix pval = pval, replace
estadd scalar pval_joint = pval_joint, replace
eststo scmf_`earn'`i'`j'_pw
est save scmf_`earn'`i'`j'_pw, replace
		}
	}
}






*********************************************************
** output **

** table (not reported) **

foreach earn in pinc pearnhh {
	global tb "table_earn_`earn'.tex"
	if "`earn'" == "pinc" {
		global out "scmf_pinc1 scmf_pinc2 scmf_pinc3 scmf_pinc4 scmf_pinc12_pw scmf_pinc13_pw scmf_pinc14_pw scmf_pinc23_pw scmf_pinc24_pw scmf_pinc34_pw"	
	}
	else {
		global out "scmf_pearnhh1 scmf_pearnhh2 scmf_pearnhh3 scmf_pearnhh4 scmf_pearnhh12_pw scmf_pearnhh13_pw scmf_pearnhh14_pw scmf_pearnhh23_pw scmf_pearnhh24_pw scmf_pearnhh34_pw"		
	}
	
	global cell "cells(`"b(star pattern(1 1 1 1 0 0 0 0 0 0) fmt(2)) se(par pattern(1 1 1 1 0 0 0 0 0 0) fmt(2)) pval(pattern(0 0 0 0 1 1 1 1 1 1) fmt(2))"')"  
	global ncol = 11
	global reg_opt "collabels(none) mlabels(none) eqlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) style(tex) append"
	global pval_opt "drop(*) collabels(none) mlabels(none) eqlabels(none) style(tex) append"
	

	cap erase $tb

	* all households 
	estout $out using $tb, $cell keep(scmf_o_mean:shock2) varlabels(scmf_o_mean:shock2 "All displacement for workers (\$\hat{\gamma}_1$)") posthead(\multicolumn{$ncol}{l}{\textbf{All households}} \\) collabels(none) mlabels("Q1" "Q2" "Q3" "Q4" "Q1_Q2" "Q1_Q3" "Q1_Q4" "Q2_Q3" "Q2_Q4" "Q3_Q4") eqlabels(none) starlevels(* 0.1 ** 0.05 *** 0.01) style(tex) append
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
	
}




//---------------------------------------------------------------------------------//
//----------------------------- Figure E.1 in appendix ----------------------------// 
//---------------------------------------------------------------------------------//

* l12pinc (earnings from main job)
foreach eq in sm_o sf_o cm_o cf_o cm_p cf_p {
	if "`eq'" == "sm_o" {
		local subtitle "{bf:a.} Male displacement for workers"
	}	
	else if "`eq'" == "sf_o" {
		local subtitle "{bf:b.} Female displacement for workers"
	}
	else if "`eq'" == "cm_o" {
		local subtitle "{bf:c.} Male displacement for workers"
	}
	else if "`eq'" == "cf_o" {
		local subtitle "{bf:d.} Female displacement for workers"
	}	
	else if "`eq'" == "cm_p" {
		local subtitle "{bf:e.} Male displacement for partners"
	}
	else {
		local subtitle "{bf:f.} Female displacement for partners"
	}
	coefplot matrix(`eq'_pinc), ci((`eq'_pinc_sel `eq'_pinc_seu)) ciopts(recast(rconnected) lcolor(gray) lpattern(dash) mcolor(gray) msymbol(smcircle)) vertical recast(connected) lcolor(black) yline(0, lpattern(dot) lcolor(black) lwidth(medthick)) mcolor(black) msymbol(smcircle) grid(none) graphregion(color(white)) bgcolor(white) coeflabels(c1="Low" c2="Below-median" c3="Above-median" c4="High", labsize(small) wrap(6)) xtitle("") ytitle("") yscale(r(-4 10)) ylabel(-4(2)10, nogrid labsize(small)) title(`subtitle') saving(fqt_pinc_`eq', replace) 	// c1="Q1" c2="Q2" c3="Q3" c4="Q4"
}

graph combine fqt_pinc_sm_o.gph fqt_pinc_sf_o.gph, title({bf:1-adult households}) saving(fqt_pinc_1ad, replace) 

graph combine fqt_pinc_cm_o.gph fqt_pinc_cf_o.gph, title({bf:2-adult households}) saving(fqt_pinc_2adm, replace)

graph combine fqt_pinc_cm_p.gph fqt_pinc_cf_p.gph, saving(fqt_pinc_2adf, replace) 

graph combine fqt_pinc_1ad.gph fqt_pinc_2adm.gph fqt_pinc_2adf.gph, rows(3) b1(Labor earnings quartile, size(small)) l1(Mental distress, size(small)) ysize(8) xsize(6.15)
graph export "figure_pinc.eps", as(eps) name("Graph") replace

erase fqt_pinc_sm_o.gph 
erase fqt_pinc_sf_o.gph
erase fqt_pinc_cm_o.gph 
erase fqt_pinc_cm_p.gph
erase fqt_pinc_cf_o.gph 
erase fqt_pinc_cf_p.gph
erase fqt_pinc_1ad.gph
erase fqt_pinc_2adm.gph
erase fqt_pinc_2adf.gph


//---------------------------------------------------------------------------------//
//----------------------------- Figure E.2 in appendix ----------------------------// 
//---------------------------------------------------------------------------------//

* l12pearnhh (income share)
foreach eq in cm_o cf_o cm_p cf_p {
	if "`eq'" == "cm_o" {
		local subtitle "{bf:a.} Male displacement for workers"
	}
	else if "`eq'" == "cf_o" {
		local subtitle "{bf:b.} Female displacement for workers"
	}
	else if "`eq'" == "cm_p" {
		local subtitle "{bf:c.} Male displacement for partners"
	}	
	else {
		local subtitle "{bf:d.} Female displacement for partners"
	}
	coefplot matrix(`eq'_pearnhh), ci((`eq'_pearnhh_sel `eq'_pearnhh_seu)) ciopts(recast(rconnected) lcolor(gray) lpattern(dash) mcolor(gray) msymbol(smcircle)) vertical recast(connected) lcolor(black) yline(0, lpattern(dot) lcolor(black) lwidth(medthick)) mcolor(black) msymbol(smcircle) grid(none) graphregion(color(white)) bgcolor(white) coeflabels(c1="Low" c2="Below-median" c3="Above-median" c4="High", labsize(small) wrap(6)) xtitle("") ytitle("") yscale(r(-4 10)) ylabel(-4(2)10, nogrid labsize(small)) title(`subtitle') saving(fqt_pearnhh_`eq', replace) 	
}

graph combine fqt_pearnhh_cm_o.gph fqt_pearnhh_cf_o.gph fqt_pearnhh_cm_p.gph fqt_pearnhh_cf_p.gph, rows(2) title({bf:2-adult households},size(medium)) b1(Household income share quartile quartile, size(small)) l1(Mental distress, size(small)) ysize(5.33) xsize(6.15)
graph export "figure_pearnhh.eps", as(eps) name("Graph") replace

erase fqt_pearnhh_cm_o.gph 
erase fqt_pearnhh_cm_p.gph 
erase fqt_pearnhh_cf_o.gph 
erase fqt_pearnhh_cf_p.gph





