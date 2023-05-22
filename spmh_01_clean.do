


/********************************************************************************************************/
/* DATA CLEANING */
/********************************************************************************************************/

* Note: not all variables that are generated have been used in the analysis.


//------------------------- within wave subset and transform  -------------------------//

******** labour force indicator ********

local varstokeep esbrd esdtl
foreach w in a b c d e f g h i j k l m n o p q r s {
	 local rawdir "XX"			// this is the hilda directory
	 use "`rawdir'\stata_c\Combined_`w'190c", clear
if ("`varstokeep'"!="") {
	local tokeep                                 								
	foreach var of local varstokeep {            								
		capture confirm variable `w'`var'        								
		if (!_rc) local tokeep `tokeep' `w'`var' 								
		}
	keep xwaveid `tokeep' 														
    }
save "h`w'_filter", replace
}
use "`rawdir'\stata_ehr\longitudinal_weights_s190c"
keep xwaveid
sort xwaveid
foreach w in a b c d e f g h i j k l m n o p q r s {
	merge 1:1 xwaveid using "h`w'_filter", nogen
}
save "h_lf_filter", replace

egen nevilf = rowtotal(*esbrd)													
egen hmkr = rowtotal(*esdtl)													
gen lfft = 1
replace lfft = 0 if nevilf == 51 | hmkr == 102									
gen nevemp = inlist(nevilf, 34, 51)  											
gen nevuemp = (nevilf == 17)													
keep xwaveid nevemp nevuemp
save "h_lf_filter", replace


******** variable selection and transformation ********

mat cpi = (74.5, 76.6, 78.6, 80.6, 82.6, 85.9, 87.7, 91.6, 92.9, 95.8, 99.2, 100.4, 102.8, 105.9, 107.5, 108.6, 110.7, 113, 114.8)	// cpi, 2001 to 2019 adjusted to 2012 levels

forvalues i = 7/19 {															
local w = word(c(alpha), `i')
	 local rawdir "XX"		// again, the hilda directory
	 use "`rawdir'\stata_c\Combined_`w'190c", clear
renpfix `w'	
di "wave `i'"

isvar xwaveid hhpxid hhidate hhwte hhmsr hhsra hhra hhstate xhhraid edhigh1 anlote lssexor			///
hhd0_4 hhd5_9 hhd1014 hhd1524 mrclc ordf mrcms mrcurr hehtyp hhfty jbmcnt jbcasab nlmact ancobn		///
esbrd ehtujyr hhura wscmg hiwscei ujljt ehtjbyr jbemlyr ehtjbyr ujljtyr ujljtwk						///
jttrwrk jbcasab jbmhruc	jbmi61 pjoi61 pjoti61 ujlji61 jbmo61 jbmo62 pjoo61 pjoto61 ujljo61			///
jbmemsz nlreajt losateo	jspsuit	jbmssec pjljr pjljrea pjorea pjotrea ujljrea 						///
jbmii2 pjoii2 pjotii2 ujljii2 jbmi62 pjoi62 pjoti62 ujlji62	es jbhruc oifsupi						///
pdk10s pdk10rc pddepr pdeff pdenerv pderles pdhless pdnerv pdrless pdsad pdtired pdwless hecpmhp 	///
bmi lspact lsdrex gh1 ghpf ghrp ghbp ghgh ghvt ghsf ghre ghmh gh9b gh9c gh9d gh9f gh9h ghrht 		///
lefrd* lemar lesep lercl leprg lebth leins leinf ledsc ledrl ledfr 									///
levio lepcm lejls lejlf lertr lejob leprm lefni lefnw lemvd											///
losatnl losatlc hhda10 hhec10 hhed10 xpyhltp xpyphi xpyphrm	xpphia losat losatyh ujwku ujyru 		/// 
chksdw nrhave mschgdv mschgmr mschgrs mschgsp			

keep `r(varlist)'																
	
	** NA
	isvar xwaveid hhpxid xhhraid hhidate	 									
	ds `r(varlist)', not														
	foreach v of var `r(varlist)' {
		replace `v' = . if `v' < 0
	}																			
	
merge 1:1 xwaveid using "`rawdir'\stata_ehr\Master_s190c", keepusing(sex yob yodeath) nogen  		     
merge 1:1 xwaveid using "`rawdir'\stata_ehr\longitudinal_weights_s190c", keepusing(wlec_s) nogen  		 
gen wave = `i' 																	
merge 1:1 xwaveid using "h_lf_filter", nogen								
destring xhhraid, replace														
	
	
************ Individual information ************

	******** Demographic ********

	** agecat **
	gen age = 2000 + `i' - yob if yodeath == -1
	replace age = . if yodeath != -1
	gen agecat = 0
	replace agecat = 1 if age >= 15 & age <= 24
	replace agecat = 2 if age >= 25 & age <= 44
	replace agecat = 3 if age >= 45 & age <= 64
	replace agecat = 4 if age >= 65
	replace agecat = . if age == .	
	la def agecatlab 0 "[0] < 15" 1 "[1] 15 <= age <= 24" 2 "[2] 25 <= age <= 44" 3 "[3] 45 <= age <= 64" 4 "[4] age >= 65"
	la val agecat agecatlab 
	
	** female **
	gen female = (sex == 2) if !mi(sex)
	la def femlab 0 "[0] Male" 1 "[1] Female"
	la val female femlab
	
	** noneng **
	gen noneng = (anlote == 1) if !mi(anlote)
	la def nelab 0 "[0] No" 1 "[1] Yes"
	la val noneng nelab
	
	** nonaub **
	gen nonaub = (ancobn != 1101)
	
	** cohabiting **
	gen cohab = (mrclc == 1 | ordf == 1)  					
	
	** alone **
	gen alone = (inlist(mrcms, 2, 3, 4, 6) | ordf == 2 | hehtyp == 1 | mi(mrcms) ///
	| mi(mrclc) | mi(ordf) | mi(hehtyp)) & cohab != 1
	
	******** Health ********
	
	** sfgh **
	gen sfgh = ((100-ghpf) + ghrp + (100-ghbp) + (100-ghgh) + ghvt + (100-ghsf) + ghre)/7			
	replace sfgh = 100 - sfgh
	
	** ghmh **
	replace ghmh = 100 - ghmh
	xtile ghmhqt = ghmh, nq(100)															 
	gen ghmhc = 0 if !mi(ghmh)													
	replace ghmhc = 1 if (ghmhqt <= 84 & ghmhqt > 63) & !mi(ghmh)				
	replace ghmhc = 2 if (ghmhqt <= 95 & ghmhqt > 84) & !mi(ghmh)				
	replace ghmhc = 3 if (ghmhqt >95) & !mi(ghmh)									
	
	** srh **
	replace gh1 = 5 - gh1														
	la de srhlab 0 "[0] Poor" 1 "[1] Fair" 2 "[2] Good" 3 "[3] Very good" 4 "[4] Excellent" 
	la val gh1 srhlab 
	
	** hexp **
	gen hexp = (xpyhltp + xpyphi + xpyphrm) / 1000
	
	
	******** Labour market ********
		
	** pinc hinc **
	replace wscmg = (wscmg * 52.1429 / 10000) * (100 / cpi[1,`i'])												
	replace hiwscei = (hiwscei * 52.1429 / 10000) * (100 / cpi[1,`i'])		


	** empl uempl nilf **
	tab esbrd, gen(lfstat)
	local raw lfstat1 lfstat2 lfstat3
	local new empl uempl nilf													
	rename (`raw') (`new')
	replace uempl = 1 if empl == 0
	
	** casual worker **
	gen casual = 0
	replace casual = 1 if jbmcnt == 2 | jbcasab == 1
	
	** tenure **
	egen tenure = rowtotal(ujljt ujljtyr), missing 			
	
	** yrpaid **
	replace ujljtwk = ujljtwk / 52
	egen yrpaid = rowtotal(jbemlyr ehtjbyr ujljtyr ujljtwk), missing				
	
	** company size **
	replace jbmemsz = 2 if jbmemsz == 8											
	replace jbmemsz = 3 if jbmemsz == 9 										
	
	** jobsec **
	forval j = 1/10 {
		if `j' == 10 {
			replace jspsuit = `j' if jspsuit > (`j'-1)*10 & jspsuit < `j'*10	 
		} 
		else {
			replace jspsuit = `j' if jspsuit > (`j'-1)*10 & jspsuit <= `j'*10
		}
	}	
	replace jspsuit = 10 if jspsuit == 100 

	egen jobsec = rowtotal(losateo jspsuit jbmssec), missing
	
	** work status **
	gen wstat = 0 if uempl == 1 & !mi(uempl)									
	replace wstat = 1 if casual == 1 
	replace wstat = 2 if jbmhruc < 35 & !mi(jbmhruc) & casual != 1							
	replace wstat = 3 if jbmhruc >= 35 & !mi(jbmhruc) & casual != 1							
	la def jstatlab 0 "[0] unemployed" 1 "[1] casual" 2 "[2] part time" 3 "[3] full time" 
	la val wstat jstatlab
	
	** employment type **
	tab es, gen(slf)															
	
	** voluntary unemployment **
	gen volue = 0
	replace volue = 1 if inlist(nlmact, 1, 3, 6, 7) 	

	** superannuation **
	replace oifsupi = oifsupi / 1000
	
	
	******** Educational ********
	
	gen edusec = 0 if edhigh1 == 10 | edhigh1 == . 			
	replace edusec = 1 if inlist(edhigh1, 8, 9)	
	replace edusec = 2 if edhigh1 == 5 		
	replace edusec = 3 if edhigh1 == 4 	
	la def eduseclab 0 "[0] undetermined" 1 "[1] some high school (year 11 and below, year 12)" 2 "[2] technical school (certificate III or IV)" 3 "[3] advanced diploma or diploma"
	la val edusec eduseclab
	gen iedusec = inlist(edusec, 2, 3)
	
	gen eduuni = inlist(edhigh1, 1, 2, 3) 										
	
	gen eduvoc = (jttrwrk == 1) 							
	replace eduvoc = 1 if nlreajt == 1 			 								
	
************ Household information ************
	
	** children **
	gen fdepch = inlist(hhfty,4,5,13,14)										
	gen fdepst = inlist(hhfty,7,8,16,17)											
	gen ndepch = hhd0_4 + hhd5_9 + hhd1014 						
	gen idepch = (inlist(hhfty, 4,5,7,8,13,14,16,17))							
	
	forvalues n = 1/3 {
		gen ndepchc`n' = (ndepch == `n') 
	}
	gen ndepchcg3 = (ndepch >3 & ndepch != .)
	
	** regional unemployment **
	gen ur = hhura * hhmsr
	bysort xhhraid: egen avgurp = mean(ur)
	//drop ur

	** life event index (lei) **
	ds le???																	 
	la de noyes 0 "[0] No" 1 "[1] Yes"											
	foreach v of var `r(varlist)' {												
		replace `v' = `v' - 1													
		la val `v' noyes																									
	}	
	recode le??? (missing = 0) , prefix(les_)										
	gen lei = 50*les_lemar + 65*les_lesep + 45*les_lercl + 40*les_leprg + 39*les_lebth + 53*les_leins + 44*les_leinf + 100*les_ledsc + 63*les_ledrl + 37*les_ledfr + 53*les_levio + 44*les_lepcm + 63*les_lejls + 50*les_lejlf + 38*les_lefni + 58*les_lefnw + 20*les_lemvd
	drop les_*	
	sum lei
	gen slei = (lei - `r(min)') / (`r(max)'-`r(min)') * 100
	
	** neighbourhood coherence (nbh) **
	gen nbh = losatnl + losatlc
	sum nbh
	gen snbh = (nbh - `r(min)') / (`r(max)'-`r(min)') * 100
	
	
************ Family type ************
	
	gen cp = inrange(hhfty, 1, 12) 						
	gen lp = (inrange(hhfty, 13, 21) | inlist(hhfty, 24, 27))


************ Outcome variables ************

	** obesity **
	gen ovwt = (bmi >= 25)
	replace ovwt = . if mi(bmi)
	la def ovwtlab 0 "[0] not overweight" 1 "[1] overweight" 
	la val ovwt ovwtlab
	
	** physical inactivity: lspact **
	replace lspact = lspact / 4 & !mi(lspact) 	
	replace lspact = 0 if lspact < 1 & !mi(lspact)
	replace lspact = 1 if lspact >= 1 & !mi(lspact)
	replace lspact = 1 - lspact if !mi(lspact) 
	la def lspactlab 0 "[0] less than three times a week" 1 "[1] at least three times a week" 
	la val lspact lspactlab 

	** overdrinking: lsdrex **
	if inlist(`i', 7, 9, 11, 13, 15, 17, 19) {
		replace lsdrex = lsdrex / 5
		replace lsdrex = 0 if lsdrex < 1
		replace lsdrex = 1 if lsdrex >= 1
		la def lsdrexlab 0 "[0] less than once a week" 1 "[1] at least once a week" 
		la val lsdrex lsdrexlab 
	}
	
	** mental health, kessler **
	if inlist(`i', 7, 9, 11, 13, 15, 17, 19) {
		gen hidis = 0
		replace hidis = 1 if inlist(pdk10rc, 3, 4) 
		replace hidis = . if mi(pdk10rc) 		
		la def hidislab 0 "[0] low or moderate distress" 1 "[1] high or very high distress" 
		la val hidis hidislab
	}
	if inlist(`i', 7, 9, 11, 13, 15, 17, 19) {
	la def kitemlab  0 "[0] none of the time" 1 "[1] a little of the time" 2 "[2] some of the time" 3 "[3] most of the time" 4 "[4] all of the time"
	foreach kitem in pddepr pdhless pdsad pderles pdrless pdeff pdtired pdwless pdenerv pdnerv {
		replace `kitem' = 5 - `kitem' if !mi(`kitem') 
		la val `kitem' kitemlab 
	}
		gen depmd = pddepr + pdhless + pdsad 	
		gen motag = pderles + pdrless			
		gen fatig = pdeff + pdtired				
		gen worth = pdwless						
		gen anxty = pdenerv + pdnerv			
	}
	
	if inlist(`i', 7, 9, 11, 13, 15, 17, 19) {
		foreach kitem in depmd motag fatig worth anxty {
			replace `kitem' = 0 if `kitem' == .
		}
	}
	
	if inlist(`i', 7, 9, 11, 13, 15, 17, 19) {
		gen ghkess = (pdk10s - 10) / 40 * 100 	
	}
	
	if inlist(`i', 7, 9, 11, 13, 15, 17, 19) {
		foreach kitem in pdk10s depmd motag fatig worth anxty {
			egen s`kitem' = std(`kitem'), mean(50) std(10) 
			replace s`kitem' = round(s`kitem')
		}
	}
	
	
	if inlist(`i', 7, 9, 11, 13, 15, 17, 19) {	
		egen ssfmh = std(ghmh), mean(50) std(10)		
		replace ssfmh = round(ssfmh)
		replace ssfmh = spdk10s if mi(ssfmh)
		la def sfitemlab1  0 "[0] none of the time" 1 "[1] a little of the time" 2 "[2] some of the time" 3 "[3] most of the time" 4 "[4] all of the time"	
		la def sfitemlab2  0 "[0] all of the time" 1 "[1] most of the time" 2 "[2] some of the time" 3 "[3] a little of the time" 4 "[4] none of the time"	
	
	foreach sfitem in gh9b gh9c gh9d gh9f gh9h	{
		replace `sfitem' = `sfitem' - 1 if `sfitem' >= 3
		if "`sfitem'" != "gh9h" {
			replace `sfitem' = 5 - `sfitem' 
			la val `sfitem' sfitemlab1
		}
		else {
			replace `sfitem' = `sfitem' - 1  	
			la val `sfitem' sfitemlab2
		}
	}
	gen sfdep = gh9c + gh9f 				
	gen sfanx = gh9b + gh9d
	gen sfnaf = gh9h
	}	
	
	if inlist(`i', 7, 9, 11, 13, 15, 17, 19) {
		foreach sfitem in sfdep sfanx sfnaf {
			egen s`sfitem' = std(`sfitem'), mean(50) std(10)
			replace s`sfitem' = round(s`sfitem')
		}
	}


************ Instrumental variables ************

	bysort xhhraid jbmi61: gen ind1prop = _N 							
	bysort xhhraid: replace ind1prop = ind1prop/_N 							
	gen bkind1ur = ind1prop * hhura / 100 										
	
	bysort xhhraid jbmi62: gen ind2prop = _N 							
	bysort xhhraid: replace ind2prop = ind2prop/_N 							
	gen bkind2ur = ind2prop * hhura / 100 										
	
	bysort xhhraid jbmo61: gen occ1prop = _N									
	bysort xhhraid: replace occ1prop = occ1prop/_N
	gen bkocc1ur = occ1prop * hhura / 100
	
	bysort xhhraid jbmo62: gen occ2prop = _N									
	bysort xhhraid: replace occ2prop = occ2prop/_N
	gen bkocc2ur = occ2prop * hhura / 100
	
	drop ind1prop ind2prop occ1prop occ2prop
	
save "h`i'", replace
}



//------------------------- cross wave differencing -------------------------//

** create wide variables for differencing **
forval i = 7/19 {  																
use "h`i'", clear
local diff xwaveid hidis pdk10s spdk10s sdepmd smotag sfatig sworth sanxty ///
ovwt lspact lsdrex ssfmh ssfdep ssfnaf ssfanx 
isvar `diff'
keep `r(varlist)'	
foreach v in `diff' { 
capture confirm var `v', exact													
	if c(rc) == 111 {															
		gen `v'`i' = .															
	} 
	else if c(rc) == 0 {														
		gen `v'`i' = `v' 															
	}
}
keep xwaveid *`i'*																
drop xwaveid`i'																	
save "h`i'_todiff", replace
}
use "h7_todiff", clear
forval i = 8/19 {
	merge 1:1 xwaveid using "h`i'_todiff", nogen
}

** difference kessler 10 **
forval i = 7(2)17 {
	local j = `i' + 2
	foreach var in hidis pdk10s spdk10s sdepmd smotag sfatig sworth sanxty ///
ovwt lspact lsdrex ssfmh ssfdep ssfnaf ssfanx {
		gen d2`var'`j' = (`var'`j' - `var'`i')
	}
}
foreach k in 7 8 10 12 14 16 18 {
	foreach var in hidis pdk10s spdk10s sdepmd smotag sfatig sworth sanxty ///
ovwt lspact lsdrex ssfmh ssfdep ssfnaf ssfanx {
		gen d2`var'`k' = .
	}	
}

keep xwaveid d2* 


//------------------------- wide to long -------------------------//

reshape long d2hidis d2pdk10s d2spdk10s d2sdepmd d2smotag d2sfatig d2sworth d2sanxty ///
d2ovwt d2lspact d2lsdrex d2ssfmh d2ssfdep d2ssfnaf d2ssfanx, i(xwaveid) j(wave)

replace d2ovwt = 0 if d2ovwt < 0		
replace d2lsdrex = 0 if d2lsdrex < 0	
replace d2hidis = 0 if d2hidis < 0		

save "h_diff", replace



//------------------------- time-invariant characteristics -------------------------//

foreach i in 12 16 {															
	use "h`i'.dta", clear
	keep xwaveid lssexor
	rename lssexor lssexor`i'
	save "h`i'_lssexor", replace
}


//------------------------- append waves and merge -------------------------//

clear			
use "h7.dta"
forvalues i = 8/19 {
	append using "h`i'"
}
merge 1:1 xwaveid wave using "h_diff", nogen
merge m:1 xwaveid using "h12_lssexor", nogen 									
merge m:1 xwaveid using "h16_lssexor", nogen 									
drop lssexor


//------------------------- data labelling -------------------------//

local raw xwaveid hhpxid hhidate pdk10s spdk10s d2pdk10s d2spdk10s pdk10rc lspact d2lspact lsdrex d2lsdrex lefrd gh1 wscmg hiwscei jbmemsz jspsuit hhsra hhstate xhhraid hhda10 hhec10 hhed10 jbmi61 jbmi62 jbmo61 jbmo62 jbhruc hecpmhp oifsupi xpphia ujwku ujyru
local new index index_sp intv_date kess skess d2kess d2skess kessc inact d2inact ovdrk d2ovdrk shock srh pinc hinc csize emplop remar state area decdis dececr decedo ind1da ind2da occ1da occ2da hrswk psycht super phia uewk ueyr
rename (`raw') (`new')

local vars1 "shock age agecat female noneng nonaub ghmh ghmhc sfgh srh lei nbh"
local labels1 `""Fired or made redundant" "Age" "Age categories" "Female" "Speak language other than English" "Born outside of Australia" "SF36 mental health (5 items)" "SF36 mental health (4 categories)" "SF36 general health (31 items)" "Self-rated health" "Life events index" "Neighborhood coherence""'	

local vars2 "casual pinc hinc nevemp nevuemp yrpaid csize jobsec occ1da ind1da ind2da wstat"
local labels2 `""Casual worker" "Labor earnings in 10000 AUD" "Household income in 10000 AUD" "Never employed" "Never unemployed" "Previous labor force participation (in years)" "Company size (<20 20-99 100-499 500-999 1000-4999 5000-19999 >20000" "Perceived job security (11 categories, 0 totally dissatisfied to 10 totally satisfied)" "Previous occupation (ANZSCO 2006 1-digit, 8 categories)" "Previous industry sector (ANZSIC 2006 division 1 digit, 19 categories)" "Previous industry sector (ANZSIC 2006 division 2 digits, 86 categories)" "Working status (3 categories)""'

local vars3 "edusec iedusec eduuni eduvoc ndepch idepch ndepchc1 ndepchc2 ndepchc3 ndepchcg3 avgurp remar state lei nbh wave cohab alone"
local labels3 `""Secondary schooling (4 categories)" "Technical school or diploma" "University degree or above" "Vocational training" "Number of dependent children below 15" "Has dependent children below 15" "Has one depedent child below 15" "Has two depedent children below 15" "Has three depedent children below 15" "Has more than three depedent children below 15" "Unemployment rate by postcode" "Remoteness area" "State" "Life events index" "Neighborhood satisfaction" "Survey wave" "Couples cohabiting" "Never married and not living with someone in a relationship""'

local vars4 "ovwt hidis depmd motag fatig worth anxty sfdep sfanx sfnaf"
local labels4 `""Overweight status" "Highly or very highly distressed" "Depressed mood" "Motor agitation" "Fatigue" "Worthless guilt" "Anxiety" "Short-form depressed mood" "Short-form anxiety" "Short-form negative affect""'

local vars5 "ghkess skess sdepmd smotag sfatig sworth sanxty bkind1ur bkind2ur bkocc1ur bkocc2ur"
local labels5 `" "Bounded kessler 10 score" "Standardised kessler 10 score" "Standardised depressed mood" "Standardised motor agitation" "Standardised fatigue" "Standardised worthless guilt" "Standardised anxiety" "Bartik industry (1-digit) IV" "Bartik industry (2-digit) IV" "Bartik occupation (1-digit) IV" "Bartik occupation (2-digit) IV""'

local vars `"`vars1' `vars2' `vars3' `vars4' `vars5'"'
local labels `"`labels1' `labels2' `labels3' `labels4' `labels5'"'
local n: word count `vars'
forval i = 1/`n'{
	local a: word `i' of `vars'
	local b: word `i' of `labels'
	la var `a' "`b'"
}

local vars6 "d2kess d2skess d2hidis d2sdepmd d2smotag d2sfatig d2sworth d2sanxty d2ovwt d2inact d2ovdrk"
local labels6 `""Change in Kessler 10 score over 2 years" "Change in standardised Kessler 10 score over 2 years" "Becoming highly distressed over 2 years" "Change in standardised depressed mood over 2 years" "Change in standardised motor agitation over 2 years" "Change in standardised fatigue over 2 years" "Change in standardised worthless guilt over 2 years" "Change in standardised anxiety over 2 years" "Becoming overweight over 2 years" "Becoming inactive over 2 years" "Overdrink more over 2 years""' 

local vars7	"ssfmh ssfdep ssfanx ssfnaf d2ssfmh d2ssfdep d2ssfanx d2ssfnaf"
local labels7 `""Standardised short-form mental health" "Standardised short-form depressed mood" "Standardised short-form anxiety" "Standardised short-form negative affect" "Change in standardised short-form mental health over 2 years" "Change in standardised short-form depressive mood over 2 years" "Change in standardised short-form anxiety over 2 years" "Change in standardised short-form negative affect over 2 years""'

local vars8 "decdis dececr decedo slf1 slf2 slf3 slf4 hexp"
local labels8 `""SEIFA2001 decile of relative socio-economic disadvantage index" "SEIFA2001 decile of economic resources index" "SEIFA2001 decile of education and occupation index" "Employee" "Employer" "Own account worker" "Contributing family member" "Health expenditure, person level (in 1000 AUD)""'

local vars9 "fdepch fdepst lssexor12 lssexor16 cp lp volue slei snbh"
local labels9 `""Family type: has children < 15" "Family type: has dependent student >= 15" "Sexual orientation, wave 12" "Sexual orientation, wave 16" "Couple family" "Lone parent/person" "Voluntarily unemployed (nlmact = 1,3,6,7)" "Min-max standardised life events index" "Min-max standardised neighbourhood satisfaction""'

local vars `"`vars6' `vars7' `vars8' `vars9'"'
local labels `"`labels6' `labels7' `labels8' `labels9'"'
local n: word count `vars'
forval i = 1/`n'{
	local a: word `i' of `vars'
	local b: word `i' of `labels'
	la var `a' "`b'"
}



** NA **
global covar age female noneng ghbp ghgh ghmh ghkess ghpf ghre ghrp ghsf ghvt ghrht srh	///
pinc nevuemp tenure	empl uempl yrpaid csize jobsec occ1da ind1da						///
slf1 slf2 slf3 slf4 es hrswk casual wstat edusec iedusec eduuni eduvoc idepch ndepch	///
ndepchc1 ndepchc2 ndepchc3 ndepchcg3 avgurp remar state lei nbh slei snbh decdis dececr ///
decedo cohab area hexp ancobn 
local n = 0
local num : list sizeof global(covar)
foreach v of global covar {
	qui egen mode = mode(`v')
	qui replace `v' = mode if `v' == .											
	qui drop mode	
	local n = `n' + 1
	di "`n' of `num'"														
}


//------------------------- lagging covariates -------------------------//

drop sex  
order index wave
sort index wave
destring index index_sp intv_date, replace

xtset index wave 

foreach var in empl age female noneng nonaub ghbp ghgh ghmh ghpf ghre ghrp ghsf ghvt 		///
pinc nevuemp tenure yrpaid csize jobsec occ1da ind1da wstat edusec iedusec eduuni eduvoc  	///
idepch ndepch ndepchc1 ndepchc2 ndepchc3 ndepchcg3 avgurp remar state slei snbh area 		///
decdis dececr decedo slf1 slf2 slf3 slf4 es hrswk fdepch fdepst casual cohab ghkess srh skess {
	qui gen l2`var' = L2.`var'
	qui gen l1`var' = L1.`var'												
	gen l12`var' = l2`var'														
	replace l12`var' = l1`var' if L.shock != 1 & shock == 1						
	qui drop l2`var' l1`var'
}													


foreach var in shock lejob volue {
	gen l`var' = L.`var'
	egen `var'2 = rowtotal(l`var' `var'), missing
	drop l`var'
	replace `var'2 = 1 if `var'2 > 1 & !mi(`var'2)
	replace `var'2 = 0 if `var'2 == .
}
replace volue2 = 0 if shock2 == 1 & volue2 == 1
gen atcue2 = 0
replace atcue2 = 1 if uempl == 1 & shock2 != 1 & volue2 != 1


foreach var in bkind1ur bkind2ur bkocc1ur bkocc2ur {
	gen l`var' = L.`var'
	egen `var'a2 = rowmean(l`var' `var')
	drop l`var'
}

gen l2d2skess = L2.d2skess
gen l2d2ssfmh = L2.d2ssfmh


foreach var in mschgdv mschgsp mschgmr mschgrs lebth uempl {
	forvalues i = 1 / 5 {
		gen f`i'`var' = F`i'.`var'
		label var f`i'`var' `"`: var label `var'', `i' year(s) later"' 
	}
}
												
local vars1 "shock2 lejob2 l12empl l12age l12female l12noneng l12nonaub l12ghbp l12ghgh l12ghmh l12ghpf l12ghre l12ghrp l12ghsf l12ghvt l12srh"
local labels1 `""Fired or made redundant, summed over two periods" "Changed job indicator, summed over two periods" "Employed lagged" "Age lagged" "Female lagged" "Non-English lagged" "Born outside of Australia lagged" "SF bodily pain (2 items) lagged" "SF36 general health (5 items) lagged" "SF36 mental health (5 items) lagged" "SF36 physical functioning (10 items) lagged" "SF36 role-emotional (3 items) lagged" "SF36 role-physical (4 items) lagged" "SF36 social functioning (2 items) lagged" "SF36 vitality (4 items) lagged" "Self rated health lagged""'

local vars2 "l12pinc l12nevuemp l12tenure l12yrpaid l12csize l12jobsec l12occ1da l12ind1da l12wstat l12slf1 l12slf2 l12slf3 l12slf4 l12es l12hrswk"
local labels2 `""Labour earnings in 1000 AUD lagged" "Never unemployed lagged" "Tenure in years lagged" "Years in paid work lagged" "Company size lagged" "Perceived job security lagged" "Occupation lagged" "Industry sector lagged" "Working status lagged" "Employee lagged" "Employer lagged" "Own account worker lagged" "Contributing family member lagged" "Employment status lagged" "Hours per week worked in all jobs lagged""'

local vars3 "l12edusec l12iedusec l12eduuni l12eduvoc l12idepch l12ndepch l12ndepchc1 l12ndepchc2 l12ndepchc3 l12ndepchcg3 l12avgurp l12remar l12state l12slei l12snbh l12area l12decdis l12dececr l12decedo"
local labels3 `""Secondary schooling lagged" "Secondary education lagged" "University degree or above lagged" "Vocational training lagged" "Has dependent children lagged" "Number of dependent children lagged" "One dependent child lagged" "Two dependent children lagged" "Three dependent children lagged" "More than three dependent children lagged" "Unemployment rate by postcode lagged" "Remoteness area lagged"  "State lagged" "Min-max standardised life events index lagged" "Min-max standardised neighbourhood coherence lagged" "Area lagged" "SEIFA2001 decile of index of relative socio-economic disadvantage lagged" "SEIFA2001 decile of index of economic resources lagged" "SEIFA2001 decile of index of education and occupation lagged""'

local vars4 "l12fdepch l12fdepst l12casual l12cohab l12skess volue2 atcue2 l2d2skess l2d2ssfmh ghkess bkind1ura2 bkind2ura2 bkocc1ura2 bkocc2ura2"
local labels4 `""Family type: has children < 15 lagged" "Family type: has dependent student >= 15" "Casual worker lagged" "Couples cohabiting lagged" "SK10 lagged" "Voluntarily unemployed, summed over two periods" "Anticipated unemployment, summed over two periods" "Outcome for placebo regression" "Outcome for robustness placebo regression" "Bounded K10 lagged" "Bartik industry IV (1-digit) averaged" "Bartik industry IV (2-digit) averaged" "Bartik occupation IV  (1-digit) averaged" "Bartik occupation IV  (2-digit) averaged""'

local vars `"`vars1' `vars2' `vars3' `vars4'"'
local labels `"`labels1' `labels2' `labels3' `labels4'"'
local n: word count `vars'
forval i = 1/`n'{
	local a: word `i' of `vars'
	local b: word `i' of `labels'
	la var `a' "`b'"
}

save "hl_d.dta", replace


//------------------------- partner data -------------------------//

use "hl_d.dta", clear

keep index wave d2skess skess d2hidis hidis d2sdepmd d2smotag d2sfatig d2sworth d2sanxty d2ssfmh ssfmh lssexor12 lssexor16			///
age female noneng nonaub ghbp ghgh ghmh ghkess ghpf ghre ghrp ghsf ghvt ghrht pinc nevuemp wstat iedusec eduuni eduvoc uempl srh	///
l12age l12female l12noneng l12nonaub l12ghbp l12ghgh l12ghmh l12ghpf l12ghre l12ghrp l12ghsf l12ghvt l12pinc l12nevuemp l12wstat 	///
l12edusec l12iedusec l12eduuni l12eduvoc shock2 lejob2 volue2 atcue2 d2skess d2ssfmh l2d2skess l2d2ssfmh l12ghkess hexp l12srh 			

isvar wave		 															
ds `r(varlist)', not														
foreach v of var `r(varlist)' {
	rename `v' `v'_sp
}		
save "hl_i.dta", replace


//------------------------- both partners -------------------------//

use "hl_d.dta", clear
merge m:1 index_sp wave using "hl_i.dta", keep(match master) 					 
tab _merge, gen(htype)
rename htype1 noparintv															
rename htype2 hasparintv														
drop _merge	
	
duplicates tag index index_sp, gen(pairs)
order index index_sp pairs														
gen samesex = (female_sp == female)												
gen lg = (lssexor12 == 2 | lssexor16 == 2)
gen btqia = (inlist(lssexor12, 3,4,5) | inlist(lssexor16, 3,4,5))
gen lgbtqia = (inlist(lssexor12, 2,3,4,5) | inlist(lssexor16, 2,3,4,5))
egen hhhexp = rowtotal(hexp hexp_sp) 	

sort index
gen random = uniform()  														

la var random "Random number from uniform distribution" 
la var pairs "Same spouse indicator"
la var samesex "Same sex couple indicator"
la var noparintv "No partner interviewed"
la var hasparintv "Has partner interviewed"
la var lg "Lesbian or gay"
la var btqia "Bisexual, transgender, queer or questioning, intersex, and asexual or allied"
la var lgbtqia "Lesbian, gay, bisexual, transgender, queer or questioning, intersex, and asexual or allied"
la var hhhexp "Household health expenditure"
 
save "hl_di_full.dta", replace

keep index wave btqia 
rename index index_sp 
rename btqia btqia_sp
save "hl_di_btqia.dta", replace


use "hl_di_full.dta", clear
merge m:1 index_sp wave using "hl_di_btqia.dta", keep(match master) nogen					
	gen mainearn = 0 if !mi(index_sp)
	replace mainearn = 1 if pinc > pinc_sp & !mi(index_sp)
	xtset index wave
	qui gen l2mainearn = L2.mainearn
	qui gen l1mainearn = L1.mainearn
	gen l12mainearn = l2mainearn
	replace l12mainearn = l1mainearn if L.shock != 1 & shock == 1						
	qui drop l2mainearn l1mainearn
	la var mainearn "Main earner"
	la var l12mainearn "Main earner lagged"
	replace mainearn = 0 if mainearn == .
	replace l12mainearn = 0 if l12mainearn == .
	
	replace hinc = pinc if hinc == . | pinc > hinc
	gen pearnhh = 1		
	replace pearnhh = pinc / hinc if !mi(pinc) & !mi(hinc) & hinc != 0
	gen l12pearnhh = L2.pearnhh
	replace l12pearnhh = L1.pearn if L.shock != 1 & shock == 1
	la var pearnhh "Household income share"
	la var l12pearnhh "Household income share lagged"
save "hl_di_full.dta", replace


//------------------------- missing treatment -------------------------//

use "hl_di_full.dta", clear
gen lfilt = (age >= 18 & age <= 65 & lp == 1 & noparintv == 1 & l12empl == 1 & (uempl == 1 | lejob2 == 0))

replace skess = ssfmh if mi(skess) & lfilt == 1
replace skess_sp = ssfmh_sp if mi(ssfmh_sp) & lfilt == 1
replace d2skess = d2ssfmh if mi(d2skess) & lfilt == 1
replace d2skess_sp = d2ssfmh_sp if mi(d2skess_sp) & lfilt == 1 
xtset index wave
egen sghgh = std(ghgh) if lfilt == 1, mean(50) std(10)
replace skess = sghgh if mi(skess) & !mi(sghgh) & lfilt == 1
replace d2skess = D2.sghgh if mi(d2skess) & !mi(D2.sghgh) & lfilt == 1
xtset, clear

foreach cardvar in age ghbp ghgh ghmh ghpf ghre ghrp ghsf ghvt pinc yrpaid jobsec 				///
hrswk pearnhh avgurp slei snbh dececr decedo decdis age_sp ghbp_sp ghgh_sp ghmh_sp ghpf_sp ghre_sp 		///
ghrp_sp ghsf_sp ghvt_sp pinc_sp {
	gen `cardvar'2 = `cardvar'^2 
	gen l12`cardvar'2 = l12`cardvar'^2 
}

drop sghgh lfilt

egen hhid = group(index)
la var hhid "Household ID"
save "hl_di_full.dta", replace


//------------------------- clean up -------------------------//

forvalues i = 7/19 {
	erase h`i'.dta
	erase h`i'_todiff.dta
}
foreach w in a b c d e f g h i j k l m n o p q r s {
	erase h`w'_filter.dta
}
erase h_diff.dta
erase h12_lssexor.dta
erase h16_lssexor.dta
erase h_lf_filter.dta
erase hl_d.dta 
erase hl_i.dta
erase hl_di_btqia.dta

