


/* Title: Job Displacement and the Mental Health of Households: Burden Sharing Countercts Spillover */
/* Date: January 2023 */
/* Author: Yuejun Zhao */


/********************************************************************************************************/
/* MASTER FILE */
/********************************************************************************************************/

clear all
macro drop _all 
set more off
set maxvar 10000	
capture log close
set seed 2020
set sortseed 2020

net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
set scheme cleanplots


sysdir set PLUS "XX"		// ado files here	
cd "XX"						// change directory here

log using "burden_sharing", replace text 

do "spmh_01_clean.do"		
do "spmh_02_ebal.do"
do "spmh_03_base.do"
do "spmh_05_area.do"
do "spmh_06_type.do"
do "spmh_09_desc.do"
do "spmh_11_het.do"
do "spmh_12_earn.do"

log close

