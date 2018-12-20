/******************************  METADATA *********************************
Replication for AIS subsample robustness check, in response to Hanich et al. (2018)'s comment on
McDermott, Meng, McDonald, and Costello, "The Blue Paradox: Preemptive Overfishing in Marine Reserves", PNAS, 2018.

Script Description: This STATA script generates all figures in the manuscript and supplemental information. Code developed for STATA v13.
Date: November 1, 2018
**************************************************************************/
/******************************  HEADER **********************************/
	clear all
	set trace off
	set more off
	set matsize 4000
	capture log close

//Set directories
	cd .. //The assumed relative path is "blueparadox/STATA" (i.e. one directory up). Users must set this local directory manually if that is not the case.

	global inputDir "../data"
	global dataDir "data"
	global figuresDir "../figures/hanich-reply"

	sysdir set PERSONAL STATA_toolbox //set directory for user-written STATA programs

//Global parameters
	global PIPA_implement "01jan2015" // PIPA implementation date


/******************************  MERGE DATA **********************************/

	//Get subsample of boats already broadcasting from 2012-1-1 to 2013-9-1
	insheet using $inputDir/pre_anticipation_mmsi_filter.csv, comma names clear
	tempfile temp
	save "`temp'"

	insheet using $inputDir/gfw_split.csv, comma names clear
	destring mmsi, force replace

	merge mmsi using "`temp'", uniqusing sort
	tab _merge
	keep if _merge==3


	drop if area=="NA"
	destring area,  force replace

	sum area if region=="PIPA"
	local PIPA_area=r(mean)
	sum area if regexm(region, "Gilbert Islands")
	local Gilbert_area=r(mean)
	sum area if regexm(region, "Line Group")
	local Line_area=r(mean)

	//create PIPA dummy (=1 in PIPA, =0 if in Gilbert or Line Island)
	gen PIPA=1 if region=="PIPA" // create region ID
	replace PIPA=0 if regexm(region, "Gilbert Islands") | regexm(region, "Line Group")
	drop if regexm(region, "Phoenix Group Non-PIPA") //dropping non-PIPA Phoenix Group
	drop region

	//collapse to date-by-PIPA dummy
	collapse (sum) logistic_hours nnet_hours, by(date PIPA)

	//destring date
	gen date2=date(date, "YMD") //format date variable
	format %td date2
	drop date
	rename date2 date

	//impute zero for days with missing data
	xtset PIPA date
	tsfill, full


	//get area
	gen area=`PIPA_area' if PIPA==1
	replace area=`Gilbert_area' + `Line_area' if PIPA==0
	replace area=area/1000 //make it in thousand km^2
	label var area "in thousand km squared"

	//outcome variable transformations
	local oList "logistic_hours nnet_hours"
	foreach oVar in `oList' {
		replace `oVar'=0 if missing(`oVar')
		gen `oVar'_n=`oVar'/area
		rename `oVar'_n `oVar'_n_pre
	}

	keep date PIPA *_hours_n_pre

	//Merge with master data set
	merge date PIPA using $dataDir/region_day_ready, unique sort
	tab _merge
	drop _merge

/******************************  FIGURE **********************************/

	//Parameters
	local nknots=6 // number of spline knots
	local NW_bw=60 // Newey-West bandwdith
	local level =.05 // confidence interval

	local max=`nknots'-1

	//Load Data
	xtset PIPA time1

	//Make splines
	mkspline date_pre=date if date<td(${PIPA_implement}), cubic displayknots nknots(`nknots')
	mkspline date_post=date if date>=td(${PIPA_implement}), cubic displayknots nknots(`nknots')

	//Interactions
	forvalues k=1(1)`max' {
			gen PIPAXdate_pre`k'=PIPA*date_pre`k'
	}

	forvalues k=1(1)`max' {
			gen PIPAXdate_post`k'=PIPA*date_post`k'
	}

	xtset PIPA date

//Model 0: Benchmark model

	//Pre
	xi: ivreg2 logistic_hours_n PIPAXdate_pre* i.PIPA date_pre*  if date<td(${PIPA_implement}), ///
		bw(`NW_bw')
		loc z_value = invnormal(`level')

    //Point estimates and confidence intervals
    gen diff_pre_h=0
    gen diff_pre_h_lb =0
    gen diff_pre_h_ub = 0

    qui sum date if date<td(${PIPA_implement})
    local dMin=r(min)
    local dMax=r(max)

    forvalues d=`dMin'(1)`dMax' {
        local expList  "_b[_IPIPA_1]"
		forvalues k=1(1)`max' {
			qui sum PIPAXdate_pre`k' if date==`d' & PIPA==1
            local t = r(mean)
            local expList "`expList' + _b[PIPAXdate_pre`k']*`t'"
        }
        qui lincom `expList'
        replace diff_pre_h=r(estimate) if date==`d'
        qui replace diff_pre_h_lb = diff_pre_h - `z_value'*r(se) if date==`d'
        qui replace diff_pre_h_ub = diff_pre_h + `z_value'*r(se) if date==`d'
    }

	//Post
	xi: ivreg2 logistic_hours_n PIPAXdate_post* i.PIPA date_post* if date>=td(${PIPA_implement}), ///
		bw(`NW_bw')

	loc z_value = invnormal(`level')

    //Point estimates and confidence intervals
    gen diff_post_h=0
    gen diff_post_h_lb =0
    gen diff_post_h_ub = 0

    qui sum date if date>=td(${PIPA_implement})
    local dMin=r(min)
    local dMax=r(max)

    forvalues d=`dMin'(1)`dMax' {
        local expList  "_b[_IPIPA_1]"
		forvalues k=1(1)`max' {
			qui sum PIPAXdate_post`k' if date==`d' & PIPA==1
            local t = r(mean)
            local expList "`expList' + _b[PIPAXdate_post`k']*`t'"
        }
        qui lincom `expList'
        replace diff_post_h=r(estimate) if date==`d'
        qui replace diff_post_h_lb = diff_post_h - `z_value'*r(se) if date==`d'
        qui replace diff_post_h_ub = diff_post_h + `z_value'*r(se) if date==`d'
    }

	gen diff_h=diff_pre_h if date<td(${PIPA_implement})
	replace diff_h=diff_post_h if date>=td(${PIPA_implement})

//Model 2 using subsample of boats broadcasting from 2012-1-1 to 2013-9-1
	local M=1

	//Pre
	xi: qui ivreg2 logistic_hours_n_pre PIPAXdate_pre* i.PIPA date_pre*  if date<td(${PIPA_implement}), ///
		bw(`NW_bw')
		loc z_value = invnormal(`level')

    //Point estimates
    gen diff_pre_h_`M'=0
    qui sum date if date<td(${PIPA_implement})
    local dMin=r(min)
    local dMax=r(max)

    forvalues d=`dMin'(1)`dMax' {
        local expList  "_b[_IPIPA_1]"
		forvalues k=1(1)`max' {
			qui sum PIPAXdate_pre`k' if date==`d' & PIPA==1
            local t = r(mean)
            local expList "`expList' + _b[PIPAXdate_pre`k']*`t'"
        }
        qui lincom `expList'
        replace diff_pre_h_`M'=r(estimate) if date==`d'
    }

	//Post
	xi: qui ivreg2 logistic_hours_n_pre  PIPAXdate_post* i.PIPA date_post* `controls' if date>=td(${PIPA_implement}), ///
		bw(`NW_bw')

	loc z_value = invnormal(`level')

    //Point estimates
    gen diff_post_h_`M'=0

    qui sum date if date>=td(${PIPA_implement})
    local dMin=r(min)
    local dMax=r(max)

    forvalues d=`dMin'(1)`dMax' {
        local expList  "_b[_IPIPA_1]"
		forvalues k=1(1)`max' {
			qui sum PIPAXdate_post`k' if date==`d' & PIPA==1
            local t = r(mean)
            local expList "`expList' + _b[PIPAXdate_post`k']*`t'"
        }
        qui lincom `expList'
        replace diff_post_h_`M'=r(estimate) if date==`d'
    }

	gen diff_h_`M'=diff_pre_h_`M' if date<td(${PIPA_implement})
	replace diff_h_`M'=diff_post_h_`M' if date>=td(${PIPA_implement})

	drop if PIPA==0


//Combined figure
	local yMax=.4
	tw ///
	(rarea diff_pre_h_ub diff_pre_h_lb date, lwidth(.1) clcolor(maroon) alcolor(white) alwidth(.001) fcolor(maroon) fintensity(20)) ///
	(rarea diff_post_h_ub diff_post_h_lb date, lwidth(.5) clcolor(maroon) alcolor(white) alwidth(.001) fcolor(maroon) fintensity(20) ///
	tline(${PIPA_implement}, lcolor(gs6) lwidth(.1) lpattern(dash)) ///
	tlabel(01jan2012 01jan2013 01jan2014 01jan2015 01jan2016 01jan2017, format(%tdmy)) ///
	yline(0, lwidth(.2) lcolor(gs12))) ///
	(line diff_h date if date<td(${PIPA_implement}), lwidth(.2) lcolor(maroon)) ///
	(line diff_h date if date>=td(${PIPA_implement}), lwidth(.2) lcolor(maroon)) ///
	(line diff_h_1 date if date<td(${PIPA_implement}), lwidth(.2) lcolor(maroon) lpattern(dash)) ///
	(line diff_h_1 date if date>=td(${PIPA_implement}), lwidth(.2) lcolor(maroon) lpattern(dash)), ///
	ytitle("Diff. in daily fishing hours per area") ///
	yscale(range(0 `yMax')) ylabel(-.6(.2)`yMax', nogrid) ///
	xtitle("Date") ///
    legend( order(3 5 7) ///
	label (1 "") label(2 " ")  ///
    label(3 "M0: Benchmark sample") label(4 " ") ///
    label(5 "M1: Prior-broadcasting boat subsample") label(6 " ") ///
    ring(0) position(7) ///
    col(1) row(5) size(vsmall)) ///
	graphregion(color(white))


	graph export $figuresDir/figure2.pdf, replace
