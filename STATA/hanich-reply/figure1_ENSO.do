/******************************  METADATA *********************************
Replication for ENSO robustness check in response to Hanich et al. (2018)'s comment on
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


/******************************  MERGE DATA *********************************/

//Extract kaplan extended data (Jan 1899 to Sept. 20)
//citation here: http://iridl.ldeo.columbia.edu/SOURCES/.Indices/.nino/.EXTENDED/datasetdatafiles.html
	insheet using $inputDir/monthly_nino34.csv, comma names clear

	gen dateENSO=date(time, "MY")
	gen year=year(date)
	gen mofy=month(date)
	gen month=mofd(date)

	gen nino34_2=nino34^2

	//merge with GFW data
	merge month using $dataDir/region_day_ready, uniqmaster sort
	tab _merge
	drop if _merge==1
	drop _merge

/******************************  FIGURES *********************************/
	xtset PIPA date
	sort PIPA date

	tw ///
	(line sst date if PIPA==1, lwidth(.15) lcolor(cranberry)) ///
	(line sst date if PIPA==0, lwidth(.15) lcolor(ltblue)) ///
	(line nino34 date if PIPA==0, lwidth(.3) lcolor(olive) yaxis(2) ///
	tline(${PIPA_implement}, lcolor(gs6) lwidth(.3) lpattern(dash)) ///
	tlabel(01jan2012 01jan2013 01jan2014 01jan2015 01jan2016 01jan2017, format(%tdmy)) ///
	), ///
	ytitle("Sea surface temperature (degree C)", axis(1)) ///
	ytitle("NINO index (degree C)", axis(2)) ///
	ylabel(, nogrid axis(1)) ///
	ylabel(, nogrid labsize(large) axis(2)) ///
	xtitle("") ///
    legend( order(1 2 3) ///
	label (1 "PIPA SST") ///
	label(2 "Kiribati control SST")  ///
    label(3 "NINO3.4 index") ///
    ring(0) position(11) ///
    col(1) row(5) size(medsmall)) ///
	graphregion(color(white))	///
	saving($figuresDir/SST.gph, replace)


//Part 4: DiD figure
	local oVar "logistic_hours_n" //outcome variable
	local nknots=6 // number of spline knots
	local NW_bw=60 // Newey-West bandwdith
	local level =.05 // confidence interval
	local max=`nknots'-1
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

//Part 1: Model 1 (benchmark)
	//Pre
	xi: ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  if date<td(${PIPA_implement}), ///
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
	xi: ivreg2 `oVar' PIPAXdate_post* i.PIPA date_post* if date>=td(${PIPA_implement}), ///
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

//Part 2: Model 2 (Model 1 + SST)
	local controls "sst sst2"
	local M=1

	//Pre
	xi: ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  `controls' if date<td(${PIPA_implement}), ///
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
	xi: qui ivreg2 `oVar' PIPAXdate_post* i.PIPA date_post* `controls' if date>=td(${PIPA_implement}), ///
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

//Part 3: Model 2 (+ nino34)
	gen PIPAXnino34=PIPA*nino34
	gen PIPAXnino34_2=PIPA*nino34_2

	local controls "PIPAXnino34 nino34 PIPAXnino34_2 nino34_2"
	local M=2

	//Pre
	xi: ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  `controls' if date<td(${PIPA_implement}), ///
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
	xi: ivreg2 `oVar' PIPAXdate_post* i.PIPA date_post* `controls' if date>=td(${PIPA_implement}), ///
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


//Combined graph
	sort date

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
	(line diff_h_1 date if date>=td(${PIPA_implement}), lwidth(.2) lcolor(maroon) lpattern(dash)) ///
	(line diff_h_2 date if date<td(${PIPA_implement}), lwidth(.2) lcolor(maroon) lpattern(shortdash_dot)) ///
	(line diff_h_2 date if date>=td(${PIPA_implement}), lwidth(.2) lcolor(maroon) lpattern(shortdash_dot)) ///
	(line date date, yaxis(2) lwidth(none)), ///
	ytitle("Diff. in daily fishing hours per area", axis(1)) ///
	yscale(range(0 `yMax')) ylabel(-.4(.2)`yMax', nogrid axis(1)) ///
	ytitle(" ", axis(2) size(vhuge)) ///
	ylabel(, nogrid noticks nolabels axis(2)) ///
	xtitle("Date") ///
    legend( order(3 5 7) ///
	label (1 "") label(2 " ")  ///
    label(3 "M0: Benchmark") label(4 " ") ///
    label(5 "M1: M0 + SST") label(6 " ") ///
    label(7 "M2: M0 + NINO3.4") label(8 " ") ///
    ring(0) position(11) ///
    col(1) row(5) size(medsmall)) ///
	graphregion(color(white)) ///
	saving($figuresDir/effort.gph, replace)

	graph combine $figuresDir/SST.gph $figuresDir/effort.gph, rows(2) graphregion(color(white)) xcommon altshrink

	rm $figuresDir/SST.gph
	rm $figuresDir/effort.gph

	graph export $figuresDir/figure1.pdf, replace
