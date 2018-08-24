/******************************  METADATA *********************************
Replication for McDermott, Meng, McDonald, and Costello, "The Blue Paradox: Preemptive Overfishing in Marine Reserves", PNAS, 2018.

Script Description: This STATA script generates all figures in the manuscript and supplemental information. Code developed for STATA v13.
Date: July 24, 2018
**************************************************************************/
/******************************  HEADER **********************************/
	clear all
	set trace off
	set more off
	set matsize 4000
	capture log close

//Set directories  
	cd . //The assumed relative path is "blueparadox/STATA". Users must set this local directory manually if that is not the case. 

	global dataDir "data"
	global figuresDir "../figures"

	sysdir set PERSONAL STATA_toolbox //set directory for user-written STATA programs

//Global parameters
	global PIPA_implement "01jan2015" // PIPA implementation date

/********************************************************************************************
											Main Program
*****************************************************************************************/
capture program drop main
program define main
	
//Sub-programs 

	figure_main //sub-program for Figure 3 

	figure_spec //sub-program for Figure S1

	figure_knots //sub-program for Figure S2

	figure_stderr //sub-program for Figure S3
	
	figure_outcome //sub-program for Figure S4


end // program main


/**********************************************************************
							Sub Programs
*************************************************************************/

/********************* Figure 3 ****************************/
capture program drop figure_main
program define figure_main

//Parameters
	local oVar "logistic_hours_n" //outcome variable
	local nknots=6 // number of spline knots
	local NW_bw=60 // Newey-West bandwdith 
	local level =.05 // confidence interval

	local max=`nknots'-1

//Load Data
	//Main dataset
	use $dataDir/region_day_ready, replace
	xtset PIPA time1


	//Make splines
	mkspline date_pre=date if date<td(${PIPA_implement}), cubic displayknots nknots(`nknots')
	mkspline date_post=date if date>=td(${PIPA_implement}), cubic displayknots nknots(`nknots')

//PART 1: BOTH AREAS IN LEVELS

	//Estimate separate response functions
	//PIPA, pre
	reg `oVar' date_pre* if PIPA==1 & date<td(${PIPA_implement})		
	gen `oVar'_h1_pre=_b[_cons]
	forvalues k=1(1)`max' {
		replace `oVar'_h1_pre = `oVar'_h1_pre + _b[date_pre`k']*date_pre`k'
	}

	//PIPA, post
	reg `oVar' date_post*  if PIPA==1 & date>=td(${PIPA_implement})	
	gen `oVar'_h1_post=_b[_cons]
	forvalues k=1(1)`max' {
		replace `oVar'_h1_post = `oVar'_h1_post + _b[date_post`k']*date_post`k'
	}

	//non-PIPA, pre
	reg `oVar' date_pre*  if PIPA==0 & date<td(${PIPA_implement})	
	gen `oVar'_h0_pre=_b[_cons]
	forvalues k=1(1)`max' {
		replace `oVar'_h0_pre = `oVar'_h0_pre + _b[date_pre`k']*date_pre`k'
	}

	//non-PIPA, post
	reg `oVar' date_post*  if PIPA==0 & date>=td(${PIPA_implement})	
	gen `oVar'_h0_post=_b[_cons]
	forvalues k=1(1)`max' {
		replace `oVar'_h0_post = `oVar'_h0_post + _b[date_post`k']*date_post`k'
	}

	gen `oVar'_h=`oVar'_h1_pre if PIPA==1 & date<td(${PIPA_implement})
	replace `oVar'_h=`oVar'_h1_post if PIPA==1 & date>=td(${PIPA_implement})
	replace `oVar'_h=`oVar'_h0_pre if PIPA==0 & date<td(${PIPA_implement})
	replace `oVar'_h=`oVar'_h0_post if PIPA==0 & date>=td(${PIPA_implement})

	//FIGURE 
	local yMax=1
	gen diff=.0001 if _n==_N //just for creating purple legend

	tw(sc `oVar' date if PIPA==1 & `oVar'<=`yMax', msize(tiny) msymbol(smcircle) mcolor(cranberry*.4)) ///
	(sc `oVar' date if PIPA==0, msize(tiny) msymbol(smcircle) mcolor(ltblue) ///
	tline(${PIPA_implement}, lcolor(gs6) lwidth(.3) lpattern(dash)) ///
	tline(01sep2013, lcolor(gs6) lpattern(solid) lwidth(.3)) ///
	tlabel(none)) ///
	(line `oVar'_h date if PIPA==1 & date<td(${PIPA_implement}),  lwidth(.5) lcolor("228 26 28")) ///
	(line `oVar'_h date if PIPA==1 & date>=td(${PIPA_implement}), lwidth(.5) lcolor("228 26 28")) ///
	(line `oVar'_h date if PIPA==0 & date<td(${PIPA_implement}),  lwidth(.5) lcolor("55 126 184")) ///
	(line `oVar'_h date if PIPA==0 & date>=td(${PIPA_implement}),  lwidth(.5) lcolor("55 126 184")) ///
	(line diff date, lwidth(.5) lcolor(maroon)), ///
	ytitle("Daily fishing hours per area") ///
	yscale(range(0 `yMax')) ylabel(0(.25)`yMax', nogrid) ///
	xtitle("") ///
    legend( order(3 5 7) label(1 "") label(2 "") label(3 "PIPA") label(4 "") label(5 "Kiribati control") label(6 "") label(7 "Difference") ///
    col(1) row(3) ring(0) position(2) size(small)) ///
    graphregion(margin(t=0)) ///
	graphregion(color(white)) name(both_level) ///
	saving($figuresDir/both_level_spline.gph, replace) 


//PART 2: DIFFERENCE ACROSS AREAS
	xtset PIPA date 

//Pre
	forvalues k=1(1)`max' {
			gen PIPAXdate_pre`k'=PIPA*date_pre`k'
	}

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
	forvalues k=1(1)`max' {
			gen PIPAXdate_post`k'=PIPA*date_post`k'
	}

	xi: ivreg2 `oVar' PIPAXdate_post* i.PIPA date_post*  if date>=td(${PIPA_implement}), ///
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

	//Prepare graph
	drop if PIPA==0
	gen diff_h=diff_pre_h if date<td(${PIPA_implement})
	replace diff_h=diff_post_h if date>=td(${PIPA_implement})
	gen zero=0	

	//Question 1: What is the percentage increase in fishing in PIPA pre announcement relative to control?
	gen sign=sign(diff_h)
	gen D_sign=D.sign
	sum date if D_sign>0 & !missing(D_sign)
	local zeroDate=r(mean)
	gen diff_temp=diff_h
	replace diff_temp=0 if date<`zeroDate' 
	
	sum `oVar'_h1_pre if date>=`zeroDate' & date<td(${PIPA_implement})
	return list
	local PIPA_pre_level=r(sum) 

	sum `oVar'_h0_pre if date>=`zeroDate' & date<td(${PIPA_implement})
	return list
	local control_pre_level=r(sum) 

	local pct = (`PIPA_pre_level'-`control_pre_level')/(`control_pre_level')
	display `pct' //1.2881501

	//Question 2: number of days pre vs. number of days post
	// get pre diff cumulative
	sum diff_temp if date<td(${PIPA_implement})
	return list 
	local pre_total=r(sum)

	// get equivalent post cumulative number of days
	gen post_total=sum(diff_temp) if date>=td(01jan2015)
	replace diff_temp=0 if  date>=td(${PIPA_implement}) & abs(post_total)>`pre_total'
	count if date>=td(${PIPA_implement}) & diff_temp!=0 //548


//PART 3: COMBINED FIGURE
	local yMax=.4
	tw ///
	(rarea diff_temp zero date, fcolor(gs12) fintensity(20) lwidth(0) ///
	yline(0, lwidth(.2) lcolor(gs12))	///
	text(-.05 20365 "Preemptive fishing = 1.5 years of ban", size(2.85))) ///
	(rarea diff_pre_h_ub diff_pre_h_lb date, lwidth(.1) clcolor(maroon) alcolor(white) alwidth(.001) fcolor(maroon) fintensity(20)) ///
	(rarea diff_post_h_ub diff_post_h_lb date, lwidth(.5) clcolor(maroon) alcolor(white) alwidth(.001) fcolor(maroon) fintensity(20)) ///
	(line diff_h date if date<td(${PIPA_implement}), lwidth(.5) clcolor(maroon) alcolor(white) alwidth(.001) fcolor(maroon) fintensity(20)) ///
	(line diff_h date if date>=td(${PIPA_implement}), lwidth(.5) clcolor(maroon) alcolor(white) alwidth(.001) fcolor(maroon) fintensity(20) ///
	tline(${PIPA_implement}, lcolor(gs6) lwidth(.3) lpattern(dash)) ///
	tline(01sep2013, lcolor(gs6) lpattern(solid) lwidth(.3)) ///
	tlabel(01jan2012 01jan2013 01jan2014 01jan2015 01jan2016 01jan2017, format(%tdmy))), ///
	ytitle("Diff. in daily fishing hours per area") ///
	yscale(range(0 `yMax')) ylabel(-.4(.2)`yMax', nogrid) ///
	xtitle("Date") ///
    legend(off) ///
    graphregion(margin(b=0)) ///
    graphregion(margin(t=0)) ///
	graphregion(color(white)) name(both_diff) saving($figuresDir/both_diff_spline.gph, replace) 
	graph combine $figuresDir/both_level_spline.gph $figuresDir/both_diff_spline.gph, rows(2) graphregion(color(white)) commonscheme

	graph export $figuresDir/figure3.pdf, replace

	rm $figuresDir/both_diff_spline.gph
	rm $figuresDir/both_level_spline.gph

end

/**************** Figure S1 **********************/
capture program drop figure_spec
program define figure_spec

//Parameters
	local oVar "logistic_hours_n" //outcome variable
	local nknots=6 // number of spline knots
	local NW_bw=60 // Newey-West bandwdith 
	local level =.05 // confidence interval

	local max=`nknots'-1

//Load Data
	use $dataDir/region_day_ready, replace
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
	xi: qui ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  `controls' if date<td(${PIPA_implement}), ///
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

//Part 3: Model 3 (Model 2 + month FEs)
	local controls "sst sst2 i.mofy"
	local M=2

	//Pre
	xi: qui ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  `controls' if date<td(${PIPA_implement}), ///
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

	drop if PIPA==0


//Part 4: Combined graph
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
	(line diff_h_2 date if date>=td(${PIPA_implement}), lwidth(.2) lcolor(maroon) lpattern(shortdash_dot)), ///
	ytitle("Diff. in daily fishing hours per area") ///
	yscale(range(0 `yMax')) ylabel(-.4(.2)`yMax', nogrid) ///
	xtitle("Date") ///
    legend( order(3 5 7) ///
	label (1 "") label(2 " ")  ///
    label(3 "M0: Benchmark") label(4 " ") ///
    label(5 "M1: M0 + SST") label(6 " ") ///
    label(7 "M2: M1 + month FE") label(8 " ") ///
    ring(0) position(2) ///
    col(1) row(5) size(vsmall)) ///
	graphregion(color(white))

	graph export $figuresDir/figureS1.pdf, replace


end

/**************** Figure S2 **********************/
capture program drop figure_knots
program define figure_knots

//Parameters
	local oVar "logistic_hours_n" //outcome variable
	local NW_bw=60 // Newey-West bandwdith 
	local level =.05 // confidence interval

//Load Data
	use $dataDir/region_day_ready, replace
	xtset PIPA time1

//Part 1: Benchmark model (knots=6)
	local nknots=6 // number of spline knots
	local max=`nknots'-1

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

	drop PIPAXdate_pre* date_pre* PIPAXdate_post* date_post*

//Part 2: Model 1 (knots=5)
	local M=1
	local nknots=5
	local max=`nknots'-1

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

	//Pre
	xi: qui ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  `controls' if date<td(${PIPA_implement}), ///
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

	drop PIPAXdate_pre* date_pre* PIPAXdate_post* date_post*

//Part 3: Model 2 (knots=7)
	local M=2
	local nknots=7
	local max=`nknots'-1

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

	//Pre
	xi: qui ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  `controls' if date<td(${PIPA_implement}), ///
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

	drop PIPAXdate_pre* date_pre* PIPAXdate_post* date_post*

	drop if PIPA==0

//Part 4: Combined graph
	local yMax=.4
	tw ///
	(line diff_h date if date<td(${PIPA_implement}), lwidth(.3) lcolor(maroon) ///
	tline(${PIPA_implement}, lcolor(gs6) lwidth(.1) lpattern(dash)) ///
	tlabel(01jan2012 01jan2013 01jan2014 01jan2015 01jan2016 01jan2017, format(%tdmy)) ///
	yline(0, lwidth(.2) lcolor(gs12))) ///
	(line diff_h date if date>=td(${PIPA_implement}), lwidth(.3) lcolor(maroon)) ///
	(line diff_h_1 date if date<td(${PIPA_implement}), lwidth(.3) lcolor(maroon) lpattern(dash)) ///
	(line diff_h_1 date if date>=td(${PIPA_implement}), lwidth(.3) lcolor(maroon) lpattern(dash)) ///
	(line diff_h_2 date if date<td(${PIPA_implement}), lwidth(.3) lcolor(maroon) lpattern(shortdash)) ///
	(line diff_h_2 date if date>=td(${PIPA_implement}), lwidth(.3) lcolor(maroon) lpattern(shortdash)), ///
	ytitle("Diff. in daily fishing hours per area") ///
	yscale(range(0 `yMax')) ylabel(-.4(.2)`yMax', nogrid) ///
	xtitle("Date") ///
    legend( order(1 3 5) ///
	label (1 "M0: 6 knots") label(2 " ")  ///
    label(3 "M1: 5 knots") label(4 " ") ///
    label(5 "M2: 7 knots") label(6 " ") ///
    ring(0) position(2) ///
    col(1) row(5) size(vsmall)) ///
	graphregion(color(white))

	graph export $figuresDir/figureS2.pdf, replace

end

/**************** Figure S3 **********************/
capture program drop figure_stderr
program define figure_stderr

//Parameters
	local oVar "logistic_hours_n" //outcome variable
	local nknots=6 // number of spline knots
	local NW_bw=60 // Newey-West bandwdith 
	local level =.05 // confidence interval

	local max=`nknots'-1

//Load Data
	use $dataDir/region_day_ready, replace
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

//Part 1: Model 1 (15 day Newey-West window)
	local NW_bw=15

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
	gen diff_h_lb=diff_pre_h_lb if date<td(${PIPA_implement})
	replace diff_h_lb=diff_post_h_lb if date>=td(${PIPA_implement})
	gen diff_h_ub=diff_pre_h_ub if date<td(${PIPA_implement})
	replace diff_h_ub=diff_post_h_ub if date>=td(${PIPA_implement})

//Part 2: Benchmark model (60 day Newey-West window)
	local M=1
	local NW_bw=60

	//Pre
	xi: ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  if date<td(${PIPA_implement}), ///
		bw(`NW_bw')	
		loc z_value = invnormal(`level')			

    //Point estimates and confidence intervals
    gen diff_pre_h_lb_`M' =0
    gen diff_pre_h_ub_`M' = 0
    
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
        qui replace diff_pre_h_lb_`M' = diff_pre_h - `z_value'*r(se) if date==`d'
        qui replace diff_pre_h_ub_`M' = diff_pre_h + `z_value'*r(se) if date==`d'
    }

	//Post
	xi: ivreg2 `oVar' PIPAXdate_post* i.PIPA date_post* if date>=td(${PIPA_implement}), ///
		bw(`NW_bw')
	loc z_value = invnormal(`level')			

    //Point estimates and confidence intervals
    gen diff_post_h_lb_`M' =0
    gen diff_post_h_ub_`M' = 0
    
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
        qui replace diff_post_h_lb_`M' = diff_post_h - `z_value'*r(se) if date==`d'
        qui replace diff_post_h_ub_`M' = diff_post_h + `z_value'*r(se) if date==`d'
    }

	gen diff_h_lb_`M'=diff_pre_h_lb_`M' if date<td(${PIPA_implement})
	replace diff_h_lb_`M'=diff_post_h_lb_`M' if date>=td(${PIPA_implement})
	gen diff_h_ub_`M'=diff_pre_h_ub_`M' if date<td(${PIPA_implement})
	replace diff_h_ub_`M'=diff_post_h_ub_`M' if date>=td(${PIPA_implement})

//Part 3: Model 2 (90 day Newey-West window)
	local M=2
	local NW_bw=90

	//Pre
	xi: ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  if date<td(${PIPA_implement}), ///
		bw(`NW_bw')	
		loc z_value = invnormal(`level')			

    //Point estimates and confidence intervals
    gen diff_pre_h_lb_`M' =0
    gen diff_pre_h_ub_`M' = 0
    
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
        qui replace diff_pre_h_lb_`M' = diff_pre_h - `z_value'*r(se) if date==`d'
        qui replace diff_pre_h_ub_`M' = diff_pre_h + `z_value'*r(se) if date==`d'
    }

	//Post
	xi: ivreg2 `oVar' PIPAXdate_post* i.PIPA date_post* if date>=td(${PIPA_implement}), ///
		bw(`NW_bw')
	loc z_value = invnormal(`level')			

    //Point estimates and confidence intervals
    gen diff_post_h_lb_`M' =0
    gen diff_post_h_ub_`M' = 0
    
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
        qui replace diff_post_h_lb_`M' = diff_post_h - `z_value'*r(se) if date==`d'
        qui replace diff_post_h_ub_`M' = diff_post_h + `z_value'*r(se) if date==`d'
    }

	gen diff_h_lb_`M'=diff_pre_h_lb_`M' if date<td(${PIPA_implement})
	replace diff_h_lb_`M'=diff_post_h_lb_`M' if date>=td(${PIPA_implement})
	gen diff_h_ub_`M'=diff_pre_h_ub_`M' if date<td(${PIPA_implement})
	replace diff_h_ub_`M'=diff_post_h_ub_`M' if date>=td(${PIPA_implement})


//Part 4: Model 3 (month-level clusters)
	local M=3

	//Pre
	xi: ivreg2 `oVar' PIPAXdate_pre* i.PIPA date_pre*  if date<td(${PIPA_implement}), ///
		cluster(month)
		loc z_value = invnormal(`level')			

    //Point estimates and confidence intervals
    gen diff_pre_h_lb_`M' =0
    gen diff_pre_h_ub_`M' = 0
    
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
        qui replace diff_pre_h_lb_`M' = diff_pre_h - `z_value'*r(se) if date==`d'
        qui replace diff_pre_h_ub_`M' = diff_pre_h + `z_value'*r(se) if date==`d'
    }

	//Post
	xi: ivreg2 `oVar' PIPAXdate_post* i.PIPA date_post* if date>=td(${PIPA_implement}), ///
		bw(`NW_bw')
	loc z_value = invnormal(`level')			

    //Point estimates and confidence intervals
    gen diff_post_h_lb_`M' =0
    gen diff_post_h_ub_`M' = 0
    
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
        qui replace diff_post_h_lb_`M' = diff_post_h - `z_value'*r(se) if date==`d'
        qui replace diff_post_h_ub_`M' = diff_post_h + `z_value'*r(se) if date==`d'
    }

	gen diff_h_lb_`M'=diff_pre_h_lb_`M' if date<td(${PIPA_implement})
	replace diff_h_lb_`M'=diff_post_h_lb_`M' if date>=td(${PIPA_implement})
	gen diff_h_ub_`M'=diff_pre_h_ub_`M' if date<td(${PIPA_implement})
	replace diff_h_ub_`M'=diff_post_h_ub_`M' if date>=td(${PIPA_implement})

	drop if PIPA==0

//Part 5: Combined figure 
	local yMax=.6
	tw ///
	(line diff_h date if date<td(${PIPA_implement}), lwidth(.3) lcolor(maroon) ///
	tline(${PIPA_implement}, lcolor(gs6) lwidth(.1) lpattern(dash)) ///
	tlabel(01jan2012 01jan2013 01jan2014 01jan2015 01jan2016 01jan2017, format(%tdmy)) ///
	yline(0, lwidth(.2) lcolor(gs12))) ///
	(line diff_h date if date>=td(${PIPA_implement}), lwidth(.3) lcolor(maroon)) /// 
	(line diff_h_lb date if date<td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(dash)) ///
	(line diff_h_lb date if date>=td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(dash)) ///
	(line diff_h_ub date if date<td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(dash)) ///
	(line diff_h_ub date if date>=td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(dash)) ///
	(line diff_h_lb_1 date if date<td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(shortdash)) ///
	(line diff_h_lb_1 date if date>=td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(shortdash)) ///
	(line diff_h_ub_1 date if date<td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(shortdash)) ///
	(line diff_h_ub_1 date if date>=td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(shortdash)) ///
	(line diff_h_lb_2 date if date<td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(shortdash_dot)) ///
	(line diff_h_lb_2 date if date>=td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(shortdash_dot)) ///
	(line diff_h_ub_2 date if date<td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(shortdash_dot)) ///
	(line diff_h_ub_2 date if date>=td(${PIPA_implement}), lwidth(.1) lcolor(maroon) lpattern(shortdash_dot)) ///
	(line diff_h_lb_3 date if date<td(${PIPA_implement}), lwidth(.35) lcolor(maroon) lpattern(dot)) ///
	(line diff_h_lb_3 date if date>=td(${PIPA_implement}), lwidth(.35) lcolor(maroon) lpattern(dot)) ///
	(line diff_h_ub_3 date if date<td(${PIPA_implement}), lwidth(.35) lcolor(maroon) lpattern(dot)) ///
	(line diff_h_ub_3 date if date>=td(${PIPA_implement}), lwidth(.35) lcolor(maroon) lpattern(dot)), ///
	ytitle("Diff. in daily fishing hours per area") ///
	yscale(range(0 `yMax')) ylabel(-.4(.2)`yMax', nogrid) ///
	xtitle("Date") ///
    legend( order(7 3 11 15) ///
	label (1 " ") label(2 " ")  ///
    label(3 "M1: Newey-West, 15-day") label(4 " ") ///
    label(5 " ") label(6 " ") ///
    label(7 "M0: Newey-West, 60-day") label(8 " ") ///
    label(9 " ") label(10 " ") ///
    label(11 "M2: Newey-West, 90-day") label(12 " ") ///
    label(13 " ") label(14 " ") ///
    label(15 "M3: Month cluster") label(16 " ") ///
    label(17 " ") label(18 " ") ///          
    ring(0) position(2) ///
    col(1) row(5) size(vsmall)) ///
	graphregion(color(white))

	graph export $figuresDir/figureS3.pdf, replace


end


/**************** Figure S4 **********************/
capture program drop figure_outcome
program define figure_outcome

//Parameters
	local nknots=6 // number of spline knots
	local NW_bw=60 // Newey-West bandwdith 
	local level =.05 // confidence interval

	local max=`nknots'-1

//Load Data
	use $dataDir/region_day_ready, replace
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

//Part 1: Benchmark model using logistic-predicted effort

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

//Part 2: Model 2 using logistic-predicted effort
	//local controls "sst sst2"
	local M=1

	//Pre
	xi: qui ivreg2 nnet_hours_n PIPAXdate_pre* i.PIPA date_pre*  if date<td(${PIPA_implement}), ///
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
	xi: qui ivreg2 nnet_hours_n PIPAXdate_post* i.PIPA date_post* `controls' if date>=td(${PIPA_implement}), ///
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

//Part 3: Combined figure 
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
    label(3 "M0: logistic-based effort") label(4 " ") ///
    label(5 "M1: neural net-based effort") label(6 " ") ///
    ring(0) position(2) ///
    col(1) row(5) size(vsmall)) ///
	graphregion(color(white))

	graph export $figuresDir/figureS4.pdf, replace

end


/************************************* Run Main Program ****************************************/

main

/*================================== END ==============================================*/



