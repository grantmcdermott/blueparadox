/******************************  METADATA *********************************
Replication for McDermott, Meng, McDonald, and Costello, "The Blue Paradox: Preemptive Overfishing in Marine Reserves", PNAS, 2018.

Script Description: This STATA script inputs and merges data for regression analysis. Code developed for STATA v13.
Date: July 24, 2018
**************************************************************************/

/******************************  HEADER **********************************/
	clear all
	set trace off
	set more off
	set matsize 10000
	capture log close


//Set directories  
	cd . //The assumed relative path is "blueparadox/STATA". Users must set this local directory manually if that is not the case. 

	global inputDir "../data" 
	global dataDir "data" 
	global figuresDir "../figures"


********************* MAIN ****************************/

//Part 1: Load raw SST data
	insheet using $inputDir/sst_split.csv, comma names clear
	drop if region=="not_pipa"
	gen PIPA=1 if regexm(region,"pipa") 
	replace PIPA=0 if regexm(region, "gilbert") | regexm(region, "line")
	drop if regexm(region, "not_pipa") //dropping non-PIPA Phoenix Group
	drop region	
	collapse (mean) sst, by(date PIPA)

	gen date2=date(date, "YMD") //format date variable
	format %td date2
	drop date
	rename date2 date

	tempfile temp
	save `temp'

//Part 2: Load raw GFW data
	insheet using $inputDir/gfw_split.csv, comma names clear

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

//create time variables
	gen mofy=month(date)
	gen month=mofd(date)
	format month %tm

	//creating time index
	xtset PIPA date
	sort PIPA date
	bys PIPA: gen time1=_n

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
	}

//Part 3: Merge data
	merge date PIPA using `temp', unique sort
	drop _merge
	gen sst2=sst^2

	drop area nnet_hours logistic_hours

//Part 4: Label data
	label var PIPA "PIPA dummy (=1 if PIPA)"
	label var date "calendar date"
	label var mofy "Month of the year (1-12)"
	label var month "calendar month"
	label var time1 "daily time index"
	label var logistic_hours_n "fishing hours per area (logistic-based)"
	label var nnet_hours_n "fishing hours per area (neural net-based)"
	label var sst "average sea surface temperature (degrees C)"
	label var sst2 "average sea surface temperature squared"

	save $dataDir/region_day_ready, replace




