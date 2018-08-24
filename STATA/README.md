# STATA replication code for "The blue paradox: Preemptive overfishing in marine reserves"

This sub-directory contains the data and STATA scripts needed to replicate Figs. 3 and S1-S4 of McDermott, Meng, *et al*. (PNAS, 2018). The code was written by Kyle Meng (kmeng@bren.ucsb.edu).

## Requirements

These scripts were developed for STATA v13.

## Structure

The STATA section of the repo contains the following files and sub-directories:

*Note that the assumed relative path for running all of the STATA code is `blueparadox/STATA` (i.e. the location of this README). This relative path should be set automatically if you open STATA by clicking on any of the do-files below. However, users should manually set their local directory if that is not the case.*

1. `prepare_data.do`: STATA do-file for loading, cleaning and merging the [raw data files](https://github.com/grantmcdermott/blueparadox/tree/master/data). Running this file will generate `data/region_day_ready.dta`, which is the STATA data file used for the actual analysis (see below). 

2. `data/region_day_ready.dta`: STATA-ready dataset produced from running the above do-file. This is the primary dataset for executing all of the analysis.

3. `figures.do`: STATA do-file for executing the analysis. Running this do-file will generate Figs. 3 and S1-S4 from the paper and then export them to the `blueparadox/figures` directory higher up in the repo.

4. `STATA_toolbox/i/ivreg2.ado`: STATA ado program for the regression procedure used in the paper. 
