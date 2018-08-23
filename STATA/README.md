# STATA replication code for "The blue paradox: Preemptive overfishing in marine reserves"

This sub-directory contains the data and STATA scripts needed to replicate Figs. 3 and S1-S4 of McDermott, Meng, *et al*. (PNAS, 2018). The code was written by Kyle Meng (kmeng@bren.ucsb.edu).

## Requirements

These scripts were developed for STATA v13.

## Structure

This STATA section of the repo contains two sub-directories: 1) `code/` and 2) `data/`.

### 1) `code/`

STATA code for executing the analysis. It contains 3 files: 
1. `code/prepare_data.do` loads, cleans and merges the raw data files (described in `..data/README.md`) to generate `data/region_day_ready.dta`, the STATA data file used for Figures 3, S1-S4. Users must change the local directory to one's own machine.
2. `code/figures.do` generates all figures. Users should change the local directory to one's own machine. Note, however, that it is assumed the figures will be exported to a parent `blueparadox/figures` directory in accordance with the structure of this repo. 
3. `code/STATA_toolbox/i/ivreg2.ado` is the STATA ado program for the regression procedure used in the paper. 

### 2) `data/`

This directory contains `data/region_day_ready.dta`, which is the primary dataset ready for use in the STATA analysis. (Not to be confused with the raw/input data directory that is shared by both the STATA and *R* scripts in the repo; note the missing `../` at the beginning of the relative path.) 
