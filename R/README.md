# *R* replication code for "The blue paradox: Preemptive overfishing in marine reserves"

This sub-directory contains the *R* code used in McDermott, Meng, *et al*. (2018). It contains a single script file, `R/blueparadox.R`, which can be used to reproduce Figs. 1-2, 4-5, and S5-S6 from the paper. In addition, while Fig. 3 was originally produced using STATA (see the accompanying sub-directory), the script contains code for producing a mock-up version in *R*.

This *R* code was jointly written by Grant McDermott (grantmcd@uoregon.edu) and Gavin McDonald (gmcdonald@bren.ucsb.edu).

## Requirements

#### Step 1: Install R and R libaries

**Note:** The code was most recently tested and updated against *R* 4.0.3. *R* is free, open-source and available for download [here](https://www.r-project.org/).

We use [**renv**](https://rstudio.github.io/renv/) to snapshot the project environment. Run the following command(s) from your *R* console to pull in all of the necessary libraries.

```r
# renv::init()   ## Only necessary if you didn't clone/open the repo as an RStudio project
renv::restore()  ## Enter "y" when prompted
```

#### Step 2: Install system dependencies (only if applicable)

While the `renv::restore()` command above should install [package binaries](https://packagemanager.rstudio.com/) on most operating systems (OS), it will not necessarily import *system* dependencies on some Linux builds. In our case, the most obvious system dependencies are related to the underlying geospatial libaries that power the [**sf**](https://r-spatial.github.io/sf/#installing) package. You can double check that you have met the requirements for your OS by clicking the previous link. As an alternative, you can also try running the [`remotes::system_requirements()`](https://remotes.r-lib.org/reference/system_requirements.html) command. For example, if we wanted to check what requirements were required for Ubuntu 20.04, we could run:

```r
remotes::system_requirements(os = 'ubuntu', os_release = '20.04', 
                             path = 'renv/library/R-4.0/x86_64-pc-linux-gnu/sf/')
```

#### Optional: Fonts

We use the `extrafont` package embed [Open Sans](https://fonts.google.com/specimen/Open+Sans) fonts in some of the figures. Please note that the Open Sans fonts would have to be installed separately on your system and thus requires some minor setup upon first use. See [here](https://github.com/wch/extrafont/blob/master/README.md) for instructions. Feel free to skip this step if that all sounds like too much work. The code will automatically use one *R*'s default Arial fonts if others are not available.