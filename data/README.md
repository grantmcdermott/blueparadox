# Data for "The blue paradox: Preemptive overfishing in marine reserves"

This sub-directory contains all the data used in McDermott, Meng, *et al*. (PNAS, 2018).

The primary data file is `gfw_split.csv`. This contains vessel-by-day level fishing effort data within the Kiribati EEZ over 2012-2016, split across the EEZ's three distinct sub-areas (the Phoenix Islands, the Line Islands, and the Gilbert Islands). Vessels are identified by unique [MMSI](https://en.wikipedia.org/wiki/Maritime_Mobile_Service_Identity) numbers and the dataset includes various other covariates of interest, such as flag of origin and owner company. The data are obtained from [Global Fishing Watch](http://globalfishingwatch.org). 

Several auxilliary data files are also used at different points in the analysis: 
- `sst_split.csv`: Mean daily sea surface temperatures split out by Kiribati EEZ sub-area. These data were obtained from Google Earth Engine. A reproducible script for doing so may be found [here](https://code.earthengine.google.com/37b28087611328018b11eac46d8d1966).
- `wdpa.csv`: World Database on Protected Areas (WDPA). Obtained from [Protected Planet](https://www.protectedplanet.net).
- `mpa_by_region.csv`: MPA-coverage-by-FAO-region (including MPAs already designated and non terrestrial). Also obtained from [Protected Planet](https://www.protectedplanet.net/marine).
- `stock_status.csv` and `status_by_FAO_region.csv`: Current fish stock status and stock status by FAO region. Obtained from [Costello *et al.* (2016)](http://www.pnas.org/content/113/18/5125).
- `fao_names.csv`: FAO names for labelling.
- Various shapefiles for map plotting, including the Kiribati EEZ (and PIPA) and the global FAO regions. These are held in the `shapefiles/` sub-directory.