library(sf)
library(maps)
library(rworldmap)
## Note: Need development version of ggsn for map to work
library(ggsn) ## devtools::install_github('oswaldosantos/ggsn')
library(sp)
library(raster)
library(rms)
library(tidyverse)
library(broom)
library(estimatr)
library(furrr)
# library(grid) ## Loads with ggsn
library(scales)
library(lubridate)
library(cowplot)
library(extrafont)

#######################################################
########## LOAD FUNCTIONS AND GLOBAL ELEMENTS #########
#######################################################

### Assign global elements for figures. 

## Assign font. Note that fonts should be registered to the extrafont package DB 
## first. If the below font is not available, then extrafont package will use the 
## Arial default. For instructions, see: https://github.com/wch/extrafont
## Open Sans can be downloaded here: https://fonts.google.com/specimen/Open+Sans.
## Alternatively, you can try the showtext package: https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html
font_type <- choose_font(c("Open Sans", "sans")) 

## Set the plotting theme.
theme_set(
  theme_cowplot() + 
    theme(
      text = element_text(family = font_type),
      legend.justification = "center",
      strip.background = element_rect(colour = "white", fill = "white"),
      panel.spacing = unit(2, "lines")
      )
  )

## Key PIPA announcement and enforcement dates
anticip_date <- ymd("2013-09-01") ## http://www.earthisland.org/journal/index.php/eij/article/somethings_fishy/
enforce_date <- ymd("2015-01-01")
key_dates <- c(anticip_date, enforce_date)

## Use Robin World projection for maps
proj_string <- "+proj=robin +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +lon_0=180" 

####################################################
########## READ IN THE PRIMARY SOURCE DATA #########
####################################################

## 1) GFW data: Daily, vessel-level fishing effort effort in Kirbati, split by sub-area.
gfw <- read_csv("data/gfw_split.csv") 

## 2) SST from NOAA via Google Earth Engine. 
## See: https://code.earthengine.google.com/37b28087611328018b11eac46d8d1966
sst <- read_csv("data/sst_split.csv") 


####################################
########### MAIN FIGURES ########### 
####################################

################################
### Fig. 1 (MPAs over time) ####
################################

## Read in the World Database on Protected Areas (WDPA).
## Downloaded from: https://www.protectedplanet.net/
## Will get some warning messages but these can be ignored.
wdpa <- read_csv("data/wdpa.csv")

## Collapse into yearly MPA totals (i.e. How many MPAs were designated that year)
mpas <- 
  wdpa %>%
  ## MARINE: Allowed values: 0 (100% Terrestrial PA), 1 (Coastal: marine and terrestrial PA), and 2 (100 % marine PA).
  ## STATUS: Allowed values: Proposed, Inscribed, Adopted, Designated, Established
  ## STATUS_YR: Year of enactment of status (STATUS field).
  filter(MARINE %in% c(1,2) & STATUS == "Designated" & STATUS_YR != 0) %>%
  group_by(STATUS_YR) %>%
  ## NAME: Name of the protected area (PA) as provided by the data provider.
  ## REP_M_AREA: Marine area in square kilometers.
  summarize(
    num_mpas = n_distinct(NAME),
    area_km2 = sum(REP_M_AREA)
    ) %>%
  ungroup() %>%
  arrange(STATUS_YR) %>%
  mutate(
    num_mpas_culm = cumsum(num_mpas),
    area_km2_culm = cumsum(area_km2)
    )
rm(wdpa)

## Cumulative MPAs over time
fig1 <-
  mpas %>%
  filter(STATUS_YR>=1900) %>%
  ggplot(aes(x=STATUS_YR, y=num_mpas_culm)) +
  geom_point(aes(size=area_km2_culm/1e6, color=area_km2_culm/1e6), alpha=0.6) +
  scale_y_continuous(labels = scales::comma) +
  ylab("Cumulative number of MPAs") +
  scale_color_continuous(
    name = expression(atop("Cumulative area", (paste("million km" ^2))))
    ) +
  scale_size_continuous(
    name = expression(atop("Cumulative area", (paste("million km" ^2))))
    ) +
  guides(
    color= guide_legend(),
    size = guide_legend()
    ) +
  theme(axis.title.x = element_blank())
#
fig1 +
  ggsave(
    "figures/PNGs/figure1.png",
    width = 6, height = 4
    )
#
fig1 +
  ggsave(
    "figures/figure1.pdf",
    width = 6, height = 4,
    device = cairo_pdf
    )

dev.off()
rm(mpas, fig1)


#############################
### Fig. 2 (Kiribati Map) ###
#############################

## Read in the Kiribati EEZ shapefile
eez_kiribati <- 
  read_sf(
    dsn = "data/shapefiles/World_EEZ_v9_20161021_LR", 
    layer = "eez_lr"
    ) %>%
  subset(
    GeoName %in% c("Kiribati Exclusive Economic Zone (Phoenix Islands)",
                   "Kiribati Exclusive Economic Zone (Line Islands)",
                   "Kiribati Exclusive Economic Zone (Gilbert Islands)")
    ) %>%
  st_transform(proj_string)

## Read in the the PIPA shapefile (which is nested inside the Kirbati EEZ)
pipa_shape <- 
  read_sf(
    dsn = "data/shapefiles/pipa_shapefile", 
    layer = "worldheritagemarineprogramme"
    ) %>%
  st_transform(proj_string)

## Define a bounding box for Kiribati
kbbox <- c(167, 214, -14.25, 8.25)
## Create a spatial polygon of the Kiribati bounding box
kiribati_bounding_box <- as(raster::extent(kbbox), "SpatialPolygons")
## Set the projection for the Kiribati bounding box (have to do first in this case)
proj4string(kiribati_bounding_box) <- "+proj=longlat +datum=WGS84 +no_defs"
## Transform Kiribati bounding box into SF
kiribati_bounding_box <- st_as_sf(kiribati_bounding_box) 

## Create a (Pacific-centered) world sf object from the maps package
world2 <- st_as_sf(maps::map("world2", plot=F, fill=T))

## Create a plot basemap, including the Kiribati EEZ, PIPA, Line Islands, 
## Gilbert Islands, and the global map. This base map will then be manipulated 
## to make the Kirbati map and the global inset map. These elements will finally 
## be combined.
base_map <- 
  ggplot() +
  geom_sf(
    data = eez_kiribati,# %>% st_transform(proj_string),
    aes(fill = "Phoenix Islands (Non-PIPA)"),
    lwd=0.25
    ) +
  geom_sf(
    data = pipa_shape,# %>% st_transform(proj_string),
    aes(fill = "PIPA     "),
    lwd=0.25
    ) +
  geom_sf(
    data = eez_kiribati %>%
      subset(GeoName %in% c("Kiribati Exclusive Economic Zone (Line Islands)",
                            "Kiribati Exclusive Economic Zone (Gilbert Islands)")
             ), #%>%
            # st_transform(proj_string),
    aes(fill = "Kiribati control    "),
    lwd=0.25
    ) +
  geom_sf(
    data = world2,
    fill = "black", col = "black", lwd = 0
    ) +
  theme_bw() +
  theme(
    legend.title=element_blank(),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor = element_line(color = "white"),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank()
    ) +
  scale_fill_manual(
    values=c("Phoenix Islands (Non-PIPA)" = "lightgrey",
             "PIPA     " = "#e41a1c",
             "Kiribati control    " = "#377eb8"),
    breaks=c("PIPA     ", "Kiribati control    ", "Phoenix Islands (Non-PIPA)")
    ) 

## Create the world map for inset
w_map <- 
  base_map +
  geom_sf(
    data = kiribati_bounding_box %>% st_transform(proj_string),
    fill = NA, alpha = 1, lwd=0.25
    ) +
  guides(fill=FALSE)


## Make the bounding box object
map_bbox <- 
  kiribati_bounding_box %>%
  st_transform(proj_string) %>%
  st_bbox()

## Crop the base map, add scale bar and legend
k_map <- 
  base_map +
  coord_sf(
    xlim = c(map_bbox["xmin"],map_bbox["xmax"]), 
    ylim = c(map_bbox["ymin"],map_bbox["ymax"])
    )  +
  ggsn::scalebar(
    data = eez_kiribati,# %>% st_transform(proj_string),
    dist = 1000,
    dd2km = FALSE,
    st.dist = 0.05,
    st.size = 5,
    location="topleft"
    ) +
  xlab("") +
  ylab("") +
  theme(
    panel.grid.major = element_line(color = "white"),
    legend.position = "bottom",
    legend.direction="horizontal",
    legend.text=element_text(size=12)
    )

## Finally, combine the world map inset and the Kiribati map
fig2 <- 
  ggdraw() +
  draw_plot(k_map,0, 0, 0.8, 0.8) +
  draw_plot(w_map,0.65,0.55,0.35,0.35)

## Save the map
save_plot(
  "figures/PNGs/figure2.png",
  fig2,
  base_width = 6, base_height = 4
  )
## Save the map
save_plot(
  "figures/figure2.pdf",
  fig2,
  base_width = 6, base_height = 4
  )

dev.off()
rm(fig2, k_map, w_map, kiribati_bounding_box, base_map, eez_kiribati, 
   pipa_shape, kbbox, map_bbox, world2)

#################################################
### Fig. 3 (PIPA vs non-PIPA fishing effort) ####
#################################################

## First, we need to combine Gilbert Islands and Line Group into a single  
## "Control" region. We'll drop the"Phoenix Group Non-PIPA" in the process 
## (which we're ignoring b/c of spatial spillover concerns).

## Start by getting the total area for this combined control region.
control_area_km2 <- 
  gfw %>%
  filter(region %in% c("Gilbert Islands", "Line Group")) %>%
  filter(!is.na(area_km2)) %>%
  distinct(area_km2) %>%
  pull(area_km2) %>% 
  sum()

## Next, replace the area_km2 value for the (combined) control region and create 
## normalized measures of fishing effort.
gfw <-
  gfw %>%
  filter(region != "Phoenix Group Non-PIPA") %>%
  mutate(region = ifelse(region=="PIPA", "PIPA", "Control")) %>%
  # filter(!is.na(area_km2)) %>%
  mutate(area_km2 = ifelse(region=="Control", control_area_km2, area_km2)) %>%
  mutate(
    raw_hours_norm = raw_hours/area_km2,
    logistic_hours_norm = logistic_hours/area_km2,
    nnet_hours_norm = nnet_hours/area_km2
    )

## Collapse into a new data frame of daily means, i.e. summed across all vessels 
## by region.
gfw_daily <- 
  gfw %>%
  group_by(Date, region, yr, mnth, wk, dofw) %>%
  select(contains("hours")) %>% ## Will automatically select group variables above too.
  summarise_at(vars(raw_hours:nnet_hours_norm), sum, na.rm = T) %>%
  ungroup

## Add regional SST (making sure to adjust the control region as per the above)
gfw_daily <-
  gfw_daily %>%
  left_join(
    sst %>%
      filter(region != "not_pipa") %>%
      mutate(region = ifelse(region=="pipa", "PIPA", "Control")) %>%
      group_by(Date, region) %>%
      summarise(sst = mean(sst, na.rm=T)) 
    ) %>%
  ungroup

## Lastly, multiply fishing effort by 1,000 to get (normalised) hours per 
## 1,000 km^2. Not strictly necessary, but helpful for output readability (e.g.
## regression tables that default to three decimal places). 
gfw_daily <-
  gfw_daily %>%
  mutate_at(vars(raw_hours:nnet_hours_norm), funs(.*1000)) 

## Regression analysis

## Choose the number of knots for fitting the restricted cubic spline (RCS) in 
## the pre- and post-enforcement periods, respectively.
knots_pre <- 6
knots_post <- 6

## Now, create a faceted plot with the "break even" difference at the bottom.
## Choose which fishing effort measure(s) to use
f_measure <- c("logistic_hours_norm", "nnet_hours_norm")[1] ## Remove the '[1]' to produce figs for both measures
lapply(f_measure, function(f_var) {
  
  f_var <- enexpr(f_var)
  
  gfw_daily_main <- gfw_daily
  
  gfw_daily_diff <- 
    gfw_daily_main %>% 
    select(Date, region, !!f_var) %>% 
    spread(region, !!f_var) %>%
    mutate(y_var = PIPA - Control) %>% 
    mutate(
      region = "Difference", 
      grp = "Diff"
    ) %>% 
    select(Date, region, y_var, grp)
  
  gfw_daily_main <-
    gfw_daily_main %>%
    mutate(grp = "Main") %>% 
    rename("y_var" = enexpr(f_var)) %>% 
    select(Date, region, y_var, grp)
  
  ## Compute the RCSs separately for the pre- and post-enforcement periods
  gfw_daily_new <- bind_rows(gfw_daily_main, gfw_daily_diff)
  gfw_daily_new$region <- factor(gfw_daily_new$region, levels  = c("PIPA", "Control", "Difference"))
  gfw_daily_new$grp <- factor(gfw_daily_new$grp, levels  = c("Main", "Diff"))
  
  gfw_daily1 <-
    gfw_daily_new %>%
    filter(Date<enforce_date) %>%
    mutate(smooth = predict(lm_robust(y_var ~ region*rcs(as.numeric(Date), knots_pre))))
  gfw_daily2 <-
    gfw_daily_new %>%
    filter(Date>=enforce_date) %>%
    mutate(smooth = predict(lm_robust(y_var ~ region*rcs(as.numeric(Date), knots_post))))
  gfw_daily_new <- bind_rows(gfw_daily1, gfw_daily2)

  ## Find the "break even" point at which the enforced ban finally makes up for the
  ## equivalent increase in effort.
  # start_date <- anticip_date
  start_date <-
    gfw_daily_new %>%
    filter(grp=="Diff", Date<=anticip_date) %>%
    arrange(desc(Date)) %>%
    filter(y_var<=0) %>%
    slice(1) %>%
    pull(Date) + days(1)
  
  break_even <- 
    gfw_daily_new %>% 
    filter(grp=="Diff", Date>=start_date) %>%  
    mutate(culm_diff = cumsum(smooth)) %>% 
    filter(Date>enforce_date, culm_diff<=0) %>% 
    slice(1) %>% 
    pull(Date)

  ann_text <-
    data.frame(
      Date = ymd("2015-03-01"), 
      y_var = -0.05,
      lab = paste0("Break even = ",round((enforce_date%--%break_even) / dyears(1), 1), " years"),
      grp = factor("Diff", levels  = c("Main", "Diff")),
      region <- factor("Differences", levels  = c("PIPA", "Control", "Difference"))
      )

  fig3 <-
    gfw_daily_new %>%
    filter(y_var <= 1) %>% ## To improve visual inspection
    filter(grp=="Main") %>%
    ggplot(aes(x=Date, y=y_var, col=region, fill=region)) +
    geom_hline(yintercept = 0, lwd = 0.25, col = "gray") +
    ## Add the shaded "break even" ribbon.
    geom_ribbon(
      data = gfw_daily_new %>% filter(grp=="Diff", Date>=start_date, Date<=break_even),
      inherit.aes = F, ## Otherwise causes scales="free" option of facet_wrap to fail for some reason
      aes(x = Date, ymin=0, ymax=smooth),
      fill="gray", col=NA, alpha=0.5
      ) +
    geom_point(alpha=0.3, size = 0.5) +
    geom_line(data = gfw_daily_new %>% filter(grp=="Main", Date<enforce_date), aes(y=smooth), lwd = 1) +
    geom_line(data = gfw_daily_new %>% filter(grp=="Main", Date>=enforce_date), aes(y=smooth), lwd = 1) +
    stat_smooth(
      data = gfw_daily_new %>% filter(grp=="Diff", Date<enforce_date),
      method = "lm_robust", formula = y ~ rcs(x, knots_pre),
      show.legend = F
      ) +
    stat_smooth(
      data = gfw_daily_new %>% filter(grp=="Diff", Date>=enforce_date),
      method = "lm_robust", formula = y ~ rcs(x, knots_post),
      show.legend = F
      ) +
    geom_vline(xintercept = anticip_date, col = "gray15") +
    geom_vline(xintercept = enforce_date, col = "gray15", lty = 5) +
    ## Add "break even" text.
    geom_text(
      data = ann_text,
      aes(label=lab), hjust = 0, family = font_type, col = "black", show.legend = F
      ) +
    labs(
      y = "Daily fishing hours per '000 km",
      caption = "Note: Mock-up figure only. See accompanying STATA code for correct standard errors, etc."
      ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_colour_manual(
      values = c("PIPA"="#E41A1C", "Control"="#377EB8", "Difference"="maroon"),
      breaks = c("PIPA", "Control", "Difference")
      ) +
    scale_fill_manual(
      values = c("PIPA"="#E41A1C", "Control"="#377EB8", "Difference"="maroon"),
      breaks = c("PIPA", "Control", "Difference")
      ) +
    facet_wrap(~grp, scales = "free_y", ncol = 1) +
    theme(
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.key.width = unit(3, "lines"),
      axis.title.x = element_blank(),
      strip.text = element_blank(),
      plot.caption = element_text(face="italic")
      ) 
  
  f_name <- "figure3-R-mockup"
  if(f_var!="logistic_hours_norm") {
    f_name <- paste0(f_name, "-", gsub("_hours_norm","",f_var))
  }
  
  fig3 +
    ggsave(
      paste0("figures/PNGs/", f_name,".png"),
      width = 10, height = 8
      )
  fig3 +
    ggsave(
      paste0("figures/", f_name,".pdf"),
      width = 10, height = 8,
      device = cairo_pdf
      )
  
  print(fig3)

  message(paste0("Break even date is ", break_even))
  
  ## Separately, find the magnitude of the anticipatory effect; i.e. how much
  ## extra fishing was experienced in PIPA relative to the Control region.
  ## Will be used in Figs. 4 and 5.
  if(f_var=="logistic_hours_norm") { ## Change to "nnet_hours_norm" if desired. (Same result.)
    anticip_effect <<-
      gfw_daily %>% 
      filter(Date>=start_date, Date<enforce_date) %>% 
      group_by(region) %>%
      summarise(culm_hours = sum(logistic_hours_norm)) %>%
      spread(region, culm_hours) %>%
      mutate(anticip_effect = round(PIPA/Control, 1)) %>%
      pull(anticip_effect)
  }
  
})

dev.off()
rm(sst, control_area_km2, f_measure)
  
############################################################################
### Fig. 4 (Global extrapolation exercise I: MPA coverage by FAO region) ###
############################################################################

## Read in the FAO regions and convert to sf object
## http://www.fao.org/geonetwork/srv/en/main.home?uuid=ac02a460-da52-11dc-9d70-0017f293bd28
fao_regions <- 
  read_sf(
    dsn = "data/shapefiles/FAO_AREAS", 
    layer = "FAO_AREAS"
    )  %>%
  filter(F_LEVEL=="MAJOR") %>%
  mutate(F_CODE = as.character(F_CODE)) %>%
  st_as_sf() 

## Read in the MPA-coverage-by-FAO-region data.
## Source: WDPA, including MPAs already designated and not terrestrial.
## https://www.protectedplanet.net/marine
mpa_by_region <- read_csv("data/mpa_by_region.csv")

## Fishing effort announcement effect scalar from PIPA D-i-D analysis.
## Should already have been calculated as part of Figure 3 above...
# anticip_effect <- 2.3 

## What MPA size should be applied to each FAO region?
## i.e. The IUCN global target of 30% by 2030: https://portals.iucn.org/congress/motion/053
mpa_size <- 0.3

## Read in current stock status by FAO region. 
## Source: Costello et al. (2016); http://www.pnas.org/content/113/18/5125
## Current F/FMSY for each region is catch-weighted mean for that region.
## These numbers match supplementary materials from that publication.
status_by_fao_region <- 
  read_csv("data/status_by_FAO_region.csv") %>%
  mutate(current_FvFmsy = CatchWtMeanF) %>%
  ## Add column for expected F/FMSY under MPA announcement scenario
  ## Apply announcement effect scalar to proportion of current F/FMSY equal to the increase in MPA size
  ## Leave remaining F/FMSY proportion equal to current levels
  left_join(mpa_by_region %>% rename(RegionFAO = F_CODE)) %>%
  ## If MPA size is already greater than target, don't need to increase MPA size
  mutate(
    mpa_increase = ifelse(fraction_mpa > mpa_size, 0, mpa_size - fraction_mpa),
    ## Proportionally increase current F/FMSY based on announcement effect, and 
    ## fraction of fishing pressure that would be affected by MPA announcement.
    mpa_FvFmsy = (current_FvFmsy*anticip_effect*mpa_increase) + (current_FvFmsy * (1-mpa_increase))
    )

## Read in the stock status from Costello et al. (2016).
## This contains current stock status (F/FMSY) for all stocks in the database.
stock_status <- read.csv("data/stock_status.csv")

## Do the following for each stock: 1) Figure out which FAO regions it exists in, 
## 2) take the average MPA increase across regions, and then 3) figure out what 
## the status would be after MPA announcement.
plan(multiprocess)
stock_status_processed <- 
  furrr::future_map(unique(stock_status$IdOrig), function(x) {
    
    temp_status <- 
      stock_status %>%
      filter(IdOrig == x) %>%
      mutate(RegionFAO = as.character(RegionFAO))
    
    ## Turn character string of FAO regions into vector
    regions <- unique(na.omit(as.numeric(unlist(strsplit(unlist(temp_status$RegionFAO), "[^0-9]+")))))
    
    ## Take average MPA increase across regions
    average_mpa_increase <- 
      status_by_fao_region %>%
      filter(RegionFAO %in% regions) %>%
      .$mpa_increase %>%
      mean()
    
    temp_status %>%
      mutate(
        average_mpa_increase,
        ## Proportionally increase current F/FMSY based on announcement effect, 
        ## and fraction of fishing pressure that would be affected by MPA announcement.
        mpa_FvFmsy = (current_FvFmsy*anticip_effect * average_mpa_increase) + (current_FvFmsy * (1-average_mpa_increase))
        )
    
    }
    ) %>%
  bind_rows()

## Calculate the current fraction of fisheries experiencing overfishing, followed
## by the fraction after MPA announcement.
percent_overfishing <- 
  stock_status_processed %>%
  mutate(
    current_overfishing = ifelse(current_FvFmsy > 1, 1, 0),
    after_overfishing = ifelse(mpa_FvFmsy > 1, 1, 0)
    ) %>%
  summarize(
    overfishing_before = sum(current_overfishing)/n()*100,
    overfishing_after = sum(after_overfishing)/n()*100
    )

## Get the FAO names for labelling
fao_names <- read.csv("data/fao_names.csv")

## Finally, we are ready to plot the MPA coverage by FAO region.
fig4 <- 
  status_by_fao_region %>%
  mutate(RegionFAO = as.character(RegionFAO)) %>%
  left_join(
    fao_names %>%
      select(RegionFAO = Code, name = Name_en) %>%
      mutate(RegionFAO = as.character(RegionFAO))
    ) %>%
  mutate(region_name = paste(RegionFAO,name,sep=" - ")) %>%
  ggplot(aes(y = fraction_mpa, x = reorder(factor(region_name), fraction_mpa))) +
  geom_hline(yintercept = 0.3, linetype = 2,alpha=0.5) +
  geom_bar(stat="identity") +
  geom_text(aes(label = scales::percent(fraction_mpa), y = fraction_mpa + 0.061),size=3.25) +
  xlab("FAO Major Fishing Area") +
  ylab("Current MPA Coverage") +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(labels = scales::percent, limits = c(0,.49), breaks = seq(0,0.5,0.1)) +
  theme(
    panel.grid.major = element_line(color = "white"), 
    panel.grid.minor = element_line(color = "white"),        
    axis.text=element_text(size=10, colour="black")
    )

## Save to disk
fig4 +
  ggsave(
    "figures/PNGs/figure4.png",
    width=5, height=3
    )
fig4 +
  ggsave(
    "figures/figure4.pdf",
    width=5, height=3,
    device = cairo_pdf
    )

dev.off()
rm(fig4, stock_status, stock_status_processed, fao_names, mpa_by_region)

##########################################################################
### Fig. 5 (Global extrapolation exercise II: Map of temporary changes ###
##########################################################################

## Use a conventional (Prime Meridian-centered) Robinson projection for this figure 
proj_string_fao <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" ## Robinson World

## Make long format names in prep for figure
fao_regions_long <- 
  fao_regions %>%
  ## Add before/after F/FMSY info to FAO regions sf object
  left_join(
    status_by_fao_region %>%
      select(F_CODE = RegionFAO,
                    `Current~italic(F/F["MSY"])` = current_FvFmsy,
                    `italic(F/F["MSY"])~after~reserve~announcement` = mpa_FvFmsy
                    ) %>%
      mutate(F_CODE = as.character(F_CODE))
    ) %>% 
  ## Make tibble long, so that we can facet by period
  gather(Period, FvFmsy,`Current~italic(F/F["MSY"])`:`italic(F/F["MSY"])~after~reserve~announcement`) %>%
  st_as_sf() %>%
  ## Reproject to Robinson
  st_transform(proj_string_fao)

# ## Create a (Prime Meridian-centered) world sf object from the maps package
world_land <-
  st_as_sf(rworldmap::countriesLow) %>% ## data(countriesLow) is from the rworldmap package
  st_transform(proj_string_fao)

# Create global plot of current and expected fishery status by FAO region  
fig5 <- 
  ggplot() +
  geom_sf(data = world_land, color = "black", fill = "black") +
  geom_sf(
    data = fao_regions_long,
    aes(fill = FvFmsy), lwd = 0.08, color = "black"
    ) + 
  ## Colourblind-friendly colours
  scale_fill_gradientn(
    colors = c("#360263","darkorchid","gold","orangered1"),
    values = scales::rescale(c(0,1,1.01,max(fao_regions_long$FvFmsy,na.rm=TRUE))),
    limits = c(0,max(fao_regions_long$FvFmsy,na.rm=TRUE)),
    guide = "colorbar",
    name = expression(~italic(F/F["MSY"]))
    ) +
  facet_wrap(~Period,ncol=1,labeller=label_parsed) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "white"), 
    panel.grid.minor = element_line(color = "white"),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    strip.background = element_rect(fill = "white")
    )

## Save to disk
fig5 +
  ggsave(
    filename="figures/PNGs/figure5.png",
    width=4, height=3
    )
fig5 +
  ggsave(
    filename="figures/figure5.pdf",
    width=4, height=3
    )

dev.off()
rm(fig5, fao_regions, fao_regions_long, status_by_fao_region, world_land)

##########################################
### Figs. S5 and S6 (Fleet footprint) ####
##########################################

## First create a data frame of fleet footprints (i.e. relative fishing effort)
## by country.
fleets_country <-
  gfw %>%
  filter(region == "PIPA") %>%
  filter(Date < enforce_date) %>%
  mutate(Period = ifelse(Date < anticip_date, "Pre", "Post")) %>%
  mutate(flag_iso3 = ifelse(is.na(flag_iso3), "N/A", flag_iso3)) %>%
  group_by(flag_iso3, Period) %>%
  summarise_at(vars(c(raw_hours:nnet_hours),c(raw_hours_norm:nnet_hours_norm)), sum, na.rm = T) %>%
  group_by(Period) %>%
  mutate(logistic_hours_rel = logistic_hours/sum(logistic_hours)) %>%
  gather(key, value, -(flag_iso3:Period)) %>%
  filter(key=="logistic_hours_rel") %>%
  select(-key) %>%
  ungroup %>%
  spread(Period, value) %>%
  mutate(Missing = factor(ifelse(is.na(Pre)|is.na(Post), 1, 0))) %>%
  filter(!is.na(Pre) & !is.na(Post)) %>%
  select(Pre, Post, everything()) %>% 
  arrange(desc(Pre))
fleets_country

## Plot the figure
figS5 <-
  fleets_country %>%
  filter(flag_iso3 != "N/A") %>%
  ggplot(aes(x=Pre, y=Post, label=flag_iso3)) +
  geom_abline(slope = 1, lwd = 0.25, lty = 2, col = "grey25") +
  geom_point(stroke = .25, alpha = .7, size = 3) + 
  geom_point(shape = 1, stroke = .25, size = 3) +
  # geom_point() +
  # geom_text(vjust = 0, hjust = 0, nudge_x = 0.05) +
  geom_text(vjust = 0, nudge_y = 0.1) +
  scale_x_log10(labels = scales::percent, limits = c(-Inf, 1), breaks = c(0.001, 0.01, 0.1, 1)) +
  scale_y_log10(labels = scales::percent, limits = c(-Inf, 1), breaks = c(0.001, 0.01, 0.1, 1)) +
  # labs(x = "Before announcement", y = "After announcement") +
  labs(
    x = "Before preemptive phase", 
    y = "During preemptive phase"
    ) +
  theme(legend.position = "none")
#
figS5 +
  ggsave(
    "figures/PNGs/figureS5.png",
    width = 8, height = 6
    )
#
figS5 +
  ggsave(
    "figures/figureS5.pdf",
    width = 8, height = 6,
    device = cairo_pdf
    )

## As above, but now by owner company
fleets_owner <-
  gfw %>%
  filter(region == "PIPA") %>%
  filter(Date < enforce_date) %>%
  mutate(Period = ifelse(Date < anticip_date, "Pre", "Post")) %>%
  mutate(owner = ifelse(is.na(owner), "N/A", owner)) %>%
  group_by(owner, Period) %>%
  summarise_at(vars(c(raw_hours:nnet_hours),c(raw_hours_norm:nnet_hours_norm)), sum, na.rm = T) %>%
  group_by(Period) %>%
  mutate(logistic_hours_rel = logistic_hours/sum(logistic_hours)) %>%
  gather(key, value, -(owner:Period)) %>%
  filter(key=="logistic_hours_rel") %>%
  select(-key) %>%
  ungroup %>%
  spread(Period, value) %>%
  mutate(Missing = factor(ifelse(is.na(Pre)|is.na(Post), 1, 0))) %>%
  # filter(!is.na(Pre) & !is.na(Post)) %>%
  select(Pre, Post, everything()) %>% 
  arrange(desc(Pre))
fleets_owner

## Now draw the plot
figS6 <-
  fleets_owner %>%
  filter(owner != "N/A") %>%
  ggplot(aes(x=Pre, y=Post, label=owner)) +
  geom_abline(slope = 1, lwd = 0.25, lty = 2, col = "grey25") +
  geom_point(stroke = .25, alpha = .7, size = 3) + 
  geom_point(shape = 1, stroke = .25, size = 3) +
  # # geom_text(vjust = 0, hjust = 0, nudge_x = 0.05) +
  # geom_text(vjust = 0, nudge_y = 0.1) +
  scale_x_log10(labels = scales::percent, limits = c(-Inf, 1), breaks = c(0.001, 0.01, 0.1, 1)) +
  scale_y_log10(labels = scales::percent, limits = c(-Inf, 1), breaks = c(0.001, 0.01, 0.1, 1)) +
  # labs(x = "Before announcement", y = "After announcement") +
  labs(
    x = "Before preemptive phase", 
    y = "During preemptive phase"
  ) +
  theme(legend.position = "none")
#
figS6 +
  ggsave(
    "figures/PNGs/figureS6.png",
    width = 8, height = 6
  )
#
figS6 +
  ggsave(
    "figures/figureS6.pdf",
    width = 8, height = 6,
    device = cairo_pdf
  )

rm(figS5, figS6)
dev.off()
