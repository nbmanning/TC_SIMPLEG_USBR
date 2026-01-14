# 00) Script Information --------------------------

# Title: 1_processResults_SIMPLEG.R

# Purpose: Run this script to:
## create folders in the directory
## get SIMPLE-G results from .txt into raster  
## import all of the necessary source shapefiles

# Initial SIMPLE-G script by: Iman Haqiqi
# Initial date: Aug 2019

# Edited by: Nick Manning 
# Initial edit date: May 2023
# Last edited: January 2026

# REQUIRES:
## SIMPLE-G Result files as '.txt' for scenarios low (l), medium (m) and high (h)

# OUTPUTS:
## 'raster' folder with spatial results as rasters (per scenario)
## SpatRasters with the CornSoy spatial results (per scenario)
## Shapefiles for World, US-MW, BR, & Cerrado saved as a .RData file (doesn't change per scenario)  

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NOTE: Users have to change SCENARIO ID section for each different scenario
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0) Load Libraries & Set Constants ---- 
rm(list = ls())

## Libraries ##
library(tidyverse) # need several packages from here: ggplot2, dplyr, stringr
library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(geobr) # use to load BR & Cerrado extent shapefiles
library(tigris) # use to load US and US-MW shapefiles
library(sf) # spatial shapefile manipulation 
library(stringr) # use to manipulate result .txt file
library(rworldmap) # getting simple BR Border

## Constants ##

## SCENARIO ID #########

# NOTE: change this when you change the result file to one of three TXT files

# # # # # # # # # # # # # # # # # # 
# UNCOMMENT FOR LOW SCENARIO
# # # # # # # # # # # # # # # # # # 
# pct <- "_l"
# pct_model <- "l"
# pct_title <- " - Low"

# # # # # # # # # # # # # # # # # # 
# UNCOMMENT FOR MEDIUM SCENARIO
# # # # # # # # # # # # # # # # # # 
# pct <- "_m" 
# pct_model <- "m" 
# pct_title <- "" 
# # pct_title <- " - Med"

# # # # # # # # # # # # # # # # # # 
# UNCOMMENT FOR HIGH SCENARIO
# # # # # # # # # # # # # # # # # # 
# pct <- "_h" 
# pct_model <- "h" 
# pct_title <- " - High"


# Define the string to search for in file names
search_string <- "" # used to be the model version

# create vars to house results 
folder_derived <- "../Data_Derived/"
folder_figures <- "../Figures/"
folder_results <- "../Results/SIMPLEG/"

# List all files and folders in the current directory
files_derived <- list.dirs(folder_derived)
files_figures <- list.dirs(folder_figures)

# Check if any file name contains the search string
if (!(any(grepl(search_string, files_derived)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_derived, search_string))
  
  cat("Derived Data Folder", search_string, "created.\n")
} else {
  cat("A Derived Data folder", search_string, " already exists.\n")
}

# check for figures folder
if (!(any(grepl(search_string, files_figures)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_figures, search_string))
  
  cat("Figures Folder", search_string, "created.\n")
} else {
  cat("A Figures folder", search_string, " already exists.\n")
}

# Set Results path to SIMPLEG folders - used to be necessary for different model results  
# folder_results <- paste0("../Results/SIMPLEG", search_string, "/")
# folder_figures <- paste0(folder_figures, search_string, "/")
# 
# folder_derived <- paste0(folder_derived, search_string, "/")
# folder_results <- paste0(folder_results#, 
#                          #"stat_summary/"
#                          )

# check for stats folder 
files_stat <- list.dirs(folder_results)

if (!(any(grepl("stat_summary", files_stat)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_results, "stat_summary"))
  
  cat("Figure Folders", "stat_summary", "created.\n")
} else {
  cat("A Figures folder with the string", "stat_summary", "in its name already exists.\n")
}

# 0) Load SIMPLE-G Scenario Results as TXT --------
# set model information 
datafile_version <- "sg1x3x10_v2411_US_Heat"

# Read in SIMPLE-G data file per scenario (either l, m, or h)
# NOTE: Will only work once you've uncommented a section of SCENARIO ID
datafile   <- paste0(folder_results, datafile_version, pct, "-out.txt")

# # # # # # # # # #

# 1) Prep SIMPLE-G Results --------------------

## 1.1) import and modify the output from the SIMPLE-G model ----------
getwd()

# read results and substitute the old row-notation of using "!" on each row
old.lines  <- readLines(datafile)
new.lines <- 
  old.lines[which(str_sub(old.lines, 1,1) != " " & 
                    str_sub(old.lines, 1,1) != "!" &
                    str_sub(old.lines, 1,1) != ""  )]

# write temporary file 
# NOTE: other users need to change to local location
newfile = paste0(folder_results, "temp.txt")
writeLines(new.lines, newfile, sep="\n")

# read in new data table -- takes a bit
dat <- read.table(newfile, sep=",", header=T)


###### **SIMPLE-G Results Key** ------------

names(dat)

# GRIDOUT.GRID.VAR == The Grid Cell ID

# LON == Longitude in XXX

# LAT == Latitude in XXX

# pct_QLAND == % change in cropland harvested area

# new_QLAND ==  post-simulation (Post-Sim) area of cropland in 1000 ha

# pct_QCROP ==  % change in the gridded crop production index

# new_QCROP ==  Post-Sim quantity index for crop production in 1000-ton (corn-equivalent)

# pct_LND_MAZ ==  % change in cropland harvested area devoted to maize

# pct_LND_SOY ==  % change in cropland harvested area devoted to soybeans

# new_LND_MAZ ==  post-simulation area of maize cropland (1000 ha) 

# new_LND_SOY ==  post-simulation area of soybeans cropland (1000 ha)


# 2) Create Raster Stack & Save Rasters ----------

## 2.1) Create raster stack ------
# create X and Y coordinates -- takes a bit
dat$x <- as.numeric(round(dat$LON, digits = 1))
dat$y <- as.numeric(round(dat$LAT, digits = 1))

coordinates(dat) = ~x+y

# set as gridded data
gridded(dat) = T 

# create basic raster stack for saving rasters
prct_ras = stack(dat) 

## 2.2) Save raster stack -----
# add rasters to file - users may need to uncomment and change 'ras_file' to local location but the function below should fix that
ras_file <- paste0(folder_results, "raster/")

# create results directory
files_results <- list.dirs(folder_results)

# Check if any results folder contains the word 'raster' yet
if (!(any(grepl("raster", files_results)))) {
  # If no file name contains the word 'raster', create a raster folder
  dir.create(paste0(ras_file))
  cat("Raster Folder", ras_file, "created.\n")
} else {
  cat("A folder with the string", "raster", "in its name already exists.\n")
}  

writeRaster(prct_ras$pct_QLAND, paste0(ras_file, "qLand_pct_", pct, ".tif"), format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$new_QLAND, paste0(ras_file, "qLand_new_", pct, ".tif"), format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$pct_QCROP, paste0(ras_file, "qCrop_pct_", pct, ".tif"), format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$new_QCROP, paste0(ras_file, "qCrop_new_", pct, ".tif"), format="GTiff", overwrite=TRUE)

writeRaster(prct_ras$pct_LND_MAZ, paste0(ras_file, "MAZ_pct_", pct, ".tif"), format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$new_LND_MAZ, paste0(ras_file, "MAZ_new_", pct, ".tif"), format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$pct_LND_SOY, paste0(ras_file, "SOY_pct_", pct, ".tif"), format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$new_LND_SOY, paste0(ras_file, "MAZ_new_", pct, ".tif"), format="GTiff", overwrite=TRUE)

# add Longitude and Latitude rasters (only need to do this one time, they aren't different for diff percents)
writeRaster(prct_ras$LON, paste0(ras_file, "USBR_SIMPLEG_LON.tif"), format="GTiff", overwrite=TRUE)
writeRaster(prct_ras$LAT, paste0(ras_file, "USBR_SIMPLEG_LAT.tif"), format="GTiff", overwrite=TRUE)


## 2.3) Save 'terra' object ------ 
# create SpatRaster using terra
r <- terra::rast(dat)

# Get results for new/pct for Maize and Soy
r_maizesoy <- subset(r, c("pct_QLAND", "new_QLAND", "pct_QCROP", "new_QCROP",
                          "pct_LND_MAZ", "pct_LND_SOY", "new_LND_MAZ", "new_LND_SOY"))

# get 
r <- subset(r, c("pct_QLAND", "new_QLAND", "pct_QCROP", "new_QCROP"))

# save
saveRDS(r, file = paste0(folder_derived, "r", pct, ".rds"))
saveRDS(r_maizesoy, file = paste0(folder_derived, "r_maizesoy", pct, ".rds"))



# 3) Get Shapefiles: US-MW, BR, & Cerrado ------------

# NOTE: Only need to run once per computer - these don't change for different model results

### Load World Shapefile ###
shp_world <- st_read(system.file("shapes/world.gpkg", package="spData"))

### Load US Shapefile ###
shp_us <- states(cb = TRUE, resolution = "20m") %>%
  filter(!STUSPS %in% c("AK", "HI", "PR"))

#### Load US-MW Shapefile ###
shp_us_mw <- shp_us %>%
  filter(STUSPS %in% c("IA", "IL", "IN", "KS", "MI", "MN",
                       "MO", "ND", "NE", "OH", "SD", "WI"))

#### Load Cerrado Shapefile ###
shp_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T) %>%
  dplyr::filter(name_biome == "Cerrado")


#### Load BR Shapefile ###
shp_br <- read_country(
  year = 2019,
  simplified = T,
  showProgress = T)

#### Load Simple BR Border Shapefile ###
data("countriesCoarse")
shp_br_border <- countriesCoarse %>% subset(SOV_A3 == "BRA")
shp_br_border <- st_combine(st_as_sf(shp_br_border))

## Load Cerrado Outline ##
# get Brazil States outline
shp_br_states <- read_state(
  year = 2019,
  simplified = T)

# filter to Cerrado States
shp_cerr_states <- shp_br_states %>%
  dplyr::filter(abbrev_state %in% c("TO","MA","PI","BA","MG",
                                    "SP","MS","MT","GO","DF"))


# 4) Save Shapefiles ------
save(shp_br, shp_br_border, shp_br_states,
     shp_cerr, shp_cerr_states,
     shp_us, shp_us_mw,
     shp_world,
     file = paste0(folder_derived, "shp_usbr.RData"))
