# 00) Script Information ------------------------

# Title: 0_dataprep_BR_SIMPLEG.R
# Author: Nick Manning
# Purpose: 
## Import and clean data needed to modify SIMPLE-G (2010 BR agriculture stats and MAPSPAM rasters).

# Creation Date: 7/27/23
# Last Updated:  January 2026

# Relevant Links: 
# SPAM2010: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRFF8V

# Requires: 
## SPAM 2010 data; accessible from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRFF8V
### Soy Physical Area raster
### Soy Production raster
### Maize Physical Area raster
### Maize Production raster

# Outputs: 
# 'spam2010_soyb_maize_parea_prod_yield_br.tif' which are the Planted Area, Production, and Yield rasters for Brazil soybean and maize from MAPSPAM 
# 'spam2010_soyb_maize_parea_prod_yield_cerr.tif' which are the Planted Area, Production, and Yield rasters for the Cerrado soybean and maize from MAPSPAM

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list=ls())
getwd()

# 0) Load Libraries & Set Constants -----------
library(geobr) # loading Cerrado & BR shapefiles
library(terra) # raster loading/manipulation and plotting
library(sidrar) # getting data from SIDRA
library(RColorBrewer) # adding color palettes to maps 
library(dplyr) # pipes

# data import & export
# NOTE: We pull the SPAM data from a folder within 'Data_Source' called 'SPAM2010'
path_import <- "../Data_Source/"

# 1) 2010 BR stats from SIDRA data ------

## 1.0) Notes on 'sidrar' package -------------

# Search terms:
## "Tabela 1612: Área plantada, área colhida, quantidade produzida, rendimento médio e valor da produção das lavouras temporárias"


# Units: 
## Quantidade produzida = quantity produced (Toneladas)
## Rendimento médio da produção = average production yield (kg/ha)


# Item Codes:
# 109:Área plantada (Hectares) 

# 1000109: Área plantada - percentual do total geral (%) 

# 216:Área colhida (Hectares)

# 1000216: Área colhida - percentual do total geral (%)

# 214: Quantidade produzida (Toneladas)
## Quantity Produced (Tons)

# 112: Rendimento médio da produção (Quilogramas por Hectare)
## Average production yield (kg per ha)

# 215: Valor da produção (Mil Cruzeiros, Mil Cruzados , Mil Cruzados Novos, Mil Cruzeiros Reais [], Mil Reais)

# 1000215: Valor da produção - percentual do total geral (%)

## 1.1) National BR Raw Production (Prod & Planted) ------- 
raw_sidra_br <- get_sidra(x = 1612, 
                          variable =  c(214, 216), # production and yield # or for first six (excluding value of production) c(109, 1000109, 216, 1000216,214, 112) 
                          period = "2010",# list_year_range, #2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
                          geo = "Brazil", # Brazil, State, or Município
                          geo.filter = NULL,
                          classific = "c81",
                          category = list(c(0, 2713, 2711)), # 2713 = Soja (em grão); 2711 = Milho (corn) (em grão)
                          header = T,
                          format = 3)

# 2010 harvested area of corn (ha)
## 12678875

# 2010 harvested area of soy (ha)
## 23327296

# 2010 production of corn (tons) 
## 55364271

# 2010 production of soy (tons)
## 68756343



# 2) Prepare MAPSPAM 2010 data for Brazil ---------------------

# get units and descriptions for SPAM data here: https://mapspam.info/methodology/

# From the readme: global_X_CROP_Y.tif 
## X = variable (production (P), physical area (A), harvested area (H), yield (Y))
## Y = Technologies (rainfed, irrigated, etc.); A = All technologies (for simplicity)

# bring in physical area (ha)
spam_soy_parea <- rast(paste0(path_import, "SPAM2010/spam2010V2r0_global_A_SOYB_A.tif"))
spam_maize_parea <- rast(paste0(path_import, "SPAM2010/spam2010V2r0_global_A_MAIZ_A.tif"))

# bring in harvested area (ha)
spam_soy_harvarea <- rast(paste0(path_import, "SPAM2010/spam2010V2r0_global_H_SOYB_A.tif"))
spam_maize_harvarea <- rast(paste0(path_import, "SPAM2010/spam2010V2r0_global_H_MAIZ_A.tif"))

# bring in production (mt)
spam_soy_prod <- rast(paste0(path_import, "SPAM2010/spam2010V2r0_global_P_SOYB_A.tif"))
spam_maize_prod <- rast(paste0(path_import, "SPAM2010/spam2010V2r0_global_P_MAIZ_A.tif"))

# join all together to one variable
spam <- c(spam_soy_parea, spam_maize_parea, spam_soy_prod, spam_maize_prod)

spam_harv_prod <- c(spam_soy_harvarea, spam_maize_harvarea, spam_soy_prod, spam_maize_prod)


## 2.1) Clip to Brazil to test ----

# Load BR Shapefile
shp_br <- read_country(year = 2010, 
                       simplified = T, 
                       showProgress = T)

# get extent as terra object for plotting
ext_br <- vect(ext(shp_br))

# get basic BR results by cropping and masking to just BR extent
r_br <- terra::crop(spam, ext_br, mask = T) 
r_br <- mask(r_br, shp_br)

r_br_source <- r_br 

## 2.2) Get results --------------
## Note: area = (total area)/(ha)

### 2.2.1A) Area (% of grid cell) ------ 
# get area of each grid cell (will be different bc of projection)
r_br_area <- cellSize(r_br$spam2010V2r0_global_A_SOYB_A, unit = "ha")

# get harvested percentages by dividing by area of each grid cell 
spam_soy_parea_perc <- (r_br$spam2010V2r0_global_A_SOYB_A/r_br_area) #* 100
spam_maize_parea_perc <- (r_br$spam2010V2r0_global_A_MAIZ_A/r_br_area) #* 100


### 2.2.1B) Area (% of total) -----
# get the sum of the total crop (either soybean or maize) produced across BR; should be one value 
sum_parea_soy <- as.numeric(
  global(classify(r_br$spam2010V2r0_global_A_SOYB_A, cbind(NA, 0)), fun = "sum"))

sum_parea_maize <- as.numeric(
  global(classify(r_br$spam2010V2r0_global_A_MAIZ_A, cbind(NA, 0)), fun = "sum"))

# divide rasters by total sum for each crop to get the percent of the total per each grid cell
spam_soy_parea_perctotal <- r_br$spam2010V2r0_global_A_SOYB_A / sum_parea_soy
spam_maize_parea_perctotal <- r_br$spam2010V2r0_global_A_MAIZ_A / sum_parea_maize


### 2.2.2) Production (% of total) ------
# get production percentage (% of total per grid cell)
## production per grid cell = (mt)/(total BR prod) 

# first, get total sum of Brazil soy or maize
# NOTE: I set all NA's to 0 when calculating the sums of metric tons of crops produced

# get sums 
sum_prod_soy <- as.numeric(
  global(classify(r_br$spam2010V2r0_global_P_SOYB_A, cbind(NA, 0)), fun = "sum"))

sum_prod_maize <- as.numeric(
  global(classify(r_br$spam2010V2r0_global_P_MAIZ_A, cbind(NA, 0)), fun = "sum"))

# divide rasters by total sum for each crop
spam_soy_prod_perc <- r_br$spam2010V2r0_global_P_SOYB_A / sum_prod_soy
spam_maize_prod_perc <- r_br$spam2010V2r0_global_P_MAIZ_A / sum_prod_maize


### 2.2.3) Yield per grid cell (production / harvested area) (mt / ha) ------
spam_soy_yield <- r_br$spam2010V2r0_global_P_SOYB_A / r_br$spam2010V2r0_global_A_SOYB_A
spam_maize_yield <- r_br$spam2010V2r0_global_P_MAIZ_A / r_br$spam2010V2r0_global_A_MAIZ_A


## 2.3) Combine to Get Percentage Raster Stack ---------
# Combine percentage rasters 
spam_perc <- c(spam_soy_parea_perc, spam_maize_parea_perc, spam_soy_prod_perc, spam_maize_prod_perc)

spam_perc_total <- c(spam_soy_parea_perctotal, spam_maize_parea_perctotal, spam_soy_prod_perc, spam_maize_prod_perc)

spam_perc_all <- c(spam_soy_parea_perc, spam_maize_parea_perc, spam_soy_parea_perctotal, spam_maize_parea_perctotal, spam_soy_prod_perc, spam_maize_prod_perc, spam_soy_yield, spam_maize_yield)


## 2.4) Plot Brazil -------

# rename layers from _all
names(spam_perc_all)
names(spam_perc_all) <- c("soy_parea_perc_gridcell","maize_parea_perc_gridcell",
                          "soy_parea_perc_total","maize_parea_perc_total",
                          "soy_prod_perc_total","maize_prod_perc_total",
                          "soy_yield_gridcell", "maize_yield_gridcell")

# plot with 2 columns and 3 rows 
plot(spam_perc_all, nc = 2, nr = 4, col = brewer.pal(7, "PiYG"))

plot(spam_perc_all[["soy_yield_gridcell"]], col = brewer.pal(7, "Greens"),
     main = "2010 Brazil Soy Yield per Grid Cell")
lines(shp_br, lwd = 0.8, lty = 3, col = "darkgray")


plot(spam_perc_all[["maize_yield_gridcell"]], col = brewer.pal(7, "Greens"),
     main = "2010 Brazil Maize Yield per Grid Cell")
lines(shp_br, lwd = 0.8, lty = 3, col = "darkgray")



## 2.5) Clip to Cerrado to Plot -------

# Load Cerrado Shapefile
shp_br_cerr <- read_biomes(
  year = 2019,
  simplified = T,
  showProgress = T
) %>% dplyr::filter(name_biome == "Cerrado")

# get Cerrado extent as terra object 
ext_cerr <- vect(ext(shp_br_cerr))

# crop, mask, and plot 
r_cerr <- terra::crop(spam_perc_all, ext_cerr, mask = T)
r_cerr <- mask(r_cerr, shp_br_cerr)

# plot specific raster with the outline of the Cerrado
plot(r_cerr[["soy_yield_gridcell"]], col = brewer.pal(9, "Greens"),
     main = "2010 Cerrado Soy Yield per Grid Cell")
lines(shp_br_cerr, lwd = 0.8, lty = 3, col = "darkgray")


plot(r_cerr[["maize_yield_gridcell"]], col = brewer.pal(9, "Greens"),
     main = "2010 Cerrado Maize Yield per Grid Cell")
lines(shp_br_cerr, lwd = 0.8, lty = 3, col = "darkgray")



## 2.6) Export SPAM BR & Cerrado Data --------
writeRaster(spam_perc_all, paste0(path_import, "spam2010_soyb_maize_parea_prod_yield_br.tif"),
            overwrite = T)
writeRaster(r_cerr, paste0(path_import, "spam2010_soyb_maize_parea_prod_yield_cerr.tif"),
            overwrite = T)