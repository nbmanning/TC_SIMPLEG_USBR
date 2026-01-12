# 00) Script Information -----------------------
# Title: 0_US_Drought_shock_v4.R

# Purpose: This script calculates the US 2012 crop shock for use in SIMPLE-G-Global

# Initial script by: Iman Haqiqi
# Initial date: ~October 2024

# Edited by: Nick Manning 
# Last edited: July 2025

# REQUIRES:
## US_Drought_Shock folder with
### Coords.csv
### colsom12.txt
### AGLAND_CROPLAND_ACRES.csv
### AGLAND_CROP_INSURANCE_ACRES_FIPS.csv
### CROP_TOTALS_SALES_USD_FIPS.csv
### cb_2018_us_state_500k.shp

## 'grid_id_xyg.tif' which is a file typically used in SIMPLE-G with the grid cell ID numbers for eachgrid cell around the world


# OUTPUTS:
## 'shock_v4_df.RData' which contains:
### df.xyz = the XY loss rates that get converted to raster
### loss.aggr = the variable we're going to change by filtering only to drought and heat   

## 'shock_v4_r99.RData' which contains raster 'r' which is the Loss Rate 
## 'y12d.txt' & 'y12d.har' which are the files that are input into SIMPLE-G
## 'Figures/SIMPLEG_Shock_v4.png' which is the map of the percent loss of corn+soy from drought and heat


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0) Load libraries --------
rm(list = ls())
getwd()

# load required libraries for raster analysis
library(terra) # load Coords.csv
library(dplyr) # df loading and editing

# load for plotting
library(tidyterra)
library(RColorBrewer)
library(ggplot2)

# 1) Create a data frame with FIPS and XY coordinates and SIMPLE-G grid IDs ----
# US coordinates and FIPS
us.xy.file = "../US_Drought_Shock_v4/Coords.csv" #NOTE: re-name folder to "../US_Drought_Shock_v4" after moving this code to the "Code" folder 
tmp = read.csv(us.xy.file, header=T, sep=",")

# re-generate coordinates to ensure decimal accuracy
tmp$x = round(tmp$x * 120)/120
tmp$y = round(tmp$y * 120)/120

#create a raster and plot
tmp.ras = rast(tmp, type="xyz")
plot(tmp.ras)

us.xy = tmp[c("x","y","FIPS", "VCRP_2010", "QLND_2010")]

# 2) Create a data frame of loss by FIPS for 2012 ----

# read the USDA RMA Causes of Loss
loss.file = "../US_Drought_Shock_v4/colsom_2012/colsom12.txt" #NOTE: needed to re-name folder here
loss.dt = read.csv(loss.file, header =F, sep="|")

#read the column labels
loss.id = "../US_Drought_Shock_v4/Codes.csv"
id = read.csv(loss.id, header =F, sep=",")

names(loss.dt) = id$V1
names(loss.dt) 

# generate FIPS codes
loss.dt$FIPS = loss.dt$State_Code * 1000 + loss.dt$County_Code

unique(loss.dt$Cause_Description)

# calculate heat separately to compare

# first run this to get rid of empty spaces in comm. ID
# regex means all spaces (\\s) after the last letter (+$)
loss.dt <- loss.dt %>%
  mutate(Commodity_ID = gsub("\\s+$", "", Commodity_ID))

# filter to calculate losses only from drought+heat, corn/soy, and months of the drought (may-oct)
loss.dt <- loss.dt %>% 
  filter(Cause_Description %in% c("Drought", "Heat")) %>%
  filter(Commodity_ID %in% c("CORN", "SOYBEANS")) %>% 
  filter(Month_Name %in% c("MAY", "JUN", "JUL", "AUG", "SEP", "OCT"))

# select only indemnity and FIPS amounts
loss.df = loss.dt[c("FIPS", "Indemnity_Amount")]
loss.aggr = aggregate( . ~ FIPS, data =loss.df, FUN= sum, na.rm=T ) ## Q: Doesn't this technically include ALL forms of indemnity loss?
summary(loss.aggr)


# 3) Create a data frame of insured acres ----

## 3.1) read the insured acres from USDA-NASS Census of AG ----
my.file = "../US_Drought_Shock_v4/AGLAND_CROP_INSURANCE_ACRES_FIPS.csv" 
my.dt = read.csv(my.file, header =T, sep=",")
my.df <- my.dt %>% mutate_all(as.numeric)
summary(my.df)

insured.acres = my.df[c("FIPS", "X2012")]
names(insured.acres) = c("FIPS", "Insured.Acres")
head(insured.acres)


## 3.2) read the cropland acres from USDA-NASS Census of AG ----
my.file = "../US_Drought_Shock_v4/AGLAND_CROPLAND_ACRES.csv"
my.dt = read.csv(my.file, header =T, sep=",")
my.df <- my.dt %>% mutate_all(as.numeric)
summary(my.df)

crop.acres = my.df[c("FIPS", "X2012")]
names(crop.acres) = c("FIPS", "Crop.Acres")
head(crop.acres)

## 3.3) read the crop sales from USDA-NASS Census of AG ----
my.file = "../US_Drought_Shock_v4/CROP_TOTALS_SALES_USD_FIPS.csv"
my.dt = read.csv(my.file, header =T, sep=",")
my.df <- my.dt %>% mutate_all(as.numeric)
summary(my.df)

sales.df = my.df
head(sales.df)

## 3.4) how much would the 2012 sale would be without drought? ----
## average of 2007-2017 or observed sales in 2012, which one is bigger
sales.df$Expected_Sale = pmax(sales.df$X2012, 
                              (sales.df$X2007 + sales.df$X2017)/2,
                              pmin(sales.df$X2007 , sales.df$X2017, na.rm=T), 
                              na.rm=T)

crop.sales = sales.df[c("FIPS", "Expected_Sale")]


# 4) Merge data sets and calculate the rates (averages) ---- 
fips.df = merge(loss.aggr, insured.acres, by = "FIPS", all.x=T) 
fips.df = merge(fips.df,      crop.acres, by = "FIPS", all.x=T)
fips.df = merge(fips.df,      crop.sales, by = "FIPS", all.x=T)

head(fips.df) # Q: 

fips.df$AvgIndemAcre = fips.df$Indemnity_Amount / fips.df$Insured.Acres
fips.df$AvgSalesAcre = fips.df$Expected_Sale   / fips.df$Crop.Acres
fips.df$LossRate = fips.df$AvgIndemAcre   / fips.df$AvgSalesAcre

summary(fips.df)

# Avg. Loss Rate before cutting off at 0.99 
# print(paste("Average Loss Rate (c+s):", mean(fips.df$LossRate, na.rm = T)))

## 4.1) create a data-frame for all XY assuming uniform rates in each county FIPS ----
df.xyz = merge(us.xy, fips.df[c("FIPS", "LossRate", "AvgIndemAcre", "AvgSalesAcre")], by = "FIPS", all.x=T)
dim(df.xyz)
summary(df.xyz)

df.xyz[is.na(df.xyz)] = 0

## convert data-frame to raster
s = rast(df.xyz[-1], type="xyz")
t=rast(res=1/12)
crs(s) = crs(t)
plot(s)

## truncate as the loss rate should be always less than 1
r = s$LossRate
r[r > 0.99] = 0.99

# Avg. Loss Rate after cutting off at 0.99 
print(paste("Average Loss Rate (c+s, post-truncate):", global(r$LossRate, fun = "mean", na.rm = T)))

# 
fips2 <- fips.df$LossRate
fips2[fips2 > 0.99] = 0.99
fips2[is.na(fips2)] = 0

ms_shock_mean <- mean(fips2, na.rm = T) * 100 # 0.1703 before setting NA to 0; 0.1692 after setting all NA to 0
ms_shock_med <- median(fips2, na.rm = T) * 100

paste0("We found an average shock of ", format(round(ms_shock_mean, 2), nsmall = 2),  
       "% (median: ", format(round(ms_shock_med, 2), nsmall = 2), "%).")

# save df's for: 
## df.xyz = the XY loss rates that get converted to raster; don't need fips.df bc fips.df feeds into this
## loss.aggr = the variable we're going to change by filtering only to drought and heat   
save(df.xyz, loss.aggr,
     file = "../Data_Derived/shock_v4_df.RData")

# save raster
## r = Loss Rate 
save(r, file = "../Data_Derived/shock_v4_r99.RData")


# 5) Plot the shock ----
## Reproject 
## change the projection
u = terra::project(r, "+init=epsg:2163")
#u = terra::project(r, "+init=epsg:4326")


v = vect("../US_Drought_Shock_v4/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") #NOTE: Need to copy/paste the US shp folder to v2

v = crop(v,s)

w = terra::project(v, "+init=epsg:2163")
#w = terra::project(v, "+init=epsg:4326")

# Define breaks and colors using RColorBrewer
breaks <- c(0, 5, 10, 50, 99)
colors <- brewer.pal(n = length(breaks) - 1, name = "YlOrRd")

# Cut the 'value' column into categories based on the breaks
# Necessary as it is continuous at the moment
u_cut <- u %>%
  mutate(LossRate = LossRate * 100) %>% 
  mutate(category = cut(LossRate, breaks = breaks, include.lowest = TRUE))

# plot
ggplot()+
  geom_spatraster(data = u_cut %>% subset("category"), aes(fill = category))+
  scale_fill_manual(values = colors, name = "Percent Loss", na.value = "white") +
  geom_spatvector(data = w, fill = "transparent", color = "gray40", lwd = 0.2)+
  #geom_spatvector_text(data = w, aes(label = STUSPS))+
  theme_void() +
  theme(axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16)) +
  guides(fill = guide_legend(direction = "horizontal"))

# save
ggsave("../Figures/SIMPLEG_Shock_v4.png",
       height = 10, width = 15, dpi = 300)

# 6) Shock for SIMPLE-G global  ----
xygID  <- rast("../US_Drought_Shock_v4/grid_id_xyg.tif")
plot(xygID)

nGrid = 1316744

x = resample(-r*100, xygID)
x[is.na(x)] = 0
plot(x)

y = c(xygID, x*xygID/xygID)
plot(y)
y

my.df = as.data.frame(y, xy=T)
my.df = my.df[!is.na(my.df$grid_id_xyg),]
my.df = my.df[ order(my.df$grid_id_xyg),]
head(my.df)

my.df$LossRate[my.df$LossRate < -80] = -80

all.equal(my.df$grid_id_xyg, c(1:nGrid))

v = "y12d"
print(v)
txt <- paste0(nGrid, ' real row_order header "',v,'" longname "US 2012 Drought Shock, revision d" ;')

# txt.file = paste0("../US_Drought_Shock_v4/", v,".txt") # NOTE: this works to get the .txt file to the right folder but does not convert to HAR
txt.file = paste0(v,".txt")

write(txt, txt.file) # NOTE: Need to manually move y12d from the working directory (for me, "Code") to US_Drought_Shock_v4

my.var = "LossRate"

write.table(my.df[ , my.var], txt.file, 
            row.names=F, col.names=F, quote=F, sep=",", dec = ".", append=T)

system(paste0("txt2har.exe ", v,".txt ", v,".har"))