# 00) Script Information ------------------------

# Title: 3c_MapBiomas.R
# Purpose: 
## Bring in MapBiomas Collection 8 and plot measured land change against our scenarios

# Initial date: Aug 23, 2024
# Last edited: October 2025

# REQUIRES:
## MapBiomas Transition Collection 8 Data: "SOURCE_transonly_col8_mapbiomas_municip.csv"
## Scenario data frame 'lmh_cerrado_soyarea.Rdata' from '3a_scenario_dotplot.R'
## Simulated Cerrado land change raster: 'r_cerr'

# OUTPUTS:
## Collection 8 Cleaned Data: 'mapb_col8_clean_long.Rdata'

## Figures:
### Facet plot of Cerrado land transition maps per year interval: 'cerr_fromveg.png' 
### Line plot of land transition values per classification category: 'cerr_to_soybean.png'
### Line plot of the sum of all relevant vegetation categories, also including the SIMPLE-G results per scenario: 'cerr_to_soybean_RVC_scen.png' 
### 'cerr_fromveg' and 'cerr_to_soybean_RVC_scen' combined to one figure for the MS:  '_line_updated.png' 

## Tables:
### Amount of land transition per year interval from MapBiomas (Supplemental Table): _landtrans_CerrBrazil.xlsx 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  

# 0) Load Libraries & Set Constants ------------------------------------------------------------------------
rm(list = ls())

## Constants ------

### Loading & Saving ###

# date_string is for other model runs
date_string <- ""

# set folders here
folder_stat <- paste0("../Results/SIMPLEG-", date_string, "/stat_summary/")
folder_plot <- "../Figures/trans_mapbiomas/"
folder_source <- "../Data_Source/MapBiomas/"
folder_derived <- "../Data_Derived/"
folder_fig <- "../Figures/"
folder_results <- "../Results/SIMPLEG/"

## Libraries -------
library(dplyr)
library(ggplot2)
library(stringr)
library(stringi) # removing accents
library(geobr) # load BR shapefiles 
library(sf) # st_intersection() and crs() functions
library(RColorBrewer) # maps 
library(cowplot) # plot_grid() function
library(terra) # managing r_cerr SpatRaster
library(tidyr) # pivot_longer() function


# 1) Load in MapBiomas Transition ------
# Load collection 8 data in tabular form 
csv_br_trans_m <- read.csv(paste0(folder_source, "SOURCE_transonly_col8_mapbiomas_municip.csv"), encoding = "UTF-8")
names(csv_br_trans_m)


## 1.1) Tidy -----

df <- csv_br_trans_m

# remove all accents
df$state <- stri_trans_general(str = df$state,  id = "Latin-ASCII")
df$biome <- stri_trans_general(str = df$biome,  id = "Latin-ASCII")
names(df)

# select levels and years to reduce df size 
df <- dplyr::select(df, c("state","municipality", "geocode", "biome", 
                          "from_level_3", "to_level_3",
                          "from_level_4", "to_level_4",
                          #"X1985.1986", "X1986.1987", "X1987.1988", "X1988.1989", "X1989.1990", 
                          #"X1990.1991", "X1991.1992", "X1992.1993", "X1993.1994", "X1994.1995", "X1995.1996", "X1996.1997", "X1997.1998", "X1998.1999",    
                          "X1999.2000", "X2000.2001", "X2001.2002", "X2002.2003", "X2003.2004",    "X2004.2005",    "X2005.2006",   
                          "X2006.2007",    "X2007.2008",    "X2008.2009",    "X2009.2010",   "X2010.2011", "X2011.2012",    "X2012.2013",   
                          "X2013.2014",    "X2014.2015",    "X2015.2016",  "X2016.2017",    "X2017.2018",   
                          "X2018.2019",    "X2019.2020",    "X2020.2021"))

# remove all but the last four digits of all the columns 
names(df) <- str_sub(names(df), - 4, - 1)
names(df)

# rename columns 
# BEWARE HERE, this is manual for now, if you change the 'select' above then you need to change this as well 
colnames(df)[colnames(df) %in% c("tate", "lity", "code", "iome", "el_3", "el_3",  "el_4", "el_4")] <- c("state", "municipality", "geocode", "biome", 
                                                                                                        "from_level_3", "to_level_3",
                                                                                                        "from_level_4", "to_level_4")
names(df)

## 1.2) Make 'long' -----
# gather to make into a long dataset using pivot_longer (since gather() has been replace)
# NOTE: change the number if you changed 'select' above
ncol(df)

df <- pivot_longer(
  df,
  cols = 9:ncol(df),
  names_to = "year",
  values_to = "ha"
)


## 1.3) Save df -----
save(df, file = paste0(folder_derived, "mapb_col8_clean_long.Rdata"))
# NOTE: THIS INCLUDES ALL 



# 2) Plot Transition Results -----

# set relevant vegetation class categories
list_from_lv3 <- c("Forest Formation", "Savanna Formation", "Wetland",
                   "Grassland", "Pasture", "Forest Plantation",
                   "Mosaic of Agriculture and Pasture",
                   "Magrove", "Flooded Forest",
                   "Shrub Restinga", "Other Non Forest Natural Formation", "Wooded Restinga",
                   "Perennial Crops")

# filter Mapbiomas data to only focus on transitions to "Soybeans" & From-To's that do not stay the same
df <- df %>%
  filter(to_level_4 == "Soy Beans") %>%
  filter(to_level_4 != from_level_4)

## 2.1) Facet Map of Cerrado Transition ----

### 2.1.1) Prep Spatial Data ---------

# NOTE: Municipality & Cerrado Shapefiles come from 'geobr' package

# Load municipality shapefile
# Read all municipalities in the country at a given year
shp_muni <- read_municipality(code_muni="all", year=2018)

# Load Other Shapefiles 
load(paste0(folder_derived, "shp_usbr.RData"))

# get municipalities that are at all within the Cerrado
shp_muni_in_cerr <- st_intersection(shp_muni, shp_cerr)

# get just the codes column and keep as shapefile
shp_code_muni_in_cerr <- shp_muni_in_cerr %>%  dplyr::select(code_muni)

# get territory codes for municipalities in intersection as numeric
muni_codes_cerr <- shp_muni_in_cerr$code_muni

# filter to only municipalities in Cerrado
df_cerr <- df %>%
  filter(geocode %in% muni_codes_cerr) %>%
  filter(biome == "Cerrado")

### 2.1.2) Aggregate -------

# make a panel with Land Conversion Facet + Line Plot (A, B)

# get aggregate sum of the entire Cerrado for stats
agg_cerr <- df_cerr %>%
  aggregate(ha ~ year, sum) %>%
  mutate(year = as.numeric(year)) 

# get agg sum of certain 'from' classes for mapping
agg_cerrmuni_fromveg <- df_cerr %>%
  filter(from_level_3 %in% list_from_lv3) %>%
  aggregate(ha ~ year + geocode, sum) %>%
  mutate(year = as.numeric(year)) 

# make shape -- from veg
shp_cerrmuni_fromveg <- shp_code_muni_in_cerr %>%
  left_join(agg_cerrmuni_fromveg,
            join_by(code_muni == geocode)) %>%
  filter(year >= 2012 & year <= 2017) %>% 
  mutate(years = paste0(year-1,"-",year))


### 2.1.3) Plot Facet Map -------

# plot
p_trans_shp <- ggplot(shp_cerrmuni_fromveg)+
  geom_sf(mapping = aes(fill = ha/1000), color= NA)+
  scale_fill_distiller(palette = "YlOrRd", direction = 1)+
  facet_wrap("years")+
  coord_sf()+
  theme_minimal()+
  
  labs(
    title = "Land Conversion Across Cerrado",
    subtitle = "From Relevant Vegetation Classes to Soybean",
    fill = "Conversion (kha)")+
  
  theme(
    plot.title = element_text(hjust = 0.5, size = 32),
    plot.subtitle = element_text(hjust = 0.5, size = 20),
    legend.position = "top",
    strip.text.x = element_text(size = 14)#,
    #legend.key.size = unit(0.8, "cm")
  )

p_trans_shp

# save
ggsave(filename = paste0(folder_fig, "cerr_fromveg.png"),
       plot = p_trans_shp,
       width = 8, height = 8,
       dpi = 300)



## 2.2) Plot Line Plot of Cerrado Conversion ------

# set calculated r_cerr as a variable (note: unit is kha)
r_cerr <- readRDS(paste0(folder_derived, "r_m_Cerrado.rds"))
#r_cerr <- rast(r_cerr) # may need to run if below code doesn't work

sg_cerr_rawch_soy <- as.numeric(global(r_cerr$rawch_SOY, fun = "sum", size = Inf, na.rm = T))

# filter to only include the relevant classes
classes_few <- c(
  #"Temporary Crops", 
  "Forest Formation", "Mosaic of Agriculture and Pasture",
  "Pasture", "Savanna Formation", "Grassland")

df_cerr_agg <- df_cerr %>%
  aggregate(ha ~ year + biome + from_level_3 + to_level_4, sum) %>%
  mutate(year = as.numeric(year)) 

df_cerr_agg_from3 <- df_cerr_agg %>% 
  filter(from_level_3 %in% classes_few) %>% 
  mutate(years = paste0(year-1,"-",year))

# filter to only land that came from one of the RVCs-to-Soybean
agg_cerr_fromveg <- df_cerr %>%
  filter(from_level_3 %in% list_from_lv3) %>%
  aggregate(ha ~ year, sum) %>%
  mutate(
    biome = "Cerrado",
    from_level_3 = "Sum of RVCs",
    to_level_4 = "Soy Beans",
    year = as.numeric(year),
    years = paste0(year-1,"-",year)
  )

# add to "from3" df
df_cerr_RVC <- rbind(df_cerr_agg_from3, agg_cerr_fromveg)

# add year transitions
df_cerr_RVC <- df_cerr_RVC %>% mutate(years = paste0(year-1,"-",year))

# Plot line plot in Mha
existing_colors <- scales::hue_pal()(length(unique(df_cerr_RVC$from_level_3)))

ggplot(df_cerr_RVC, aes(x=years, group = from_level_3, y=ha/1000000, color = from_level_3)) +
  geom_line() +
  geom_point(fill = "white", size = 1.2) +
  xlab("") +
  labs(
    y = "Land Conversion from Previous Year (Mha)",
    color = "From-To Conversions"
  )+
  # add vertical line in 2012
  geom_vline(aes(xintercept = "2012-2013", color = "Post-Drought Year"),
             linetype="dotted", linewidth=0.5)+
  # add horizontal line where we calculated Cerrado transition 
  geom_hline(aes(yintercept = sg_cerr_rawch_soy/1000, color = "SIMPLE-G"),
             linetype="dotted", linewidth=0.5)+
  theme_bw()+
  # Manual color mapping for legend
  scale_color_manual(
    values = c(
      "SIMPLE-G" = "black", 
      "Post-Drought Year" = "red",
      # Others
      "Forest Formation" = "#F8766D",
      "Grassland" = "#B79F00",
      "Mosaic of Agriculture and Pasture" = "#00BA38",
      "Pasture" = "#00BFC4",
      "Savanna Formation" = "#619CFF",
      "Sum of RVCs" = "#F564E3"
    ))+
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    
    axis.title.y = element_text(size = 16),
    
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 11)
  )


# save
ggsave(paste0(folder_fig, "cerr_to_soybean.png"),
       width = 14, height = 7)


## 2.3) MapBiomas w/ SIMPLE-G Results -----
load(file = paste0(folder_results, "lmh_cerrado_soyarea.Rdata"))

# check if columns are in kha, if so, convert to Mha
cols_to_check <- c("chg", "pre", "post")
df_cerr_soyarea_lmh <- df_cerr_soyarea_lmh %>% 
  mutate(
    across(all_of(c("chg", "pre", "post")),
           ~ if_else(Unit == "kha", .x / 1000, .x),
           .names = "{.col}"),
    Unit = if_else(Unit == "kha", "Mha", Unit)
  )

scen_l <- df_cerr_soyarea_lmh %>% filter(modeltype == "l") %>% pull(chg)
scen_m <- df_cerr_soyarea_lmh %>% filter(modeltype == "m") %>% pull(chg)
scen_h <- df_cerr_soyarea_lmh %>% filter(modeltype == "h") %>% pull(chg)


# Create the plot for ONLY RVC's

# Set upper and lower bounds based on the scenarios

y_main <- scen_m
y_upper <- scen_l # just because it's typically a larger value
y_lower <- scen_h

x_int <- "2012-2013"
x_start <- "2011-2012"
x_end <- "2013-2014"

# Create the plot
p_trans_line <- ggplot(agg_cerr_fromveg %>% filter(year >= 2011 & year <= 2017), 
                       aes(x = years, y = ha / 1000000, group = from_level_3, color = from_level_3)) +
  
  # Main transition lines
  geom_line() +
  geom_point(fill = "white", size = 1.2) +
  
  # Axis and labels
  xlab("") +
  labs(
    y = "Land Conversion from Previous Year (Mha)",
    color = "From-To Conversions"
  ) +
  
  # Vertical line for post-drought year
  geom_vline(aes(xintercept = x_int, color = "Post-Drought Year"),
             linetype = "dotted", linewidth = 0.5) +
  
  # Horizontal segments for SIMPLE-G estimate and bounds
  geom_segment(aes(x = x_start, xend = x_end, 
                   y = y_main, yend = y_main, 
                   color = "SIMPLE-G Estimate"),
               linetype = "dashed", linewidth = 1) +
  geom_segment(aes(x = x_start, xend = x_end, 
                   y = y_upper, yend = y_upper, 
                   color = "Low Elas. Scenario"),
               linetype = "dotdash", linewidth = 0.8) +
  geom_segment(aes(x = x_start, xend = x_end, 
                   y = y_lower, yend = y_lower, 
                   color = "High Elas. Scenario"),
               linetype = "dotted", linewidth = 0.8) +
  
  # Set theme settings
  theme_bw() +
  theme(
    plot.title = element_text(size = 17, hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "bottom",
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 16),
    axis.text.y = element_text(size = 16)
  ) +
  
  # Manual color mapping for legend
  scale_color_manual(
    values = c("SIMPLE-G Estimate" = "blue", 
               "Low Elas. Scenario" = "darkblue", 
               "High Elas. Scenario" = "lightblue",
               "Sum of RVCs" = "black", 
               "Post-Drought Year" = "red"))


p_trans_line

# save
ggsave(paste0(folder_fig, "cerr_to_soybean_RVC_scen.png"),
       width = 14, height = 7)

### Save Transition Table -----
table_landtrans_cerr <- df_cerr_RVC %>% 
  filter(year >= 2013 & year <= 2015) %>% 
  filter(from_level_3 == "Sum of RVCs")

print("TABLE: Land Transition in Cerrado:")
print(table_landtrans_cerr)

## Repeat Cerrado Process but with Brazil ##
# get all muni codes in Brazil
muni_codes_br <- shp_muni$code_muni

# filter Mapbiomas 'df' to Brazil muni's
df_br <- df %>% 
  filter(geocode %in% muni_codes_br)

# get agg sum of certain 'from' classes
agg_br_fromveg <- df_br %>%
  filter(from_level_3 %in% list_from_lv3) %>% 
  aggregate(ha ~ year, sum) 

# get BRAZIl df to match CERRADO df so we can merge them together
stat_agg_br_fromveg <- agg_br_fromveg %>% 
  mutate(biome = "Brazil",
         from_level_3 = "Sum of RVCs",
         to_level_4 = "Soy Beans",
         year = as.numeric(year),
         years = paste0(year-1,"-", year))

table_landtrans_br <- stat_agg_br_fromveg %>% 
  filter(year >= 2013 & year <= 2015) %>% 
  filter(from_level_3 == "Sum of RVCs")

# merge tables 
table_landtrans <- table_landtrans_cerr %>% 
  rbind(table_landtrans_br) %>% 
  mutate(area_mha = ha/1000000) %>% 
  rename(region = biome)
print(table_landtrans)


## SAVE ##
openxlsx::write.xlsx(table_landtrans, file =paste0(folder_results, "_landtrans_CerrBrazil.xlsx"))


## 2.4) Plot together ----------
p_trans <- plot_grid(
  # Top plot with extra padding for alignment
  plot_grid(p_trans_shp, NULL, ncol = 1, rel_heights = c(1, 0.05), labels = "A"), 
  # Bottom plot
  plot_grid(NULL, p_trans_line, NULL, ncol = 3, rel_widths = c(0.2, 1, 0.2), labels = "B"),
  nrow = 2,
  rel_heights = c(1.5, 1), # Adjust heights: top plot taller
  align = "v",
  axis = "tb"
)

ggsave(paste0(folder_fig, "_line_updated.png"),
       p_trans, width = 12, height = 14, units = "in", dpi = 300)
