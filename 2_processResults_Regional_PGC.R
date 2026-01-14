# 00) Script Information --------------------------

# Title: 2_processResults_Regional_PGC.R

# Purpose: 
## Read and plot SIMPLE-G results across regions of interest per scenario
### Section 1: Get Import/Export Figures from the regional tabular results
### Section 2 & 3: Load & Clean Spatial Data
### Section 4-7: Get & Plot Spatial Results per Specific Region 
### Section 8: Get Per-Grid_cell (PGC) results 
### Section 9: Merge Regional & Spatial Results for Final Table 

# Author: Nick Manning

# Initial date: Aug 23, 2024
# Last edited: January 2026


# REQUIRES:
## RUN '1_processResults_SIMPLEG.R' to get the r_maizesoy.Rdata and shp_usbr.RData files for Section 2
## 'regional_results.xlsx' from SIMPLE-G output files for Section 1

# FOLDER STRUCTURE:
## 'raster' which houses the results as rasters to pull into a GIS
## 'summary_tables' which houses a table with the stats for each area (min, mean, median, 1st & 3rd Quartiles, max, and NA's)
## 'stat_summary' which houses the raw values for changes in cropland area and production as an R data file to bring into another script   

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# 0) Load Libraries & Set Constants ------------------------------------------------------------------------
rm(list = ls())

## Libraries -----
# imp-exp plots
library(rio)
library(dplyr)
library(stringr)
library(ggplot2)
library(stringr)
library(tidyr)
library(openxlsx)
library(patchwork)

# SIMPLE-G maps 
library(raster) # use for initial raster stack and basic plotting
library(terra) # use to wrangle geospatial data and plot
library(RColorBrewer) # use for adding colorblind-friendly color palettes 
library(rasterVis) # use for easy violin plot 
library(reshape2) # use for melting data to then use ggplot
library(sf)
library(tidyterra) # plot using ggplot() instead of base R with 'terra'
library(ggspatial) # N arrow and Scale Bar w tidyterrra

# transition maps
library(geobr)

## Constants ------

### Loading & Saving ###

# Set model version & parameter flexibility
datafile_version <- "sg1x3x10_v2411_US_Heat"

### SCENARIO ID ####################

# NOTE: change this when you change the result file to one of three TXT files! 

# # # # # # # # # # # # # # # # # # # 
# # UNCOMMENT FOR LOW SCENARIO
# # # # # # # # # # # # # # # # # # # 
# pct <- "_l" 
# pct_model <- "l" 
# pct_title <- " - Low" 
# 
# # # # # # # # # # # # # # # # # # # 
# # UNCOMMENT FOR MEDIUM SCENARIO
# # # # # # # # # # # # # # # # # # # 
# pct <- "_m" 
# pct_model <- "m" 
# pct_title <- "" 
# # pct_title <- " - Med"
# 
# # # # # # # # # # # # # # # # # # # 
# # UNCOMMENT FOR HIGH SCENARIO
# # # # # # # # # # # # # # # # # # # 
# pct <- "_h" 
# pct_model <- "h" 
# pct_title <- " - High"

# Define the model date 
# NOTE: Assumes the results are downloaded and saved in YYYY-MM-DD format
date_string <- paste0("")
date_string_nodash <- gsub("-", "", date_string)

# create vars to house results
folder_der <- "../Data_Derived/"
folder_der_date <- paste0(folder_der, date_string)

folder_fig <- "../Figures/"
folder_fig <- paste0(folder_fig, date_string, "", pct_model, "/")

folder_results <- paste0("../Results/SIMPLEG", date_string, "/", pct_model, "/")
folder_results_impexp <- paste0("../Results/SIMPLEG", date_string,"/", pct_model, "/imports_exports/")
folder_results_all <- "../Results/SIMPLEG/"

folder_stat <- paste0(folder_results, "stat_summary/")

### Plotting ###
size_title = 2.0
size_labels = 1.5
size_axis_nums = 1.0


## Create Folders -----

files_results <- list.dirs(folder_results)
files_results_impexp <- list.dirs(folder_results_impexp)

files_fig <- list.dirs(folder_fig)

# Check if a folder exists for this set of results (e.g. if date_string == '2024-03-03') 

if (!(any(grepl(date_string, files_results)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_results))
  
  cat("Results Folder", date_string, "created.\n")
} else {
  cat("A Results folder", date_string, " already exists.\n")
}


# do the same but specifically for imp/exp folder
if (!(any(grepl(date_string, files_results_impexp)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_results_impexp))
  
  cat("Imp/Exp Results Folder", date_string, "created.\n")
} else {
  cat("An Imp/Exp Results folder", date_string, " already exists.\n")
}


# check for figures folder
if (!(any(grepl(date_string, files_fig)))) {
  # If no file name contains the search string, create a folder with that string
  dir.create(paste0(folder_fig))
  
  cat("Figures Folder", date_string, "created.\n")
} else {
  cat("A Figures folder", date_string, " already exists.\n")
}

# check for stat summary folder -- commented out because stat_summary goes to '/Results/' folder now
# if (!(any(grepl(date_string, files_stat)))) {
#   # If no file name contains the search string, create a folder with that string
#   dir.create(paste0(folder_fig))
#   
#   cat("Figure Folder", date_string, "created.\n")
# } else {
#   cat("A figures folder with the string", date_string, "in its name already exists.\n")
# }

# 0) Functions ------------------------------------------------------------------------

# list functions used multiple times here

## 0.1) ImpExp Functions --------

# Import and clean each Regional Result sheet from 'regional_results.xlsx'
## @var is the name of the Excel sheet in the Workbook
## @pct is the model elasticity scenario (l, m, h)
F_clean_sheet <- function(var, pct){
  
  # get one sheet 
  data <- data_list[[var]]
  
  # Extract just the first letter of each column name
  setting <- substr(colnames(data), 1, 1)
  
  # Get the combined model setting (l,m, or h) and variable; e.g. Pct-m
  data <- rbind(setting, data)
  
  # Combine the first two rows with a hyphen in between
  combined_row <- paste(data[2, ], data[1, ], sep = "-")
  
  # Add the combined row as the first row
  data <- rbind(combined_row, data[-c(1, 2), ])
  
  #get model info then remove 
  model_info <- data[2, ]
  data <- data[-2, ]
  
  # Update column names to model variable 
  colnames(data) <- as.character(unlist(data[1, ]))
  
  colnames(data)[1] <- "region_abv"
  
  # Remove the duplicated row
  data <- data[-1, ]
  
  
  # Assuming your data frame is named data
  # Filter columns based on certain character in row 2
  char_to_keep <- pct  # NOTE: Change this to the model setting you want to keep
  
  # Save pre-existing column names
  pre_existing_colnames <- colnames(data)
  
  # Filter columns based on predefined character in column names
  columns_to_keep <- c(TRUE, sapply(pre_existing_colnames[-1], function(col_name) substr(col_name, nchar(col_name), nchar(col_name)) == char_to_keep))
  
  # Subset the data frame to keep the desired columns
  data <- data[, columns_to_keep]
  
  # Remove spaces and numbers before regions
  ## regex: remove anything up to and including the first space
  data$region_abv <- gsub(".*\\ ", "", data$region_abv)
  data$variable <- var
  
  # split column to get crop and type 
  data <- data %>% 
    mutate(crop = str_extract(variable, "^[^ ]+"),
           type = str_extract(variable, "[^ ]+$"))
  
  # add column based on var 
  data <- data %>% 
    mutate(modeltype = case_when(
      pct == "l" ~ "low",
      pct == "m" ~ "med",
      pct == "h" ~ "high"
    ))
  
  # rename columns - can ADD TO FUNCTION USING SOMETHING LIKE paste0(-,pct)
  data <- data %>% 
    rename(
      "pct_chg" = paste0("Pct-", pct),
      "pre" = paste0("Pre-", pct),
      "post" = paste0("Post-", pct),
      "chg" = paste0("CH-", pct)
    )
  
  # convert certain columns to numeric
  data <- data %>%
    #mutate_at(vars(columns_to_convert), as.numeric)
    mutate_at(vars(c("pct_chg", "pre", "post", "chg")), as.numeric) %>% 
    mutate(chg_mmt = chg/1000)
  
  return(data)
}


# # Plotting Fxn
# ## @df is the 
# ## @y_var  
# F_ggplot_bar_vert_sep <- function(df, y_var, title_text, save_text){
#   
#   # need to do to use character evaluation
#   y_var <- rlang::sym(y_var)
#   
#   # plot
#   (p <- ggplot(df, aes(x = region_abv, y = !! y_var ))+
#       # Set color code on a True-False basis
#       geom_bar(aes(fill = !! y_var < 0), stat = "identity") + 
#       # if false, one color, if true, another
#       scale_fill_manual(guide = "none", breaks = c(TRUE, FALSE), values=c(col_neg, col_pos))+
#       coord_flip()+
#       theme_bw()+
#       labs(
#         title = title_text,
#         x = "",
#         y = ""
#       )+
#       theme(
#         plot.title = element_text(hjust = 0.5),
#         # remove y-axis text (use when merging exp and imp graphs)
#         #axis.text.y = element_blank()
#       )
#   )
#   # save
#   ggsave(paste0(folder_fig, save_text),
#          width = 6, height = 8)
#   
#   return(p)
#   
# }

# Plotting Fxn
# @df is the cleaned data frame to be plotted (likely either import or export data)
# @title_text is the text to be displayed on the plot
# @save_text is the path to save the plot to
F_ggplot_bar_vert_stack <- function(df, title_text, save_text){
  
  
  # Get the name of the input dataframe as a string
  df_name <- deparse(substitute(df))
  
  # Crop Colors 
  crop_colors <- if (grepl("exp", df_name, ignore.case = TRUE)) {
    c(
      "Soy" = "darkblue", 
      "Corn" = "lightblue")
  } else {
    c(
      "Soy" = "darkred", 
      "Corn" = "pink")
  }
  
  # Plot
  (p <- ggplot(df, aes(x = region_abv, y = chg_mmt, fill = crop)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = crop_colors, labels = c("Corn", "Soybean")) +
      coord_flip() +
      theme_bw() +
      labs(
        title = title_text,
        x = "",
        y = ""
      ) +
      theme(
        plot.title = element_text(hjust = 0.5),
        #legend.position = c(0.9, 0.9),
        legend.title = element_blank(),
      )
  )
  
  # Save
  ggsave(paste0(folder_fig, save_text),
         width = 6, height = 8)
  
  return(p)
}

## 0.2) Spatial Analysis for Areas of Interest ------- 

## NOTE: The plotting functions for each individual AOI are in their individual sections; e.g. Section 4 for plotting global results ##

# Fxn to Create and Save Violin Plots 
## NOTE: the SI code is similar but includes histograms and violin plots for more variables 
## @df is the raster or SpatRaster that we are plotting
## @area is the spatial region we are plotting, e.g. "World" or "US" or "Brazil" etc.
F_p_violin <- function(df, area){
  
  ## subset and change names ##
  
  # separate each for Maize and Soy
  df_pct_maizesoy <- df %>% 
    subset(c("pct_LND_MAZ", "pct_LND_SOY")) 
  names(df_pct_maizesoy) <- c("Maize", "Soybean")
  
  df_rawch_maizesoy <- df %>% 
    subset(c("rawch_MAZ", "rawch_SOY"))
  names(df_rawch_maizesoy) <- c("Maize", "Soybean")
  
  # violin plots for % change and raw change for maize and soy 
  # set size_title, size_labels, and size_axis_nums in the "Constants" section
  p1 <- bwplot(df_pct_maizesoy, 
               main = list(paste(area, "% Change Maize & Soybean", pct_title), cex = size_title),
               ylab = list("% Change", cex = size_labels),
               scales=list(
                 x = list(rot=45, cex = size_labels),
                 y = list(cex = size_axis_nums))
  )
  
  p2 <- bwplot(df_rawch_maizesoy, 
               main = list(paste(area, "Raw Change Maize & Soybean", pct_title), cex = size_title),
               ylab = list("Area (kha)", cex = size_labels),
               scales=list(
                 x = list(rot=45, cex = size_labels),
                 y = list(cex = size_axis_nums))
  )
  
  # save figures as PNGs - filename will include the area (e.g. US, Brazil) and model elasticity version (e.g. l, m, or h)
  png(filename = paste0(folder_fig, str_to_lower(area), pct, "_bw", "_pctchange", "_maizesoy", ".png"))
  plot(p1)
  dev.off()
  
  png(filename = paste0(folder_fig, str_to_lower(area), pct, "_bw", "_rawchange", "_maizesoy", ".png"))
  plot(p2)
  dev.off()
  
  # also plot these figures in the code window
  return(p1)
  return(p2)
}

# Fxn to compare Soybean to All and not include Maize in the final plot
## @df is the raster or SpatRaster that we are plotting
## @area is the spatial region we are plotting, e.g. "World" or "US" or "Brazil" etc.
F_p_violin_soy <- function(df, area){
  
  ## subset and change names ##
  df_pct_soy <- df %>% 
    subset(c("pct_QLAND", "pct_LND_SOY")) 
  names(df_pct_soy) <- c("All", "Soybean")
  
  
  # violin plots for % change in all simulation results and soy subsetted (because bwplot() needs two or more bwplots) 
  # set size_title, size_labels, and size_axis_nums in the "Constants" section
  p1 <- bwplot(df_pct_soy, 
               main = list(paste(area, "% Change Soybean", pct_title), cex = size_title),
               ylab = list("% Change", cex = size_labels),
               scales=list(
                 x = list(rot=45, cex = size_labels),
                 y = list(cex = size_axis_nums))
  )
  
  
  
  # save figures as PNGs - filename will include the area (e.g. US, Brazil) and model elasticity version (e.g. l, m, or h)
  png(filename = paste0(folder_fig, str_to_lower(area), pct, "_bw", "_pctchange", "_soy", ".png"))
  plot(p1)
  dev.off()
  
  
  # raw 
  df_raw_soy <- df %>%
    subset(c("rawch_QLAND", "rawch_SOY"))
  names(df_raw_soy) <- c("All", "Soybean")
  
  p2 <- bwplot(df_raw_soy,
               main = list(paste(area, "Raw Change Soybean", pct_title), cex = size_title),
               ylab = list("Area (kha)", cex = size_labels),
               scales=list(
                 x = list(rot=45, cex = size_labels),
                 y = list(cex = size_axis_nums))
  )
  png(filename = paste0(folder_fig, str_to_lower(area), pct, "_bw", "_rawchange", "_soy", ".png"))
  plot(p2)
  dev.off()
  
  # also plot these figures in the code window
  return(p1)
  return(p2)
}

# Fxn to incorporate both of these into one function
# Note that we exclude the clamping to 50,000 here as we fixed this step in our analysis
## @shp is the raw shapefile from 0_data_prep_BR_SIMPLEG.R and loaded in Section 2; differs per region of interest
## @area_name is the same as "area" in other functions - it is the name of the area of interest, e.g. "World" or "Brazil" or "US" etc. 
F_aoi_prep <- function(shp, area_name){
  
  ## Clip to AOI Extent ##
  # get extent as terra object for plotting
  ext_area <- vect(ext(shp))
  
  # set CRS of extent spatial vector
  crs(ext_area) <- crs(shp)
  
  # change so r (results raster) becomes the CRS of our shapefile 
  # we made a new variable her to not change the global r variable in our environment
  r_func <- r
  crs(r_func) <- crs(ext_area)
  
  # crop and masking to just the extent of interest
  r_aoi <- terra::crop(r_func, ext_area, mask = T) 
  r_aoi <- mask(r_aoi, shp)
  
  # save clipped and clamped raster with new AOI 
  saveRDS(r_aoi, file = paste0(folder_der_date, "r", pct, "_", area_name, ".rds"))
  
  # return as result
  return(r_aoi)
}

# Fxn to calculate total % Change
# basic function to compare two variables and calculate the percent change
F_calc_pct_change <- function(final, raw_ch){
  
  # we don't have initial, so we calculate it here
  initial = final - raw_ch
  
  # then we calculate percent change (results are in %)
  pct_change = ((final - initial)/initial)*100
  print(paste0("% Change is: ", pct_change, " %"))
}

# Fxn to create the PGC tables from the summary() function - this will be used in the EDA function
## @area_name is the same as "area" in other functions - it is the name of the area of interest, e.g. "World" or "Brazil" or "US" etc. 
## @pct is the SIMPLEG results scenario (either low, medium, or high)
F_clean_summary_tables <- function(area_name, pct){
  
  filename <- paste0(folder_results, "summary_tables/table_", area_name, pct, "_10e6_", date_string_nodash)
  csv2 <- read.csv(file = paste0(filename, ".csv"))
  
  df2 <- csv2  %>% 
    dplyr::select(-X) %>% 
    # remove all whitespace
    mutate(across(where(is.character), ~ str_replace_all(., " ", ""))) %>% 
    # rename by removing the "X.." or "X." 
    rename_with(~ str_replace_all(., "X\\.\\.?", "")) %>% 
    # split the first column to get the stats
    separate_wider_delim(new_QLAND, delim = ":", names = c("stat", "new_QLAND"), too_few = "align_start") %>%
    # remove everything before the colon (including the colon)
    mutate(across(where(is.character), ~ str_replace(., "^.*?:", ""))) %>% 
    
    # remove everything before the colon (including the colon)
    mutate(across(where(is.character), ~ str_replace(., "^.*?:", ""))) %>% 
    # convert to numeric
    mutate(across(!stat, as.numeric)) %>% 
    # get rid of the total new land columns - we're only interested in change
    dplyr::select(-c(new_QLAND, new_QCROP, new_LND_MAZ, new_LND_SOY)) %>%
    # add region column 
    mutate(reg = area_name) %>% 
    # rename columns for easy export
    rename(
      #"new_QLAND" new_QCROP new_LND_MAZ new_LND_SOY
      "Stat" = "stat",
      "Percent Change in Total Land Area (%)" = "pct_QLAND",
      "Raw Change in Total Land Area (kha)" = "rawch_QLAND", 
      "Percent Change in Total Crop Production (%)" = "pct_QCROP",
      "Raw Change in Total Crop Production (1000-ton CE)" = "rawch_QCROP",
      
      "Percent Change in Maize Area (%)" = "pct_LND_MAZ",
      "Raw Change in Maize Area (kha)" = "rawch_MAZ", 
      "Percent Change in Soybean Area (%)" = "pct_LND_SOY",
      "Raw Change in Soybean Area (kha)" = "rawch_SOY",
      "Region" = "reg") %>% 
    # remove all apostrophes (e.g. to get NA's to NAs)
    # Remove apostrophes from 'stat' column
    mutate(Stat = str_replace_all(Stat, "'", "")) %>%
    #!  # divide by 1000000 to get the accurate values (IF USING 10e6 VERSION)
    mutate(across(
      where(is.numeric),
      ~ if_else(Stat != "NAs", . / 1000000, .)
    ))
  
  df2_round <- df2 %>% 
    # Round each column to 3 decimal places if it is negative or greater than 0.01, else keep it the same
    mutate(across(
      where(is.numeric),
      ~ if_else(. < 0 | . > 0.01, round(., 3), .)
    ))
  
  
  
  # export the clean dataframe to an excel sheet so we can load it and work on spacing
  rio::export(df2_round, file = paste0(filename, ".xlsx"))
  # also export the whole values in case one rounds weird
  rio::export(df2, file = paste0(filename, "_no_round", ".xlsx"))
  
  
  ## Part 2: Auto-Generate Column Widths ##
  # load xlsx file 
  wb <- loadWorkbook(paste0(filename, ".xlsx"))
  # get sheet names 
  wb_sheet <- names(wb)
  # Adjust column widths based on column name lengths
  for (sheet in wb_sheet) {
    # Get column names
    col_names <- colnames(read.xlsx(wb, sheet))
    
    # Set column widths to match the length of each column name
    widths <- nchar(col_names) + 2 # Adding some extra space for readability
    setColWidths(wb, sheet, cols = 1:length(col_names), widths = widths)
  }
  
  # Save the updated workbook
  saveWorkbook(wb, paste0(filename, ".xlsx"), overwrite = TRUE)
}

# Fxn to get summary of data, call the violin fxn, and plot a basic map
# this is the big wrapping function that includes some of the previously defined functions
## @r_aoi is the result of 'F_aoi_prep' and is a cleaned and filtered SpatRaster of a given area
## @area_name is the same as "area" in other functions - it is the name of the area of interest, e.g. "World" or "Brazil" or "US" etc. 
F_EDA <- function(r_aoi, area_name){  
  # get and save a summary table
  table_area <- summary(r_aoi, size = Inf) # set size to not use a sample
  print(table_area)
  
  table_area_10e6 <- summary(r_aoi*1000000, size = Inf) 
  
  # set variable for file path
  tables_file <- paste0(folder_results, "summary_tables/")
  
  # essentially says, "if no file name contains the search string, create a folder with that string"
  if (!(any(grepl("summary_tables", folder_results)))) {
    dir.create(tables_file)
    # print if the fuile already exists or we created a new one
    cat("Folder ", tables_file, "created.\n")
  } else {
    cat("Folder ", tables_file, "already exists.\n")
  }
  
  # actually create the table as a CSV
  write.csv(table_area, file = paste0(folder_results, "summary_tables/", 
                                      "table_", area_name, pct, "_", date_string_nodash, ".csv"))
  
  # actually create the table as a CSV
  write.csv(table_area_10e6, file = paste0(folder_results, "summary_tables/", 
                                           "table_", area_name, pct, "_10e6_",  date_string_nodash, ".csv"))
  
  
  # clean tables 
  F_clean_summary_tables(area_name, pct = pct)
  
  ## Change Section ##
  print("Totals for Casc. Effect Graph and for Total Change")
  
  # Print total change values then calculate % Change
  cat("\n\nTotal Land Change (kha)\n\n")
  print(global(r_aoi$new_QLAND, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_QLAND, fun = "sum", na.rm = T))
  
  # Calc % change by grabbing the total (sum) values and running the % change function
  F_calc_pct_change(
    final = (global(r_aoi$new_QLAND, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_QLAND, fun = "sum", na.rm = T))[[1]]
  )
  
  # print the total change in crop production
  cat("\n\nTotal Production Change (1000 tons CE)\n\n")
  print(global(r_aoi$rawch_QCROP, fun = "sum", na.rm = T))
  
  # print the total changes in crop production for maize
  cat("\n\n Maize Results\n\n")
  print(global(r_aoi$new_LND_MAZ, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_MAZ, fun = "sum", na.rm = T))
  F_calc_pct_change(
    final = (global(r_aoi$new_LND_MAZ, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_MAZ, fun = "sum", na.rm = T))[[1]]
  )
  
  
  # print the total changes in crop production for soy  
  cat("\n\n Soy Results\n\n")
  print(global(r_aoi$new_LND_SOY, fun = "sum", na.rm = T))
  print(global(r_aoi$rawch_SOY, fun = "sum", na.rm = T))
  F_calc_pct_change(
    final = (global(r_aoi$new_LND_SOY, fun = "sum", na.rm = T))[[1]],
    raw_ch = (global(r_aoi$rawch_SOY, fun = "sum", na.rm = T))[[1]]
  )
  
  # Call EDA fxn to get and save violin plots 
  F_p_violin(r_aoi, area_name)
  F_p_violin_soy(r_aoi, area_name)
  # Plot basic initial maps - removed as this takes a lot of time 
  # terra::plot(r_aoi, 
  #             axes = F#, 
  #             #type = "interval"
  # )
}

# 1) Import / Export Plot ------------------------------------------------------------------------

## 1.1) Run Fxn to Clean & Join Import/Export Data --------

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## NOTE: MANUALLY MOVE regional_results.xlsx to each scenario folder ##
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# reset model variable here if you want to re-run with different amounts 
# pct_model <- "m"

# Load in data as xlsx (diff from previous) 
source_path <- paste0(folder_results_all, "regional_results.xlsx")
data_list <- import_list(source_path)

### 1.2.1 Exports -------
# Get Exports  
exp_soy <- F_clean_sheet(var = "Soy Exp", pct = pct_model)
exp_corn <- F_clean_sheet(var = "Corn Exp", pct = pct_model)

exp_cs <- rbind(exp_soy, exp_corn)

exp <- rbind(exp_soy, exp_corn)


# get sum by region
exp <- aggregate(exp$chg, list(exp$region_abv), FUN=sum)

# rename
names(exp) <- c("region_abv", "chg")

# get million metric tons 
exp$chg_mmt <- (exp$chg)/1000

# exclude us
exp_nous <- exp %>% filter(region_abv != "US")
#print(paste("Total Change in Exports (Mmt) (Excluding US): ", sum(exp_nous$chg_mmt)))

### 1.2.2 Imports ----------
# Get Imports  
imp_soy <- F_clean_sheet(var = "Soy Imp", pct = pct_model)
imp_corn <- F_clean_sheet(var = "Corn Imp", pct = pct_model)

imp_cs <- rbind(imp_soy, imp_corn)

imp <- rbind(imp_soy, imp_corn)

# get sum by region
imp <- aggregate(imp$chg, list(imp$region_abv), FUN=sum)

# rename
names(imp) <- c("region_abv", "chg")

# get million metric tons 
imp$chg_mmt <- (imp$chg)/1000

# exclude us
imp_nous <- imp %>% filter(region_abv != "US")

## 1.3) Vertical Barplots ------

# Run fxn to plot vertical barplot

## helpful link: https://stackoverflow.com/questions/48463210/how-to-color-code-the-positive-and-negative-bars-in-barplot-using-ggplot

# col_neg <- "red"
# col_pos <- "blue"
# 
# ### 1.3.1: Corn-Soy ----------
# # get Corn-Soy Exports
# (p_exp <- F_ggplot_bar_vert_sep(
#   df = exp_nous,
#   y_var = "chg_mmt",
#   title_text = "Change in Corn-Soy Exports (million metric ton)",
#   save_text = "bar_exp_fxn.png"
# ))
# 
# (p_exp <- F_ggplot_bar_vert_sep(
#   df = exp_soy %>% filter(region_abv != "US"),
#   y_var = "chg_mmt",
#   title_text = "Change in Soy Exports (million metric ton)",
#   save_text = "_t_bar_exp_soy.png"
# ))
# 
# (p_exp <- F_ggplot_bar_vert_sep(
#   df = exp_corn %>% filter(region_abv != "US"),
#   y_var = "chg_mmt",
#   title_text = "Change in Corn Exports (million metric ton)",
#   save_text = "_t_bar_exp_corn.png"
# ))
# 
# # get Corn-Soy Imports
# (p_imp <- F_ggplot_bar_vert_sep(
#   df = imp_nous,
#   y_var = "chg_mmt",
#   title_text = "Change in Corn-Soy Imports (million metric ton)",
#   save_text = "bar_imp_fxn.png"
# ))
# 
# # plot with the individual plots next to one another
# # labels give "A" and "B"
# (p <- plot_grid(p_imp, p_exp, labels = "auto"))
# 
# # save 
# ggsave(paste0(folder_fig, "bar_impexp.png"),
#        p,
#        width = 12, height = 6)

# Create Stacked Plot
(p_exp_stack <- F_ggplot_bar_vert_stack(
  df = exp_cs %>% filter(region_abv!="US"),
  #y_var = "chg_mmt",
  title_text = "Change in Corn and Soybean Exports (million metric ton)",
  save_text = "stackbar_cs_exp_fxn.png"
))

(p_imp_stack <- F_ggplot_bar_vert_stack(
  df = imp_cs %>% filter(region_abv!="US"),
  #y_var = "chg_mmt",
  title_text = "Change in Corn and Soybean Imports (million metric ton)",
  save_text = "stackbar_cs_imp_fxn.png"
))

(p_stack <- (p_imp_stack | p_exp_stack) + 
    plot_layout(guides = "collect") & 
    plot_annotation(tag_levels = 'a')&
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      plot.tag = element_text(size = 20))) 

# save 
ggsave(paste0(folder_fig, "stackedbar_impexp.png"),
       p_stack,
       width = 12, height = 6)


### 1.3.2 Soy -------
# Get Exports  
exp_soy <- F_clean_sheet(var = "Soy Exp", pct = pct_model)

# rename
names(exp_soy) <- c("region_abv", "chg")

# get million metric tons 
exp$chg_mmt <- (exp$chg)/1000

# exclude us
exp_nous <- exp %>% filter(region_abv != "US")
#print(paste("Total Change in Exports (Mmt) (Excluding US): ", sum(exp_nous$chg_mmt)))

### 1.2.2 Imports ----------
# Get Imports  
imp_soy <- F_clean_sheet(var = "Soy Imp", pct = pct_model)
imp_corn <- F_clean_sheet(var = "Corn Imp", pct = pct_model)
imp_cs <- rbind(imp_soy, imp_corn)

imp <- rbind(imp_soy, imp_corn)

# get sum by region
imp <- aggregate(imp$chg, list(imp$region_abv), FUN=sum)

# rename
names(imp) <- c("region_abv", "chg")

# get million metric tons 
imp$chg_mmt <- (imp$chg)/1000

# exclude us
imp_nous <- imp %>% filter(region_abv != "US")

# ## 1.4) Print Results for MS (excluding US) ------
# # US Reductions in Corn/SoyExports 
# print(paste("Total Change in US Soy Exports (Mmt) (Excluding US): ", exp_soy$chg_mmt[exp_soy$region_abv == "US"]))
# print(paste("Total Change in US Corn Exports (Mmt) (Excluding US): ", exp_corn$chg_mmt[exp_corn$region_abv == "US"]))
# exp_soy$chg_mmt[exp_soy$region_abv == "US"] + exp_corn$chg_mmt[exp_corn$region_abv == "US"]
# 
# 
# # Total Exp
# print(paste("Total Change in Exports (Mmt) (Excluding US): ", sum(exp_nous$chg_mmt)))
# # Soy/Corn Exp
# print(paste("Total Change in Soy Exports (Excluding US): ", 
#             sum(exp_soy[!(exp_soy$region_abv %in% "US"),]$chg_mmt)))
# print(paste("Total Change in Corn Exports (Excluding US): ", 
#             sum(exp_corn[!(exp_corn$region_abv %in% "US"),]$chg_mmt)))
# 
# 
# # Total Imp
# print(paste("Total Change in Imports (Mmt) (Excluding US): ", sum(imp_nous$chg_mmt)))
# # Soy/Corn Imp
# print(paste("Total Change in Soy Imports (Excluding US): ", 
#             sum(imp_soy[!(imp_soy$region_abv %in% "US"),]$chg_mmt)))
# print(paste("Total Change in Corn Imports (Excluding US): ", 
#             sum(imp_corn[!(imp_corn$region_abv %in% "US"),]$chg_mmt)))
# 
# # US Reductions in Corn/SoyExports 
# print(paste("Total Change in US Soy Imports (Mmt) (US Only): ", imp_soy$chg_mmt[imp_soy$region_abv == "US"]))
# print(paste("Total Change in US Corn Imports (Mmt) (US Only): ", imp_corn$chg_mmt[imp_corn$region_abv == "US"]))
# 
# imp_soy$chg_mmt[imp_soy$region_abv == "US"] + imp_corn$chg_mmt[imp_corn$region_abv == "US"]
# 

## 1.5) Create Clean Results Sheet for Casc Effects Plot ----

# Function for summarizing data - sums for .. and mean for ..
F_calc_totals <- function(data){
  ## add row for total changes ##
  # Calculate sum for columns A and B, and mean for column C
  summarised_data <- data %>%
    summarise(
      pre = sum(pre),
      post = sum(post),
    ) 
  
  # get character values from the dataset   
  summarised_data <- summarised_data %>% 
    mutate(
      # ca
      pct_chg = ((post-pre)/pre)*100,
      chg = post-pre,
      chg_mmt = chg/1000,
      
      region_abv = "Total",
      variable = data$variable[1],
      crop = data$crop[1],
      type = data$type[1],
      modeltype = data$modeltype[1]
    )
  
  
  # Add a row at the bottom with the total values
  data <- summarised_data %>%
    bind_rows(data, .)
  
  return(data)
}

# clean data by running through each sheet with the above function 
sheets <- names(data_list)
data_clean <- lapply(X = sheets, FUN = F_clean_sheet, pct = pct_model)
names(data_clean) <- names(data_list)
data_clean <- lapply(X = data_clean, FUN = F_calc_totals)

# save clean sheet 
rio::export(
  data_clean, 
  file = paste0(folder_results, 'regional_results_clean_', pct_model, '.xlsx'))


### 1.5.1 Global Price Changes ----
# paste("Global Soy Price Change: ", mean(data_clean$`Soy Exp Price index`$pct_chg))
# paste("Global Corn Price Change: ", mean(data_clean$`Corn Exp Price index`$pct_chg))


# 2) Load Shapefiles & SIMPLE-G Raster ------------------------------------------------------------------------

### RUN 'processResults_SIMPLEG_1.R' FIRST TO CREATE RASTER AND SHAPEFILES ###

# load files and maize-soy raster
load("../Data_Derived/shp_usbr.RData")
shp_countries <- shp_world %>% dplyr::select(name_long)

r <- readRDS(file = paste0(folder_der_date, "r_maizesoy", pct, ".rds"))

# 3) Edit Stack & Check Values ------------------------------------------------------------------------

## 3.1) Calc & Add Raw Change from % and New -------
# Formula: new - (new / ((pct_change/100)+1))

# subset 
r_pct_qland <- subset(r, "pct_QLAND")
r_new_qland <- subset(r, "new_QLAND")

r_pct_qcrop <- subset(r, "pct_QCROP")
r_new_qcrop <- subset(r, "new_QCROP")

r_pct_maize <- subset(r, "pct_LND_MAZ")
r_new_maize <- subset(r, "new_LND_MAZ")

r_pct_soy <- subset(r, "pct_LND_SOY")
r_new_soy <- subset(r, "new_LND_SOY")

# NOTE: rawch = Raw Change
r_rawch_qcrop <- r_new_qcrop - (r_new_qcrop / ((r_pct_qcrop/100)+1))
r_rawch_qland <- r_new_qland - (r_new_qland / ((r_pct_qland/100)+1))

r_rawch_maize <- r_new_maize - (r_new_maize / ((r_pct_maize/100)+1))
r_rawch_soy <- r_new_soy - (r_new_soy / ((r_pct_soy/100)+1))

# add raw change layers back into stack
r <- c(
  r, 
  r_rawch_qcrop, r_rawch_qland,
  r_rawch_maize, r_rawch_soy
)

r

# set names 
names(r) #NOTE: make sure everything is in the right order below! 
names(r) <- c(
  "pct_QLAND", 
  "new_QLAND", 
  "pct_QCROP", 
  "new_QCROP",
  
  "pct_LND_MAZ",
  "pct_LND_SOY",
  "new_LND_MAZ",
  "new_LND_SOY",
  
  "rawch_QCROP", 
  "rawch_QLAND",
  "rawch_MAZ",
  "rawch_SOY"
)

# 4) World Results  ------------------------------------------------------------------------

## 4.1) World EDA -----

# Call fxn to clip and prep data 
r_row <- F_aoi_prep(shp = shp_world, area_name = "World")

# call fxn to create EDA plots and generate stats of the clipped data 
F_EDA(r_aoi = r_row, area_name = "World")

## 4.2) World Interval Plot --------

# Set up fxn for plotting across specific intervals
F_ggplot_interval <- function(df, title_text, title_legend, save_title){
  
  # plot 
  p <- ggplot() +
    geom_spatraster(data = df, maxcell = Inf, aes(fill = cats)) +
    scale_fill_whitebox_d(palette = "pi_y_g", direction = 1)+
    
    #geom_sf(data = vect(shp_ecoregions), color = "gray60", fill = "transparent", lwd = 0.1)+
    geom_sf(data = vect(shp_countries), color = "gray30", fill = "transparent", lwd = 0.2)+
    
    theme_minimal()+ 
    labs(
      fill = title_legend,
      title = title_text,
    )+
    
    #coord_sf(crs = "ESRI:53042")+ #Winkel-Tripel 
    #coord_sf(crs = "ESRI:53030")+ # Robinson
    
    theme(
      plot.title = 
        element_text(
          hjust = 0.5, 
          size = 22
        ),
      legend.title = element_text(size = 18),
      legend.text = element_text(size = 12)
    )  
  
  # save plot
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = 14, height = 6, dpi = 300)
  
  return(p)
  
}


### 4.2.1) Total Change (C+S) -------
# example from: https://cloud.r-project.org/web/packages/tidyterra/tidyterra.pdf
# With discrete values
tmp <-  r_row %>%
  subset("rawch_QLAND")

# create df
factor <- tmp %>%
  # add column with break intervals
  mutate(
    cats =
      cut(rawch_QLAND,
          # manually set break intervals here 
          breaks = c(-5, -2.5, -1, -0.5, -0.01,
                     0.01, 0.1, 0.25, 0.5, 1))
  )

# run function to create plot
F_ggplot_interval(
  df = factor, 
  title_text = "Global Change in Cropland Area",
  title_legend = "Area (kha)",
  save_title = "gg_world_rawch_croplandarea.png")

### 4.2.2) Total Soy Change -------
# example from: https://cloud.r-project.org/web/packages/tidyterra/tidyterra.pdf
# With discrete values
tmp <-  r_row %>%
  subset("rawch_SOY")

# create df
factor <- tmp %>%
  mutate(
    cats =
      cut(rawch_SOY,
          breaks = c(-5, -3, -1, -0.1, -0.01,
                     0.01, 0.25, 0.5, 1, 2))
  )

# run Fxn
F_ggplot_interval(
  df = factor, 
  title_text = "Global Change in Soybean Cropland Area",
  title_legend = "Area (kha)",
  save_title = "gg_rawch_soy_croplandarea_4326.png")



## 4.3) World Results w/o US ------
# get extent as terra object for plotting
us_vect <- vect(shp_us)

# set CRS of extent spatial vector
crs(us_vect) <- crs(r)

## 
r_no_us <- mask(r, us_vect, inverse = T)
summary(r_no_us*1000000, size = Inf)
#summary(r*1000000, size = Inf)

# plot(r_no_us$rawch_QLAND)
F_EDA(r_aoi = r_no_us, area_name = "RoW")

# 5) US Results ------------------------------------------------------------------------

## 5.1) US EDA -------

# Call fxn to clip data 
r_us <- F_aoi_prep(shp = shp_us, area_name = "US")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_us, area_name = "US")

## 5.2) US Interval Plot -------
F_ggplot_us_interval <- function(df, title_text, title_legend, save_title){
  
  # plot 
  #"atlas", "high_relief", "arid", "soft", "muted", 
  #"purple", "viridi", "gn_yl", "pi_y_g", "bl_yl_rd", "deep"
  p <- ggplot() +
    geom_spatraster(data = df, maxcell = Inf, aes(fill = cats)) +
    #scale_fill_wiki_d(na.value = "white")
    scale_fill_whitebox_d(palette = "pi_y_g", direction = 1, drop = F)+
    
    geom_sf(data = vect(shp_us), color = "gray30", fill = "transparent", lwd = 0.2)+
    #coord_sf(crs = "EPSG:2163")+ # Robinson
    coord_sf(crs = "EPSG:4326")+ # WGS 1984
    
    theme_minimal()+
    labs(
      fill = title_legend,
      title = title_text,
    )+
    
    #coord_sf(crs = "ESRI:53042")+ #Winkel-Tripel 
    #coord_sf(crs = "ESRI:53030")+ # Robinson
    
    theme(
      plot.title = 
        element_text(
          hjust = 0.5, 
          size = 40
        ),
      legend.title = element_text(size = 24),
      legend.position = c(0.9, 0.3),
      legend.text = element_text(size = 14)
    )  
  
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = 16, height = 8, dpi = 300)
  
  return(p)
  
  
}

### 5.2.1) Total US Change (C+S) -------
# example from: https://cloud.r-project.org/web/packages/tidyterra/tidyterra.pdf
# With discrete values
tmp <-  r_us %>%
  subset("rawch_QLAND")

# create df
factor <- tmp %>%
  mutate(
    cats =
      cut(rawch_QLAND,
          breaks = c(-5, -3, -1, -0.1, -0.01,
                     0.01, 0.25, 0.5, 1, 2))
  )

# run Fxn
F_ggplot_us_interval(
  df = factor, 
  title_text = "Change in US Cropland Area",
  title_legend = "Area (kha)",
  save_title = "gg_us_rawch_croplandarea_4326.png")

### 5.2.2) US Soy Change -------
# example from: https://cloud.r-project.org/web/packages/tidyterra/tidyterra.pdf
# With discrete values
tmp <-  r_us %>%
  subset("rawch_SOY")

# create df
factor <- tmp %>%
  mutate(
    cats =
      cut(rawch_SOY,
          breaks = c(-5, -3, -1, -0.1, -0.01,
                     0.01, 0.25, 0.5, 1, 2))
  )

# run Fxn
F_ggplot_us_interval(
  df = factor, 
  title_text = "Change in US Soybean Cropland Area",
  title_legend = "Area (kha)",
  save_title = "gg_us_rawch_soy_croplandarea_4326.png")


## 5.3) US Prod Plot for SI ------
tmp <-  r_us %>%
  subset("rawch_QCROP")

# create df
factor <- tmp %>%
  mutate(
    cats =
      cut(rawch_QCROP,
          breaks = c(-10, -5, -1, -0.1, -0.01,
                     0.01, 0.5, 1, 2, 3))
    # uncomment to match breaks with cropland expansion
    # breaks = c(-5, -3, -1, -0.1, -0.01,
    #            0.01, 0.25, 0.5, 1, 2))
  )

# run Fxn
F_ggplot_us_interval(
  df = factor, 
  title_text = "Raw Change in Crop Production",
  title_legend = "CPI\n(1000-tons CE)",
  save_title = "gg_us_rawch_cropprod_4326.png")


# 6) Brazil Results --------

## 6.1) Brazil EDA -------
# Call fxn to clip data 
r_br <- F_aoi_prep(shp = shp_br, area_name = "Brazil")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_br, area_name = "Brazil")


## 6.2) Brazil + Cerrado Continuous Plot -------

# set up function for both 
F_ggplot_brcerr <- function(df, area, brks, pal, legend_title, p_title, save_title){
  
  # plot
  p <- ggplot()+
    geom_spatraster(data = df, maxcell = Inf)+
    #coord_sf(crs = "EPSG:5880")+ # SIRGAS 2000 /Brazil Polyconic
    
    # use continuous palette
    scale_fill_whitebox_c(
      #palette = "viridi", direction = 1,
      palette = pal,
      breaks = brks
    )+
    labs(
      fill = legend_title,
      title = p_title
    )+
    theme_minimal() +
    
    theme(
      # set plot size and center it 
      plot.title = element_text(size = 24, hjust = 0.5),
      # put legend in the bottom right 
      #legend.position = c(0.15, 0.2),
      legend.position = c(0.1, 0.15),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 10))#+
  
  
  
  # option to plot all the states containing any Cerrado biome
  #geom_sf(data = shp_cerr_states, color = "gray70", fill = "transparent", lwd = 0.2)+
  
  # option to plot all Brazilian states 
  #geom_sf(data = shp_br_states, color = "gray70", fill = "transparent", lwd = 0.1)+ 
  
  # option to plot the outline of the Cerrado
  #geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)#+
  
  # otion to plot BR country outline
  #geom_sf(data = shp_br_border, color = "gray20", fill = "transparent", lwd = 0.4)#+
  
  
  # set conditional width & height & outlines 
  if(area== "Cerrado"){
    w = 12
    h = 8
    
    # add outlines based on the AOI
    p <- p + 
      # option to plot all the states containing any Cerrado biome
      geom_sf(data = shp_cerr_states, color = "gray50", fill = "transparent", lwd = 0.2)+ 
      # option to plot the outline of the Cerrado
      geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)
  }
  
  else{
    w = 14
    h = 7
    
    # add outlines based on the AOI
    p <- p +
      # option to plot BR country outline
      geom_sf(data = shp_br_border, color = "gray20", fill = "transparent", lwd = 0.4)+
      # option to plot the outline of the Cerrado
      geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)
  }
  
  ggsave(plot = p, filename = paste0(folder_fig, "/", save_title),
         width = w, height = h, dpi = 300)
  
  return(p)
}

### 6.2.1) Total Brazil Change (C+S) -------
# call Fxn for Brazil
F_ggplot_brcerr(df = r_br %>% subset("rawch_QLAND"),
                brks = waiver(), 
                area = "Brazil",
                pal = "gn_yl", 
                legend_title = "Area (kha)",
                p_title = paste("Change in BR Cropland Area", pct_title),
                save_title = "gg_br_rawch_croplandarea.png")

### 6.2.2) Brazil Soy Change ------
F_ggplot_brcerr(df = r_br %>% subset("rawch_SOY"),
                brks = waiver(), 
                area = "Brazil",
                pal = "gn_yl", 
                legend_title = "Area (kha)",
                p_title = paste("Change in BR Soybean Cropland Area", pct_title),
                save_title = "gg_br_rawch_soy_croplandarea.png")

## 6.3) BR Prod Plot for SI -----
F_ggplot_brcerr(df = r_br %>% subset("rawch_QCROP"),
                brks = waiver(), 
                area = "Brazil",
                pal = "gn_yl", 
                legend_title = "CPI (tons CE)",
                p_title = paste("Change in BR Crop Production", pct_title),
                save_title = "gg_br_rawch_cropprod.png")



# 7) Cerrado Results ----------

## 7.1) Cerrado EDA -------
# Call fxn to clip data 
r_cerr <- F_aoi_prep(shp = shp_cerr, area_name = "Cerrado")

# call fxn to create EDA plots of the clipped data 
F_EDA(r_aoi = r_cerr, area_name = "Cerrado")


## 7.2) Cerrado Plot -------

### 7.2.1) Cerrado Land (C+S) Plot -------
## NOTE: Cerrado is slightly different as a scale bar and N arrow are very helpful here
p2 <- F_ggplot_brcerr(
  df = r_cerr %>% subset("rawch_QLAND"),
  area = "Cerrado",
  brks = waiver(),
  pal = "gn_yl", 
  legend_title = "Area (kha)",
  p_title = paste("Change in Cerrado Cropland Area", pct_title),
  save_title = "gg_cerr_rawch_croplandarea.png")

# call variable to save base map
p2

# Add scale bar and N arrow manually 
p2 <- p2 +       
  annotation_scale(location = "br", width_hint = 0.4) +  # Scale bar at the bottom right
  annotation_north_arrow(location = "br", which_north = "true",  # North arrow at the bottom right
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_minimal())

p2
ggsave(plot = p2, filename = paste0(folder_fig, "/", "gg_cerr_rawch_croplandarea_nscale.png"),
       width = 12, height = 8, dpi = 300)

### 7.2.2) Cerrado Soy Land Plot -------
p3 <- F_ggplot_brcerr(
  df = r_cerr %>% subset("rawch_SOY"),
  area = "Cerrado",
  brks = waiver(),
  pal = "gn_yl", 
  legend_title = "Area (kha)",
  p_title = paste("Change in Cerrado Soybean Cropland Area", pct_title),
  save_title = "gg_cerr_rawch_soy_croplandarea.png")

# call variable to save base map
p3

# Add scale bar and N arrow manually 
p3 <- p3 +       
  annotation_scale(location = "br", width_hint = 0.4) +  # Scale bar at the bottom right
  annotation_north_arrow(location = "br", which_north = "true",  # North arrow at the bottom right
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_minimal())

p3
ggsave(plot = p3, filename = paste0(folder_fig, "/", "gg_cerr_rawch_soy_croplandarea_nscale.png"),
       width = 12, height = 8, dpi = 300)


## 7.3) Cerrado Prod Plot for SI --------
p4 <- F_ggplot_brcerr(
  df = r_cerr %>% subset("rawch_QCROP"),
  area = "Cerrado",
  brks = waiver(),
  pal = "gn_yl", 
  legend_title = "CPI\n(1000-tons CE)",
  p_title = paste("Change in Cerrado Crop Production Index", pct_title),
  save_title = "gg_cerr_rawch_cropprod.png")

# call variable to save base map
p4

# Add scale bar and N arrow manually 
p4 <- p4 +       
  annotation_scale(location = "br", width_hint = 0.4) +  # Scale bar at the bottom right
  annotation_north_arrow(location = "br", which_north = "true",  # North arrow at the bottom right
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_minimal())

p4
ggsave(plot = p4, filename = paste0(folder_fig, "/", "gg_cerr_rawch_cropprod_nscale.png"),
       width = 12, height = 8, dpi = 300)

# 8) Maize/Soy Summary Statistics (Per-Grid-Cell) --------
## 8.1) Sum of total changes for CSV ----
# Fxn to get the total changes
F_sum <- function(df, layer){
  test2 <- df %>% subset(layer)
  global(test2, fun = "sum", na.rm = T)[1,]
}

# Fxn to create df of changes and calculate "PRE" values
F_area_stats <- function(df, extent_text){
  # create column of labels for easier recall
  labels <- c(
    "new_cropland_area", 
    "raw_change_cropland_area",
    "new_crop_production", 
    "raw_change_crop_production",
    
    "new_soy_area",
    "raw_change_soy_area",
    
    "new_maize_area",
    "raw_change_maize_area"
  )
  
  # get layers from input df using sum function created above
  values <- c(
    F_sum(df, "new_QLAND"),
    F_sum(df, "rawch_QLAND"),
    F_sum(df, "new_QCROP"),
    F_sum(df, "rawch_QCROP"),
    
    F_sum(df, "new_LND_SOY"),
    F_sum(df, "rawch_SOY"),
    F_sum(df, "new_LND_MAZ"),
    F_sum(df, "rawch_MAZ")
    
  )
  
  # create data frame from the other layers and their labels 
  df2 <- data.frame(labels, values)
  
  # pivot wide to create 'pre' data, then pivot long to make tidy
  df2 <- df2 %>% 
    
    # make wide so each variable is its own column for easier math
    pivot_wider(names_from = "labels", values_from = "values") %>% 
    
    # calculate PRE values
    mutate(
      extent = extent_text,
      pre_cropland_area = new_cropland_area - raw_change_cropland_area,
      pre_crop_production = new_crop_production - raw_change_crop_production,
      
      pre_soy_area = new_soy_area - raw_change_soy_area,
      pre_maize_area = new_maize_area - raw_change_maize_area
    ) %>% 
    
    # make long again so the column headers are Extent, Labels, Values
    pivot_longer(cols = -extent, names_to = "labels", values_to = "values")
}

# run fxn for each AOI
stat_SG_US_maizesoy <- F_area_stats(r_us, "US")  
stat_SG_BR_maizesoy <- F_area_stats(r_br, "Brazil")  
stat_SG_Cerrado_maizesoy <- F_area_stats(r_cerr, "Cerrado")  

# combine to one df
stat_SG_summary_maizesoy <- rbind(stat_SG_US_maizesoy, stat_SG_BR_maizesoy, stat_SG_Cerrado_maizesoy)

# save
write.csv(stat_SG_summary_maizesoy, 
          file = paste0(
            folder_results, "sg", pct, "_stat_summary_maizesoy_US_BR_Cerr_", date_string_nodash, ".csv"),
          row.names = F)

## 8.2) Changes calculated for MS Abstract -------

### text block 1 ----
# Text:  Mean area of corn and soy land expansion per grid-cell in the Cerrado (32.2 ha) was ~1.6 times higher than in Brazil as a whole (24.2 ha).
t_c1 <- as.numeric(terra::global(r_cerr$rawch_QLAND, fun = "mean", na.rm = T))
t_br1 <- as.numeric(terra::global(r_br$rawch_QLAND, fun = "mean", na.rm = T))

t_comp1 <- t_c1 / t_br1

t_c1 / t_br1

paste0("Mean area of corn and soy land expansion per grid-cell in the Cerrado (",
       round(t_c1*1000, 1), 
       " ha) was ", 
       round(t_comp1, 2), 
       " times higher than in Brazil as a whole (", 
       round(t_br1*1000, 1),
       " ha).")

### text block 2 ----

## SOY ## 
# We found, on average, that a 1 ha decrease in the amount of cropland dedicated to soybean in the US leads to a 0.20 increase in Cerrado soybean cropland. 
t_s_c2 <- as.numeric(global(r_cerr$rawch_SOY, fun = "sum", na.rm = T))
t_s_us2 <- as.numeric(global(r_us$rawch_SOY, fun = "sum", na.rm = T))
t_s_comp2 <- t_s_c2 / t_s_us2

paste0("We found, on average, that a 1 ha decrease in the amount of cropland dedicated to soybean in the US leads to a ",
       format(round(t_s_comp2*-1, 2), nsmall = 2),  
       " ha increase in Cerrado soybean cropland.")

t_s_br2 <- as.numeric(global(r_br$rawch_SOY, fun = "sum", na.rm = T))
t_s_comp2_usbr <- t_s_br2 / t_s_us2

paste0("We found, on average, that a 1 ha decrease in the amount of cropland dedicated to soybean in the US leads to a ",
       format(round(t_s_comp2_usbr*-1, 2), nsmall = 2),  
       " ha increase in Brazil soybean cropland.")


## C+S ##
t_c2 <- as.numeric(global(r_cerr$rawch_QLAND, fun = "sum", na.rm = T))
t_us2 <- as.numeric(global(r_us$rawch_QLAND, fun = "sum", na.rm = T))
t_comp2 <- t_c2 / t_us2

paste0("We found, on average, that a 1 ha decrease in the amount of cropland (corn and soy) in the US leads to a ",
       format(round(t_comp2*-1, 2), nsmall = 2),  
       " ha increase in Cerrado cropland.")

t_br2 <- as.numeric(global(r_br$rawch_QLAND, fun = "sum", na.rm = T))
t_comp2_usbr <- t_br2 / t_us2

paste0("We found, on average, that a 1 ha decrease in the amount of cropland (corn and soy) in the US leads to a ",
       format(round(t_comp2_usbr*-1, 2), nsmall = 2),  
       " ha increase in Brazil cropland.")

## 8.3) Merge Per-Grid-Cell Tables ------
## Works as long as the "_pgc" file is not in the results folder 

# Set the folder path for per-grid-cell (PGC) results
pgc_path <- paste0("../Results/SIMPLEG/", pct_model, "/summary_tables")

# List all .xlsx files containing "_no_round"
pgc_file_list <- list.files(path = pgc_path, pattern = "_no_round.*\\.xlsx$", full.names = TRUE)

# Read all files into a list of data frames using openxlsx
pgc_list <- lapply(pgc_file_list, function(file) {
  read.xlsx(file)
})

# Combine all data frames column-wise
pgc_combined_df <- do.call(rbind, pgc_list)

# View the result
print(pgc_combined_df)

# Replace periods in column names with spaces
colnames(pgc_combined_df) <- gsub("\\.", " ", colnames(pgc_combined_df))

# Define output file path
pgc_output_file <- file.path(pgc_path, paste0("_pgc_allresults_no_round", pct, ".xlsx"))

# Write to new Excel file
write.xlsx(pgc_combined_df, pgc_output_file)

# 9) Create Regional Results Table for Areas of Interest ------

## 9.0) Load Data ------
# Call the df from "Clean Results Sheet" step in Section 1
data_clean 
sheets

# call 'r_cerr' to get the summarized per-pixel values within the Cerrado
r_cerr <- rast(read_rds(paste0(folder_der_date, "r", pct, "_Cerrado.rds")))


## 9.1) Get data into one df --------

# Define regions to keep
reg_names <- c("BRA", "US", "EU", "CHINA", "S_Amer", "Total")

# Function to assign units based on 'type'
F_reg_assign_unit <- function(type) {
  if (type == "Area") {
    return("kha")
  } else if (type == "Production") {
    return("1000-Tons CE")
  } else if (type %in% c("Imp", "Exp")) {
    return("metric tons")
  } else if (tolower(type) == "index") {
    return("USD/mt")
  } else {
    return(NA)
  }
}


# Process each data frame in the list
reg_cleaned_list <- lapply(data_clean, function(df) {
  df <- df %>% 
    filter(region_abv %in% reg_names) %>% 
    dplyr::select(-chg_mmt) %>% 
    mutate(Unit = sapply(type, F_reg_assign_unit))
  return(df)
})


# Combine all processed data frames into one
reg_df <- bind_rows(reg_cleaned_list)
head(reg_df)

# Optionally write to Excel
# write.xlsx(reg_df, "../Results/st_agg.xlsx", overwrite = TRUE)

## 9.2) Calculate CornSoy -----


# Define types to aggregate
reg_cs_types <- c("Area", "Production", "Imp", "Exp", "index")

# Aggregate sum for Area, Production, Imp, Exp
reg_df_combined <- reg_df %>%
  filter(type %in% reg_cs_types & type != "index") %>%
  group_by(region_abv, type) %>%
  summarise(
    pre = sum(pre, na.rm = TRUE),
    post = sum(post, na.rm = TRUE),
    .groups = "drop"
  )



# Aggregate average for index type
reg_priceindex <- reg_df %>%
  filter(type == "index") %>%
  group_by(region_abv) %>%
  summarise(
    pre = mean(pre, na.rm = TRUE),
    post = mean(post, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(type = "index")

# Combine both
reg_df_combined <- bind_rows(reg_df_combined, reg_priceindex) %>%
  mutate(
    crop = "CornSoy",
    variable = paste("CornSoy", type),
    chg = post - pre,
    pct_chg = ((post - pre) / pre) * 100,
    modeltype = pct_model,
    Unit = case_when(
      type == "Area" ~ "kha",
      type == "Production" ~ "1000-Tons CE",
      type %in% c("Imp", "Exp") ~ "metric tons",
      type == "index" ~ "USD/mt",
      TRUE ~ NA_character_
    )
  )

# Combine this with the reg_df from before to get one huge df of change 
reg_df_combined_types <- reg_df_combined %>% 
  rbind(reg_df) 

# Create a template from final_combined
reg_template <- reg_df_combined_types[0, ]
reg_new <- "Cerrado"

# Add multiple rows for the Cerrado, calculate them similar to the "text-block" step
# NOTE: SIMPLE-G doesn't report source data "pre" values, so we have to back-calculate them with the formula: pre = post - chg 
# e.g. after the shock there were 100 kha maize, it changed +20, so there must have been 80 kha before

reg_df_newreg <- bind_rows(
  reg_template %>% add_row(
    region_abv = reg_new,
    variable = "Corn Area",
    #pct_chg = as.numeric(terra::global(r_cerr$pct_LND_MAZ, fun = "mean", na.rm = T)),
    chg = as.numeric(terra::global(r_cerr$rawch_MAZ, fun = "sum", na.rm = T)),
    pre = as.numeric(terra::global(r_cerr$new_LND_MAZ, fun = "sum", na.rm = T)) - as.numeric(terra::global(r_cerr$rawch_MAZ, fun = "sum", na.rm = T)),
    post = as.numeric(terra::global(r_cerr$new_LND_MAZ, fun = "sum", na.rm = T)),
    Unit = "kha",
    modeltype = pct_model,
    crop = "Corn",
    type = "Area"
  ),
  reg_template %>% add_row(
    region_abv = reg_new,
    variable = "CornSoy Area",
    #pct_chg = as.numeric(terra::global(r_cerr$pct_QLAND, fun = "mean", na.rm = T)),
    chg = as.numeric(terra::global(r_cerr$rawch_QLAND, fun = "sum", na.rm = T)),
    pre = as.numeric(terra::global(r_cerr$new_QLAND, fun = "sum", na.rm = T)) - as.numeric(terra::global(r_cerr$rawch_QLAND, fun = "sum", na.rm = T)),
    post = as.numeric(terra::global(r_cerr$new_QLAND, fun = "sum", na.rm = T)),
    Unit = "kha",
    modeltype = pct_model,
    crop = "CornSoy",
    type = "Area"
  ),
  reg_template %>% add_row(
    region_abv = reg_new,
    variable = "CornSoy Production",
    #pct_chg = as.numeric(terra::global(r_cerr$pct_QCROP, fun = "mean", na.rm = T)),
    chg = as.numeric(terra::global(r_cerr$rawch_QCROP, fun = "sum", na.rm = T)),
    pre = as.numeric(terra::global(r_cerr$new_QCROP, fun = "sum", na.rm = T)) - as.numeric(terra::global(r_cerr$rawch_QCROP, fun = "sum", na.rm = T)),
    post = as.numeric(terra::global(r_cerr$new_QCROP, fun = "sum", na.rm = T)),
    Unit = "metric tons",
    modeltype = pct_model,
    crop = "CornSoy",
    type = "Production"
  ),
  reg_template %>% add_row(
    region_abv = reg_new,
    variable = "Soy Area",
    #pct_chg = as.numeric(terra::global(r_cerr$rawch_SOY, fun = "sum", na.rm = T)),
    chg = as.numeric(terra::global(r_cerr$rawch_SOY, fun = "sum", na.rm = T)),
    pre = as.numeric(terra::global(r_cerr$new_LND_SOY, fun = "sum", na.rm = T)) - as.numeric(terra::global(r_cerr$rawch_SOY, fun = "sum", na.rm = T)),
    post = as.numeric(terra::global(r_cerr$new_LND_SOY, fun = "sum", na.rm = T)),
    Unit = "kha",
    modeltype = pct_model,
    crop = "Soy",
    type = "Area"
  )
)

# Calculate TOTAL or AGGREGATED percent change. This is different than the process for if we were to
# run global(r_cerr$rawch_SOY, fun = "mean"), which takes the mean of the percentage change raster, rahter than calculating the total percentage mean
# across the whole Cerrado (e.g. ((sum_final_value - sum_initial_value) / sum_initial_value) *100). 
# We opt for the latter because it is consistent with how SIMPLE-G calculates mean for regions around the world.
reg_df_newreg <- reg_df_newreg %>% 
  mutate(pct_chg = ((post-pre)/pre)*100)

reg_df_cerr <- reg_df_combined_types %>% 
  rbind(reg_df_newreg)%>% 
  dplyr::select(region_abv, variable, pct_chg, chg, pre, post, Unit, modeltype, crop, type) %>% 
  mutate(region_abv = case_when(
    region_abv == "BRA" ~ "Brazil",
    region_abv == "S_Amer" ~ "S. America (excl. Brazil)",
    region_abv == "CHINA" ~ "China",
    TRUE ~ region_abv
  ))



## 9.3) Calculate the Total Price Changes Separately ------
# NOTE: need to calculate the Total Change in Price Index differently than anything else because they are the mean of change from all regions

# Extract the chg value for region_abv == "Total"
## Corn
reg_index_c_totalchg <- data_clean[["Corn Exp Price index"]] %>%
  filter(region_abv == "Total") %>%
  pull(chg)

## Soy
reg_index_s_totalchg <- data_clean[["Soy Exp Price index"]] %>%
  filter(region_abv == "Total") %>%
  pull(chg)

## CornSoy
reg_index_cs_totalchg <- reg_index_c_totalchg + reg_index_s_totalchg

# Change them to these chg values divided by 17 (the original number of regions), except for corn+soy, which needs to be divded by the total corn and total soy regions (17+17)
reg_index_c_totalchg <- reg_index_c_totalchg/17
reg_index_s_totalchg <- reg_index_s_totalchg/17
reg_index_cs_totalchg <- reg_index_cs_totalchg/34

# Change the specific cell values to these new results 
reg_df_cerr <- reg_df_cerr %>%
  mutate(chg = ifelse(region_abv == "Total" & variable == "Corn Exp Price index", reg_index_c_totalchg, chg)) %>%
  mutate(chg = ifelse(region_abv == "Total" & variable == "Soy Exp Price index", reg_index_s_totalchg, chg)) %>% 
  mutate(chg = ifelse(region_abv == "Total" & variable == "CornSoy index", reg_index_cs_totalchg, chg))

# arrange results
reg_df_cerr <- reg_df_cerr %>% 
  mutate(region_abv = factor(region_abv, levels = c("US", "Cerrado", "Brazil", "S. America (excl. Brazil)", "China", "EU", "Total"))) %>%
  mutate(crop = factor(crop, levels = c("Soy", "Corn", "CornSoy"))) %>% 
  arrange(crop, region_abv) %>% 
  mutate(modeltype = pct_model)

# replace "soy" with "soybean"
reg_df_cerr <- reg_df_cerr %>% 
  mutate(across(everything(), ~ str_replace_all(.x, "Soy", "Soybean"))) %>% 
  mutate(
    pct_chg = pct_chg %>% as.numeric(),
    chg = chg %>% as.numeric(),
    pre = pre %>% as.numeric(),
    post = post %>% as.numeric())

## 9.4) Calculate Outside of US Changes -----
# get Total - US values 
reg_df_cerr <-
  reg_df_cerr %>%
  # Keep only the pieces we need to compute (Total - US) within each crop/type/variable
  filter(region_abv %in% c("Total", "US")) %>%
  dplyr::select(crop, type, variable, Unit, modeltype, region_abv, pre, post) %>%
  pivot_wider(
    names_from = region_abv,
    values_from = c(pre, post)
  ) %>%
  mutate(
    region_abv = "Total (excl. US)",
    pre  = pre_Total - pre_US,
    post = post_Total - post_US,
    chg = post - pre,
    pct_chg = ((post-pre)/pre)*100
  ) %>%
  dplyr::select(region_abv, crop, type, variable, Unit, modeltype, pre, post, chg, pct_chg) %>%
  bind_rows(reg_df_cerr, .)

# manually change price results to divide by the number of regions (16 instead of 17 because we are not including US)
reg_df_cerr <- reg_df_cerr %>%
  mutate(
    chg = if_else(region_abv == "Total (excl. US)" & variable == "Soybean Exp Price index", chg / 16, chg),
    chg = if_else(region_abv == "Total (excl. US)" & variable == "Corn Exp Price index", chg / 16, chg),
    chg = if_else(region_abv == "Total (excl. US)" & variable == "CornSoybean index", chg / 32, chg),
  )

## 9.5) Clean df before saving ------
# divide Area, Production, Exports, and Imports by 1000 to convert kha-->Mha and 1000-tons CE-->Mmt CE
# Note that we omit price here because the unit doesn't make sense to divide by 1000; we want USD/mt
reg_df_mha_mt <- reg_df_cerr %>% 
  mutate(
    across(c(chg, pre, post),
           ~ if_else(type != "index", . / 1000, .))) %>% 
  mutate(
    Unit = case_when(
      type == "Area" ~ "Mha",
      type == "Production" ~ "Mmt CE",
      type %in% c("Imp", "Exp") ~ "Mmt",
      type == "index" ~ "USD/mt",
      TRUE ~ NA_character_
    ))

# rearrange to a consistent order 
reg_df_mha_mt <- reg_df_mha_mt %>%
  mutate(
    crop = factor(crop, levels = c("Soybean", "Corn", "CornSoybean")),
    region_abv = factor(region_abv, levels = c(
      "US", "Cerrado", "Brazil", "S. America (excl. Brazil)",
      "China", "EU", "Total (excl. US)", "Total"
    )),
    type = factor(type, levels = c("Area", "Production", "index", "Exp", "Imp"))
  ) %>%
  arrange(crop, region_abv, type)

# last, replace index with Index
reg_df_mha_mt <- reg_df_mha_mt %>%
  mutate(across(everything(), ~ str_replace_all(.x, "index", "Index"))) %>% 
  mutate(
    pct_chg = pct_chg %>% as.numeric(),
    chg = chg %>% as.numeric(),
    pre = pre %>% as.numeric(),
    post = post %>% as.numeric())


## 9.6) Save Regional Table for Cascading Effects ------
## SAVE TABLE - REGIONAL RESULTS ##
write.xlsx(reg_df_mha_mt, paste0(folder_results, "_regional_aggregate", pct, ".xlsx"), overwrite = TRUE)

# filter just to soy area & production
reg_df_mha_mt_s <-  reg_df_mha_mt %>% 
  filter(grepl('Soybean', variable, fixed = T)) %>% 
  filter(!grepl('CornSoybean', variable, fixed = T)) %>% 
  arrange(variable) %>% 
  filter(type %in% c("Area", "Production"))

## SAVE TABLE - REGIONAL SOY RESULTS ##
write.xlsx(reg_df_mha_mt_s, paste0(folder_results, "_regional_aggregate_soy", pct, ".xlsx"), overwrite = TRUE)

# END #################