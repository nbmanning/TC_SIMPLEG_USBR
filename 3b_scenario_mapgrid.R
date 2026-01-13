# name: 3b_scenario_mapgrid.R

# purpose: create plot of us, cerrado, brazil l,m,h scenarios

# author: Nick Manning

# date created: October 2025

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list=ls())

# 0) Load Libraries -----
library(ggplot2)
library(terra)
library(tidyverse)
library(gridExtra)
library(tidyterra)

# 0) Load Shapefiles -----

# NOTE: these come from prior script "1_processResults.R"
load("../Data_Derived/shp_usbr.RData")

# 0) Set Data & Figures folder ---------
folder_data <- "../Data_Derived/"
folder_fig <- "../Figures/"

# 1) Load Clipped Result Rasters -------
l_cerrado <- rast(read_rds(paste0(folder_data, "r_l_Cerrado.rds")))
m_cerrado <- rast(read_rds(paste0(folder_data, "r_m_Cerrado.rds")))
h_cerrado <- rast(read_rds(paste0(folder_data, "r_h_Cerrado.rds")))

l_brazil <- rast(read_rds(paste0(folder_data, "r_l_Brazil.rds")))
m_brazil <- rast(read_rds(paste0(folder_data, "r_m_Brazil.rds")))
h_brazil <- rast(read_rds(paste0(folder_data, "r_h_Brazil.rds")))

l_us <- rast(read_rds(paste0(folder_data, "r_l_US.rds")))
m_us <- rast(read_rds(paste0(folder_data, "r_m_US.rds")))
h_us <- rast(read_rds(paste0(folder_data, "r_h_US.rds")))

# 2) Plot Facets Individually then Combine ----------

## USA --------------------
# Define breaks and labels
breaks <- c(-3, -2, -1, -0.1, -0.01, 0.01, 0.05, 0.1, 0.25, 0.5)
labels <- paste(head(breaks, -1), tail(breaks, -1), sep = " to ")

# Helper function to convert raster to tidy data frame
raster_to_df <- function(raster, varname, scenario_label) {
  # Cut values into categories
  cats <- cut(values(raster[[varname]]),
              breaks = breaks,
              labels = labels,
              include.lowest = TRUE,
              right = FALSE)
  
  # Add categorized values to raster
  raster$cats <- cats
  
  # Convert to data frame
  df <- as.data.frame(raster, xy = TRUE, na.rm = FALSE)
  
  # Keep only coordinates and 'cats'
  df <- df %>%
    dplyr::select(x, y, cats) %>%
    mutate(scenario = scenario_label)
  
  return(df)
}

# Prepare data frames for each scenario
df_l_us <- raster_to_df(l_us %>% subset("rawch_SOY"), 
                        varname = "rawch_SOY", 
                        scenario_label = "Low")

df_m_us <- raster_to_df(m_us %>% subset("rawch_SOY"), "rawch_SOY", "Medium")
df_h_us <- raster_to_df(h_us %>% subset("rawch_SOY"), "rawch_SOY", "High")

# Combine all into one data frame
df_all_us <- bind_rows(df_l_us, df_m_us, df_h_us) %>%
  mutate(scenario = factor(scenario, levels = c("Low", "Medium", "High")))

# Plot faceted map
p_facet_us <- ggplot(df_all_us) +
  geom_tile(aes(x = x, y = y, fill = cats), color = NA) +
  scale_fill_whitebox_d(
    palette = "pi_y_g",
    direction = 1,
    na.translate = FALSE,
    drop = F
  ) +
  facet_wrap(~ scenario) +
  geom_sf(data = vect(shp_us), color = "gray30", fill = NA, lwd = 0.2, inherit.aes = FALSE) +
  coord_sf(crs = "EPSG:4326") +
  theme_minimal() +
  labs(
    fill = "Area (kha)",
    title = "Change in Soybean Cropland Area by Scenario",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 18),
    axis.text = element_blank()
  )

# visualize plot
p_facet_us

## Brazil-------------

# Set Breaks & Labels -- will be the same for Brazil & Cerrado
breaks <- c(0, 0.01, 0.1, 0.2, 0.3, 0.4, 0.5)
labels <- paste(head(breaks, -1), tail(breaks, -1), sep = " to ")

# Example for Soybean cropland change
df_l_br <- raster_to_df(l_brazil %>% subset("rawch_SOY"), "rawch_SOY", "Low")
df_m_br <- raster_to_df(m_brazil %>% subset("rawch_SOY"), "rawch_SOY", "Medium")
df_h_br <- raster_to_df(h_brazil %>% subset("rawch_SOY"), "rawch_SOY", "High")

# Combine all scenarios
df_all_br <- bind_rows(df_l_br, df_m_br, df_h_br)

df_all_br <- bind_rows(df_l_br, df_m_br, df_h_br) %>%
  mutate(scenario = factor(scenario, levels = c("Low", "Medium", "High"))) # re-order so it's not alphabetical

# plot
p_facet_br <- ggplot(df_all_br) +
  geom_tile(aes(x = x, y = y, fill = cats), color = NA) +
  scale_fill_whitebox_d(
    palette = "gn_yl",
    direction = 1,
    na.translate = FALSE, 
    drop = F
  ) +
  facet_wrap(~ scenario) +
  geom_sf(data = vect(shp_br_border), color = "gray30", fill = NA, lwd = 0.2, inherit.aes = FALSE) +
  geom_sf(data = shp_cerr, color = "black", fill = "transparent", lwd = 0.3)+
  coord_sf(crs = "EPSG:4326") +
  #coord_sf(crs = "EPSG:5880") + # brazil polyconic 
  theme_minimal() +
  labs(
    fill = "Area (kha)",
    #title = "Change in Brazil Soybean Cropland Area by Scenario",
    title = "",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14),
    #strip.text = element_text(size = 18),
    strip.text = element_blank(),
    axis.text = element_blank()
  )

# p_facet_br

## Cerrado ----------

# Filter cerrado scenarios
df_l_cerr <- raster_to_df(l_cerrado %>% subset("rawch_SOY"), "rawch_SOY", "Low")
df_m_cerr <- raster_to_df(m_cerrado %>% subset("rawch_SOY"), "rawch_SOY", "Medium")
df_h_cerr <- raster_to_df(h_cerrado %>% subset("rawch_SOY"), "rawch_SOY", "High")

# Combine all scenarios
df_all_cerr <- bind_rows(df_l_cerr, df_m_cerr, df_h_cerr) %>%
  mutate(scenario = factor(scenario, levels = c("Low", "Medium", "High")))

# plot
p_facet_cerr <- ggplot(df_all_cerr) +
  geom_tile(aes(x = x, y = y, fill = cats), color = NA) +
  scale_fill_whitebox_d(
    palette = "gn_yl",
    direction = 1,
    na.translate = FALSE, drop = F
  ) +
  facet_wrap(~ scenario) +
  geom_sf(data = vect(shp_cerr), color = "gray30", fill = NA, lwd = 0.2, inherit.aes = FALSE) +
  #geom_sf(data = shp_cerr_states, color = "gray50", fill = "transparent", lwd = 0.2) + 
  coord_sf(crs = "EPSG:4326") +
  #coord_sf(crs = "EPSG:5880") + # cerrado polyconic 
  theme_minimal() +
  labs(
    fill = "Area (kha)",
    #title = "Change in Cerrado Soybean Cropland Area by Scenario",
    title = "",
    x = "",
    y = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 30),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 14),
    #strip.text = element_text(size = 18),
    strip.text = element_blank(),
    axis.text = element_blank()
  )

# p_facet_cerr


## Combine Facets ----------
# combine facet plots using patchwork pacakage 
combined_all <- (p_facet_us / p_facet_br / p_facet_cerr) +
  plot_layout(guides = "keep", heights = c(1, 1, 1)) &
  theme(legend.position = "right")

combined_all

# save
ggsave(combined_all, filename = paste0(folder_fig, "_facet_scenario.png"), width = 20, height = 20, dpi = 300)