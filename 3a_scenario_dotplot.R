# name: 3_scenario_dotplot.R

# purpose: 
## create dataframe with the Cerrado soybean area xpansion across all scenarios
## create dot plot showing the % change in each of the cascading effects across the scenarios

# author: Nick Manning

# date created: October 2025

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm(list = ls())

# 0) Load Libraries ----------
library(readxl)
library(dplyr)
library(ggplot2)

# 0) Define File Paths -------
# Define file paths
file_l <- "../Results/SIMPLEG/l/_regional_aggregate_l.xlsx"
file_m <- "../Results/SIMPLEG/m/_regional_aggregate_m.xlsx"
file_h <- "../Results/SIMPLEG/h/_regional_aggregate_h.xlsx"

file_results <- "../Results/SIMPLEG/"

# 1) Load & Clean Data -------

## 1.1) Get Cerrado Scenario Results --------
# NOTE: Data is created in "processResults2_SIMPLEG.R" script in Section 8.3.4: Save Regional Table for Cascading Effects 

# Read in data
df_l <- read_excel(file_l) 
df_m <- read_excel(file_m) #%>% mutate(modeltype = "m")
df_h <- read_excel(file_h) #%>% mutate(modeltype = "h")

# Change character rows to numeric across all dfs
F_to_num <- function(df){
  df <- df %>% 
    mutate(
      pct_chg = pct_chg %>% as.numeric(),
      chg = chg %>% as.numeric(),
      pre = pre %>% as.numeric(),
      post = post %>% as.numeric())
  return(df)
}

df_l <- F_to_num(df_l)
df_m <- F_to_num(df_m)
df_h <- F_to_num(df_h)

# Combine all scenarios
df_all <- bind_rows(df_l, df_m, df_h)

# replace "soy" with "soybean"
df_all <- df_all %>% 
  mutate(across(where(is.character),
                ~ str_replace_all(.x, regex("(?i)Soy(?!bean)"), "Soybean")))

# filter to just the values from the cascading effects figure
df <- df_all %>% 
  filter(
    variable == "Soybean Area" & region_abv == "US" |
      variable == "Soybean Production" & region_abv == "US" |
      variable == "Soybean Exp Price index" & region_abv == "US" |
      variable == "Soybean Exp" & region_abv == "US" |
      variable == "Soybean Exp Price index" & region_abv == "Total" |
      variable == "Soybean Production" & region_abv == "Brazil" |
      variable == "Soybean Area" & region_abv == "Brazil" |
      variable == "Soybean Area" & region_abv == "Cerrado" 
  )

### Save All Scenario Data #####
df_cerr_soyarea_lmh <- df %>% 
  filter(region_abv == "Cerrado") 
  
  
save(df_cerr_soyarea_lmh, file = paste0(file_results, "lmh_cerrado_soyarea.Rdata"))


## 1.2) Format data for plot -----
# Pivot to wide format for plotting
df_wide <- df %>%
  dplyr::select(region_abv, variable, modeltype, pct_chg) %>%
  tidyr::pivot_wider(names_from = modeltype, values_from = pct_chg)

# Create a label for y-axis
df_wide <- df_wide %>%
  mutate(label = paste(region_abv, variable, sep = " - ")) 

# re-name for more clear labeling
df_wide <- df_wide %>% 
  mutate(label = case_when(
    label == "US - Soybean Area" ~ "US Soybean Area",
    label == "US - Soybean Production" ~ "US Soybean Production",
    label == "US - Soybean Exp" ~ "US Soybean Exports",
    label == "US - Soybean Exp Price index" ~ "US Soybean Prices",
    label == "Total - Soybean Exp Price index" ~ "Global Soybean Prices",
    label == "Brazil - Soybean Area" ~ "Brazil Soybean Area",
    label == "Brazil - Soybean Production" ~ "Brazil Soybean Production",
    label == "Cerrado - Soybean Area" ~ "Cerrado Soybean Area",
    TRUE ~ label
  ))

# set as factor for easy re-ordering
df_wide <- df_wide %>% 
  mutate(label = factor(label, levels =
                          c("US Soybean Area", "US Soybean Production",
                            "US Soybean Exports", "US Soybean Prices",
                            "Global Soybean Prices",
                            "Brazil Soybean Area", "Brazil Soybean Production",
                            "Cerrado Soybean Area")))

# 2) Plot & Save --------
# Plot
ggplot(df_wide, aes(y = label)) +
  #geom_errorbar(aes(xmax = h, xmin = l))+
  #geom_segment(aes(x = l, xend = m, yend = label), color = "#C198E0", linetype = "dotted") +
  geom_segment(aes(x = l, xend = m, yend = label), color = "darkorchid2", linetype = "dotted", linewidth = 1) +
  geom_segment(aes(x = h, xend = m, yend = label), color = "chocolate2", linetype = "dashed", linewidth = 0.8) +
  geom_point(aes(x = m), color = "black", size = 2) +
  scale_y_discrete(limits = rev(levels(df_wide$label)))+
  labs(
    #x = "Percent Change",
    x = "",
    #y = "Region - Variable",
    y = "",
    title = "Percent Change across Low, Medium, and High Elasticity Scenarios"
  ) +
  theme_minimal()+
  theme(legend.position = "bottom")+
  theme(plot.title = element_text(hjust = 0.5))

# save
ggsave(filename = "../Figures/_dotplot_scenario.png", dpi = 300,
       height = 3.5, width = 8)
