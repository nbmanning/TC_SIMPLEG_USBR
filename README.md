# README file for Telecoupling & SIMPLE-G Code Repository

Title: README.md

Purpose: Read-Me file for the SIMPLE-G US-BR telecoupling project. Below is the set-up file structure and data provided to run this code. We are provide the source data, file structure, and SIMPLE-G results in a Zenodo database here. 

Author: Nick Manning & Iman Haqiqi

Created on: July 23,2025

Last Edited: January 13, 2026 


## Notes

- 'Main' simply indicates whatever you decide to call your main file. Here it is provided as 'TC_SIMPLEG_USBR' for a clean folder containing the bare minimum to run the SIMPLE-G + Telecoupling US/Brazil Code.
- The Completed code (Sections 0, 1, 2, and 3) are for the upcoming manuscript Manning et al., 2026


## File Structure to Start:

If downloading locally, this is what should be manually created to make the process run smoothly. 
This is also the file structure that is uploaded and available from Zenodo. 

- Main/Code/
- Main/Data_Derived/
- Main/Data_Source/
- Main/Data_Source/MapBiomas/
- Main/Data_Source/SPAM2010/
- Main/Figures/
- Main/Results/
- Main/Results/SIMPLEG/
- Main/US_Drought_Shock/


## Data Needed to Start: 

### SPAM2010 DATA 
Data from the International Food Policy Research Institute's Spatial Production Allocation Model for 2010 (SPAM2010) are provided.  
Link: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PRFF8V
More info: https://www.mapspam.info/methodology/

All files have standard names, which allow direct identification of variable and technology:
spam201021r0_global_v_t.fff
where
v = variable 
t = technology
fff = format

_A_ means Physical Area, i.e. actual area of cropland plowed and where the crop is grown (in hectares)
_H_ means Harvested Area, i.e. at least as large as A but sometimes larger if the cropland is harvested more than once a year (in hectares)
_P_ means total production, i.e. how much crop is grown (in metric tons (1000 kg))

_A.tif means these rasters are the sum of all technologies (irrigated & rainfed) 

- Main/Data_Source/SPAM2010/spam2010V2r0_global_A_MAIZ_A.tif
- Main/Data_Source/SPAM2010/spam2010V2r0_global_A_SOYB_A.tif
- Main/Data_Source/SPAM2010/spam2010V2r0_global_H_MAIZ_A.tif
- Main/Data_Source/SPAM2010/spam2010V2r0_global_H_SOYB_A.tif
- Main/Data_Source/SPAM2010/spam2010V2r0_global_P_MAIZ_A.tif
- Main/Data_Source/SPAM2010/spam2010V2r0_global_P_SOYB_A.tif


### SHOCK CALCULATION 
The files needed to run US_Drought_Shock.R, which is where we generate the .txt and .har files to use as an input to SIMPLE-G. Files are in alphabetical order with folders at the end.

- Main/US_Drought_Shock/AGLAND_CROP_INSURANCE_ACRES_FIPS.csv = the USDA-NASS Census of Ag data on Insured Acres
- Main/US_Drought_Shock/AGLAND_CROPLAND_ACRES.csv = the USDA-NASS Census of Ag data on Total Cropland Acres
- Main/US_Drought_Shock/Codes.csv = the column labels used to merge with colsom12.txt to re-name these columns
- Main/US_Drought_Shock/Coords.csv = US FIPS codes with the x/y coordinates nad grid-cell IDs per code, along with the 2010 value and quantity of cropland per grid-cell. Since there can be multiple grid-cells within one FIPS code, this is necessary. 
- Main/US_Drought_Shock/CROP_TOTALS_SALES_USD_FIPS.csv = the USDA-NASS Census of Ag data on Total Crop Sales
- Main/US_Drought_Shock/grid_id_xyg.tif = ID's for each grid-cell used in a SIMPLE-G Modeling run 
- Main/US_Drought_Shock/new_loss_rate.csv = the crop loss rates per FIPS code based on the way we are calculating loss (see Methods)

- Main/US_Drought_Shock/cb_2018_us_state_500k/ = Cartographic Boundaries of the states in the USA from the US Census Bureau
- Main/US_Drought_Shock/colsom_2012/colsom12.txt = the USDA-RMA Causes of Loss file which shows the amount of insured crop lost due to drought and heat in terms of indemnity payments (USD) made per county per crop.


### MAPBIOMAS DATA 
MapBiomas Coverage statistics by biomes, states and municipalities - Collection 8 v1
Details available from: https://brasil.mapbiomas.org/wp-content/uploads/sites/4/2023/08/ATBD-Collection-8-v1.docx.pdf

- Main/Data_Source/MapBiomas/SOURCE_transonly_col8_mapbiomas_municip.csv


### SIMPLE-G RESULTS 
The scenario results for varying substitution elasticities (Low (l), Medium (m), and High (h)) are provided here as .txt files. These elasticities are how flexible the world is to using things other than crops that we applied the shock to. We expect with more flexibility (h) there will be less cropland expansion.

- Main/Results/SIMPLEG/sg1x3x10_v2411_US_Heat_l-out 
- Main/Results/SIMPLEG/sg1x3x10_v2411_US_Heat_m-out
- Main/Results/SIMPLEG/sg1x3x10_v2411_US_Heat_h-out


The results in tabular form (unformatted) are provided as an Excel file

- Main/Results/SIMPLEG/regional_results.xlsx


## Flow Diagram

The following is an outline for how the data and code fit together:

- Blue "Figure" components are figures generated for the manuscript
- Scripts beginning with "0" are run prior to exporting the Shock file and running the simulation in SIMPLE-G


![Alt Text: Flow Diagram for Telecoupling & SIMPLE-G Project](../Figures/_FlowDiagram.png)