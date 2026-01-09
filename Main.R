
## Main R script, which runs the analysis 
## Jules Selles 
## Scripts adapted from CCAMLR trend analysis:
## https://github.com/CCAMLR-Science/Trend_Analysis
## Update : 12 2023
## Update by Félix Massiot-Granier (09/01/2026)
## Github: XXXX



# Setup -------------------------------------------------------------------


# Remove SIOFA .xlsx catch/effort and biological data from ./Data/ folder
rm(list=ls())

# Use project Root
library('here')
setwd(here::here())

#Timestamp for Outpus
Time=format(Sys.time() ,"%d-%b-%Y")



# packages ----------------------------------------------------------------

# Load required package, and install it if not 
list.of.packages <- c('ggplot2','MAPPOEPA', 'CCAMLRGIS','terra','geosphere','rnaturalearth','ggtext','oceanmap',# plot and spatial object
                      'dplyr','rotations', # data manipulation # removed 
                      'readxl', #read xlsx
                      'lubridate', # manipulate date 
                      'BERT', # ccamlr trend analysis
                      'lm.beta',#regression
                      'DiagrammeR' #diagram 
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, type = 'binary')
lapply(list.of.packages, require, character.only = TRUE)



# Sources -----------------------------------------------------------------



# ggplot theme and plots
source('Functions/ggplot2/SimpleTheme_ggplot2.R')
source("Functions/PlotVar.R")

# Install BERT
# devtools::install_github("ccamlr/BERT", build_vignettes = TRUE)
## if errors show up try reinstalling BERT, but only ONCE

# BERT modified functions 
source('Functions/CPUE_seabed_rev.R')
source("Functions/est_fish_weight_rev.R")
source("Functions/assign_areas_rev.R")
source("Functions/extract_catch_data_tag_est_rev.R")
source("Functions/SIOFA_Season.R")

# Other functions
source('Functions/Utils/cut_borders.R')

# Fishable areas ------------------------------------------------------------
update.fishableArea=FALSE

if(update.fishableArea==TRUE){
  source('Fishable_area/RefArea_Shapefiles.R')
  source('Fishable_area/FishableArea_2025.R')
}

fishable_area = read.csv('Data/FishableArea2025.csv')

# Main parameters -----------------------------------------------------------

# Season of estimation
Est_Season = 2024
Min_Season = Est_Season-4

# Reference areas
RefArea=c("HIMI","CI")
RefArea.selected <- "CI"
# RefArea.selected=c('HIMI')

# Set biomass and CV for Reference Areas
HIMI_biomass_est=23485  #31111 
HIMI_CV_biomass_est=0.0435  #0.0281

CI_biomass_est=35740 # 2025 Stock Assessmment ( not published) 
CI_CV_biomass_est=0.0581 

# set cpue calculation - if 'dist' kg/km line either kg/ham  # WARNING !! FMG

CPUE_mod = 'dist' #'dist' 
if (CPUE_mod=='dist'){
  HIMI_cpue_est= 146.26 #159.966 #ccamlr secretariat value last 3 years average #WG-SAM-2025/06,
  HIMI_CV_cpue_est=0.1
  CI_cpue_est=193 #Include depredation mean = 32% in the last 3 years
  CI_CV_cpue_est=0.1
  # CI_CV_cpue_est=0.5 # To calculate
  
} else{
  
  HIMI_cpue_est=   #0.216 #ccamlr statistical bulletin last 3 years average  
  HIMI_CV_cpue_est=0.1
  #CI_cpue_est=0.245
  #CI_CV_cpue_est=0 #0.1
}

# Chapmann biomass estimates: minimum recaptures per year threshold
n_min_chapman=3

# Moving average up to 3 years for CPUE or not 
Tail<-FALSE

#Harvest rate used for the trend decision 
HarvestRateTrend = 0.04 #Other harvest rate

#List MU in the proper order
Mu=c("DC", "SIR")

#Set number of bootstrap iterations
n_boot=10000

#Chose whether to output data extracts ("Y") or not ("N")
Output="Y" # "Y" or "N"



# Run analysis ------------------------------------------------------------

## 1. Load data ------------------------------------------------------------------
source("01_LoadData.R")

## 2. Estimate Biomass -----------------------------------------------------------
source("02_EstimateBiomass.R")

## 3. Trend analysis -------------------------------------------------------------
source("03_AnalyseTrend.R")

## 4. Inspect tagging data -------------------------------------------------------
source("04_TaggingData.R")
