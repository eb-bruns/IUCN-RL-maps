### 0-set_working_directory.R
### Authors: Shannon M Still & Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, UC Davis Arboretum & Botanic Garden
### Funding: 
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
#   -- United States Botanic Garden (grant to The Morton Arboretum)
### Last Updated: October 2025; June 2023; first written Dec 2020
### R version 4.5.1

### DESCRIPTION:
  ## This script sets the working environment for the computer on which you are
  #     working. You need to add your own computer and file paths.
  ## This script also creates the initial folder structure used for the
  #     scripts in the spatial-analysis-workflow.

################################################################################
# Set working environment depending on your computer
################################################################################

# The following line gives your "nodename", which you will paste within the 
#   quotes in line 28
Sys.info()[4]


## E.g. for Emily Bruns (you need to update with your own paths)
if (Sys.info()[4] == "Ians-MacBook-Air.local") {
  # I like my main directory in Google Drive for easy sharing; to do this, 
  #   you first need to install "Drive for desktop", see here:
  #   https://support.google.com/a/users/answer/13022292?hl=en
  # It's also fine to keep everything local and not linked to the cloud
  main_dir <- "/Users/emilybruns/Library/CloudStorage/GoogleDrive-emily.b.bruns@gmail.com/My Drive/The Morton Arboretum/spatial_data_working"
  log_loc <- "/Users/emilybruns/Library/CloudStorage/GoogleDrive-emily.b.bruns@gmail.com/My Drive/The Morton Arboretum/spatial_data_working/passwords.txt"
  #gap_dir <- "/Users/emily/Documents/GitHub/GapAnalysis_EBB/R"
  print(paste("Working from the lovely", Sys.info()[4]))
  
} else {
  # default, which sets the working directory as the folder from which you
  #   opened the scripts/project
  setwd(getwd())
  print("You should add your info to the 0-set_working_directory.R script so
    this line automatically sets up all the working directories you'll be using!")
}

################################################################################
# Initialize main folders used in spatial-analysis-workflow
################################################################################

# assign the main folders you'll reference in multiple scripts, and create
#   them if they're not already present; you don't need to change anything here

# place your target_taxa_with_synonyms.csv file in this folder
taxa_dir <- "target_taxa"
if(!dir.exists(file.path(main_dir, taxa_dir)))
  dir.create(file.path(main_dir, taxa_dir), recursive=T)

# you will add layers to this folder in 1-prep_gis_layers.R
gis_dir <- "gis_layers"
if(!dir.exists(file.path(main_dir, gis_dir)))
  dir.create(file.path(main_dir, gis_dir), recursive=T)

# you will add folders and files here as you get, compile, refine, and visualize
#   occurrence data throughout the spatial-analysis-workflow sequence
occ_dir <- "occurrence_data"
if(!dir.exists(file.path(main_dir, occ_dir)))
  dir.create(file.path(main_dir, occ_dir), recursive=T)
raw_occ <- "raw_occurrence_data"
if(!dir.exists(file.path(main_dir, occ_dir, raw_occ)))
  dir.create(file.path(main_dir, occ_dir, raw_occ), recursive=T)
standardized_occ <- "standardized_occurrence_data"
if(!dir.exists(file.path(main_dir, occ_dir, standardized_occ)))
  dir.create(file.path(main_dir, occ_dir, standardized_occ), recursive=T)
