### 3-get_occurrence_data.R
### Authors: Emily Beckman Bruns & Shannon M Still
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden, UC Davis Arboretum & Botanic Garden
### Funding: 
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
#   -- United States Botanic Garden (cooperative agreement with San Diego
#        Botanic Garden & grant to The Morton Arboretum)
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: October 2025; June 2023; first written Feb 2020
### R version 4.5.1

### DESCRIPTION:
  ## This script provides instructions and code chunks for downloading and
  #   creating standard column names for wild occurrence points from:
  # 1- GLOBAL DATABASES:
  #     A) Global Biodiversity Information Facility (GBIF)
  #     B) Regional network of North American herbaria (SEINet Portal Network)
  ## Please note that not all data from these sources are reliable. The aim of 
  #   this script is to get easily-downloadable occurrence data, which
  #   can then be sorted and vetted for the user's specific purposes.

### INPUTS:
  ## (optional) target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym).
  ## You can also create this by hand if you have a short list.

### OUTPUTS:
  ## The raw data is downloaded to the corresponding folder in 
  #   occurrence_data > raw_occurrence_data, then a file with standardized 
  #   columns is saved to standardized_occurrence_data > input_datasets folder 
  #   and named like so:
  # A) gbif.csv
  # B) seinet.csv

################################################################################
# Load libraries
################################################################################

my.packages <- c('tidyverse','textclean','data.table','rgbif')
# install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
    rm(my.packages)

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script
  # change this path based on where the script is located on your computer:
source("/Users/emilybruns/Documents/GitHub/redlist_maps/spatial-analysis-workflow/0-set_working_directory.R")
    
# create folder for output data
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,"input_datasets")))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,"input_datasets"), 
             recursive=T)
data_out <- "input_datasets"
    
################################################################################
# Load or create target taxa list
################################################################################

# read in taxa list
taxon_list <- read.csv(file.path(main_dir,taxa_dir,"target_taxa_with_synonyms.csv"),
                       header=T, colClasses="character",na.strings=c("","NA"))
head(taxon_list); nrow(taxon_list)

### WE'RE JUST USING THE ACCEPTED ONES NOW!!!!
taxon_list <- taxon_list %>% filter(taxon_name == taxon_name_accepted)

# list of target taxon names
taxon_names <- sort(taxon_list$taxon_name)

# create list of only species names as well, to do initial removal of non-target 
#   taxa when downloading at the genus level; full name match in next script
target_sp_names <- unique(sapply(taxon_list$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1]))

### you can also create a target taxa list by hand instead...
  # include synonyms if you want to find them:
#taxon_names <- c("Asimina incana","Asimina longifolia","Asimina triloba")
  # if you have any infrataxa, just keep the genus and specific epithet here:
#target_sp_names <- c("Asimina incana","Asimina longifolia","Asimina triloba")

################################################################################
# Download & do basic standardization of occurrence data from each database
################################################################################

## Some sections have two options for downloading data: manually via
#  the website, or automatically using the API; choose whichever works for you


###
###############
###############################################
### A) Global Biodiversity Information Facility (GBIF)
###    https://www.gbif.org
###############################################
###############
###

# create new folder if not already present
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"GBIF")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"GBIF"), recursive=T)

###
### OPTION 1: automatic download via API
### (can go down to option 2 -manual download- if this isn't working)
###

# load GBIF account user information
  # if you don't have account yet, go to https://www.gbif.org then click
  # "Login" in top right corner, then click "Register"
# either read in a text file with username, password, and email (one on each
#   line) or manually fill in below (if you're not saving this script publicly):
login <- read_lines(log_loc)
  user  <- login[1] #username
  pwd   <- login[2] #password
  email <- login[3] #email

# get GBIF taxon keys for target species
keys <- sapply(taxon_names,function(x) name_backbone(name=x)$speciesKey,
              simplify = "array"); keys
# remove duplicate and NULL keys
keys_nodup <- keys[!duplicated(keys) & keys != "NULL"]
# create data frame of keys and matching taxon_name & species
gbif_codes <- map_df(keys_nodup,~as.data.frame(.x),.id="taxon_name")
names(gbif_codes)[2] <- "speciesKey"

# cycle through species by species so you get a unique DOI for each, which
#  you will need for the citation in the Red List assessment
for(i in 1:length(taxon_names)){
  gbif_taxon_key <- gbif_codes[i,2]
# download GBIF data (Darwin Core Archive format)
  user  <- login[1] #username
  pwd   <- login[2] #password
  email <- login[3] #email
  gbif_download <- occ_download(
                  pred_in("taxonKey", gbif_taxon_key),
                  format = "DWCA",
                  user=user,pwd=pwd,
                  email=email)
    rm(user, pwd, email)
  # load gbif data just downloaded
    # download and unzip before reading in
  download_key <- gbif_download
    # must wait for download to complete before continuing;
    # it may take a while (up to 3 hours) if your taxon has lots of points;
    # function below will pause script until the download is ready;
    # you can also log in on the GBIF website and go to your profile to see the
    #   progress of your download
  occ_download_wait(download_key, status_ping=10, quiet=TRUE)
    # get download when its ready then unzip and read in
  occ_download_get(key=download_key[1],
    path=file.path(main_dir,occ_dir,raw_occ,"GBIF"))
  unzip(zipfile=paste0(
    file.path(main_dir,occ_dir,raw_occ,"GBIF",download_key[1]),".zip"),
    files="occurrence.txt",
    exdir=file.path(main_dir,occ_dir,raw_occ,"GBIF"),
    overwrite=F)
  file.rename(
    from = file.path(main_dir,occ_dir,raw_occ,"GBIF","occurrence.txt"),
    to = file.path(main_dir,occ_dir,raw_occ,"GBIF", 
                   paste0(gsub(" ","_",gbif_codes[i,1]),"_occurrence.txt"))
  )
}

### STANDARDIZE THE DATA

# create file list of all downloaded data
file_list <- list.files(path = file.path(main_dir,occ_dir,raw_occ,"GBIF"), 
                        pattern = "occurrence", full.names = T)
length(file_list) #10

# loop through file(s)
for(i in 1:length(file_list)){
  
  # read in data
  gbif_raw <- fread(file_list[[i]], quote="", na.strings="")
  print(paste0("Total number of records: ",nrow(gbif_raw)))
  # remove any genus-level records
  gbif_raw <- gbif_raw %>% filter(taxonRank != "GENUS")
  
  # keep only necessary columns
  gbif_raw <- gbif_raw %>% select(
      # Taxon
    "scientificName","family","genus","specificEpithet","taxonRank",
    "infraspecificEpithet",
        # *concatenate into taxonIdentificationNotes
    "identificationRemarks","identificationVerificationStatus","identifiedBy",
    "taxonRemarks",
      # Event
    "year",
      # Record-level
    "basisOfRecord","gbifID","datasetName","publisher","institutionCode",
    "rightsHolder","license","references","informationWithheld","issue",
    "occurrenceID",
      # Occurrence
    "recordedBy","establishmentMeans",
      # Location
    "decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters",
    "county","municipality","stateProvince","higherGeography","countryCode",
        # *concatenate into golocationNotes
    "georeferencedBy","georeferencedDate","georeferenceProtocol",
    "georeferenceRemarks","georeferenceSources",
    "georeferenceVerificationStatus",
        # *concatenate into locationNotes
    "locality","verbatimLocality","associatedTaxa","eventRemarks","fieldNotes",
    "habitat","locationRemarks","occurrenceRemarks","occurrenceStatus"
  ) %>% rename(nativeDatabaseID = gbifID)
  
  # add database column
  gbif_raw$database <- "GBIF"
  
  # combine a few similar columns
  gbif_raw <- gbif_raw %>% unite("taxonIdentificationNotes",
      identificationRemarks:taxonRemarks,na.rm=T,remove=T,sep=" | ")
    gbif_raw$taxonIdentificationNotes <-
      gsub("^$",NA,gbif_raw$taxonIdentificationNotes)
  gbif_raw <- gbif_raw %>% unite("locationNotes",
    associatedTaxa:occurrenceStatus,na.rm=T,remove=T,sep=" | ")
    gbif_raw$locationNotes <- gsub("^$",NA,gbif_raw$locationNotes)
  gbif_raw <- gbif_raw %>% unite("geolocationNotes",
    georeferencedBy:georeferenceVerificationStatus,na.rm=T,remove=T,sep=" | ")
    gbif_raw$geolocationNotes <- gsub("^$",NA,gbif_raw$geolocationNotes)
  
  # create taxon_name column
  gbif_raw$taxon_name <- NA
  subsp <- gbif_raw %>% filter(taxonRank == "SUBSPECIES")
    tryCatch(subsp$taxon_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",
      subsp$infraspecificEpithet), error = \(e) message("No subspecies"))
  var <- gbif_raw %>% filter(taxonRank == "VARIETY")
    tryCatch(var$taxon_name <- paste(var$genus,var$specificEpithet,"var.",
      var$infraspecificEpithet), error = \(e) message("No varieties"))
  form <- gbif_raw %>% filter(taxonRank == "FORM")
    tryCatch(form$taxon_name <- paste(form$genus,form$specificEpithet,"f.",
      form$infraspecificEpithet), error = \(e) message("No forms"))
  spp <- gbif_raw %>% filter(taxonRank == "SPECIES")
    spp$taxon_name <- paste(spp$genus,spp$specificEpithet)
  gbif_raw <- Reduce(rbind,list(subsp,var,form,spp))
    rm(subsp,var,form,spp)
  #print(sort(unique(gbif_raw$taxon_name)))
  
  # create species_name column
  gbif_raw$species_name <- NA
  gbif_raw$species_name <- sapply(gbif_raw$taxon_name, function(x)
    unlist(strsplit(x," var. | subsp. | f. "))[1])
  
  # keep only target species
  gbif_raw2 <- gbif_raw %>%
    filter(species_name %in% target_sp_names)
  print(paste0("Number of records for target taxa: ",nrow(gbif_raw)))
  print("Species removed; if you want to keep any of these, add them to your target taxa list...")
  print(unique(setdiff(gbif_raw,gbif_raw2)[,"taxon_name"]))
  
  # check establishmentMeans and recode if needed
    # check and add below as needed
  gbif_raw2$establishmentMeans <- as.character(gbif_raw2$establishmentMeans)
  print(sort(unique(gbif_raw2$establishmentMeans)))
  gbif_raw2 <- gbif_raw2 %>%
    mutate(establishmentMeans = recode(establishmentMeans,
      "Uncertain" = "UNCERTAIN",
      "Native" = "NATIVE",
      "Introduced" = "INTRODUCED"))
  
  # write file
  write.csv(gbif_raw2, file.path(main_dir,occ_dir,standardized_occ,data_out,
    paste0("gbif",i,".csv")),row.names=FALSE)
  rm(gbif_raw); rm(gbif_raw2)
  
}



###
###############
###############################################
# B) Regional network of North American herbaria (SEINet Portal Network)
#    https://symbiota.org/seinet/
###############################################
###############
###

# create new folder if not already present
if(!dir.exists(file.path(main_dir,occ_dir,raw_occ,"NorthAm_herbaria")))
  dir.create(file.path(main_dir,occ_dir,raw_occ,"NorthAm_herbaria"), recursive=T)

## First, download raw data:
# Go to https://swbiodiversity.org/seinet/collections/harvestparams.php

###### !! FOR THE CALIFORNIA SPEICES I'M ALSO USING 
######    CONSORTIUM OF CALIFORNIA HERBARIA:
######  https://www.cch2.org/portal/collections/search/index.php
###### It looks like it has some extra records compared to SEINet

# We will now download data for each target genus individually (if you have
#   more than one); alternatively, if you are just looking for a few
#   taxa you can search for and download each taxon individually!
# Type your target genus name into the "Scientific Name" box and click
#   "List Display"
# Click the Download Specimen Data button (grey square with arrow pointing
#   down into a box) in the top right corner.
# IMPORTANT: In the pop-up window, select the "Darwin Core" radio button,
#   un-check everything in the "Data Extensions" section, and
#   select the "UTF-8 (unicode)" radio button; leave other fields as-is.
# Click "Download Data"
# If you have more than one target genus, repeat the above steps for the
#   other genera.
# Move all the folders you downloaded into the folder
#   occurrence data > raw_occurrence_data > NorthAm_herbaria

# read in data
# this code pulls the "occurrences.csv" from each genus folder, for compilation
file_list <- list.files(file.path(main_dir,occ_dir,raw_occ,"NorthAm_herbaria"),
                        pattern = "SymbOutput", full.names = T)
file_dfs <- lapply(file_list, function(i){
  read.csv(file.path(i,"occurrences.csv"), colClasses = "character",
           na.strings=c("", "NA"), strip.white=T, fileEncoding="UTF-8")})
seinet_raw <- data.frame()
for(file in seq_along(file_dfs)){
  seinet_raw <- rbind(seinet_raw, file_dfs[[file]])
}; nrow(seinet_raw)

# remove genus-level records
seinet_raw <- seinet_raw %>% filter(taxonRank != "Genus")

# create taxon_name column
  ## check out taxon rank for "Morph" species -- looks like it's "var."
  unique(seinet_raw[seinet_raw$taxonRank == "Morph",16])
# this method is not perfect; the taxonRank isn't always categorized correctly
subsp <- seinet_raw %>% filter(taxonRank == "Subspecies")
tryCatch(subsp$taxon_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",
                          subsp$infraspecificEpithet), error = \(e) message("No subspecies"))
var <- seinet_raw %>% filter(taxonRank == "Variety" | taxonRank == "Morph")
tryCatch(var$taxon_name <- paste(var$genus,var$specificEpithet,"var.",
                        var$infraspecificEpithet), error = \(e) message("No varieties"))
form <- seinet_raw %>% filter(taxonRank == "Form")
tryCatch(form$taxon_name <- paste(form$genus,form$specificEpithet,"f.",
                         form$infraspecificEpithet), error = \(e) message("No forms"))
spp <- seinet_raw %>% filter(is.na(taxonRank) | taxonRank == "Species" |
                               taxonRank == "Subform")
spp$taxon_name <- paste(spp$genus,spp$specificEpithet)
seinet_raw <- Reduce(bind_rows,list(subsp,var,form,spp))
seinet_raw$taxon_name[which(is.na(seinet_raw$taxon_name))] <-
  seinet_raw$scientificName[which(is.na(seinet_raw$taxon_name))]
# check out taxon names:
sort(unique(seinet_raw$taxon_name))
# sometimes there are strange characters we need to replace:
#seinet_raw$taxon_name <- gsub("Ã\u0097","",seinet_raw$taxon_name)
# keep only necessary columns & rename to fit standard
seinet_raw <- seinet_raw %>% select(
  "taxon_name","family","genus","specificEpithet","taxonRank",
  "infraspecificEpithet","scientificName","identificationRemarks",
  "identifiedBy","taxonRemarks","decimalLatitude","decimalLongitude",
  "coordinateUncertaintyInMeters","basisOfRecord","year","id",
  "references","occurrenceID","locality","county","municipality","stateProvince",
  "country","associatedTaxa","habitat","locationRemarks",
  "occurrenceRemarks","georeferencedBy","georeferenceProtocol",
  "georeferenceRemarks","georeferenceSources",
  "georeferenceVerificationStatus","institutionCode",
  "rightsHolder","rights","recordedBy","individualCount",
  "establishmentMeans","informationWithheld")
seinet_raw <- seinet_raw %>% rename(nativeDatabaseID = id)
seinet_raw <- seinet_raw %>% rename(license = rights)

# add database column & datasetName column
seinet_raw$database <- "NorthAm_herbaria"
seinet_raw$datasetName <- seinet_raw$institutionCode

# combine a few similar columns
seinet_raw <- seinet_raw %>% unite("taxonIdentificationNotes",
                                   identificationRemarks:taxonRemarks,na.rm=T,remove=T,sep=" | ")
seinet_raw$taxonIdentificationNotes <-
  gsub("^$",NA,seinet_raw$taxonIdentificationNotes)
seinet_raw <- seinet_raw %>% unite("locationNotes",
                                   associatedTaxa:occurrenceRemarks,na.rm=T,remove=T,sep=" | ")
seinet_raw$locationNotes <- gsub("^$",NA,seinet_raw$locationNotes)
seinet_raw <- seinet_raw %>% unite("geolocationNotes",
                                   georeferencedBy:georeferenceVerificationStatus,na.rm=T,remove=T,sep=" | ")
seinet_raw$geolocationNotes <- gsub("^$",NA,seinet_raw$geolocationNotes)

# create species_name column
seinet_raw$species_name <- NA
seinet_raw$species_name <- sapply(seinet_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])

# keep only target species
seinet_raw2 <- seinet_raw %>%
  filter(species_name %in% target_sp_names)
print("Species removed; if you want to keep any of these, add them to your target taxa list...")
unique(setdiff(seinet_raw,seinet_raw2)[,"taxon_name"])
seinet_raw <- seinet_raw2; rm(seinet_raw2)

# check a few standards and recode if needed
# basisOfRecord
seinet_raw$basisOfRecord <- str_to_upper(seinet_raw$basisOfRecord)
seinet_raw$basisOfRecord <- gsub(" ","_",seinet_raw$basisOfRecord)
unique(seinet_raw$basisOfRecord) # check and add below as needed
seinet_raw <- seinet_raw %>%
  mutate(basisOfRecord = recode(basisOfRecord,
                                "PRESERVEDSPECIMEN" = "PRESERVED_SPECIMEN",
                                "PHYSICALSPECIMEN" = "PHYSICAL_SPECIMEN",
                                "ESPÉCIMEN_PRESERVADO" = "PRESERVED_SPECIMEN",
                                "EJEMPLAR_HERBORIZADO" = "PRESERVED_SPECIMEN",
                                "LIVINGSPECIMEN" = "LIVING_SPECIMEN",
                                "HUMANOBSERVATION" = "HUMAN_OBSERVATION",
                                .missing = "UNKNOWN"))
# establishmentMeans
seinet_raw$establishmentMeans <- str_to_upper(seinet_raw$establishmentMeans)
seinet_raw$establishmentMeans <- gsub("\\.","",seinet_raw$establishmentMeans)
sort(unique(seinet_raw$establishmentMeans))
seinet_raw <- seinet_raw %>%
  mutate(establishmentMeans = recode(establishmentMeans,
                                     "NATIVE" = "NATIVE",
                                     "INTRODUCED" = "INTRODUCED",
                                     "UNCERTAIN" = "UNCERTAIN",
                                     "ALIEN" = "INTRODUCED",
                                     "CLONAL" = "UNCERTAIN",
                                     "WILD" = "NATIVE",
                                     "NATURALIZED" = "INTRODUCED",
                                     "ESCAPE FROM CULTIVATION" = "INTRODUCED",
                                     "ESTABLISHED NON-NATIVE" = "INTRODUCED",
                                     "INTRODUCED; VOLUNTEER" = "INTRODUCED",
                                     "NATIVE/NATURALIZING" = "NATIVE",
                                     "NATURALIZED" = "INTRODUCED",
                                     "NON-NATIVE" = "INTRODUCED",
                                     "NONNATIVE" = "INTRODUCED",
                                     "WILD CAUGHT" = "NATIVE",
                                     "VOLUNTEER" = "UNCERTAIN",
                                     "WILD COLLECTION" = "NATIVE",
                                     "WILD: NATIVE/NATURALIZED" = "NATIVE",
                                     .default = "CULTIVATED",
                                     .missing = "UNCERTAIN"))

# write file
write.csv(seinet_raw, file.path(main_dir,occ_dir,standardized_occ,data_out,
                                "seinet.csv"), row.names=FALSE)
rm(seinet_raw)


