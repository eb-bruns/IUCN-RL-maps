### 5-flag_occurrence_points.R
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
### R version  4.5.1

### DESCRIPTION:
  ## This script flags potentially suspect points by adding a column for each 
  #   type of flag, where FALSE = flagged. 
  ## Much of the flagging is done through or inspired by the
  #   CoordinateCleaner package, which was created for "geographic cleaning
  #   of coordinates from biologic collections...Cleaning geographic coordinates
  #   by multiple empirical tests to flag potentially erroneous coordinates, 
  #   addressing issues common in biological collection databases."
  #   See the article here:
  #   https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13152
  # The script also removes duplicates across data sources by looking at 
  #   occurrence ID, reference ID, and lat-long + year.
  # The flagging columns are as follows:
  # .cen = within 500 meters of county or state centroid
  # .urb = within an urban area
  # .inst = within 100 meters of a biodiversity institution
  # .con = country listed in country column does not match lat-long country
  # .outl = outlier using CoordinateCleaner method = "quantile" and mltpl = 4
  # .nativectry = outside native country of occurrence
  # .yr1950 = recorded prior to 1950
  # .yr1980 = recorded prior to 1980
  # .yrna = no record year provided
  # .unc = coordinate uncertainty above your threshold (currently 1000 meters)
  # .elev = outside elevation range provided (currently skipping)
  # .rec = not current and/or native based on basisOfRecord and establishmentMeans
  # .inat = iNaturalist record
  # .spatdup = spatial duplicates based on lat-long rounded to 3 digits after decimal

### INPUTS:
  ## target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym).
  ## taxon_points_raw (folder)
  #   Occurrence data compiled in 4-compile_occurrence_points.R
  ## world_countries_10m.shp & urban_areas_50m.shp
  #   These shapefiles were created in 2-prep_gis_layers.R

### OUTPUTS:
  ## taxon_points_ready-to-vet (folder) 
  #   For each taxon in your target taxa list, a CSV of occurrence records with 
  #   newly-added flagging columns (e.g., Asimina_triloba.csv)
  ## occurrence_record_summary_YYYY_MM_DD.csv
  #   Add to the summary table created in 4-compile_occurrence_points.R: number of
  #   flagged records in each flag column

################################################################################
# Load libraries
################################################################################

my.packages <- c('tidyverse','textclean','CoordinateCleaner','tools','terra')
#install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# use 0-set_working_directory.R script:
  # change this path based on where the script is located on your computer:
source("/Users/emilybruns/Documents/GitHub/redlist_maps/spatial-analysis-workflow/0-set_working_directory.R")

# create folder for output data
data_out <- "taxon_points_ready-to-vet"
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,data_out)))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,data_out), 
             recursive=T)

# assign folder where you have input data (saved in 4-compile_occurrence_points.R)
data_in <- "taxon_points_raw"

################################################################################
# Read in data
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, taxa_dir,"target_taxa_with_synonyms.csv"),
                       header=T, colClasses="character",na.strings=c("","NA"))

# read in world countries layer created in 2-prep_gis_layers.R
world_polygons <- vect(file.path(main_dir,gis_dir,"world_countries_10m",
                             "world_countries_10m.shp"))

# read in urban areas layer created in 2-prep_gis_layers.R
urban_areas <- vect(file.path(main_dir,gis_dir,"urban_areas_50m",
                              "urban_areas_50m.shp"))

################################################################################
# Iterate through taxon files and flag potentially suspect points
################################################################################

## set threshold for coordinate uncertainty flag; anything greater than this 
##  value (meters) will be flagged
max_uncertainty <- 1000

# list of taxon files to iterate through
taxon_files <- list.files(path=file.path(main_dir,occ_dir,standardized_occ,data_in), 
                          ignore.case=FALSE, full.names=FALSE, recursive=TRUE)
target_taxa <- file_path_sans_ext(taxon_files)

# start a table to add summary of results for each species
summary_tbl <- data.frame(
  taxon_name_accepted = "start", 
  unflagged_pts = "start", 
  selected_pts = "start", 
  .cen = "start", 
  .urb = "start",
  .inst = "start",
  .con = "start", 
  .outl = "start", 
  .nativectry = "start", 
  .yr1950 = "start", 
  .yr1980 = "start",
  .yrna = "start",
  .unc = "start",
  #.elev = "start",
  .rec = "start",
  .inat = "start",
  .spatdup = "start",
    stringsAsFactors=F)


## iterate through each species file to flag suspect points...

for (i in 1:length(target_taxa)){

  taxon_file <- target_taxa[i]
  taxon_nm <- mgsub(taxon_file, c("_","_var_","_subsp_"), 
                                c(" "," var. "," subsp. "))

  # bring in records
  taxon_now <- read.csv(file.path(main_dir,occ_dir,standardized_occ,data_in,
    paste0(taxon_file, ".csv")))
  # print the taxon name we're working with
  cat("-----\n","Starting ", taxon_nm, ", taxon ", i, " of ", length(target_taxa), ".\n", sep="")
  cat("Number of records: ",nrow(taxon_now),"\n",sep="")
  
  
  # now we will go through a set of tests to flag potentially suspect records...

  
  ### FLAG RECORDS THAT HAVE COORDINATES NEAR COUNTRY AND STATE/PROVINCE CENTROIDS
  flag_cen <- CoordinateCleaner::cc_cen(
    taxon_now,
    lon = "decimalLongitude", lat = "decimalLatitude",
    # buffer = radius around country/state centroids (meters); default=1000
    buffer = 500, value = "flagged")
  taxon_now$.cen <- flag_cen
  
  
  ## FLAG RECORDS THAT HAVE COORDINATES IN URBAN AREAS
  if(nrow(taxon_now)<2){
    taxon_now$.urb <- NA
    print("Taxa with fewer than 2 records will not be tested.")
  } else {
    taxon_now <- as.data.frame(taxon_now)
    flag_urb <- CoordinateCleaner::cc_urb(
      taxon_now,
      lon = "decimalLongitude",lat = "decimalLatitude",
      ref = urban_areas, value = "flagged")
    taxon_now$.urb <- flag_urb
    ## can also decide to unflag if not an iNaturalist record
    #taxon_now <- taxon_now %>%
    #  mutate(.urb = case_when(
    #    .urb == FALSE & datasetName == "iNaturalist research-grade observations" ~ FALSE,
    #    TRUE ~ TRUE
    #  ))
  }
  
  
  ### FLAG RECORDS THAT HAVE COORDINATES NEAR BIODIVERSITY INSTITUTIONS
  flag_inst <- CoordinateCleaner::cc_inst(
    taxon_now,
    lon = "decimalLongitude", lat = "decimalLatitude",
    # buffer = radius around biodiversity institutions (meters); default=100
    buffer = 100, value = "flagged")
  taxon_now$.inst <- flag_inst
  
  
  ### COMPARE THE COUNTRY LISTED IN THE RECORD VS. THE LAT-LONG COUNTRY; flag
  #   when there is a mismatch; CoordinateCleaner package has something like 
  #   this but also flags when the record doesn't have a country...
  #   you could use that function if you want to flag NAs, or edit below:
  taxon_now <- taxon_now %>% mutate(.con=(ifelse(
    (as.character(latlong_countryCode) == as.character(countryCode_standard) &
       !is.na(latlong_countryCode) & !is.na(countryCode_standard)) |
      is.na(latlong_countryCode) | is.na(countryCode_standard), TRUE, FALSE)))
  cat("Testing country listed\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.con))," records.\n",sep="")
  
  
  ### FLAG SPATIAL OUTLIERS
  taxon_now <- as.data.frame(taxon_now)
  flag_outl <- CoordinateCleaner::cc_outl(
    taxon_now,
    lon = "decimalLongitude",lat = "decimalLatitude",
    species = "taxon_name_accepted", 
    # read more about options for the method and the multiplier:
    #   https://www.rdocumentation.org/packages/CoordinateCleaner/versions/2.0-20/topics/cc_outl
    # if you make the multiplier larger, it will flag less points.
    # the default is 5; you may need to experiment a little to see what works
    #   best for most of your target taxa (script #6 helps you view flagged pts)
    method = "quantile", mltpl = 4, 
    value = "flagged")
  taxon_now$.outl <- flag_outl
  
  
  ### CHECK LAT-LONG COUNTRY AGAINST "ACCEPTED" NATIVE COUNTRY DISTRUBUTION; 
  #   flag when the lat-long country is not in the list of native countries;
  #   we use the native countries compiled in 1-get_taxa_metadata.R, which
  #   combines the IUCN Red List, BGCI GlobalTreeSearch, and manually-added data
  native_ctrys <- unique(unlist(strsplit(taxon_now$all_native_dist_iso2, "; ")))
  if(!is.na(native_ctrys[1])){
  # flag records where native country doesn't match record's coordinate location
    taxon_now <- taxon_now %>% 
      mutate(.nativectry=(ifelse(latlong_countryCode %in% native_ctrys, 
                                 TRUE, FALSE)))
  } else {
  # if no country data for the taxon, leave unflagged
    taxon_now$.nativectry <- TRUE
  }
  cat("Testing native countries\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.nativectry))," records.\n",sep="")

  
  ### FLAG OLDER RECORDS, based on two different year cutoffs (1950 & 1980)
    # 1950 cutoff
  taxon_now <- taxon_now %>% mutate(.yr1950=(ifelse(
    (as.numeric(year)>1950 | is.na(year)), TRUE, FALSE)))
  cat("Testing year < 1950\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.yr1950))," records.\n",sep="")
    # 1980 cutoff
  taxon_now <- taxon_now %>% mutate(.yr1980=(ifelse(
    (as.numeric(year)>1980 | is.na(year)), TRUE, FALSE)))
  cat("Testing year < 1980\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.yr1980))," records.\n",sep="")
  
  
  ### FLAG RECORDS THAT DON'T HAVE A YEAR PROVIDED
  taxon_now <- taxon_now %>% mutate(.yrna=(ifelse(
    !is.na(year), TRUE, FALSE)))
  cat("Testing year NA\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.yrna))," records.\n",sep="")
  
  
  ### FLAG RECORDS WITH COORDINATE UNCERTAINTY ABOVE YOUR THRESHOLD (FLAGS NA!) 
  taxon_now <- taxon_now %>% mutate(.unc=(ifelse(
    as.numeric(coordinateUncertaintyInMeters)<max_uncertainty & 
      !is.na(coordinateUncertaintyInMeters), TRUE, FALSE)))
  cat("Testing coordinate uncertainty\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.unc))," records.\n",sep="")
  
  
  ### FLAG RECORDS OUTSIDE YOUR ELEVATION RANGE, BY TAXON
  ###   for this you need an 'elevation_range' column in your 
  ###   target_taxa_with_synonyms.csv file
  #if("elevation_range" %in% colnames(taxon_now)){
  #  elev_range <- str_squish(taxon_now$elevation_range)[1]
  #  if(!is.na(elev_range)){
  #    elev_range <- unique(unlist(strsplit(taxon_now$elevation_range, "-")))
  #    elev_min <- as.numeric(elev_range[1])
  #    elev_max <- as.numeric(elev_range[2])
  #    taxon_now <- taxon_now %>% mutate(.elev=(ifelse(
  #      (as.numeric(elevationInMeters)>=elev_min &
  #       as.numeric(elevationInMeters)<=elev_max) |
  #        is.na(elevationInMeters), TRUE, FALSE)))
  #  } else {
  #    taxon_now <- taxon_now %>% mutate(.elev=TRUE)
  #  }
  #  cat("Testing elevation\n",sep="")
  #  cat("Flagged ",length(which(!taxon_now$.elev))," records.\n",sep="")
  #}
  
  
  ### FLAG RECORDS THAT ARE NOT CURRENT/NATIVE BASED ON TWO COLUMNS:
  ###   basisOfRecord AND/OR establishmentMeans
  taxon_now <- taxon_now %>% mutate(.rec=(ifelse(
    basisOfRecord != "FOSSIL_SPECIMEN" & 
    basisOfRecord != "LIVING_SPECIMEN" &
    basisOfRecord != "UNKNOWN" &  
    establishmentMeans != "INTRODUCED" & 
    establishmentMeans != "MANAGED" &
    establishmentMeans != "CULTIVATED", 
      TRUE, FALSE)))
  cat("Testing basisOfRecord & establishmentMeans\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.rec))," records.\n",sep="")
  
  
  ### FLAG RECORDS THAT ARE FROM iNATURALIST
  taxon_now <- taxon_now %>% mutate(.inat = (ifelse(
    grepl("iNaturalist", datasetName), FALSE, TRUE)))
  cat("Testing if iNaturalist\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.inat))," records.\n\n",sep="")
  
  
  ### FLAG RECORDS THAT ARE SPATIAL DUPLICATES
  ## In this section we "thin" our points by flagging points that are near each
  #   other, to make the data easier to vet and visualize.
  ## There are multiple ways to flag spatial duplicates, such as by grid cell, 
  #   the distance between points (e.g., randomly via a package like spThin), etc.
  ## The section below flags spatial duplicates based on rounded latitude
  #   and longitude. This is a simple fix that doesn't involve spatial 
  #   calculations. One additional positive of this method is that you can
  #   choose priority datasets to keep points from.
  ## First, create rounded latitude and longitude columns for flagging duplicates;
  #   number of digits can be changed based on how dense you want data; via this
  #   StackExchange post (https://gis.stackexchange.com/a/8674/7913)...
  #   "The first decimal place is worth up to 11.1 km: it can distinguish the position of one large city from a neighboring large city.
  #    The second decimal place is worth up to 1.1 km: it can separate one village from the next.
  #    The third decimal place is worth up to 110 m: it can identify a large agricultural field or institutional campus.
  #    The fourth decimal place is worth up to 11 m: it can identify a parcel of land. It is comparable to the typical accuracy of an uncorrected GPS unit with no interference."
  #   If your target taxa are rare, you may want more digits; if your target 
  #   taxa are common/widespread, you probably want fewer digits (more points flagged)
  taxon_now$lat_round <- round(taxon_now$decimalLatitude,digits=3)
  taxon_now$long_round <- round(taxon_now$decimalLongitude,digits=3)
  ## Next, sort before flagging duplicates --
  # whatever you sort to the top will be unflagged when there is a duplicate further down
  ## sort by source database
  taxon_now$database <- factor(taxon_now$database,
                             levels = c("NorthAm_herbaria","GBIF"))
    taxon_now <- taxon_now %>% arrange(database)
  ## sort by basis of record
  taxon_now$basisOfRecord <- factor(taxon_now$basisOfRecord,
                                  levels = c("PRESERVED_SPECIMEN","MATERIAL_SAMPLE","MATERIAL_CITATION",
                                             "OBSERVATION","HUMAN_OBSERVATION","OCCURRENCE","PHYSICAL_SPECIMEN",
                                             "MACHINE_OBSERVATION","FOSSIL_SPECIMEN","LIVING_SPECIMEN",
                                             "UNKNOWN"))
    taxon_now <- taxon_now %>% arrange(basisOfRecord)
  ## sort by establishment means
  taxon_now$establishmentMeans <- factor(taxon_now$establishmentMeans,
                                       levels = c("NATIVE","WILD","UNCERTAIN","INTRODUCED","MANAGED","CULTIVATED",
                                                  "DEAD"))
    taxon_now <- taxon_now %>% arrange(establishmentMeans)
  ## sort by coordinate uncertainty
  taxon_now$coordinateUncertaintyInMeters <-
    as.numeric(taxon_now$coordinateUncertaintyInMeters)
    taxon_now <- taxon_now %>% arrange(taxon_now$coordinateUncertaintyInMeters)
  ## sort by year
  taxon_now <- taxon_now %>% arrange(desc(year))
  ## sort by other flags (fewest flags at top)
  cols_to_check <- c(".cen",".urb",".inst",".con",".outl",".nativectry",
    ".yr1950",".yr1980",".yrna",".unc",".rec",".inat")
  taxon_now$num_flags <- rowSums(!taxon_now[cols_to_check], na.rm = TRUE)
  taxon_now <- taxon_now %>% arrange(num_flags)
  ## flag spatial duplicates --
  unflagged_uids <- taxon_now %>%
    group_by(taxon_name_accepted,lat_round,long_round) %>%
    distinct(taxon_name_accepted,lat_round,long_round,.keep_all=T) %>%
    ungroup() %>% 
    select(UID)
  taxon_now <- taxon_now %>% 
    mutate(.spatdup=(ifelse(UID %in% unique(unlist(unflagged_uids)), TRUE, FALSE))) %>%
    select(-num_flags)
  cat("Testing if spatial duplicate\n",sep="")
  cat("Flagged ",length(which(!taxon_now$.spatdup))," records.\n\n",sep="")
  

  
  ##########################################  
  # Remove exact duplicates
  ##########################################
  
  # first sort by database
  taxon_now$database <- factor(taxon_now$database, levels = c("NorthAm_herbaria","GBIF"))
  taxon_now <- taxon_now %>% arrange(database)
  # and basis of record
  taxon_now$basisOfRecord <- factor(taxon_now$basisOfRecord,
                                  levels = c("PRESERVED_SPECIMEN","MATERIAL_SAMPLE","MATERIAL_CITATION",
                                             "OBSERVATION","HUMAN_OBSERVATION","OCCURRENCE","PHYSICAL_SPECIMEN",
                                             "MACHINE_OBSERVATION","FOSSIL_SPECIMEN","LIVING_SPECIMEN",
                                             "UNKNOWN"))
  taxon_now <- taxon_now %>% arrange(basisOfRecord)
  # and establishment means
  taxon_now$establishmentMeans <- factor(taxon_now$establishmentMeans,
                                       levels = c("NATIVE","WILD","UNCERTAIN","INTRODUCED","MANAGED","CULTIVATED",
                                                  "DEAD"))
  taxon_now <- taxon_now %>% arrange(establishmentMeans)
  ##*## and put flagged points at the bottom, based on our specific flags used
  taxon_now <- taxon_now %>% arrange(desc(.con), desc(.outl), desc(.nativectry),
    desc(.yr1950), desc(.yrna), desc(.rec))
  nrow(taxon_now)
  
  # remove dups by occurrence ID
  taxon_now <- taxon_now %>%
    group_by(occurrenceID) %>%
    mutate(all_source_databases = paste(unique(database), collapse = ', ')) %>%
    distinct(occurrenceID,.keep_all=T) %>%
    ungroup()
  nrow(taxon_now)
  
  # remove dups by reference ID
  taxon_now <- taxon_now %>%
    group_by(references) %>%
    mutate(all_source_databases = paste(unique(database), collapse = ', ')) %>%
    distinct(references,.keep_all=T) %>%
    ungroup()
  nrow(taxon_now)
  
  # remove dups by exact lat-long and year
  taxon_now <- taxon_now %>%
    group_by(decimalLatitude,decimalLongitude,year) %>%
    mutate(all_source_databases = paste(unique(database), collapse = ', ')) %>%
    distinct(decimalLatitude,decimalLongitude,year,.keep_all=T) %>%
    ungroup()
  nrow(taxon_now)
  
  ##########################################
  
  
  
  # create some subsets to count how many records are in each, for summary table...

  # count of completely unflagged points
  total_unflagged <- taxon_now %>%
    filter(.cen & .urb & .inst & .con & .outl & .nativectry & .yr1950 & 
             .yr1980 & .yrna & .unc & #.elev & 
             .rec & .inat & .spatdup)
  
  # OPTIONAL count of unflagged points based on selected filters you'd like 
  #   to use, to see how many points there are; change as desired; commented- 
  #   out lines are the filters you don't want to use
  select_unflagged <- taxon_now %>%
    filter(
            #.cen & 
            #.urb &
            #.inst & 
            .con & 
            .outl &
            .nativectry &
            .yr1950 & 
            #.yr1980 & 
            .yrna &
            #.unc &
            #.elev &
            .rec #&
            #.inat &
            #.spatdup  
    )
  
  # add data to summary table
  summary_add <- data.frame(
    taxon_name_accepted = taxon_nm,
    unflagged_pts = nrow(total_unflagged),
    selected_pts = nrow(select_unflagged),
    .cen = sum(!taxon_now$.cen),
    .urb = sum(!taxon_now$.urb),
    .inst = sum(!taxon_now$.inst),
    .con = sum(!taxon_now$.con),
    .outl = sum(!taxon_now$.outl),
    .nativectry = sum(!taxon_now$.nativectry),
    .yr1950 = sum(!taxon_now$.yr1950),
    .yr1980 = sum(!taxon_now$.yr1980),
    .yrna = sum(!taxon_now$.yrna),
    .unc = sum(!taxon_now$.unc),
    #.elev = sum(!taxon_now$.elev),
    .rec = sum(!taxon_now$.rec),
    .inat = sum(!taxon_now$.inat),
    .spatdup = sum(!taxon_now$.spatdup),
    stringsAsFactors=F)
  summary_tbl[i,] <- summary_add

  # WRITE NEW FILE
  write.csv(taxon_now, file.path(main_dir,occ_dir,standardized_occ,data_out,
    paste0(taxon_file, ".csv")), row.names=FALSE)

}

# add summary of points to summary we created in 4-compile_occurrence_points.R
file_nm <- list.files(path = file.path(main_dir,occ_dir,standardized_occ),
                      pattern = "summary_of_occurrences", full.names = T)
orig_summary <- read.csv(file_nm, colClasses = "character")
  # keep just the first four columns, in case you're running this script a second time
orig_summary <- orig_summary %>% select(taxon_name_accepted:num_water_records)
summary_tbl2 <- full_join(orig_summary,summary_tbl,by="taxon_name_accepted")
summary_tbl2

# write summary table
write.csv(summary_tbl2, file.path(main_dir,occ_dir,standardized_occ,
  paste0("summary_of_occurrences_", Sys.Date(), ".csv")),row.names = F)
