### 6-visualize_occurrence_points.R
### Author: Emily Beckman Bruns
### Supporting institutions: The Morton Arboretum, Botanic Gardens Conservation 
#   International-US, United States Botanic Garden, San Diego Botanic Garden,
#   Missouri Botanical Garden
### Funding: 
#   -- Institute of Museum and Library Services (IMLS MFA program grant
#        MA-30-18-0273-18 to The Morton Arboretum)
#   -- United States Botanic Garden (cooperative agreement with San Diego
#        Botanic Garden & grant to The Morton Arboretum)
#   -- NSF (award 1759759 to The Morton Arboretum)
### Last Updated: October 2025; June 2023; first written March 2020
### R version 4.5.1

### DESCRIPTION:
  ## This script creates an interactive (HTML) occurrence point map for each 
  #   target taxon, for exploring and (optionally) retrieving the UIDs of points 
  #   to be removed before final anlayses. Includes checkbox toggles that show 
  #   points flagged in 5-flag_occurrence_points.R

### INPUTS:
  ## target_taxa_with_synonyms.csv
  #   List of target taxa and synonyms; see example in the "Target taxa list"
  #   tab in Gap-analysis-workflow_metadata workbook; Required columns include: 
  #   taxon_name, taxon_name_accepted, and taxon_name_status (Accepted/Synonym).
  ## taxon_points_ready-to-vet (folder) 
  #   Occurrence data output from 5-flag_occurrence_points.R
  ## world_countries_10m.shp
  #   Shapefile created in 2-prep_gis_layers.R script. It's the Natural Earth 
  #   10m countries layer with the lakes cut out and some ISO_2A issues fixed.

### OUTPUTS:
  ## visualize_taxon_points (folder)
  #   For each taxon in your target taxa list, an interactive HTML map is
  #   created, which can be downloaded and opened in your browser for exploring 
  #   (e.g., Asimina_triloba__map-for-vetting.html)
  ## manual_point_edits.csv
  #   File that can be filled in manually while reviewing the maps - optional; 
  #   see instructions in script #7 "INPUTS"

################################################################################
# Load libraries
################################################################################

# install rnaturalearthdata package if you don't have it yet
#install.packages('rnaturalearthdata') # my version is 0.1.0

# load packages
my.packages <- c('tidyverse','textclean','rnaturalearth','leaflet')
  # versions I used (in the order listed above): 2.0.0, 0.9.3, 1.7-29, 0.3.3, 2.1.2
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
data_out <- "visualize_taxon_points"
if(!dir.exists(file.path(main_dir,occ_dir,standardized_occ,data_out)))
  dir.create(file.path(main_dir,occ_dir,standardized_occ,data_out), 
             recursive=T)

# assign folder where you have input data (saved in 5-flag_occurrence_points.R)
data_in <- "taxon_points_ready-to-vet"

################################################################################
# Prep supplemental data needed for mapping
################################################################################

# read in taxa list
taxon_list <- read.csv(file.path(main_dir,taxa_dir,"target_taxa_with_synonyms.csv"), 
                       header=T, colClasses="character",na.strings=c("","NA"))

# select accepted taxa
target_taxa <- taxon_list %>% filter(taxon_name_status == "Accepted")
nrow(target_taxa)

# save file that can be (optionally) used for manually flagging points for removal
edits <- data.frame(taxon_name_accepted = target_taxa$taxon_name_accepted,
                    remove_id	= "", remove_bounding_box = "", keep_id	= "", 
                    reviewer_notes	= "", reviewer_name = "")
if(file.exists(file.path(main_dir,occ_dir,standardized_occ,"manual_point_edits.csv"))){
  "You've already created this file; if you'd like to overwrite it, run the 'else' statement manually"
} else {
  write.csv(edits, file.path(main_dir,occ_dir,standardized_occ,
                             "manual_point_edits.csv"), row.names = F)
}

# make taxon list with underscores added where spaces, to format for reading/
#   writing when we cycle through in our loop below
taxa_cycle <- unique(mgsub(target_taxa$taxon_name_accepted, 
                           c(" ","var.","subsp."), c("_","var","subsp")))
taxa_cycle

# list of native countries for each target taxon
#countries <- target_taxa$all_native_dist_iso2

# read in world countries layer from rnaturalearth package
#world_polygons <- ne_countries(scale = 50, type = "countries", 
 #                               returnclass = "sf")


################################################################################
# Use leaflet package to create interactive maps to explore (html)
################################################################################

### cycle through each species file and create map
for(i in 1:length(taxa_cycle)){

  ## read in occurrence records (output from 5-flag_occurrence_points.R)
  taxon_now <- read.csv(file.path(main_dir,occ_dir,standardized_occ,data_in,
                                  paste0(taxa_cycle[i],".csv")))

  ## create a color palette for the map's points, based on source database
    # set database as factor and order as you'd like for viewing overlapping
    #   points; earlier databases will be shown on top of latter databases
  database_order <- c("NorthAm_herbaria","GBIF")
  taxon_now$database <- factor(taxon_now$database, levels = database_order)
  taxon_now <- taxon_now %>% arrange(desc(database))
    # create color palette (one color for each database)
    # there are plenty of palette creation tools that can help you create 
    #   a palette that is accessible (friendly to those with color vision 
    #   deficiency); some examples:
    #     http://medialab.github.io/iwanthue/
    #     https://venngage.com/tools/accessible-color-palette-generator#colorGenerator
    #     https://toolness.github.io/accessible-color-matrix/
    #   or search "color picker" in Google and choose colors manually by copying
    #   the HEX number
  colors <- c(#"#adbb3f"
              #"#819756"
              #"#5fbb9a"
              #"#6a9ebd"
              #"#7b83cc"
              "#3c2c7a",
              "#7264de"
              )
  database.pal <- colorFactor(palette=colors, levels = database_order)

  # initial filter of occurrence points, for records we know we don't want
  taxon_now <- taxon_now %>%
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

  ## create interactive map
  try(final_map <- leaflet() %>%
    ## Base layers
    addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
    addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Esri.WorldGrayCanvas") %>%
    addControl(paste0("<b>",taxa_cycle[i]), position = "topright") %>%
    ## Notes text boxes
    addControl(
#      "Toggle the checkboxes below on and off to view flagged<br/>  
#      points (colored red). If no points turn red when the box<br/>
#      is checked, there are no points flagged in that category.<br/>
      "<b>Click each point for details<br/>about the record</b>",
       position = "topright") %>%
    ## Occurrence points, colored by database
    addCircleMarkers(data = taxon_now, ~decimalLongitude, ~decimalLatitude,
                     popup = ~paste0(
                       "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
                       "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
                       "<b>Dataset name:</b> ",datasetName,"<br/>",
                       "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
                       "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
                       "<b>Record year:</b> ",year,"<br/>",
                       "<b>Latitude:</b> ",decimalLatitude,"<br/>",
                       "<b>Longitude:</b> ",decimalLongitude,"<br/>",
                       "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
                       "<b>Reference:</b> ",references,"<br/>",
                       "<b>UID:</b> ",UID),
                     color = ~database.pal(database),radius = 4,
                     fillOpacity = 0.9, stroke = T) %>%
#    ## Occurrence points flagged in each group (can toggle)....
#     # .urb (points in urban areas)
#     addCircleMarkers(data = taxon_now %>% filter(!.urb & database!="Ex_situ"),
#                      ~decimalLongitude, ~decimalLatitude,
#                      popup = ~paste0(
#                        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
#                        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
#                        "<b>Dataset name:</b> ",datasetName,"<br/>",
#                        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
#                        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
#                        "<b>Record year:</b> ",year,"<br/>",
#                        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
#                        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
#                        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
#                        "<b>Reference:</b> ",references,"<br/>",
#                        "<b>UID:</b> ",UID),
#                      radius=3, stroke=T, color="black", weight=1,
#                      fillColor="red", fillOpacity=0.8,
#                      group = "In urban area") %>%
#     # .yr1980 (recorded prior to 1980)
#     addCircleMarkers(data = taxon_now %>% filter(!.yr1980 & database!="Ex_situ"),
#                      ~decimalLongitude, ~decimalLatitude,
#                      popup = ~paste0(
#                        "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
#                        "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
#                        "<b>Dataset name:</b> ",datasetName,"<br/>",
#                        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
#                        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
#                        "<b>Record year:</b> ",year,"<br/>",
#                        "<b>Latitude:</b> ",decimalLatitude,"<br/>",
#                        "<b>Longitude:</b> ",decimalLongitude,"<br/>",
#                        "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
#                        "<b>Reference:</b> ",references,"<br/>",
#                        "<b>UID:</b> ",UID),
#                      radius=3, stroke=T, color="black", weight=1,
#                      fillColor="red", fillOpacity=0.8,
#                      group = "Recorded prior to 1980") %>%
#       # .inat (records from iNaturalist)
#       addCircleMarkers(data = taxon_now %>% filter(!.inat & database!="Ex_situ"),
#                        ~decimalLongitude, ~decimalLatitude,
#                        popup = ~paste0(
#                          "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
#                          "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
#                          "<b>Dataset name:</b> ",datasetName,"<br/>",
#                          "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
#                          "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
#                          "<b>Record year:</b> ",year,"<br/>",
#                          "<b>Latitude:</b> ",decimalLatitude,"<br/>",
#                          "<b>Longitude:</b> ",decimalLongitude,"<br/>",
#                          "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
#                          "<b>Reference:</b> ",references,"<br/>",
#                          "<b>UID:</b> ",UID),
#                        radius=3, stroke=T, color="black", weight=1,
#                        fillColor="red", fillOpacity=0.8,
#                        group = "iNaturalist (reseach grade)") %>%
# 		  # .spatdup (spatial duplicates)
# 		  addCircleMarkers(data = taxon_now %>% filter(!.spatdup & database!="Ex_situ"),
# 		                   ~decimalLongitude, ~decimalLatitude,
# 		                   popup = ~paste0(
# 		                     "<b>Accepted species name:</b> ",taxon_name_accepted,"<br/>",
# 		                     "<b>Verbatim taxon name:</b> ",taxon_name,"<br/>",
# 		                     "<b>Dataset name:</b> ",datasetName,"<br/>",
# 		                     "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
# 		                     "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
# 		                     "<b>Record year:</b> ",year,"<br/>",
# 		                     "<b>Latitude:</b> ",decimalLatitude,"<br/>",
# 		                     "<b>Longitude:</b> ",decimalLongitude,"<br/>",
# 		                     "<b>Coordinate uncertainty in meters:</b> ",coordinateUncertaintyInMeters,"<br/>",
# 		                     "<b>Reference:</b> ",references,"<br/>",
# 		                     "<b>UID:</b> ",UID),
# 		                   radius=3, stroke=T, color="black", weight=1,
# 		                   fillColor="red", fillOpacity=0.8,
# 		                   group = "Spatial duplicate (~100m)") %>%
    ## Layers control (check boxes you can toggle on and off; 'overlayGroup' names
    ##    are associated with the 'group' names assigned in each section above)
    addLayersControl(
      baseGroups = c("Esri.WorldTopoMap","CartoDB.Positron",
                     "OpenStreetMap","Esri.WorldGrayCanvas"),
#      overlayGroups = c("In urban area",
#                        "Recorded prior to 1980",
#                        "Spatial duplicate (~100m)",
#                        "iNaturalist (reseach grade)"
#                        ),
      options = layersControlOptions(collapsed = FALSE)) %>%
#    # you can un-check groups in the control panel with a 'hideGroup';
#    # it's nice to hide the filters you're not interested in
#      hideGroup("In urban area") %>%
#      hideGroup("Recorded prior to 1980") %>%
#      hideGroup("iNaturalist (reseach grade)") %>%
#      hideGroup("Spatial duplicate (~100m)") %>%
    ## Legend
    addLegend(pal = database.pal, values = unique(taxon_now$database),
              title = "Occurrence point</br>source database", 
              position = "topright", opacity = 0.8) %>%
    addControl(
    "Emily Bruns | The Morton Arboretum | October 2025",
    position = "bottomright")
      
  )
  final_map

  ## save map
  try(htmlwidgets::saveWidget(final_map, file.path(main_dir,occ_dir,
                                                   standardized_occ,data_out,
    paste0(taxa_cycle[i], "__map-for-vetting.html"))))

  cat("\tMapped ", taxa_cycle[i], ", ", i, " of ", length(taxa_cycle), ".\n\n", sep="")
}
