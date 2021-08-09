#!/usr/bin/Rscript --vanilla

# =============================================================================
# AUTHORS: Andrew Harris (andrew.harris@humboldt.edu)
#        Max Blasdel
#        Jerome Carman
#        Chih-Wei Hsu
# 
#       Schatz Energy Research Center
#       Humboldt State University 
# -------------------------------------------------------------------------
# VERSION:
#
# 1.0 2019-07-26: Ported from .Rmd to .R command line tool
# 1.1 2019-09-17: Features added for report analysis
# -------------------------------------------------------------------------
# OBJECTIVE:
# This script will run the C-BREC model through the use case, but not through
# prescribed burn, decay, or wildfire.
#
# ---------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#
# ---------------------------------
# =============================================================================

###############################################
# Load needed library functions
###############################################
library("optparse") # command line interface functions
library("raster") # spatial data manipulation
library("rgdal") # shapefile processing
library("rgeos") # additional spatial data operations
library('data.table') # main structure of code and data format
library('future.apply') # for running in parallel

###############################################
# Source relevant module and function files.
###############################################
source("CBREC1.0/harvest-processing.R")
source("CBREC1.0/prescribed-burn-functions.R")
source("CBREC1.0/decay-functions.R")
source('CBREC1.0/wildfire-functions.R')
source('CBREC1.0/power-plant-functions.R')
source("CBREC1.0/misc-functions.R")
source("CBREC1.0/natural-gas.R") # emissions factors for NG
source("CBREC1.0/user_input_cases.R") # get the cases to run for a given thp 
source("CBREC1.0/save-output.R") # save files into a file system

# helper functions
source("CBREC1.0/rasterDataTable.R") # script to optimize converting rasters to data.table
source("CBREC1.0/load-shapefile.R") # loads shapefiles from a specified folder

options(stringsAsFactors = F,
        show.error.locations =T)
#########################
# COMMAND LINE OPTIONS 
#########################
option_list <- list(
  make_option(c("-v", "--verbose"), action="store_true", default=F, help="Print extra output [default]"),
  make_option(c("-i", "--infile"), type="character", default='CBREC_input_directory/user_input_files/cbrec_inputs.csv', help="Input file specifying scenario and study area [default \"%default\"]",metavar="input-file")
)
if(interactive()){
  args <- parse_args(OptionParser(option_list = option_list))
}else{
  args <- parse_args(OptionParser(option_list = option_list))
  if(!file.exists(args$infile)) {
    stop("Input file does not exist. Check filepath.")
  }
}

user_input_filepath <- args$infile

#############################################################################################################################################
# All user options and filepaths are in an external csv file
#############################################################################################################################################
user_inputs <- fread(user_input_filepath, stringsAsFactors = FALSE)
initial_user_inputs <- copy(user_inputs)

#####################################################
# Load up emissions factor/rate data from csv files.
#####################################################
equipment_emissions <- fread(user_inputs[variable=='equipment_emissions_filepath',value])

# power plant emissions values
power_emission_factors <- fread(user_inputs[variable=='power_plant_emissions_filepath',value])

# replace slashes with dot to avoid naming issues
names(power_emission_factors) <-
  gsub("/", ".", names(power_emission_factors)) 

# remove unneeded files
rm(option_list,
   args,
   user_input_filepath)
#############################################################################################################################################

########################################################################################################
##############################    NOTES ON HOW CBREC RUNS   ############################################
## 
## CBREC involves a lot of data preparation to run
## This data is both spatial, depending on the project area, and tabular, depending on which case is being run
## 
## CBREC is currently set up in three large loops 
##
## 1) The first loops through the spatially explicit variables; slope, decay, transportation distances, and resource base
## These variables are repeated for each case that is being run. There will be slight overlap with the transportation rasters, but a 'use' case
## is always run with 'reference' cases making this data relevant to load at this point
##
## 2) The second loops through each case. These are the variables that define the harvest type and choices. 
## This loop is run in parallel by using the same spatial inputs and running each case for the desried project area
##
## 3) The third loop is the time series over 100 years. This is also run in parallel since it is run inside the parallelized second loop
## This loop takes the emissions results and expands them over a 100 year time period.
##
## Currently outputs a single file as a nested list of each tile and all desired cases
## If the model is run on a large area this file will be untenably large
##
#########################################################################################################

##########################
# Load initial data
##########################

# Load tiles used to organize fire data
tile_full <- readOGR(user_inputs[variable == 'tile_shape_file', value])

# get list of tile IDs
tile_list <- tile_full$ID

# tiles with no fire data that will cause an error
no_data_tiles <- c('10155',
                   '2535',
                   '7585',
                   '7638')

# remove no data tiles
tile_list <- subset(tile_list, tile_list %notin% no_data_tiles)

### MAKE SURE there is fire data for each tile that is being run
# wildfire data will be pulled from the file path defined in cbrec_inputs.csv

Study_Area_Raster <- raster(user_inputs[variable=='fcid_code_filepath',value])

Slope_Raster <- raster(user_inputs[variable=='slope_filepath',value])
names(Slope_Raster) <- "cell_slope"

# load the three decay rasters 
Decay_Stack <- list(raster(user_inputs[variable=='CWD_decay_filepath',value]),
                    raster(user_inputs[variable=='FWD_decay_filepath',value]),
                    raster(user_inputs[variable=='foliage_decay_filepath',value]))  

# Read in the residue load values from a csv file. These values are indexed by FCID_code which relates to the study_area_raster
# Note that the file loaded here post-processed raw residue to combine stem and bark into a single stem column
fcid_table <- fread(user_inputs[variable == 'residue_treatment_filepath', value], 
                    drop = "TPA") # drop unneeded column

setkey(fcid_table, Treatment)
setkey(treatment_name_lookup, treat.code)

# change name of treatment
fcid_table <-
  fcid_table[treatment_name_lookup][, Treatment := NULL]

# If the user inputs specify a study area mask file, load the shapefile, crop the original raster to the shapefile extents, 
# and mask the raster to the shape file.

# If there are polygons in the desired folder, load them, otherwise run the tile list
if(length(dir(user_inputs[variable=='shapefile_folder',value]) != 0)) {
  
  # read in a polygon file
  polygons <- loadShapefile(folder = user_inputs[variable == "shapefile_folder", value], 
                            crs = crs(tile_full)) # project everything to study area raster
  
  # check if the polygon has an "ID" atribute
  # If it does, rename to reduce conflicts later on
  if(sum(grepl("^ID$", names(poly))) > 0 ) {
    
    # Change ID to OBJECTID, this could be an issue if there is a non-unique value columm called ID, unlikely
    names(poly) <-
      gsub("^ID$", "OBJECTID", names(poly))
    # } else if(sum(grepl("^OBJECTID$", names(poly))) > 0 ){
    #   
    #   # polys <- split(polygons, polygons$OBJECTID)
    #   
    # } else{
    #   
    #   # create ID column
    #   # polygons$OBJECTID <- seq(1:nrow(polygons))
  }
  
  # split into a list by ID
  polys <- split(polygons, polygons$OBJECTID)
  
} else {
  # subset tiles to tile_list
  tile_full <-
    tile_full[tile_list, ]
  
  polys <- split(tile_full, tile_full$ID)
}

# Set thp analysis flag
# this will ideally be a variable in a function
thp = 1

# Running a subset of THP+ polygons
run_polys <- 
  dir("CBREC_output_directory/results", pattern = '[0-9]')

# rerun the last polygon to make sure it is complete
# When testing the model is 'stopped' intermittenly which does not guarantee complete runs for each polygon
run_polys <-
  run_polys[1:length(run_polys) - 1]

# subset polygons
polys <-
  polys[!(names(polys) %in% run_polys)]

# record the polygons which are being run
# write.csv(vals <- rapply(polys, function(poly) {
#         as.vector(poly$OBJECTID)
#     }, 
#     how = "unlist"), # return unlisted object
#     paste0(Sys.Date(), "_polys_run.txt")) # record date of model run

#################################
# This lapply wraps the whole model and loops through each polygon
###################################

# set the number of workers. When running on Hagrid, a maximum of 26 workers is advisable to accomodate other projects
# if unsure of number of cores run parallel::detectCores()
# maximum workers should be parallel::detectCores() - 1
# set number of workers
works <- 26

plan(multiprocess, workers = works) # check this value and change if memory concerns appear

# time testing
start <- Sys.time() 
print(start)

lapply(polys, function(poly) {
  
  ###############################
  # Spatial data preparation
  ###############################
  # get poly number
  poly_num <- poly$OBJECTID
  
  # output folder
  out_folder <- "CBREC_output_directory/results/"
  
  if(!dir.exists(paste0(out_folder, poly_num))) {
    dir.create(paste0(out_folder, poly_num))
  }
  
  # create rasters for each polygon region
  study_area_raster <- crop(Study_Area_Raster, extent(poly))
  study_area_raster <- mask(study_area_raster, poly)
  
  
  slope_raster <- crop(Slope_Raster, extent(poly)) # Crops to the min and max x-y range
  slope_raster <- mask(slope_raster, poly) # Replaces masked x-y coords with NAs
  
  # Convert to data.table and remove cells with no data 
  study_area_FCID <- na.omit(as.data.table(study_area_raster, xy = T))      
  
  # Set names for clarity 
  names(study_area_FCID) <- c('x','y','FCID_code')
  
  # Repeat with slope
  slope_dt <- na.omit(as.data.table(slope_raster, xy = T))
  
  # setkeys
  setkey(study_area_FCID, x, y)
  setkey(slope_dt,x,y)
  
  # Combine the slope with the FCID data tables
  study_area_FCID <- slope_dt[study_area_FCID]
  
  # remove slopes above 80%
  study_area_FCID <- study_area_FCID[cell_slope < as.numeric(user_inputs[variable == "residue_slope_cutoff", value])]
  
  # Round the x and y values to the first decimal, otherwise rounding errors are going to muck with joins with the decay rates.
  study_area_FCID[,':='(x = round(x, digits = 1), y = round(y, digits = 1))]
  
  # Add heating value and carbon fraction.
  biomass_properties <- fread(user_inputs[variable == 'biomass_properties_filepath', value])
  
  # set keys
  setkey(biomass_properties,FCID_code)
  setkey(study_area_FCID,FCID_code)
  
  # merge and remove unneeded data table
  study_area_FCID <- biomass_properties[study_area_FCID]
  biomass_properties <- NULL
  
  # Remove FCIDs with no biomass properties
  study_area_FCID <- study_area_FCID[!is.na(Carbon_frac) & !is.na(Ash_frac) & !is.na(HHV), ]
  
  # Re-key
  setkey(study_area_FCID,x,y)
  
  ##########
  # Load Decay rasters
  ##########
  decay_stack <- lapply(Decay_Stack, function(r) {
    
    # crop and mask to tile
    r <- crop(r, extent(poly))
    r <- mask(r, poly)
    
    # convert to data table
    r <- na.omit(as.data.table.raster(r, xy = T))
    r[,':='(x = round(x,digits = 1), y = round(y,digits = 1))]
    setkey(r, x, y)
    return(r)
  })
  
  # combine with other spatial data
  study_area_FCID <- decay_stack[[1]][decay_stack[[2]][decay_stack[[3]][study_area_FCID]]]
  
  # rename to more intuitive column headers
  setnames(study_area_FCID,
           c("rast_cwd_V3","rast_fwd_V3","foliage_V3"),
           c("CWD_cm","FWD_cm","Foliage_cm"))
  
  # remove unneeded raster
  rm(decay_stack)
  
  ############################################ Transportation begin #####################################################################
  # Determine specific travel distances - cell to road, then road to plant.
  #######################################################################################################################################
  
  # Load the cell to road raster
  cell_to_road_raster <- raster(paste0(user_inputs[variable == "transportation_filepath", value],
                                       "Transportation_DistanceToRoad.tif"))
  
  # Trim to study area or may hit memory problems.
  cell_to_road_raster <- crop(cell_to_road_raster,extent(poly))
  cell_to_road_raster <- mask(cell_to_road_raster,poly)
  
  cell_to_road <- as.data.table.raster(cell_to_road_raster, xy = T)
  
  # Convert the meters to miles
  cell_to_road[, Transportation_DistanceToRoad := Transportation_DistanceToRoad * 0.000621371] # meters per mile
  
  # Rename the value column, bearing in mind that the distance raster is in meters.
  setnames(cell_to_road,c('x','y','CellToRoadDist_miles'))
  
  # Combine with studay_area_FCID. This first requires rounding coordinates to the nearest tenth.
  cell_to_road[,':='(x = round(x,digits=1),y = round(y,digits=1))]
  setkey(cell_to_road, x, y)
  setkey(study_area_FCID, x, y)
  
  study_area_FCID <- cell_to_road[study_area_FCID]
  cell_to_road_raster <- NULL
  cell_to_road <- NULL
  
  # Only run the road to plant distance calculation if non-default plants were selected
  if (!user_inputs[variable=="power_plant_location",value] %in% c("CurrentGenCombustionPlantDefault",
                                                                  "CurrentGenIG/CombustionPlantDefault",
                                                                  "CurrentGenChpPlantDefault",
                                                                  "NextGenThermochemicalPlantDefault",
                                                                  "5MWPlant",
                                                                  "LessThan1MWPlant")){
    
    # Repeat for the road to plant raster. The file name will be derived from the user-selected plant code.
    road_to_plant_raster <- raster(paste(user_inputs[variable=="transportation_filepath",value],user_inputs[variable=="power_plant_location",value],".tif",sep=""))
    
    # Again, trim to the study area, or we may hit memory problems.
    road_to_plant_raster <- crop(road_to_plant_raster, extent(poly))
    road_to_plant_raster <- mask(road_to_plant_raster, poly)
    
    # convert to data.table
    road_to_plant <- na.omit(as.data.table.raster(road_to_plant_raster, xy = T))
    
    # rename for consistancy through the scripts
    setnames(road_to_plant,c("x", "y", "RoadToPlant_miles") )
    
    # Combine with case.data. This first requires rounding coordinates to the nearest tenth.
    road_to_plant[,':='(x = round(x, digits = 1),y = round(y, digits = 1))]
    
    # Key the data
    setkey(road_to_plant,x,y)
    
    # Add a column to indicate the specific plant location.
    if(user_inputs[variable=="power_plant_location",value]=="NearestOperationalPlant") {
      # There may be multiple plant IDs. Read in the plant ID raster, pair in codes from a plant ID raster, and fill gaps in the operational plant raster with default max distance.
      nearest_plant_raster <- raster(paste(user_inputs[variable=="transportation_filepath",value],"NearestPlantID.tif",sep=""))
      
      # The "NearestPlantID" raster is ever so slightly misaligned with the other rasters, despite having the same projection. So, we resample it.
      nearest_plant_resampled <- projectRaster(nearest_plant_raster,road_to_plant_raster,method = "ngb") # nearest neighbor
      nearest_plant <- as.data.table(nearest_plant_resampled, xy = T)
      
      setkey(nearest_plant,NearestPlantID)
      
      # Read in location ID names, and join with nearest_plant
      location_names <- fread(user_inputs[variable=="power_plant_location_code_filepath",value], 
                              drop = c('Status', 'MW_Nmpl', 'Prjct_T', 'City', 'County'))
      
      # Key and join
      setkey(location_names,OID)
      nearest_plant <- location_names[nearest_plant]
      
      # Trim unneeded columns, and re-name
      nearest_plant[,':='(OID=NULL)]
      
      # Clarify names
      setnames(nearest_plant,"Name","plant_location")
      
      # Round coordinates
      nearest_plant[,':='(x = round(x,digits=1),y = round(y,digits=1))]
      
      # Combine with road_to_plant
      setkey(road_to_plant,x,y)
      setkey(nearest_plant,x,y)
      road_to_plant <- nearest_plant[road_to_plant]
    } else {
      road_to_plant[,plant_location:=user_inputs[variable=="power_plant_location",value]]
    }
    
    setkey(road_to_plant,x,y)
    study_area_FCID <- road_to_plant[study_area_FCID]
    
    # trim out unneeded rasters
    road_to_plant_raster <- NULL
    road_to_plant <- NULL
  } else{
    # when default plants selected, create the road to plant miles variable with the generic distance by user input
    study_area_FCID[,RoadToPlant_miles := user_inputs[variable=="power_plant_generic_distance",as.numeric(value)]] 
    study_area_FCID[,plant_location := user_inputs[variable=="power_plant_location",value]] 
  }
  
  # set power plant name to append to file name
  plant_location <- unique(study_area_FCID$plant_location)
  
  ################################################
  ## End transportation
  ################################################
  ##########################
  # Create list of cases to run through model
  ##########################
  
  # for thps we will use the primary treatment to pull cases from the scenario matrix
  if(thp == T) {
    user_input_iterations <- thpCases(poly)
  }  else {
    user_input_iterations <- userInputCases(user_inputs)
  }  
  
  # Find the tiles that overlap with the polygons of interest
  study_area_tiles <- raster::intersect(tile_full, poly)
  
  # get a list of tiles by ID attribute for loading the fire data
  tile_list <- unique(study_area_tiles$ID)
  
  # create variable to split data on
  user_input_iterations[, MYID := seq(1,nrow(user_input_iterations))]
  
  # split into list for each row; These are the cases the model will run in parallel
  user_input_iterations <- split(user_input_iterations, user_input_iterations$MYID)
  
  # run fuel processing and decay/fire models on each case in the list
  
  #####################################################
  ############# PARALLELIZE HERE ######################
  #####################################################
  
  future_lapply(user_input_iterations, function(case) {
    
    #Loop through for each case to be analyzed
    stopifnot(nrow(case) == 1)
    
    # Copy the original study_area_FCID data into a data structure that will dynamically change.
    case.data <- copy(study_area_FCID)
    
    # set scenario ID for use in naming and wildfire retrival
    scenario_ID <-
      paste(case$treatment,
            case$prescribed.burn.type,
            case$frac.piled,
            case$frac.scattered,
            "first",
            case$biomass.collection,
            case$pulp.market,
            case$ID,
            sep = "-")
    
    # replace case treatment value with other format
    # there are inconsistancies in the data throughout
    # case$treatment <- 
    #   na.omit(treatment_name_lookup$treat.code[match(unlist(treatment_name_lookup),
    #                                                  case$treatment)])
    
    # Replace the user inputs with the appropriate variables from user_input_iterations
    user_inputs[variable=='fraction_piled_residues',value := as.character(case[, frac.piled])]
    user_inputs[variable=='fraction_scattered_residues',value := as.character(case[, frac.scattered])]
    user_inputs[variable=='treatment_type',value := case[, treatment]]
    user_inputs[variable=='burn_type',value := case[, prescribed.burn.type]]
    user_inputs[variable=='biomass_collection',value := case[, biomass.collection]]
    user_inputs[variable=='has_pulp_market',value := case[, pulp.market]]
    
    # The technically available residues will be based on slope, silvicultural treatment, the fraction of piled/scattered residues, burn type, 
    # biomass collection, and pulp market. Within a scenario, treatment, burn type, biomass collection, and pulp market are identical across 
    # the entire study area. 
    
    # Create a smaller scenario matrix filtered through the user inputs, and merge with case.data
    complete_scenario_matrix <- fread(user_inputs[variable=='scenario_matrix_file',value])
    
    complete_scenario_matrix <- subset(complete_scenario_matrix, ID == case$ID)
    
    # Combine case.data with the FCID-specific residue loading, specific to out scenario treatment.
    setkey(fcid_table,FCID2018)
    setkey(case.data,FCID_code)
    
    # Join residue data with case data
    case.data <- fcid_table[treat.name == case$treatment][case.data]
    
    # classify slope into two categories
    case.data[, Slope := ifelse(cell_slope < 40, "LT40", "GT40")]
    
    # select relevant columns
    complete_scenario_matrix[, c("ID",
                                 "Silvicultural Treatment",
                                 'Fraction_Piled_Residues', 
                                 'Fraction_Scattered_Residues', 
                                 'Burn Type', 
                                 'Biomass Collection', 
                                 'Pulp Market'):= NULL]
    
    # key data to join
    setkey(case.data, Slope)
    setkey(complete_scenario_matrix, Slope)
    
    # merge with case data
    case.data <- complete_scenario_matrix[case.data] 
    
    # remove scenario matrix
    complete_scenario_matrix <- NULL 
    
    ############################################################################################################
    # We now have a spatially-indexed data table with residue loading information, decay rates, and cell slope
    # Calculate the technically recoverable residue
    ############################################################################################################
    
    # Combine the technically available fractions with the gross resource base.
    case.data[,':='(Recovered_Stem9Plus_tonsAcre = Recovered_Stem9Plus * Stem9Plus_tonsAcre,
                    Recovered_Stem6to9_tonsAcre = Recovered_Stem6to9 * Stem6to9_tonsAcre,
                    Recovered_Stem4to6_tonsAcre = Recovered_Stem4to6 * Stem4to6_tonsAcre,
                    Recovered_Branch_tonsAcre = Recovered_Branch * Branch_tonsAcre,
                    Recovered_Foliage_tonsAcre = Recovered_Foliage * Foliage_tonsAcre,
                    Piled_Stem9Plus_tonsAcre = Piled_Stem9Plus * Stem9Plus_tonsAcre,
                    Piled_Stem6to9_tonsAcre = Piled_Stem6to9 * Stem6to9_tonsAcre,
                    Piled_Stem4to6_tonsAcre = Piled_Stem4to6 * Stem4to6_tonsAcre,
                    Piled_Branch_tonsAcre = Piled_Branch * Branch_tonsAcre,
                    Piled_Foliage_tonsAcre = Piled_Foliage * Foliage_tonsAcre,
                    Scattered_Stem9Plus_tonsAcre = Scattered_Stem9Plus * Stem9Plus_tonsAcre,
                    Scattered_Stem6to9_tonsAcre = Scattered_Stem6to9 * Stem6to9_tonsAcre,
                    Scattered_Stem4to6_tonsAcre = Scattered_Stem4to6 * Stem4to6_tonsAcre,
                    Scattered_Branch_tonsAcre = Scattered_Branch * Branch_tonsAcre,
                    Scattered_Foliage_tonsAcre = Scattered_Foliage * Foliage_tonsAcre
    )]
    
    # Trim out the columns we no longer need.
    #warning("This chunk of code will need to be re-written if we remove bark columns from the scenario matrix")
    case.data[,':='(Slope=NULL, Recovered_Stem9Plus = NULL, Stem9Plus_tonsAcre = NULL, Recovered_Stem6to9 = NULL, Stem6to9_tonsAcre = NULL, Recovered_Stem4to6 = NULL,
                    Stem4to6_tonsAcre = NULL, Recovered_Bark9Plus = NULL, Recovered_Bark6to9 = NULL, Recovered_Bark4to6 = NULL, Recovered_Branch = NULL, Branch_tonsAcre = NULL, 
                    Recovered_Foliage = NULL, Foliage_tonsAcre = NULL, Merchantable_Stem9Plus = NULL, Merchantable_Stem6to9 = NULL, Merchantable_Stem4to6 = NULL,  Merchantable_Bark9Plus = NULL,
                    Merchantable_Bark6to9 = NULL, Merchantable_Bark4to6 = NULL, Merchantable_Branch = NULL, Merchantable_Foliage = NULL, Piled_Stem9Plus = NULL, 
                    Piled_Stem6to9 = NULL, Piled_Stem4to6 = NULL, Piled_Bark9Plus = NULL, Piled_Bark6to9 = NULL, Piled_Bark4to6 = NULL, Piled_Branch = NULL, Piled_Foliage = NULL,
                    Scattered_Stem9Plus = NULL, Scattered_Stem6to9 = NULL, Scattered_Stem4to6 = NULL, Scattered_Bark9Plus = NULL, Scattered_Bark6to9 = NULL,
                    Scattered_Bark4to6 = NULL, Scattered_Branch = NULL, Scattered_Foliage = NULL
    )]
    
    ###################################################################################################################
    # Add wildfire data and emissions factors. These are stored in .rds files by tile and scenario-definition variables
    ###################################################################################################################
    
    wildfire_data <- wildfire_processing_fun(user_inputs[variable == "wildfire_data_directory",value],
                                             scenario_ID, 
                                             case$prescribed.burn.type,
                                             tile_list, # Apply the tile list here to retrieve the correct data
                                             poly_num) 
    
    
    # Address rounding in coordinates
    wildfire_data[,':='(x=round(x,digits=1),y=round(y,digits=1))]
    setkey(wildfire_data,x,y)
    
    ############################################################
    # Prepare columns for Burn/Decay/Wildfire Emissions
    ############################################################
    
    # Combine all mass categories into CWD, FWD, and foliage. The duff column begins empty 
    # There will be several categories with different decay rates - recovered, scattered (including merchantable breakage), field piled, and landing piled
    case.data[,':='(Recovered_CWD_tonsAcre = Recovered_Stem9Plus_tonsAcre + Recovered_Stem6to9_tonsAcre + Recovered_Stem4to6_tonsAcre,
                    Scattered_CWD_tonsAcre = Scattered_Stem9Plus_tonsAcre + Scattered_Stem6to9_tonsAcre + Scattered_Stem4to6_tonsAcre,
                    Piled_CWD_tonsAcre = Piled_Stem9Plus_tonsAcre + Piled_Stem6to9_tonsAcre + Piled_Stem4to6_tonsAcre
    )]
    
    # Clear out old columns, as well as merchantable branch/foliage
    case.data[,':='(Recovered_Stem9Plus_tonsAcre = NULL, Recovered_Stem6to9_tonsAcre = NULL, Recovered_Stem4to6_tonsAcre = NULL,
                    Scattered_Stem9Plus_tonsAcre = NULL, Scattered_Stem6to9_tonsAcre = NULL, Scattered_Stem4to6_tonsAcre = NULL,
                    Piled_Stem9Plus_tonsAcre = NULL, Piled_Stem6to9_tonsAcre = NULL, Piled_Stem4to6_tonsAcre = NULL
    )]
    
    # Convert residue from imperial units to metric; 1 ton equals 0.907185 tonnes
    case.data[,':='(
      Recovered_Branch_tonsAcre = Recovered_Branch_tonsAcre * 0.907185,
      Recovered_Foliage_tonsAcre = Recovered_Foliage_tonsAcre * 0.907185, 
      Piled_Branch_tonsAcre = Piled_Branch_tonsAcre * 0.907185,
      Piled_Foliage_tonsAcre = Piled_Foliage_tonsAcre * 0.907185,
      Scattered_Branch_tonsAcre = Scattered_Branch_tonsAcre * 0.907185,
      Scattered_Foliage_tonsAcre = Scattered_Foliage_tonsAcre * 0.907185,
      Recovered_CWD_tonsAcre = Recovered_CWD_tonsAcre * 0.907185,
      Scattered_CWD_tonsAcre = Scattered_CWD_tonsAcre * 0.907185,
      Piled_CWD_tonsAcre = Piled_CWD_tonsAcre * 0.907185
    )]
    
    # Rename residue into decay class categories and make metric tonnes explicit
    setnames(case.data,
             c("Recovered_Branch_tonsAcre","Recovered_Foliage_tonsAcre","Piled_Branch_tonsAcre","Piled_Foliage_tonsAcre","Scattered_Branch_tonsAcre","Scattered_Foliage_tonsAcre","Recovered_CWD_tonsAcre","Scattered_CWD_tonsAcre","Piled_CWD_tonsAcre"),
             c("Recovered_FWD_tonnesAcre","Recovered_Foliage_tonnesAcre","Piled_FWD_tonnesAcre","Piled_Foliage_tonnesAcre","Scattered_FWD_tonnesAcre","Scattered_Foliage_tonnesAcre","Recovered_CWD_tonnesAcre","Scattered_CWD_tonnesAcre","Piled_CWD_tonnesAcre"))
    
    # Copy the residue columns for Piled and Scattered residues. These will be the initial mass residues in Year 1. We will also need some blank columns to track material exposed to fire but left unburned.
    case.data[,':='(Recovered_CWD_tonnesAcre_INITIAL = Recovered_CWD_tonnesAcre,
                    Recovered_FWD_tonnesAcre_INITIAL = Recovered_FWD_tonnesAcre,
                    Recovered_Foliage_tonnesAcre_INITIAL = Recovered_Foliage_tonnesAcre,
                    Piled_CWD_tonnesAcre_INITIAL = Piled_CWD_tonnesAcre,
                    Piled_FWD_tonnesAcre_INITIAL = Piled_FWD_tonnesAcre,
                    Piled_Foliage_tonnesAcre_INITIAL = Piled_Foliage_tonnesAcre,
                    Scattered_CWD_tonnesAcre_INITIAL = Scattered_CWD_tonnesAcre,
                    Scattered_FWD_tonnesAcre_INITIAL = Scattered_FWD_tonnesAcre,
                    Scattered_Foliage_tonnesAcre_INITIAL = Scattered_Foliage_tonnesAcre
    )]
    
    # Make columns for duff, mass previously exposed to fire but unburnt, and year.i columns.
    case.data[,':='(Duff_tonnesAcre=0, decay_CWD_mass_year_i=0, decay_FWD_mass_year_i=0, decay_Foliage_mass_year_i=0,
                    fire_exposed_CWD_mass_year_i=0, fire_exposed_FWD_mass_year_i=0, fire_exposed_Foliage_mass_year_i=0, fire_exposed_Duff_mass_year_i=0,
                    prev_fired_CWD_tonnesAcre=0, prev_fired_FWD_tonnesAcre=0, prev_fired_Foliage_tonnesAcre=0, prev_fired_Duff_tonnesAcre=0)]
    
    # REMOVED FOR YEAR 0 RUN: Init output data.table that will store run results
    # mass_tracking <- data.table(
    #   In.field.non.char.scattered_tonnes = rep(0,100),
    #   In.field.non.char.piled_tonnes = rep(0,100),
    #   In.field.char.scattered_tonnes = rep(0,100),
    #   In.field.char.piled_tonnes = rep(0,100),
    #   wildfire.burned.residue_tonnes = rep(0,100),
    #   decayed.residue_tonnes = rep(0,100)
    # )
    
    # emissions_tracking <- data.table(
    #   wildfire.CO2_tonnes = rep(0,100), 
    #   wildfire.CO_tonnes = rep(0,100), 
    #   wildfire.CH4_tonnes = rep(0,100), 
    #   wildfire.NOx_tonnes = rep(0,100), 
    #   wildfire.N2O_tonnes = rep(0,100), 
    #   wildfire.PMUnder10um_tonnes = rep(0,100), 
    #   wildfire.PMUnder2.5um_tonnes = rep(0,100), 
    #   wildfire.SO2_tonnes = rep(0,100), 
    #   wildfire.VOC_tonnes = rep(0,100), 
    #   decay.CO2_tonnes = rep(0,100),
    #   decay.CH4_tonnes = rep(0,100)
    # )
    
    # Init data table to track year zero values
    year_0_mass_tracking <- data.table(
      field.residue.removed_tonnes = 0,
      # power.plant.waste_tonnes = 0,
      power.plant.waste.flyash_ash_tonnes = 0,
      power.plant.waste.flyash_char_tonnes = 0,
      broadcast.burned.residue_tonnes = 0,
      pile.burned.residue_tonnes = 0,
      residue.burned.to.electricity_tonnes = 0,
      residue.burned.to.heat_tonnes = 0,
      total.biomass.mobilized_tonnesAcre = 0
    )
    
    year_0_emissions_tracking <- data.table(
      broadcast.burn.CO2_tonnes = 0, 
      broadcast.burn.CO_tonnes = 0, 
      broadcast.burn.CH4_tonnes = 0, 
      broadcast.burn.NOx_tonnes = 0,
      broadcast.burn.N2O_tonnes = 0,
      broadcast.burn.PMUnder10um_tonnes = 0, 
      broadcast.burn.PMUnder2.5um_tonnes = 0, 
      broadcast.burn.SO2_tonnes = 0, 
      broadcast.burn.VOC_tonnes = 0, 
      pile.burn.CO2_tonnes = 0, 
      pile.burn.CO_tonnes = 0, 
      pile.burn.CH4_tonnes = 0, 
      pile.burn.NOx_tonnes = 0,
      pile.burn.N2O_tonnes = 0,
      pile.burn.PMUnder10um_tonnes = 0, 
      pile.burn.PMUnder2.5um_tonnes = 0, 
      pile.burn.SO2_tonnes = 0, 
      pile.burn.VOC_tonnes = 0,
      collection.processing.diesel.CO2_tonnes = 0, 
      collection.processing.diesel.CO_tonnes = 0, 
      collection.processing.diesel.CH4_tonnes = 0, 
      collection.processing.diesel.NOx_tonnes = 0,
      collection.processing.diesel.N2O_tonnes = 0,
      collection.processing.diesel.PMUnder10um_tonnes = 0, 
      collection.processing.diesel.PMUnder2.5um_tonnes = 0, 
      collection.processing.diesel.SO2_tonnes = 0, 
      collection.processing.diesel.VOC_tonnes = 0, 
      transportation.diesel.CO2_tonnes = 0, 
      transportation.diesel.CO_tonnes = 0, 
      transportation.diesel.CH4_tonnes = 0, 
      transportation.diesel.NOx_tonnes = 0,
      transportation.diesel.N2O_tonnes = 0,
      transportation.diesel.PMUnder10um_tonnes = 0, 
      transportation.diesel.PMUnder2.5um_tonnes = 0, 
      transportation.diesel.SO2_tonnes = 0, 
      transportation.diesel.VOC_tonnes = 0, 
      power.plant.for.electricity.CO2_tonnes = 0, 
      power.plant.for.electricity.CO_tonnes = 0, 
      power.plant.for.electricity.CH4_tonnes = 0, 
      power.plant.for.electricity.NOx_tonnes = 0,
      power.plant.for.electricity.N2O_tonnes = 0,
      power.plant.for.electricity.PMUnder10um_tonnes = 0, 
      power.plant.for.electricity.PMUnder2.5um_tonnes = 0, 
      power.plant.for.electricity.SO2_tonnes = 0, 
      power.plant.for.electricity.VOC_tonnes = 0, 
      #    power.plant.for.heat.CH4.offset_tonnes = 0, 
      powerplant.energy.production_MWh = 0,
      powerplant.energy.production_MMBtu = 0
    )
    
    #############################################################################################################################################
    # Calculate emissions associated with case choices 
    
    # set harvest volume variable which gets overwritten if there is residue collection
    harvest_vol <- "none"
    
    if(user_inputs[variable=="biomass_collection",value]!="No") {
      
      # Determine equipment used 
      # This varies on harvest volume, which is determined through average cell residue density
      
      # Calculate harvest/processing emissions, and adjust mass totals.
      density.threshold <- 13 # BDT/acre
      
      # The cell density threshold is applied to the entire study area; if the mean recovered cell density is less than the density threshold, we've got a low volume harvest.
      mean.study.area.density <- sum(case.data[,mean(Recovered_CWD_tonnesAcre)],case.data[,mean(Recovered_FWD_tonnesAcre)],case.data[,mean(Recovered_Foliage_tonnesAcre)])
      
      # If the total residue volume is less than 1000 BDT, we've got a small harvest.
      total.recovered.residue <- case.data[,sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)]
      
      if(mean.study.area.density > density.threshold & total.recovered.residue > 1000) {
        harvest_vol <- "high"
      } else {
        harvest_vol <- "low"
      }
      
      harvest_processing_output <- harvest_processing_fun(case.data, 
                                                          # high_volume_cell_threshold_density,
                                                          case$residue_moisture,
                                                          case$harvest_comminution_opt,
                                                          harvest_vol)
      
      # 3 outputs in harvest_processing_output:
      # harvest_processing_output[[1]] : Collection & Processing emissions
      # harvest_processing_output[[2]]: Transportation emissions
      # harvest_processing_output[[3]]: case.data with addtional columns
      case.data <- copy(harvest_processing_output[[3]])
      
      # The mass_to_plant is the field residue removed. By dividing that mass by the total study land area (nrow(case.data) * cell_to_acres) we get the tonnes/acre
      year_0_mass_tracking[,':='(field.residue.removed_tonnes = sum(case.data[,mass_to_plant_tonnesAcre]) * cell_to_acres,
                                 # total.biomass.mobilized_tonnesAcre = sum(case.data[,mass_to_plant_tonnesAcre]) / poly$ACRES )] - we want this to be based on forested area only, not total study area.
                                 total.biomass.mobilized_tonnesAcre = case.data[,mean(mass_to_plant_tonnesAcre)] )]
      
      # Harvesting and power plant emissions are given in kg, we need them in tonnes
      year_0_emissions_tracking[,':='(
        collection.processing.diesel.CO2_tonnes = harvest_processing_output[[1]]$CO2_kg/1000, 
        collection.processing.diesel.CO_tonnes = harvest_processing_output[[1]]$CO_kg/1000, 
        collection.processing.diesel.N2O_tonnes = harvest_processing_output[[1]]$N2O_kg/1000,
        collection.processing.diesel.CH4_tonnes = harvest_processing_output[[1]]$CH4_kg/1000, 
        collection.processing.diesel.NOx_tonnes = harvest_processing_output[[1]]$NOx_kg/1000, 
        collection.processing.diesel.PMUnder10um_tonnes = harvest_processing_output[[1]]$PMUnder10um_kg/1000, 
        collection.processing.diesel.PMUnder2.5um_tonnes = harvest_processing_output[[1]]$PMUnder2.5um_kg/1000, 
        collection.processing.diesel.SO2_tonnes = harvest_processing_output[[1]]$SOx_kg * equipment_SO2_SOx_fraction / 1000, 
        collection.processing.diesel.VOC_tonnes = harvest_processing_output[[1]]$VOC_kg/1000, 
        
        transportation.diesel.CO2_tonnes = harvest_processing_output[[2]]$CO2_kg/1000, 
        transportation.diesel.CO_tonnes = harvest_processing_output[[2]]$CO_kg/1000, 
        transportation.diesel.N2O_tonnes = harvest_processing_output[[2]]$N2O_kg/1000,
        transportation.diesel.CH4_tonnes = harvest_processing_output[[2]]$CH4_kg/1000, 
        transportation.diesel.NOx_tonnes = harvest_processing_output[[2]]$NOx_kg/1000, 
        transportation.diesel.PMUnder10um_tonnes = harvest_processing_output[[2]]$PMUnder10um_kg/1000, 
        transportation.diesel.PMUnder2.5um_tonnes = harvest_processing_output[[2]]$PMUnder2.5um_kg/1000, 
        transportation.diesel.SO2_tonnes = harvest_processing_output[[2]]$SOx_kg * equipment_SO2_SOx_fraction / 1000, 
        transportation.diesel.VOC_tonnes = harvest_processing_output[[2]]$VOC_kg/1000
      )]
      harvest_processing_output <- NULL
      
      #######################################################################################################################################
      # Calculate power plant emissions.
      #######################################################################################################################################
      
      # calculate emissions and energy associated with power plants
      if(user_inputs[variable=='power_plant_location',value]=='NearestOperationalPlant') { 
        # Multiple potential power plant locations
        power_plant_output <- power_plant_fun(case.data, 
                                              #power_emission_factors, 
                                              unique(case.data[,plant_location]),
                                              user_inputs[variable=="chp_plant", value])
      } else { 
        # Single power plant location
        power_plant_output <- power_plant_fun(case.data, 
                                              #power_emission_factors, 
                                              user_inputs[variable=='power_plant_location',value],
                                              user_inputs[variable=="chp_plant", value])
      }
      
      # power_plant_output has 3 items:
      # power_plant_output[[1]]: power plant emissions
      # power_plant_output[[2]]: power plant enery production
      # power_plant_output[[3]]: power plant CHP heat production
      
      year_0_mass_tracking[,':='(
        
        power.plant.waste.flyash_char_tonnes = power_plant_output[[1]]$flyash_char_kg/1000, # char
        power.plant.waste.flyash_ash_tonnes = power_plant_output[[1]]$flyash_ash_kg/1000, # ash included
        
        residue.burned.to.electricity_tonnes = sum(case.data[,mass_to_plant_tonnesAcre]),
        residue.burned.to.heat_tonnes = power_plant_output[[3]]$power_plant_cogen_mass
      )]
      
      year_0_emissions_tracking[,':='(
        power.plant.for.electricity.CO2_tonnes = power_plant_output[[1]]$CO2_kg/1000, 
        power.plant.for.electricity.CO_tonnes = power_plant_output[[1]]$CO_kg/1000, 
        power.plant.for.electricity.CH4_tonnes = power_plant_output[[1]]$CH4_kg/1000, 
        power.plant.for.electricity.NOx_tonnes = power_plant_output[[1]]$NOx_kg/1000,
        power.plant.for.electricity.N2O_tonnes = power_plant_output[[1]]$N2O_kg/1000,
        power.plant.for.electricity.PMUnder10um_tonnes = power_plant_output[[1]]$PMUnder10um_kg/1000, 
        power.plant.for.electricity.PMUnder2.5um_tonnes = power_plant_output[[1]]$PMUnder2.5um_kg/1000, 
        power.plant.for.electricity.SO2_tonnes = (power_plant_output[[1]]$SOx_kg * plant_SO2_SOx_fraction) / 1000, 
        power.plant.for.electricity.VOC_tonnes = power_plant_output[[1]]$VOC_kg/1000,
        
        # track overall power production
        powerplant.energy.production_MWh = power_plant_output[[2]],
        
        # CHP output; convert to MMBTUs
        powerplant.energy.production_MMBtu = power_plant_output[[3]]$power_plant_cogen_heat / 1000000 # Power plant cogen heat is in BTU.
      )]
      power_emission_factors <- NULL
      power_plant_output <- NULL
    }
    
    # Calculate equivalent emissions from natural gas
    year_0_emissions_tracking[,':=' (
      ng_off_CO2_tonnes = -(powerplant.energy.production_MMBtu * co2) / 1000,
      ng_off_CO_tonnes = -(powerplant.energy.production_MMBtu * co) / 1000,
      ng_off_CH4_tonnes = -(powerplant.energy.production_MMBtu * ch4) / 1000,
      ng_off_NOx_tonnes = -(powerplant.energy.production_MMBtu * nox) / 1000,
      ng_off_N2O_tonnes = -(powerplant.energy.production_MMBtu * n2o) / 1000,
      ng_off_PMUnder2.5um_tonnes = -(powerplant.energy.production_MMBtu * pm2.5) / 1000,
      ng_off_SO2_tonnes = -(powerplant.energy.production_MMBtu * so2) / 1000,
      ng_off_VOC_tonnes = -(powerplant.energy.production_MMBtu * voc) / 1000
    )]
    
    # REMOVED FOR YEAR 0 RUN: Run a loop for each year
    # for (year.i in 1:100) {
    #   mass_tracking[year.i,':='(
    #     In.field.non.char.scattered_tonnes = case.data[,sum(Scattered_CWD_tonnesAcre) + sum(Scattered_FWD_tonnesAcre) + sum(Scattered_Foliage_tonnesAcre) + sum(Duff_tonnesAcre)],
    #     In.field.non.char.piled_tonnes = case.data[,sum(Piled_CWD_tonnesAcre) + sum(Piled_FWD_tonnesAcre) + sum(Piled_Foliage_tonnesAcre)]
    #   )]
    #   
    #   # Year 1 calculates prescribed burn emissions
    #   if(year.i == 1) {
    #     
    #     mass_tracking[year.i,':='(
    #       In.field.char.scattered_tonnes = 0,
    #       In.field.char.piled_tonnes = 0
    #     )]
    #     
    #     # Calculate emission factors, combustion fractions, and char fractions
    #     case.data <- prescribed_burn_processing_fun(case.data,
    #                                                 user_inputs[variable=="wildfire_data_directory",value],
    #                                                 scenario_ID,
    #                                                 tile_list) 
    #     
    #     # Make sure that the residue segment matches correctly with the prescribed burn method
    #     if(user_inputs[variable=='burn_type',value] == "Broadcast") {
    #       prescribed_burn_output <- prescribed_burn_fun(case.data, user_inputs[variable=='burn_type',value])
    #       
    #       # 3 items from prescribed_burn_output:
    #       # prescribed_burn_output[[1]]: Prescribed burn emissions
    #       # prescribed_burn_output[[2]]: Prescribed burn mass
    #       # prescribed_burn_output[[3]]: Updated case.data
    #       
    #       # Update the case data with prescribed_burn_output[[3]]
    #       case.data <- prescribed_burn_output[[3]]
    #       
    #       # Update the emissions data with prescribed_burn_output[[1]]
    #       year_0_emissions_tracking[year.i,':='(
    #         broadcast.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
    #         broadcast.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes,
    #         broadcast.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
    #         broadcast.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
    #         broadcast.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
    #         broadcast.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
    #         broadcast.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
    #         broadcast.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
    #         broadcast.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
    #       )]
    #       
    #       # Add char production from this year to the next year's start-of-year char mass
    #       mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[2]]$char_tonnes]
    #     }
    #     
    #     if(user_inputs[variable=='burn_type',value] == "Pile") {
    #       # warning recorded on github issues
    #       # warning("Residues remaining after pile burning are added to COARSE scattered debris - a fire model inconsistency we should fix in version 1.2")
    #       prescribed_burn_output <- prescribed_burn_fun(case.data, user_inputs[variable=='burn_type',value])
    #       
    #       # 3 items from prescribed_burn_output:
    #       # prescribed_burn_output[[1]]: Prescribed burn emissions
    #       # prescribed_burn_output[[2]]: Prescribed burn mass
    #       # prescribed_burn_output[[3]]: Updated case.data
    #       
    #       # Update the case data with prescribed_burn_output[[3]]
    #       case.data <- prescribed_burn_output[[3]]
    #       
    #       # Update the emissions data with prescribed_burn_output[[1]]
    #       year_0_emissions_tracking[year.i,':='( 
    #         pile.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
    #         pile.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes, 
    #         pile.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
    #         pile.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
    #         pile.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
    #         pile.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
    #         pile.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
    #         pile.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
    #         pile.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
    #       )]
    #       
    #       # Add char production from this year to the next year's start-of-year char mass
    #       mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[2]]$char_tonnes]
    #     }
    #     if(user_inputs[variable=='burn_type',value] == "Pile and Broadcast") {
    #       prescribed_burn_output <- prescribed_burn_fun(case.data, "Broadcast")
    #       
    #       # 3 items from prescribed_burn_output:
    #       # prescribed_burn_output[[1]]: Prescribed burn emissions
    #       # prescribed_burn_output[[2]]: Prescribed burn mass
    #       # prescribed_burn_output[[3]]: Updated case.data
    #       
    #       # Update the case data with prescribed_burn_output[[3]]
    #       case.data <- prescribed_burn_output[[3]]
    #       
    #       # Update the emissions data with prescribed_burn_output[[1]]
    #       year_0_emissions_tracking[year.i,':='(
    #         broadcast.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
    #         broadcast.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes,
    #         broadcast.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
    #         broadcast.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
    #         broadcast.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
    #         broadcast.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
    #         broadcast.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
    #         broadcast.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
    #         broadcast.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
    #       )]
    #       
    #       # Add char production from this year to the next year's start-of-year char mass
    #       mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[2]]$char_tonnes]
    #       
    #       # issue tracked on Github
    #       # warning("Residues remaining after pile burning are added to COARSE scattered debris - a fire model inconsistency we should fix in version 1.2")
    #       
    #       # Pile burning has to happen after scattered debris, else it will be burned twice.
    #       prescribed_burn_output <- prescribed_burn_fun(case.data, "Pile")
    #       
    #       # 3 items from prescribed_burn_output:
    #       # prescribed_burn_output[[1]]: Prescribed burn emissions
    #       # prescribed_burn_output[[2]]: Prescribed burn mass
    #       # prescribed_burn_output[[3]]: Updated case.data
    #       
    #       # Update the case data with prescribed_burn_output[[3]]
    #       case.data <- prescribed_burn_output[[3]]
    #       
    #       # Update the emissions data with prescribed_burn_output[[1]]
    #       year_0_emissions_tracking[year.i,':='( 
    #         pile.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
    #         pile.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes, 
    #         pile.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
    #         pile.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
    #         pile.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
    #         pile.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
    #         pile.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
    #         pile.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
    #         pile.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
    #       )]
    #       
    #       # Add char production from this year to the next year's start-of-year char mass
    #       mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[2]]$char_tonnes]
    #     }
    #     prescribed_burn_output <- NULL
    #     
    #     # Clear out the combustion/char fractions and emissions factors; they will be re-added with wildfire data.
    #     case.data[,':='(CWD_Scattered_CombustionFrac = NULL,
    #                     FWD_Scattered_CombustionFrac = NULL,
    #                     Foliage_Scattered_CombustionFrac = NULL,
    #                     Duff_Scattered_CombustionFrac = NULL,
    #                     CWD_Scattered_CharFrac = NULL,
    #                     FWD_Scattered_CharFrac = NULL,
    #                     Duff_Scattered_CH4_EmFac = NULL,
    #                     Foliage_Scattered_CH4_EmFac = NULL,
    #                     FWD_Scattered_CH4_EmFac = NULL,
    #                     CWD_Scattered_CH4_EmFac = NULL,
    #                     Piled_CH4_EmFac = NULL,
    #                     Duff_Scattered_CO_EmFac = NULL,
    #                     Foliage_Scattered_CO_EmFac = NULL,
    #                     FWD_Scattered_CO_EmFac = NULL,
    #                     CWD_Scattered_CO_EmFac = NULL,
    #                     Piled_CO_EmFac = NULL,
    #                     Duff_Scattered_NOx_EmFac = NULL,
    #                     Foliage_Scattered_NOx_EmFac = NULL,
    #                     FWD_Scattered_NOx_EmFac = NULL,
    #                     CWD_Scattered_NOx_EmFac = NULL,
    #                     Piled_NOx_EmFac = NULL,
    #                     Duff_Scattered_PM10_EmFac = NULL,
    #                     Foliage_Scattered_PM10_EmFac = NULL,
    #                     FWD_Scattered_PM10_EmFac = NULL,
    #                     CWD_Scattered_PM10_EmFac = NULL,
    #                     Piled_PM10_EmFac = NULL,
    #                     Duff_Scattered_PM2.5_EmFac = NULL,
    #                     Foliage_Scattered_PM2.5_EmFac = NULL,
    #                     FWD_Scattered_PM2.5_EmFac = NULL,
    #                     CWD_Scattered_PM2.5_EmFac = NULL,
    #                     Piled_PM2.5_EmFac = NULL,
    #                     Duff_Scattered_SO2_EmFac = NULL,
    #                     Foliage_Scattered_SO2_EmFac = NULL,
    #                     FWD_Scattered_SO2_EmFac = NULL,
    #                     CWD_Scattered_SO2_EmFac = NULL,
    #                     Piled_SO2_EmFac = NULL,
    #                     Duff_Scattered_VOC_EmFac = NULL,
    #                     Foliage_Scattered_VOC_EmFac = NULL,
    #                     FWD_Scattered_VOC_EmFac = NULL,
    #                     CWD_Scattered_VOC_EmFac = NULL,
    #                     Piled_VOC_EmFac = NULL)] 
    #     
    #   } # end of year 1 calculations
    #   
    #   # Figure out the emission factors, combustion fractions, and char fractions for wildfire.
    #   case.data <- annual_fire_detail_fun(case.data, 
    #                                       year.i, 
    #                                       wildfire_data, 
    #                                       user_inputs[variable=="wildfire_probability_directory",value],
    #                                       study_area_raster,
    #                                       poly_num)
    #   
    #   # These should be resolved, but if they are not throw an error
    #   if(sum(is.na(case.data$Wildfire_Probability)) > 0) {
    #     warning("NA values detected in wildfire probability. See line 1155 and issue #14 on Github")
    #   }
    #   
    #   #################################
    #   # Decay
    #   #################################
    #   # Calculate the total decayed mass. "Previously Fired" must be a category; addressing previously fired material decay in each segment will run that decay 3 times over.
    #   decay_output <- decay_fun(case.data,residue.disposition = "Scattered",year.i)
    #   
    #   # 2 items from decay_output:
    #   # decay_output[[1]]: Decay emissions
    #   # decay_output[[2]]: Updated case.data
    #   case.data <- copy(decay_output[[2]])
    #   
    #   # update mass tracking
    #   mass_tracking[year.i, decayed.residue_tonnes := decayed.residue_tonnes + decay_output[[2]][,sum(decay_CWD_mass_year_i) + sum(decay_FWD_mass_year_i) + sum(decay_Foliage_mass_year_i)] * cell_to_acres]
    #   
    #   # update emissions tracking
    #   emissions_tracking[year.i,':='(
    #     decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
    #     decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
    #   )]
    #   
    #   decay_output <- decay_fun(case.data,residue.disposition = "Piled",year.i)
    #   # 2 items from decay_output:
    #   # decay_output[[1]]: Decay emissions
    #   # decay_output[[2]]: Updated case.data
    #   case.data <- decay_output[[2]]
    #   mass_tracking[year.i, decayed.residue_tonnes := decayed.residue_tonnes + decay_output[[2]][,sum(decay_CWD_mass_year_i) + sum(decay_FWD_mass_year_i) + sum(decay_Foliage_mass_year_i)] * cell_to_acres]
    #   emissions_tracking[year.i,':='(
    #     decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
    #     decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
    #   )]
    #   
    #   decay_output <- decay_fun(case.data,residue.disposition = "prev_fired",year.i)
    #   # 2 items from decay_output:
    #   # decay_output[[1]]: Decay emissions
    #   # decay_output[[2]]: Updated case.data
    #   case.data <- decay_output[[2]]
    #   mass_tracking[year.i, decayed.residue_tonnes := decayed.residue_tonnes + decay_output[[2]][,sum(decay_CWD_mass_year_i) + sum(decay_FWD_mass_year_i) + sum(decay_Foliage_mass_year_i)] * cell_to_acres]
    #   emissions_tracking[year.i,':='(
    #     decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
    #     decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
    #   )]
    #   
    #   # Repeat for duff. Because we only run duff decay once, we can address both previously fired and non-fired duff.
    #   decay_output <- duff_decay_fun(case.data,year.i)
    #   # 3 items from duff decay_output:
    #   # decay_output[[1]]: Decay emissions
    #   # decay_output[[2]]: Updated case.data
    #   # decay_output[[3]]: duff mass/acres decayed in year.i
    #   case.data <- decay_output[[2]]
    #   mass_tracking[year.i, decayed.residue_tonnes := decayed.residue_tonnes + decay_output[[3]] * cell_to_acres]
    #   emissions_tracking[year.i,':='(
    #     decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
    #     decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
    #   )]
    #   
    #   decay_output <- NULL
    #   
    #   #################################
    #   # Wildfire
    #   #################################
    #   # Address wildfire emissions and mass loss. 
    #   wildfire_output <- annual_wildfire_fun(case.data,residue.disposition = "Scattered",year.i)
    #   
    #   # 2 items from wildfire_output:
    #   # wildfire_output[[1]]: Wildfire emissions
    #   # wildfire_output[[2]]: Updated case.data
    #   case.data <- wildfire_output[[2]]
    #   
    #   # Add the exposed-to-fire mass to wildfire.burned.residue
    #   mass_tracking[year.i, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + wildfire_output[[2]][,sum(fire_exposed_CWD_mass_year_i) + sum(fire_exposed_FWD_mass_year_i) + sum(fire_exposed_Foliage_mass_year_i)] * cell_to_acres]
    #   
    #   # Add char produced this year to the starting char for the next year. Skip for year 100.
    #   if(year.i < 100) {
    #     mass_tracking[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + wildfire_output[[1]]$char_tonnes]
    #   }
    #   
    #   emissions_tracking[year.i,':='(
    #     wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
    #     wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
    #     wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
    #     wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
    #     wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
    #     wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
    #     wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
    #     wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
    #     wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
    #   )]
    #   
    #   # issue tracked on Github              
    #   # if(year.i==100) {warning("ALL pile residues remaining after wildfire are added to COARSE previously fired debris - a fire model inconsistency we should fix in version 1.2")}
    #   wildfire_output <- annual_wildfire_fun(case.data,residue.disposition = "Piled",year.i)
    #   # 2 items from wildfire_output:
    #   # wildfire_output[[1]]: Wildfire emissions
    #   # wildfire_output[[2]]: Updated case.data
    #   case.data <- wildfire_output[[2]]
    #   
    #   # Add the exposed-to-fire mass to wildfire.burned.residue
    #   mass_tracking[year.i, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + wildfire_output[[2]][,sum(fire_exposed_CWD_mass_year_i) + sum(fire_exposed_FWD_mass_year_i) + sum(fire_exposed_Foliage_mass_year_i)] * cell_to_acres]
    #   
    #   # Add char produced this year to the starting char for the next year. Skip for year 100.
    #   if(year.i < 100) {
    #     mass_tracking[year.i+1, In.field.char.piled_tonnes := In.field.char.piled_tonnes + wildfire_output[[1]]$char_tonnes]
    #   }
    #   
    #   emissions_tracking[year.i,':='(
    #     wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
    #     wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
    #     wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
    #     wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
    #     wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
    #     wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
    #     wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
    #     wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
    #     wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
    #   )]
    #   
    #   wildfire_output <- duff_annual_wildfire_fun(case.data,year.i)
    #   # 2 items from wildfire_output:
    #   # wildfire_output[[1]]: Wildfire emissions
    #   # wildfire_output[[2]]: Updated case.data
    #   case.data <- wildfire_output[[2]]
    #   
    #   # Add the exposed-to-fire mass to wildfire.burned.residue
    #   mass_tracking[year.i, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + wildfire_output[[2]][,sum(fire_exposed_Duff_mass_year_i)] * cell_to_acres]
    #   
    #   emissions_tracking[year.i,':='(
    #     wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
    #     wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
    #     wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
    #     wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
    #     wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
    #     wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
    #     wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
    #     wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
    #     wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
    #   )]
    #   
    #   wildfire_output <- NULL
    #   
    #   # Clear out the combustion/char fractions and emissions factors; they will be re-added next year.
    #   case.data[,':='(Wildfire_Probability = NULL, CWD_Scattered_CombustionFrac = NULL, FWD_Scattered_CombustionFrac = NULL, Foliage_Scattered_CombustionFrac = NULL, Duff_Scattered_CombustionFrac = NULL, 
    #                   CWD_Scattered_CharFrac = NULL, FWD_Scattered_CharFrac = NULL, 
    #                   Duff_Scattered_CH4_EmFac = NULL, Foliage_Scattered_CH4_EmFac = NULL, FWD_Scattered_CH4_EmFac = NULL, CWD_Scattered_CH4_EmFac = NULL, Piled_CH4_EmFac = NULL,
    #                   Duff_Scattered_CO_EmFac = NULL, Foliage_Scattered_CO_EmFac = NULL, FWD_Scattered_CO_EmFac = NULL, CWD_Scattered_CO_EmFac = NULL, Piled_CO_EmFac = NULL,
    #                   Duff_Scattered_NOx_EmFac = NULL, Foliage_Scattered_NOx_EmFac = NULL, FWD_Scattered_NOx_EmFac = NULL, CWD_Scattered_NOx_EmFac = NULL, Piled_NOx_EmFac = NULL, 
    #                   Duff_Scattered_PM10_EmFac = NULL, Foliage_Scattered_PM10_EmFac = NULL, FWD_Scattered_PM10_EmFac = NULL, CWD_Scattered_PM10_EmFac = NULL, Piled_PM10_EmFac = NULL, 
    #                   Duff_Scattered_PM2.5_EmFac = NULL, Foliage_Scattered_PM2.5_EmFac = NULL, FWD_Scattered_PM2.5_EmFac = NULL, CWD_Scattered_PM2.5_EmFac = NULL, Piled_PM2.5_EmFac = NULL,
    #                   Duff_Scattered_SO2_EmFac = NULL, Foliage_Scattered_SO2_EmFac = NULL, FWD_Scattered_SO2_EmFac = NULL, CWD_Scattered_SO2_EmFac = NULL, Piled_SO2_EmFac = NULL, 
    #                   Duff_Scattered_VOC_EmFac = NULL, Foliage_Scattered_VOC_EmFac = NULL, FWD_Scattered_VOC_EmFac = NULL, CWD_Scattered_VOC_EmFac = NULL, Piled_VOC_EmFac = NULL)] 
    # } # end of 100 year loop
    
    # list outputs into a single file to return - COMMENTED OUT MASS AND EMISSIONS TRACKING FOR YEAR 0 RUN
    list_output <- list(#"mass tracking" = mass_tracking,
                        #"emissions tracking" = emissions_tracking,
                        "year 0 mass tracking" = year_0_mass_tracking,
                        "year 0 emissions tracking" = year_0_emissions_tracking)
    
    # extract cbrec specific model 
    #cbrec_vars <- subset(user_inputs, variable %in% c("residue_moisture", "harvest_comminution_opt" ))
    
    # save output
    saveOutput(out_folder,
               poly_num,
               list_output,
               case$ID,
               case$residue_moisture,
               case$harvest_comminution_opt,
               harvest_vol,
               plant_location)
    
  }) # end of future.apply wrapper
  
  gc()
}) # end of lapply

end_time <- Sys.time() - start

# write.csv(t(data.frame("time" = end_time,
#              "runs" = length(polys),
#              "time per poly" = end_time / length(polys),
#              "workers" = works)),
#           paste0(Sys.time(), "metrics.csv"))