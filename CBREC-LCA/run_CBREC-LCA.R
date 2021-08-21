#!/usr/bin/Rscript --vanilla

#****************************************************************************************
# AUTHORS: Andrew Harris
#          Max Blasdel
#          Jerome Carman
#          Chih-Wei Hsu
# 
# Schatz Energy Research Center
# Humboldt State University 
#____________________________________________________________________________
# VERSION:
#
# 1.0    2019-07-26: Ported from .Rmd to .R command line tool
# 1.1    2019-09-17: Features added for report analysis
# 1.1.1  2020-01-25: Some features added as documented in GitHub, plus significant
#                    cleanup and re-structuring of some functions
# 1.2    2020-05-25: Many additional features added as documented on GitHub. Version
#                    used for LCA Analysis report and statewide run for webtool.
# 1.2.1  2021-08-20: Final cleaning up comments for public release, and very minor edits
#____________________________________________________________________________
# OBJECTIVE:
# This script will run the C-BREC model on input data specified by the user in 
# the external input file.
#
#______________________________
# ACTION ITEMS:
# Index   Description                                                      Status
#_____________________________________________________________________________
#****************************************************************************************

##############################    NOTES ON HOW CBREC RUNS   ###########################################

# 
# CBREC involves a lot of data preparation to run
# This data is both spatial, depending on the project area, and tabular, depending on which case is being run
# 
# CBREC is currently set up in three large loops (future_lapply(), lapply(), for() loop)
#
# 1) The future_lapply() loop loads spatially explicit data for each polygon.
#    The model parallizes on this loop and will run multiple polygons at the same time equal to the number of cores specified. 
# 
# 2) The lapply() applies each case, as defined by the scenario matrix, to each polygon.
#    Additionally, each power plant choice and harvest and processing choice is applied within each case
#
# 3) The for() loop is the time series over 100 years.
#    This runs each year of the model for the wildfire and decay
#
# Currently outputs a single file as a nested list of each polygon with all specified cases
# A more detailed illustration of this data structure is available on Github
#________________________________________________________________________________________________________

##############################################
#### LOAD LIBRARIES, FUNCTIONS, CONSTANTS ####
##############################################
setwd("/media/spin/Github/CBREC") # Automate this with gregexpr() and substr()

# Libraries ---------------------------------------------
if(!any(rownames(installed.packages()) %in% "rgdal")) {
  install.packages("rgdal") # rgdal dependency to raster package
}

# If desired, uncomment to fully clear all non-base packages that are loaded to avoid "monkey patch" conflicts across packages
#   Iteratively call unloadPackages() in order to capture packages that couldn't be unloaded because of dependency by other loaded packages
#' @source Answer by petzi at https://stackoverflow.com/questions/55655162/unload-all-loaded-packages
# unloadPackages <- function() {
#   lapply(names(sessionInfo()$otherPkgs), function(pkgs) {
#     detach(
#       paste0('package:', pkgs),
#       character.only = T,
#       unload = T,
#       force = T
#     )
#   })
# }
# unloadErrors <- unloadPackages()
# while(length(unloadErrors)>0) {
#   unloadErrors <- unloadPackages()
# }

suppressPackageStartupMessages(c(library("optparse"), # command line interface functions
                                 library("raster"), # spatial data manipulation
                                 library('data.table'), # main structure of code and data format
                                 library('future.apply'), # for running in parallel
                                 library('sf'), # reading spatial objects, faster than OGR
                                 library('tictoc'))) # time testing package; very simple

# Source modules and helper functions -------------------
source("CBREC-LCA/functions/blackCarbon.R")
source("CBREC-LCA/functions/decay-functions.R")
source("CBREC-LCA/functions/harvest-processing.R")
source("CBREC-LCA/functions/load-shapefile.R") # loads shapefiles from a specified folder
source("CBREC-LCA/functions/misc-functions.R")
source("CBREC-LCA/functions/natural-gas.R") # emissions factors for NG
source('CBREC-LCA/functions/power-plant-functions.R')
source("CBREC-LCA/functions/prescribed-burn-functions.R")
source("CBREC-LCA/functions/rasterDataTable.R") # script to optimize converting rasters to data.table
source("CBREC-LCA/functions/updateYearZero.R") # applies harvest operation values to all year zero scenarios
source('CBREC-LCA/functions/wildfire-functions.R')

# Define dataframe and raster data tables
if (!isGeneric("as.data.table")) {
  setGeneric("as.data.table", function(x, ...)
    standardGeneric("as.data.table"))
}  
setMethod('as.data.table', signature(x='data.frame'), data.table::as.data.table)
setMethod('as.data.table', signature(x='Raster'), as.data.table.raster)

# Constants ----------------------------------------------
source("CBREC-LCA/functions/constants.R")

##############################################
####       USER DEFINED VARIABLES        ####
##############################################

# Misc user-defined variables --------------------------------------
seed_val_to_set <- 2 # Used for debugging so sample() produces same result so multiple runs can be compared

# Remove tiles with no fire data that will cause an error
#   Does not impact results as no fire data means no forested area.
no_data_tiles <<- gsub(paste0(getwd(),"CBREC-Fire/output/emissions/"),"",
                       system(paste0("find ",getwd(),"CBREC-Fire/output/emissions/ -type d -empty"), intern=T))

# general options
options(stringsAsFactors = F)
options(future.fork.enable = T) # Allows parallelization with forked instances of R, or forked R sessions

# Command Line Options -----------------------------------------------
option_list <<- list(
  make_option(c("-v", "--verbose"), 
              action="store_true", 
              default=F, 
              help="Print extra output [default %default]"),
  make_option(c("-i", "--infile"), 
              type="character", 
              default='CBREC-LCA/input/CBREC-LCA_input_filepaths.csv', 
              help="CSV file specifying user inputs [default \"%default\"]",
              metavar="input-file"),
  make_option(c("-d", "--debug"), 
              action="store_true", 
              default=F, 
              help="Print extra output [default %default]"),
  make_option(c("-c","--cores"), 
              type="integer", 
              default=parallel::detectCores()/2, 
              help="Specify number of CPU cores to utilize [default %default]"),
  make_option(c("-u","--useactivitycodes"), 
              action="store_true", 
              default=F, 
              help="If running project polygons with specified treatment activity codes that you want to use, indicate this as TRUE [default %default]"),
  make_option(c("-o","--outdir"), 
              type="character", 
              default="CBREC-LCA/output/default-output-dir/", 
              help="Specify absolute or relative output directory path [default %default]")
)
if(interactive()){ # Manually set input arguments if running R interactively vs. command line
  input_args <<- parse_args(OptionParser(option_list = option_list))
  input_args$verbose <- F
  input_args$infile <- 'CBREC-LCA/input/CBREC-LCA_input_filepaths.csv'
  input_args$debug <- F
  input_args$cores <- 28
  input_args$useactivitycodes <- F
  input_args$outdir <- "CBREC-LCA/output/default-output-dir/"
}else{
  input_args <<- parse_args(OptionParser(option_list = option_list))
  if(!file.exists(input_args$infile)) {
    stop("Input file does not exist. Check filepath.")
  }
}
user_input_filepath <<- input_args$infile
debug_CBREC <<- input_args$debug
plan(multiprocess, workers = input_args$cores) # Set future plan; will use multicore when available
use_timber_harvest_treatments <<- input_args$useactivitycodes # State whether you want to apply a treatment code within project_polygons_filepath shapefile if it contains one
out_folder <<- input_args$outdir # Specify output directory that will store results and logs
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
  dir.create(paste0(out_folder,"runlogs/"))
  dir.create(paste0(out_folder,"results/"))
}

# Log File and Debugging settings ----------------------------------------------------
#   Initialize log file to catch all warnings, errors, and debugging output
#   Not yet fully implemented throught the code
log_file <<- file(paste0(out_folder, "runlogs/CBREC-LCA_RunLog_", format(Sys.time(), "%d-%b-%Y_%H-%M"), ".log"), open = 'a')
cat(paste0("Initializing CBREC-LCA: ",Sys.time(),"\n"), file = log_file)
cat(paste0("\nUsing the following input arguments:\n",
           "  Verbose = ",as.character(input_args$verbose),"\n",
           "  User Inputs File = ",input_args$infile,"\n",
           "  Debug = ",as.character(input_args$debug),"\n",
           "  Cores = ",as.character(input_args$cores),"\n",
           "  Timestep = ",as.character(input_args$timestep),"\n",
           "  Use Activity Codes = ",as.character(input_args$useactivitycodes),"\n",
           "  Output Directory = ",input_args$outdir,"\n"),
    file=log_file, append=T)

##############################################
####       LOAD ALL INPUT DATA            ####
##############################################

# Load all raw data, or if desired separately create and load RData to reduce load time
load_initial_data(user_input_filepath, no_data_tiles)

# Subset list of project polygons if debugging to reduce runtime
if(debug_CBREC) {
  set.seed(seed_val_to_set) # set seed to allow repeated results
  nPolys <<- input_args$cores # Reduce the number of all_project_polygons to keep debug runtime short; should be a multiple of the n() of workers to get accurate estimates
  all_project_polygons <<- all_project_polygons[as.integer(sample(1:length(all_project_polygons),nPolys,replace=FALSE))] #Restrict number of polygons for debuging
  
  nCases <<- "All" # Reduce the number of cases to run to keep debug runtime short. Enter an explicit integer (i.e. as.integer(26)), or "All" to run all cases.
  
  saveResults <<- T # Set to false to not write results to disk. Debug always returns results to environment variable "polygon_results".
  
  cat(paste0("Running the following polygons\n",as.character(all_project_polygons),"\n"),file=log_file, append=T)
  cat(paste0("Running ",as.character(nCases)," cases\n"),file=log_file, append=T)
  cat(paste0("Will results be saved to disk? ",as.character(saveResults),"\n"),file=log_file, append=T)
}

# Clean up environment before parallelization
rm(option_list,
   user_input_filepath)

# In case there was a crash, remove polygons that have already been run from the list
# Only applicable if the model hits an error and needs to be restarted WITHOUT rewriting polys that have been run
polys_done <- tools::file_path_sans_ext(dir(paste0(out_folder, "/results")))
all_project_polygons <- all_project_polygons[!names(all_project_polygons) %in% polys_done]

##############################################
####       START CBREC-LCA MODEL          ####
##############################################
tic(msg = "Entire Model")

if(debug_CBREC) {
  cat("\nStarting CBREC-LCA Model Calculations\n", file = log_file, append = T)
}

# lapply over project polygons. Within each lapply, lapply over all cases
polygon_output <- future_lapply(all_project_polygons, function(project_polygon) {
  
  if(debug_CBREC) {
    cat(paste0("\nRunning project polygon ID ",as.character(project_polygon$OBJECTID),"\n"), file=log_file, append=T)
  }
    
  # sourcing this function within the future
  # otherwise there are issues with the 'setgeneric' method
  source("CBREC-LCA/functions/rasterDataTable.R") # script to optimize converting rasters to data.table

  # ****************************************************************************************************
  # Spatial data preparation
  # ****************************************************************************************************
  # get project_polygon number
  poly_num <- project_polygon$OBJECTID

  # Find the fire data tiles that overlap with the polygon(s) of interest
  study_area_tiles <-suppressWarnings(st_intersection(fire_tile_full, project_polygon)) 
 
  # get a list of fire data tiles by ID attribute for loading the fire data
  tile_list <- unique(study_area_tiles$ID)
  
  # Crop all spatial data to the study area
  #   
  project_FCIDs <- crop_to_polygon(proj_polygon = project_polygon,
                                   tile_num = tile_list,
                                   study_area = Study_Area_Raster,
                                   filepath = user_inputs[variable == "pp_decay_wfprob_data_directory", value])
  
  # merge biomass_properties data table and remove FCIDs with no biomass properties
  setkey(project_FCIDs, FCID_code)
  setkey(biomass_properties, FCID_code)
  project_FCIDs <- biomass_properties[project_FCIDs]
  project_FCIDs <- project_FCIDs[!is.na(Carbon_frac) & !is.na(Ash_frac) & !is.na(HHV), ]
  
  # Re-key
  setkey(project_FCIDs, x, y)
  
  # merge power plant location data for the nearest power plant
  setkey(location_names, Nearest_Plant_ID)
  setkey(project_FCIDs, Nearest_Plant_ID)
  project_FCIDs <- location_names[project_FCIDs]
  project_FCIDs[, Nearest_Plant_ID := NULL] # drop unneeded column
  plant_location <- unique(project_FCIDs$plant_location) # set power plant name to append to file name

  # ****************************************************************************************************
  # Define case list to run
  #   If running 2018 THPs, subset case_list to primary treatment applicable to the project_polygon being run
  #   Else, run all case_list
  #   Furthermore, if debugging, just run a few cases to keep runtime short
  # ****************************************************************************************************
  if(use_timber_harvest_treatments == T) {
    case_list_toapply <- subset(case_list, pulp.market == "No" & treatment == project_polygon$treat_code) # This will break if polygon does not have $treat_code field
  } else {
    case_list_toapply <- case_list # Else use full case list as defined by user_inputs
  }
  
  if(debug_CBREC) { # Reduces number of cases to run if debugging
    if(is.integer(nCases)) {
      if(nCases > nrow(case_list_toapply)) {
        stop(paste0("nCases value of ",as.character(nCases)," is greater than length of case_list of ",as.character(length(case_list_toapply))))
      }
      set.seed(seed_val_to_set) # set seed to allow repeated results
      case_list_toapply <- case_list_toapply[as.integer(sample(1:length(case_list_toapply),nCases,replace=FALSE))] # Randomly select cases
    } else if(nCases != "All") {
      stop(paste0("nCases value of ",as.character(nCases)," is invalid."))
    }
  }
  
  case_list_toapply[, MYID := seq(1,nrow(case_list_toapply))] # create variable to split data on
  case_list_toapply <- split(case_list_toapply, case_list_toapply$MYID) # split into list for each row

  # ****************************************************************************************************
  # lapply each case in case_list_toapply --------------------------------------------------------------
  # ****************************************************************************************************
  polygon_results <- lapply(case_list_toapply, function(case) {
                
    #Loop through for each case to be analyzed
    stopifnot(nrow(case) == 1)
    
    # set scenario ID for use in naming and fire data retrival
    #   This pulls the "first" burn fire data.
    scenario_ID <-
      paste(case$treatment,
            case$prescribed.burn.type,
            gsub("%","",case$frac.piled),
            gsub("%","",case$frac.scattered),
            "first",
            case$biomass.collection,
            case$pulp.market,
            case$ID,
            sep = "-")
    
    if(debug_CBREC) {
      cat(paste0("\nRunning case ID: ", scenario_ID, "\n"), file = log_file, append = T)
    }
    
    #***************************************************************************************************
    # Initialize case.data which is a spatially-indexed data table that contains the following:
    # - Residue loading aggregated by C-BREC size classes
    # - Biomass physical properties such as decay rate, carbon content, ash content, and HHV
    # - Initialized columns for duff, mass previously exposed to fire but unburnt, and year.i columns
    # - Fire probabilities
    # Each row in the data table represents a 30m x 30m cell within the project boundary
    # This table is dynamically updated within this lapply
    #***************************************************************************************************
    case.data <- initialize_case_table(case, project_FCIDs)
    
    #***************************************************************************************************
    # Initialize Output Data Tables: These store data that will ultimately be written to disk
    # results_tracker list contains the following data tables:
    #   - treatment: all year-0 values associated with residue mobilization
    #   - postTreatment: time series of all non-treatment emissions species by source
    #***************************************************************************************************
    results_tracker <- initialize_output_tables()

    #***************************************************************************************************
    # Load Wildfire Data and Emissions Factors Specifically for This Case
    # The data that are loaded are stored in .rds files by tile and scenario-definition variables
    #***************************************************************************************************
    wildfire_data <- wildfire_processing_fun(wildfire.data.directory = user_inputs[variable == "wildfire_data_directory",value],
                                             scenario.ID = scenario_ID,
                                             prescribed.burn = case$prescribed.burn.type,
                                             tile.list = tile_list, # Apply the tile list here to retrieve the correct data
                                             poly_num = poly_num) 
    setkey(wildfire_data, x, y)
    
    # There will be cells where we have residue data but not fire data. This is due to a discrepancy between residue data and FCCS data used for fire emissions, where residue data says there are trees and FCCS disagrees. For fire modeling, 
    # we side with FCCS. There are rare cases where there is no fire data for given polygon. Check for this, and return NULL if no fire data.
    #   Ideally we check for this before we hit this lapply to avoid looping through all cases, but this will have to do for now.
    if(nrow(wildfire_data[x %in% case.data[,x] & y %in% case.data[,y],]) == 0) {
      return(NULL)
    }
    
    #***************************************************************************************************
    # Residue Mobilization Emissions Calculations ------------------------------------------------------
    #***************************************************************************************************
    if(case$biomass.collection != "No") { # Don't run if the particular case does not have residue collection
      
      if(debug_CBREC) {
        cat("Running residue mobilization calculations for year 0\n", file=log_file, append=T)
      }
      
      # calculate the number of individual harvests in each study area
      # this is used to determine a multiplier for equipment hauling emissions
      ind_harvests <- as.integer(nrow(case.data) * acres_per_cell / ave_harvest_acres)
      if (ind_harvests < 1) {ind_harvests = 1}
      
      # Harvest and Processing Emissions ----------------------------------------------------------------
      # Run harvest processing for each combination moisture, comminution, and harvest volume
      harPro_green_chip <- harvest_processing_fun(cbrec.dt = case.data,
                                                  equipment_emissions = equipment_emissions,
                                                  moisture =  "Green",
                                                  comminution =  "Chip",
                                                  harvest_vol =  "high",
                                                  harvest_trans_point = T,
                                                  case = case,
                                                  ind_harvests = ind_harvests)
      harPro_green_grind <- harvest_processing_fun(cbrec.dt = case.data,
                                                   equipment_emissions = equipment_emissions,
                                                   moisture =  "Green",
                                                   comminution =  "Grind",
                                                   harvest_vol =  "high",
                                                   harvest_trans_point = T,
                                                   case = case,
                                                   ind_harvests = ind_harvests)
      harPro_dry_grind <- harvest_processing_fun(cbrec.dt = case.data,
                                                 equipment_emissions = equipment_emissions,
                                                 moisture =  "Dry",
                                                 comminution =  "Grind",
                                                 harvest_vol =  "high",
                                                 harvest_trans_point = T,
                                                 case = case,
                                                 ind_harvests = ind_harvests)
      harPro_low <- harvest_processing_fun(cbrec.dt = case.data,
                                           equipment_emissions = equipment_emissions,
                                           moisture =  "Green",
                                           comminution =  "Chip",
                                           harvest_vol =  "Low",
                                           harvest_trans_point = T,
                                           case = case,
                                           ind_harvests = ind_harvests)
      
      # Add collection and processing results to results_tracker.
      results_tracker$treatment$CandP <- updateYearZero(data.table(),
                                                        harPro_dry_grind,
                                                        harPro_green_chip,
                                                        harPro_green_grind,
                                                        harPro_low)
      
      # set names of new data.tables
      results_tracker$treatment$CandP <- setNames(results_tracker$treatment$CandP,
                                                  c("CandP_dry_grind",
                                                    "CandP_green_chip",
                                                    "CandP_green_grind",
                                                    "CandP_low")
                                                  )
      
      # calculate black carbon for collection and processing
      # dt is updated though reference and invisible suppresses a print call to the console
      invisible(lapply(results_tracker$treatment$CandP, calcBlackCarbon, "onroaddiesel-onroad"))
      invisible(lapply(results_tracker$treatment$CandP, calcBlackCarbon, "onroaddiesel-offroad"))
      invisible(lapply(results_tracker$treatment$CandP, calcBlackCarbon, "offroaddiesel"))

      # Power Plant Emissions --------------------------------------------------------------------------
      
      # Record mass of residues delivered to power plant
      case.data[,mass_to_plant_tonnesAcre := 0] # Initialize
      case.data[,mass_to_plant_tonnesAcre := Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre] # Sum all recovered size classes
      results_tracker$treatment$field.residue.removed_tonnes = sum(case.data[,mass_to_plant_tonnesAcre]) * acres_per_cell # Record mass to plant in tracker
      results_tracker$treatment$total.biomass.mobilized_tonnesAcre = case.data[,mean(mass_to_plant_tonnesAcre)] # Record average mobilized residue density per acre (over entire project area) in tracker
      
      # Remove un-needed columns
      case.data[,':='(Recovered_CWD_tonnesAcre = NULL,
                      Recovered_FWD_tonnesAcre = NULL,
                      Recovered_Foliage_tonnesAcre = NULL)]
      
      # Calculate Power Plant Emissions
      #   calculate emissions and energy associated with power plant
      #   Running the nearest power plant and all generic power plant technologies
      pp_output <- copy(power_plant_fun(
        cbrec.dt = case.data,
        power.emissions.factors = power_emission_factors,
        power.plant = unique(case.data[, plant_location])
      ))

      # calculate black carbon and natural gas offset bc
      # invisible suppresses console print as change made by reference
      invisible(lapply(pp_output, calcBlackCarbon, em_src = "powerplant"))
      invisible(lapply(pp_output, calcBlackCarbon, em_src = "ng_offset"))
      
      # Re-set plant_location column as this is overwritten within power_plant_fun()
      setkey(case.data,x,y)
      setkey(project_FCIDs,x,y)
      case.data <- case.data[,plant_location:=project_FCIDs[,plant_location]]
      
      # Add power plant emissions to results_tracker
      results_tracker$treatment$PP <- pp_output
      
    } else { # Explicitly indicate that there are no CandP or PP emissions
      results_tracker$treatment$CandP <- 0
      results_tracker$treatment$PP <- 0
    }# end biomass collection != No

    # Time Series --------------------------------------------------------------------------------------
    # Prescribed Burn, Decay, and Wildfire Emissions Time Series Calculations
    #***************************************************************************************************
    tic(msg = "Time Series")
    if(debug_CBREC) {
      cat("Calculating prescribed burn, wildfire, and decay emissions\n", file=log_file, append=T)

    }

    # Run Loop for each year ---------------------------------------------------------------------------
    for (year.i in seq(1,100,1)) {
      
      # specifiy the row num year.i will be on
      dt_rn <- year.i # for one-year interval, year is the row number
      
      # Calculate amount of scattered and piled residues at the beginning of the year
      results_tracker$postTreatment[dt_rn,':='(
        In.field.non.char.scattered_tonnes = case.data[,sum(Scattered_CWD_tonnesAcre) + sum(Scattered_FWD_tonnesAcre) + sum(Scattered_Foliage_tonnesAcre) + sum(Duff_tonnesAcre)] * acres_per_cell,
        In.field.non.char.piled_tonnes = case.data[,sum(Piled_CWD_tonnesAcre) + sum(Piled_FWD_tonnesAcre) + sum(Piled_Foliage_tonnesAcre)] * acres_per_cell
      )]
      
      # Year 1 -----------------------------------------------------------------------------------------
      # Calculate prescribed burn emissions for year 1
      # Note that structure of prescribed_burn_output is the following:
      #   prescribed_burn_output[[1]]: Prescribed burn emissions
      #   prescribed_burn_output[[2]]: Prescribed burn mass
      #   prescribed_burn_output[[3]]: Updated case.data
      if(year.i == 1) {
        
        if(debug_CBREC) {
          cat("Running year 1 emissions calculations\n", file = log_file, append = T)
        }

        if(case$prescribed.burn.type != "None") {
          # Calculate emission factors, combustion fractions, and char fractions for all prescribed burn options for the particular case
          #   Uses output from CBREC-Fire model
          case.data <- prescribed_burn_processing_fun(cbrec.dt = case.data,
                                                      wildfire.data.directory = user_inputs[variable=="wildfire_data_directory",value],
                                                      scenario.ID = scenario_ID,
                                                      tile.list = tile_list)
        }
        
        # Prescribed burn emissions ---------------------------------------------------------------------
        if(case$prescribed.burn.type == "Broadcast") { # Calculate broadcast burn emissions, if this type of burn is applied
          # Broadcast -----------------------------------------------------------------------------------
          if(debug_CBREC) {
            cat("   Calculating prescribed burn emissions: Broadcast Burn\n", file=log_file, append=T)
          }
          
          # Calculate prescribed burn emissions
          prescribed_burn_output <- prescribed_burn_fun(case.data, case$prescribed.burn.type)
          
          # Update the case data with prescribed_burn_output[[3]]
          case.data <- prescribed_burn_output[[3]]
          
          # Add emissions data to results_tracker with prescribed_burn_output[[1]]
          results_tracker$treatment$BroadcastBurn <- data.table(
            broadcast.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
            broadcast.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes,
            broadcast.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
            broadcast.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
            broadcast.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction + prescribed_burn_output[[1]]$N2O_tonnes,
            broadcast.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
            broadcast.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
            broadcast.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
            broadcast.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
          )
          results_tracker$treatment$PileBurn <- 0 # Explicitly note that there are no pile burn emissions
          
          # Add char production from this year to the next year's start-of-year char mass with prescribed_burn_output[[2]]
          results_tracker$postTreatment[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[2]]$char_tonnes]
          
          # Add broadcast burn mass with prescribed_burn_output[[2]]
          results_tracker$treatment$BroadcastBurn[,broadcast.burned.residue_tonnes := prescribed_burn_output[[2]]$combusted.residue_tonnes]
          
          # calculate black carbon from prescribed burn type
          calcBlackCarbon(results_tracker$treatment$BroadcastBurn, em_src = "rx_burn_broadcast")
          
          # Remove un-needed variable
          prescribed_burn_output <- NULL
          
          # Clear out the combustion/char fractions and emissions factors; they will be re-added with wildfire data.
          #   firecols set in constants define column names to be removed
          set(x = case.data, j = firecols, value = NULL)
          
        } else if(case$prescribed.burn.type == "Pile") { # Calculate pile burn emissions, if this type of burn is applied
          # Pile -----------------------------------------------------------------------------------------
          if(debug_CBREC) {
            cat("   Calculating prescribed burn emissions: Pile Burn\n", file=log_file, append=T)
            cat("\nWARNING: Residues remaining after pile burning are added to COARSE scattered debris - a fire model inconsistency we should fix in a later version\n",file=log_file,append=T)
          }
          
          # Calculate prescribed burn emissions
          prescribed_burn_output <- prescribed_burn_fun(case.data, case$prescribed.burn.type)
          
          # Update the case data with prescribed_burn_output[[3]]
          case.data <- prescribed_burn_output[[3]]
          
          # Update the emissions data with prescribed_burn_output[[1]]
          results_tracker$treatment$PileBurn <- data.table( 
            pile.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
            pile.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes, 
            pile.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
            pile.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
            pile.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction + prescribed_burn_output[[1]]$N2O_tonnes,
            pile.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
            pile.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
            pile.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
            pile.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
          )
          results_tracker$treatment$BroadcastBurn <- 0 # Explicitly note that there are no broadcast burn emissions
          
          # Add char production from this year to the next year's start-of-year char mass with prescribed_burn_output[[2]]
          results_tracker$postTreatment[year.i + 1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[2]]$char_tonnes]
          
          # Add piled burn mass with prescribed_burn_output[[2]]
          results_tracker$treatment$PileBurn[,pile.burned.residue_tonnes:= prescribed_burn_output[[2]]$combusted.residue_tonnes]
        
          # calculate black carbon from prescribed burn type
          calcBlackCarbon(results_tracker$treatment$PileBurn, em_src = "rx_burn_pile")
          
          # Remove un-needed variable
          prescribed_burn_output <- NULL
          
          # Clear out the combustion/char fractions and emissions factors; they will be re-added with wildfire data.
          #   firecols set in constants define column names to be removed
          set(x = case.data, j = firecols, value = NULL)
          
          # Pile and Broadcast ----------------------------------------------------------------------------------  
        } else if(case$prescribed.burn.type == "Pile and Broadcast") { # Calculate pile and broadcast burn emissions, if this type of burn is applied
          
          if(debug_CBREC) {
            cat("   Calculating prescribed burn emissions: Pile and Broadcast Burn\n", file=log_file, append=T)
            cat("\nWARNING: Residues remaining after pile burning are added to COARSE scattered debris - a fire model inconsistency we should fix in a later version\n",file=log_file,append=T)
          }
          
          # Calculate broadcast burn emissions first *************************************************************
          #   Pile burning has to happen after broadcast burn, else remaining scattered material post pile burn will be burned a second time.
          prescribed_burn_output <- prescribed_burn_fun(case.data, "Broadcast")
          
          # Update the emissions data with prescribed_burn_output[[1]]
          results_tracker$treatment$BroadcastBurn <- data.table(
            broadcast.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
            broadcast.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes,
            broadcast.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
            broadcast.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
            broadcast.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction + prescribed_burn_output[[1]]$N2O_tonnes,
            broadcast.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
            broadcast.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
            broadcast.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
            broadcast.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
          )
          
          # Add char production from this year to the next year's start-of-year char mass with prescribed_burn_output[[2]]
          results_tracker$postTreatment[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[2]]$char_tonnes]
          
          # Add broadcast burn mass with prescribed_burn_output[[2]]
          results_tracker$treatment$BroadcastBurn[,broadcast.burned.residue_tonnes := prescribed_burn_output[[2]]$combusted.residue_tonnes]
         
          # calculate black carbon from prescribed burn type
          calcBlackCarbon(results_tracker$treatment$BroadcastBurn, em_src = "rx_burn_broadcast")
          
          # Calculate pile burn emissions second ********************************************************************
          #   Pile burning has to happen after broadcast burn, else remaining scattered material post pile burn will be burned a second time.
          prescribed_burn_output <- prescribed_burn_fun(prescribed_burn_output[[3]], "Pile")
          
          # Update the case data with prescribed_burn_output[[3]]
          case.data <- prescribed_burn_output[[3]]
          
          # Update the emissions data with prescribed_burn_output[[1]]
          results_tracker$treatment$PileBurn <- data.table(
            pile.burn.CO2_tonnes = prescribed_burn_output[[1]]$CO2_tonnes, 
            pile.burn.CO_tonnes = prescribed_burn_output[[1]]$CO_tonnes, 
            pile.burn.CH4_tonnes = prescribed_burn_output[[1]]$CH4_tonnes,
            pile.burn.NOx_tonnes = prescribed_burn_output[[1]]$NOx_tonnes,
            pile.burn.N2O_tonnes = prescribed_burn_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction + prescribed_burn_output[[1]]$N2O_tonnes, # add N2O from drip torch
            pile.burn.PMUnder10um_tonnes = prescribed_burn_output[[1]]$PMUnder10um_tonnes,
            pile.burn.PMUnder2.5um_tonnes = prescribed_burn_output[[1]]$PMUnder2.5um_tonnes,
            pile.burn.SO2_tonnes = prescribed_burn_output[[1]]$SO2_tonnes,
            pile.burn.VOC_tonnes = prescribed_burn_output[[1]]$VOC_tonnes
          )
          
          # Add char production from this year to the next year's start-of-year char mass
          results_tracker$postTreatment[year.i+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + prescribed_burn_output[[2]]$char_tonnes]
          
          # Add piled burn mass with prescribed_burn_output[[2]]
          results_tracker$treatment$PileBurn[,pile.burned.residue_tonnes := prescribed_burn_output[[2]]$combusted.residue_tonnes]
          
          # calculate black carbon from prescribed burn type
          calcBlackCarbon(results_tracker$treatment$PileBurn, em_src = "rx_burn_pile")
          
          # Remove un-needed variable
          prescribed_burn_output <- NULL
          
          # Clear out the combustion/char fractions and emissions factors; they will be re-added with wildfire data.
          #   firecols set in constants define column names to be removed
          set(x = case.data, j = firecols, value = NULL)
          
        } else { # Explicitly set broadcast and pile burn list entries to zero
          results_tracker$treatment$BroadcastBurn <- 0
          results_tracker$treatment$PileBurn <- 0
        } # End prescribed burn emissions calculations

      } # end of year 1 calculations
      
      # Decay -----------------------------------------------------------------------------------------------------------
      # Note decay_output has the following structure:
      #   decay_output[[1]]: Decay emissions
      #   decay_output[[2]]: Updated case.data

      # Decay all scattered material (material not exposed to prescribed burn or wildfire) for year.i *******************
      decay_output <- decay_fun(cbrec.dt = case.data,
                                residue.disposition = "Scattered",
                                year.i = year.i)
      results_tracker$postTreatment[dt_rn, # Update mass tracking
                                    decayed.residue_tonnes := decayed.residue_tonnes + 
                                      decay_output[[2]][,sum(decay_CWD_mass_year_i) + 
                                                         sum(decay_FWD_mass_year_i) + 
                                                         sum(decay_Foliage_mass_year_i)] * 
                                      acres_per_cell]
      results_tracker$postTreatment[dt_rn,':='( # update emissions tracking
        decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
        decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
      )]
      
      # Decay all piled material (material not exposed to prescribed burn or wildfire) for year.i ************************
      decay_output <- decay_fun(cbrec.dt = decay_output[[2]],
                                residue.disposition = "Piled",
                                year.i = year.i)
      results_tracker$postTreatment[dt_rn, # Update mass tracking
                                    decayed.residue_tonnes := decayed.residue_tonnes + 
                                      (decay_output[[2]][,sum(decay_CWD_mass_year_i) + 
                                                          sum(decay_FWD_mass_year_i) + 
                                                          sum(decay_Foliage_mass_year_i)] * acres_per_cell)
                                    ]
      results_tracker$postTreatment[dt_rn,':='( # update emissions tracking
        decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
        decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
      )]
      
      # Decay all material exposed to prescribed burn and wildfire but left unburnt due to incomplete combustion for year.i *********************
      decay_output <- decay_fun(cbrec.dt = decay_output[[2]],
                                residue.disposition = "prev_fired",
                                year.i = year.i)
      results_tracker$postTreatment[dt_rn, # Update mass tracking
                                    decayed.residue_tonnes := decayed.residue_tonnes + 
                                      (decay_output[[2]][,sum(decay_CWD_mass_year_i) + 
                                                        sum(decay_FWD_mass_year_i) + 
                                                        sum(decay_Foliage_mass_year_i)] * acres_per_cell)
                                    ]
      results_tracker$postTreatment[dt_rn,':='( # update emissions tracking
        decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
        decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
      )]
      
      # Decay all duff for year.i ----------------------------------------------------------------------------------------
      #   Because we only run duff decay once, we can address both previously fired and non-fired duff.
      decay_output <- duff_decay_fun(cbrec.dt = decay_output[[2]],
                                     year.i = year.i)
      results_tracker$postTreatment[dt_rn, # Update mass tracking
                                    decayed.residue_tonnes := decayed.residue_tonnes + 
                                      decay_output[[3]] * 
                                      acres_per_cell]
      results_tracker$postTreatment[dt_rn,':='( # update emissions tracking
        decay.CO2_tonnes = decay.CO2_tonnes + decay_output[[1]]$CO2_tonnes,
        decay.CH4_tonnes = decay.CH4_tonnes + decay_output[[1]]$CH4_tonnes
      )]
      
      # Update case data and remove un-needed variable
      case.data <- copy(decay_output[[2]])
      decay_output <- NULL
      
      # Wildfire -----------------------------------------------------------------------------------------------------------
      # Note wildfire_output has the following structure:
      #   wildfire_output[[1]]: Wildfire emissions
      #   wildfire_output[[2]]: Updated case.data
      
      # Figure out the emission factors, combustion fractions, and char fractions for wildfire.
      case.data <- annual_fire_detail_fun(cbrec.dt = case.data,
                                          year.i = year.i,
                                          wildfire.data = wildfire_data)

      if(sum(is.na(case.data$Wildfire_Probability)) > 0 & debug_CBREC) { # These should be resolved, but if they are not throw an error
        cat("\nWARNING: NA values detected in wildfire probability. See line 1155 and issue #14 on Github\n", file=log_file, append=T)
      }
      
      # Calculate scattered mass burned in wildfire in year.i --------------------------------------------------------------
      wildfire_output <- annual_wildfire_fun(cbrec.dt = case.data,
                                             residue.disposition = "Scattered",
                                             year.i = year.i)
      results_tracker$postTreatment[dt_rn, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + # Add the exposed-to-fire mass to wildfire.burned.residue
                                      wildfire_output[[2]][,sum(fire_exposed_CWD_mass_year_i) + 
                                                            sum(fire_exposed_FWD_mass_year_i) + 
                                                            sum(fire_exposed_Foliage_mass_year_i)] * 
                                      acres_per_cell]
      if(year.i < 100) { # Add char produced this year to the starting char for the next year. Skip for year 100.
        results_tracker$postTreatment[dt_rn+1, In.field.char.scattered_tonnes := In.field.char.scattered_tonnes + wildfire_output[[1]]$char_tonnes]
      }
      results_tracker$postTreatment[dt_rn,':='( # update emissions tracking
        wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
        wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
        wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
        wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
        wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
        wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
        wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
        wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
        wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
      )]
        
      # Calculate piled mass burned in wildfire in year.i -------------------------------------------------------------------
      wildfire_output <- annual_wildfire_fun(cbrec.dt = wildfire_output[[2]],
                                             residue.disposition = "Piled",
                                             year.i = year.i)
      results_tracker$postTreatment[dt_rn, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + # Add the exposed-to-fire mass to wildfire.burned.residue
                                      wildfire_output[[2]][,sum(fire_exposed_CWD_mass_year_i) + 
                                                            sum(fire_exposed_FWD_mass_year_i) + 
                                                            sum(fire_exposed_Foliage_mass_year_i)] * 
                                      acres_per_cell]
      if(year.i < 100) { # Add char produced this year to the starting char for the next year. Skip for year 100.
        results_tracker$postTreatment[dt_rn+1, In.field.char.piled_tonnes := In.field.char.piled_tonnes + wildfire_output[[1]]$char_tonnes]
      }
      results_tracker$postTreatment[dt_rn,':='( # update emissions tracking
        wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
        wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
        wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
        wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
        wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
        wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
        wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
        wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
        wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
      )]
      
      # Calculate duff mass burned in wildfire in year.i ----------------------------------------------------------------------
      #   No char is produced from the combustion of duff
      wildfire_output <- duff_annual_wildfire_fun(cbrec.dt = wildfire_output[[2]],
                                                  year.i = year.i)
      results_tracker$postTreatment[dt_rn, wildfire.burned.residue_tonnes := wildfire.burned.residue_tonnes + # Add the exposed-to-fire mass to wildfire.burned.residue
                                      wildfire_output[[2]][,sum(fire_exposed_Duff_mass_year_i)] * acres_per_cell]
      results_tracker$postTreatment[dt_rn,':='( # update emissions tracking
        wildfire.CO2_tonnes = wildfire.CO2_tonnes + wildfire_output[[1]]$CO2_tonnes,
        wildfire.CO_tonnes = wildfire.CO_tonnes + wildfire_output[[1]]$CO_tonnes,
        wildfire.CH4_tonnes = wildfire.CH4_tonnes + wildfire_output[[1]]$CH4_tonnes,
        wildfire.NOx_tonnes = wildfire.NOx_tonnes + wildfire_output[[1]]$NOx_tonnes,
        wildfire.N2O_tonnes = wildfire.N2O_tonnes + wildfire_output[[1]]$NOx_tonnes * fire_N2O_NOx_fraction,
        wildfire.PMUnder10um_tonnes = wildfire.PMUnder10um_tonnes + wildfire_output[[1]]$PMUnder10um_tonnes,
        wildfire.PMUnder2.5um_tonnes = wildfire.PMUnder2.5um_tonnes + wildfire_output[[1]]$PMUnder2.5um_tonnes,
        wildfire.SO2_tonnes = wildfire.SO2_tonnes + wildfire_output[[1]]$SO2_tonnes,
        wildfire.VOC_tonnes = wildfire.VOC_tonnes + wildfire_output[[1]]$VOC_tonnes
      )]
      
      # Update case.data and remove un-needed variable
      case.data <- copy(wildfire_output[[2]])
      wildfire_output <- NULL
      
      # Clear out un-needed columns
      #   firecols set in constants define column names to be removed
      set(x = case.data, j = firecols, value = NULL)
      
      # calc black carbon at year 100
      if(year.i == 100){calcBlackCarbon(dt = results_tracker$postTreatment, em_src = "wildfire")}
      
      # *************************** ANDY LOOK ABOVE HERE  *********************************************

    } # end of 100 year loop
    if(debug_CBREC) {toc()} # end of time series
    
    
    # Return Results -----------------------------------------------------------------------------------
    # Aggregate results to be returned in a list
    #***************************************************************************************************
    #case_results <- list("time_series" = cbind(results_tracker$postTreatment,
    #                                           results_tracker$postTreatment),
    #                     "year0_mass" = results_tracker$treatment,
    #                     "year0_emissions" = results_tracker$treatment)
    
    # Check output for NULL results and document in the log file
    if(is.null(results_tracker)) {
      cat(paste0("Results NULL for polygon ", poly_num, " and case ", case$ID, "\n"), file = log_file, append = T)
    }
    
    case_results <- list(results_tracker)
    case_results <- setNames(case_results, case$ID)
    
    # Return results
    return(case_results)
    
  }) # end of lapply that loops through case_list_toapply
  
  # Save Results ---------------------------------------------------------------------------------------
  # Write Polygon Results to Disk
  # ****************************************************************************************************
  
  # simplify names
  polygon_results <- unlist(polygon_results, recursive = F, use.names = T)
  
  # Check again for NULL values and skip polys
  if(!is.null(polygon_results)) {
    # clean up names formatting
    names(polygon_results) <-
      gsub("^[^\\.]*\\.", "", names(polygon_results)) # remove all before first "."
    
    # Save output
    if(debug_CBREC) {
      if(saveResults) {
        cat(paste0("Writing polygon ",as.character(poly_num)," to ",paste0(out_folder,"/results/",poly_num,".rds"),"\n"), file=log_file, append=T)
        saveRDS(polygon_results,
                paste0(out_folder,"results/",poly_num,".rds")
        )
      }
      return(polygon_results) # If debugging return list to environment
    } else {
      saveRDS(polygon_results,
              paste0(out_folder,"results/",poly_num,".rds")
      )
    }
  }
  
}) # end of future.lapply that loops through all_project_polygons

# Log model run time and close log file
cat(paste0("Model Run Time: ", as.character(toc())), file = log_file, append = T)
close(log_file)