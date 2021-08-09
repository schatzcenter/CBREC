# 
# ORIGINAL AUTHOR: Chih-Wei Hsu
#                  Schatz Energy Research Center
#                  Humboldt State University
#
# **********************************************************************
# VERSION:
#
#   2019-08-13: Initialized development
#   2019-08-28: Initial development done
#   2021-03-14 (JKC): Large overhaul
#
# **********************************************************************
# ACTION ITEMS:
# Index   Description                                                      Status
# 01      Initial development                                              Done
# 02      Add in visualization component?                                  Abandoned
#
# *********************************************************************
#
# *********************************************************************
# INPUTS:
#   cbrec output case file(s): Read RDS from output folders
#   Absolute Global Warming Potential (AGWP) and Absolute Global Temperature
#     Potential (AGTP) matrices: read from input directory
#
# *********************************************************************
# OBJECTIVE:
# Read in CBREC results, and run the climate metric scripts to
# compare reference and use cases
#
# *********************************************************************
# OUTPUTS:
# An individual list for each polygon/THP/tile that contains
# lists of reference cases. Within each reference case list contains all relevant
# scenarios in the climate matric output format
# 
# rstudioapi::restartSession()

#' @description Contains two functions which output climate metrics
#' First outputs climate metric totals without a functional unit for each use and reference case
#' Second takes those outputs and adds a functional unit, calcs a CO2e and pairs use X ref cases
#' Final outputs(outputs of second function) used to make charts and graphs of outputs
#' These functions will probably be blueprints for web API functionality
#' 
#' Consider combining these two functions to produce a single output...

# Source functions ----------------------------
source("Post_Processing/CBI_postFunctions.R")
source("CBREC/functions/climate-metric-functions.R")

# Load librarys -------------------------------

# Fully clear all non-base packages that are loaded to avoid "monkey patch" conflicts across packages
#   Iteratively call unloadPackages() in order to capture packages that couldn't be unloaded because of dependency by other loaded packages
# unloadErrors <- unloadPackages()
# while(length(unloadErrors)>0) {
#   unloadErrors <- unloadPackages()
# }

# library("magrittr") # guessing this isn't needed
# library("dplyr") # pipeline data wrangling
# library("tidyr") # additional tidy functions (expand, ...)
# library("stringr") # many regex uses
# library("purrr") # use of map()
library("data.table") # alternate data structure
library("future.apply") # parallelization framework


# User-defined variables -------------------------------------------------------------


# Set parallelization plan
options(future.fork.enable = T) # allows old method of parallelization with forked 'rsessions'
plan(multiprocess, workers = 28) # check this value and change if memory concerns appear
# needs multicore to pass globals at least on windows... this option may be a source of error to watch for

## Change dirs below to match current machine needs
dataRootDir <- "CBREC_output/2020_06_harvests/"
cbrec_results <- paste0(dataRootDir,"results/") # CBREC output files

# Specify functional value(s)
# Set to NULL if desire to calculate functional value from CBREC outputs
#functional_value <- NULL

# Specify time horizon at which climate metrics will be reported
th <- list(AGWP = 100,
           AGTP = 100) #year (must be an integer value between 1 and 100)

# Specify the how much the scenarios should expand
# Silviculture level has 2366 scenarios
# fraction_piled level has 182 scenarios

# Load Constants and Paths ------------------------------------------------
# helper function
metrics_paste <- function(functional_unit, met) {
  out <- if(is.na(functional_unit)) {
    paste0("net.", met, "_MT")
  } else {
    switch (functional_unit,
                 "MT_Mobilized" = paste0("net.", met, "_MTperMT_Mobilized"),
                 "MT_Mobilized_perAcre" = paste0("net.", met, "_MTperMT_Mobilized_perAcre"),
                 "kWh" = paste0("net.", met, "_MTperkWh"),
                 "MT_Delivered" = paste0("net.", met, "_MTperMT_Delivered"),
                 "MT_Burned_inPP" = paste0("net.", met, "_MTperMT_Burned_inPP")
            )
  }
  return(out)
}

# This is if the past files have already been generated
# TODO think about combining these steps to reduce intermediate results

# make sure scenario lookup is loaded
#scenario_lookup <- scenarioExpand(case_matrix, expand_attribute = "fraction_piled")

# Specify functional unit
#' @note I don't think mt_mobilized_perAcre should be used as a functional unit since nothing else is in perAcre 
#' # we get really large CO2e values for some of the harvests that are big, lots of residues, lots of emissions, average residue density
functional_unit <- switch(menu(choices = c("MT Mobilized from Field",
                                            "MT per Acre Mobilized from Field", 
                                            "MT Delivered to Power Plant",
                                            "MT Burned in Power Plant",
                                            "Net kWh Generated from Power Plant",
                                            "None"),
                                title = "Specify the Functional Unit",
                                graphics = F),
                           "MT_Mobilized", 
                           "MT_Mobilized_perAcre",
                           "MT_Delivered", 
                           "MT_Burned_inPP",
                           "kWh",
                           NA)

# Specify the power plant you want
chosenPP <- switch (menu(choices = c("Nearest Power Plant",
                                     "Current Generation Biomass Stoker", 
                                     "Current Generation Integrated Gasification and Combustion",
                                     "Next Generation Gasification",
                                     "<1MW Next Generation Gasification"),
                         title = "Specify the Desired Power Plant",
                         graphics = F),
                    "nearest", 
                    "cur_gen",
                    "ig", 
                    "next_gen",
                    "one_mw")

# Specify if want to include the natural gas offset in results
include_ng <- switch (menu(choices = c("Include NG Offset",
                                       "DO NOT Include NG Offset"),
                           title = "Specify Whether To Include CHP and Associated Offset of Natural Gas",
                           graphics = FALSE),
                      TRUE,
                      FALSE)

# Choose the hauling distance
if(chosenPP != "nearest") {
  chosenHaulDist <- 50 # units of km
} else {
  chosenHaulDist <- NA
}

# Specify assumed comminution and processing
chosenCandP <- switch (menu(choices = c("High Volume; Dry Material, Grind",
                                        "High Volume; Green Material, Chip", 
                                        "High Volume; Green Material, Grind",
                                        "Low Volume; Dry Material, Grind"),
                             title = "Specify the Material Moisture and Comminution",
                             graphics = F),
                       "dry_grind", 
                       "green_chip",
                       "green_grind", 
                       "low_volume")

# Create output directory name
climate_metric_outputs <- paste0(dataRootDir,
                                 "processed_FU-",functional_unit,
                                 "_PP-",chosenPP,
                                 ifelse(include_ng,"_withCHP","_noCHP"),
                                 "_CP-",chosenCandP,
                                 if(!is.na(chosenHaulDist)) {paste0("_",as.character(chosenHaulDist),"km")},
                                 "/",
                                 gsub("-", "_", Sys.Date()), "/") # output from this script

# define climate metric variables used in looping
cm = list(AGWP = "AGWP",
          AGTP = "AGTP")


# Input Data --------------------------------------------------------------

#Load AGWP scenario matrices
AGWPMat <- list(
  CO2 = as.matrix(read.csv("CBREC_input_data/climate_metrics/AGWPScenarioCO2Mat_2020-2-27.csv", header=FALSE)),
  CH4 = as.matrix(read.csv("CBREC_input_data/climate_metrics/AGWPScenarioCH4Mat_2020-2-27.csv", header=FALSE)),
  N2O = as.matrix(read.csv("CBREC_input_data/climate_metrics/AGWPScenarioN2OMat_2020-2-27.csv", header=FALSE)))

#Load AGTP scenario matrices
AGTPMat <- list(
  CO2 = as.matrix(read.csv("CBREC_input_data/climate_metrics/AGTPScenarioCO2Mat_2020-6-30.csv", header=FALSE)),
  CH4 = as.matrix(read.csv("CBREC_input_data/climate_metrics/AGTPScenarioCH4Mat_2020-6-30.csv", header=FALSE)),
  N2O = as.matrix(read.csv("CBREC_input_data/climate_metrics/AGTPScenarioN2OMat_2020-6-30.csv", header=FALSE)))


# Define the Scenarios ----------------------------------------------------
scenario_lookup <- as.data.table(read.csv("Post_Processing/Scenario-case-pairings_2020-06-04.csv"))
scenario_lookup[,(1):=NULL] # Delete index column that was created by write.csv() in generateScenarioCasePairings.Rmd

# Create output directory for outputs ---------------------------------
ifelse(!dir.exists(climate_metric_outputs),
       dir.create(climate_metric_outputs, recursive = T), # create file path if needed
       FALSE)

# Find All Data -------------------------------

# find all paths
poly_list <- dir(cbrec_results, full.names = T)

# REMOVE POLYGON 15530 AS IT IS EVALUATING WITH ZERO MATERIAL. INVESTIGATE LATER
#poly_list <- poly_list[-grep("*15530",poly_list)]

#poly_i <- poly_list[1]
# Loop through each data --------------------------------------------------
# This loop uses a few custom functions for organizing and unpacking the CBREC model outputs.
# The `cbi_*` functions can be used to process outputs for other purposes as well (ex. Look at proportions of emissions from source)
# These functions are used to create two lists of cases, use and ref.
# The two lists are run separately then combined as a final output

future_lapply(poly_list, function(poly_i) {
  # Load data ---------------------------------------------------------------
  poly <- readRDS(poly_i)
  
  # Sort Data ---------------------------------------------------------------

  # Sort polygon into use and reference cases based on file structure
  use_ref_list <- cbi_sortUseRef(poly)
  
  if(length(use_ref_list$use)==0 | length(use_ref_list$ref)==0) {
    warning(paste0("Skipping Poly ID ",poly_i,". Number of use cases is ",as.character(length(use_ref_list$use)),", number of ref cases is ",as.character(length(use_ref_list$ref))))
    return(0)
  }
  
  # create separate lists for use and reference
  use_list <- use_ref_list["use"]
  
  ref_list <- use_ref_list[["ref"]] # notice different list structure for use in next function
  # while this could be 'fixed' it would only be for continuity and offers no other advantage
  
  # Select Pathway
  use_list <- cbi_sepUseCaseSources(use_cases = use_list,
                                    power_plant = chosenPP,
                                    CandP = chosenCandP,
                                    haulKM = if(is.na(chosenHaulDist)) NULL else {chosenHaulDist})
  
  # Rearrange reference case to match climate metric function
  ref_list <- cbi_processRefs(refs = ref_list)
  
  # Run Climate Metrics --------------------------------
  
  # run use cases and reference cases separately
  
  # use cases
  use_out <-
        lapply(use_list, function(use) {
          cm.out <- climateMetrics(case = use, # changes
                                   AGWPMat = AGWPMat, # constant
                                   AGTPMat = AGTPMat, # constant
                                   ngoffset = include_ng, # boolean
                                   type = "use")
          return(cm.out)
  })
  
  # ref cases
  ref_out <-
        lapply(ref_list, function(ref) {
          cm.out <- climateMetrics(case = ref, # changes
                                   AGWPMat = AGWPMat, # constant
                                   AGTPMat = AGTPMat, # constant
                                   ngoffset = include_ng, # boolean
                                   type = "ref") 
          return(cm.out)
  })
   
  # clean up names for use case (byproduct of list structure)
  names(use_out) <- gsub("use.", "", names(use_out))

  # Combine Results
  #   At this point we have time series of both mass and climate metrics
  cm_lists <- list("use" = use_out,
                  "ref" = ref_out)
  
  # Subset use and reference cases to those in scenario_lookup
  cm_lists$use <- cm_lists$use[as.numeric(names(cm_lists$use)) %in% unique(scenario_lookup$Use)]
  cm_lists$ref <- cm_lists$ref[as.numeric(names(cm_lists$ref)) %in% unique(scenario_lookup$Ref)]
  
  # get use case names
  use_names <- names(cm_lists[["use"]])

  # loop over each use case adding the specified functional unit to the lookup table
  use_ref_datas <- lapply(use_names, function(specific_use) {

    # given a use case, find all reference cases associated with the use case
    lookup_sub <- subset(scenario_lookup, Use == specific_use)
    
    if(!is.na(functional_unit)) {
      switch(functional_unit,
        "MT_Mobilized" = fun_list <- list("functional_unit_value" = cm_lists[["use"]][[specific_use]]$MT_Residue_Mobilized,
                                          "functional_unit" = "MT_Mobilized"),
        
        "MT_Mobilized_perAcre" = fun_list <-list("functional_unit_value" = cm_lists[["use"]][[specific_use]]$MT_Residue_Mobilized_perAcre,
                                                 "functional_unit" = "MT_Mobilized_perAcre"),
        
        "MT_Delivered" = fun_list <- list("functional_unit_value" = cm_lists[["use"]][[specific_use]]$MT_Residue_Delivered,
                                          "functional_unit" = "MT_Delivered"),
        
        "MT_Burned_inPP" = fun_list <- list("functional_unit_value" = cm_lists[["use"]][[specific_use]]$MT_Residue_Burned,
                                       "functional_unit" = "MT_Burned_inPP"),
        
        "kWh" = fun_list <- list("functional_unit_value" = cm_lists[["use"]][[specific_use]]$MWh_Generated * 1000, # Convert from MWh to kWh
                                 "functional_unit" = "kWh"))
      
      # append lookup_subs with functional unit data
      lookup_sub$functional_unit <- fun_list$functional_unit
      lookup_sub$functional_unit_value <- fun_list$functional_unit_value
    } else {
      lookup_sub$functional_unit <- NA
      lookup_sub$functional_unit_value <- 1
    }
    
    return(lookup_sub)
  })
  
  # Format Data -------------------------------------------------------------
  # get data into a long data table with each observation(use/ref pairing) as a row
  
  # collapse into data table
  use_ref_datas <- rbindlist(use_ref_datas)
  
  # add scenario ID to split on
  use_ref_datas$scenario_id <- seq(1:nrow(use_ref_datas))
  
  # split data table to run in apply loop
  use_ref_datas <- split(use_ref_datas, use_ref_datas$scenario_id)

  # Generate gross use, gross ref, and net emissions for each scenario. Apply functional unit if applicable
  out <-
    lapply(use_ref_datas, function(u_r_data) {
    
    # get reference case ID
    specific_ref <- as.character(u_r_data$Ref) 
    specific_use <- as.character(u_r_data$Use)
    
    # Create use and reference lists
    #   Replace gross mass with functional unit values if applicable
    if(!is.na(u_r_data$functional_unit)) {
      use <- calcFunctionalUnit(cm_lists[["use"]][[specific_use]],
                                func_val = u_r_data$functional_unit_value,
                                func_unit = u_r_data$functional_unit)
      
      # repeat with reference cases using the functional values from the chosen use case
      ref <- calcFunctionalUnit(cm_lists[["ref"]][[specific_ref]],
                                func_val = u_r_data$functional_unit_value,
                                func_unit = u_r_data$functional_unit )
    } else {
      use <- cm_lists[["use"]][[specific_use]]
      ref <- cm_lists[["ref"]][[specific_ref]]
    }
    
    # calculate the co2e value for each climate metric
    for (m in cm) {
      use <- calcCO2E(case = use,
                      timehorz = th, 
                      cm = m,
                      func_unit = ifelse(!is.na(u_r_data$functional_unit),u_r_data$functional_unit,NA))
      
      ref <- calcCO2E(case = ref,
                      timehorz = th, 
                      cm = m,
                      func_unit = ifelse(!is.na(u_r_data$functional_unit),u_r_data$functional_unit,NA))
    }
    
    # Create net emissions for the scenario
    net <- list(net.mass = data.table()) # create empty data table so we can dynamically name new columns
    emCols <- unlist(sapply(use, function(x) {names(x)[grepl("total", names(x))]}))
    invisible(lapply(names(emCols),
                     function(em) {
                       net$net.mass[, (gsub("total", "net", emCols[[em]])) := use[[em]][[emCols[[em]]]] - ref[[em]][[emCols[[em]]]] ]
                       })
              )
    net <- c(net,
             as.list(sapply(names(use)[grepl("CO2e", names(use))],
                            function(em) {
                              use[[em]] - ref[[em]]
                              }
                            )
                     )
             )
    names(net)[2:3] <- paste0("net.", names(net)[2:3])
    
    # Combine results into a list
    listOut <- list(list(use = use,
                         ref = ref,
                         net = net))
    
    # set name of list to scenario (case by case)
    names(listOut) <- paste(specific_use, "x", specific_ref)

    return(listOut)
  })  # end lapply for each case
  
  # simplfiy structure
  out <- unlist(out, recursive = F, use.names = T)
  
  # fix naming issue
  names(out) <- gsub("^.*\\.", "", names(out))
  
  # save file
  # extract numbers from file path
  file_number <- tools::file_path_sans_ext(basename(poly_i)) # name of file
  
  # save output
  saveRDS(object = out, file = paste0(climate_metric_outputs, file_number, ".rds"))
}) # end of future loop
