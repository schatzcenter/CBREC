# 
# ORIGINAL AUTHOR: Chih-Wei Hsu
#                  Schatz Energy Research Center
#                  Humboldt State University
#
# **********************************************************************
# VERSION:
#
#  1.0    2019-08-13: Initialized development
#  1.1    2019-08-28: Initial development done
#  1.2    2021-03-14 (JKC): Large overhaul
#  1.2.1  2021-08-20: Final cleaning up of comments for public release
#
# **********************************************************************
# INPUTS:
#   run_CREC-LCA.R output case file(s): Read RDS from output folders
#   Absolute Global Warming Potential (AGWP) and Absolute Global Temperature
#     Potential (AGTP) matrices: read from input directory
#
# *********************************************************************
# OBJECTIVE:
# Read in run_CBREC-LCA.R results, and run the climate metric scripts to
# compare reference and use cases
#
# *********************************************************************
# OUTPUTS:
# An individual list for each polygon/THP/tile that contains
# lists of reference cases. Within each reference case list contains all relevant
# scenarios in the climate matric output format
#

# Set working directory
setwd("/media/spin/Github/CBREC") # Automate this with gregexpr() and substr()

# Source functions ----------------------------
source("CBREC-LCA/functions/post-processing-functions.R")
source("CBREC-LCA/functions/climate-metric-functions.R")

# Load librarys -------------------------------

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

library("data.table") # alternate data structure
library("future.apply") # parallelization framework

# User-defined variables -------------------------------------------------------------

# Set parallelization plan
options(future.fork.enable = T) # allows old method of parallelization with forked 'rsessions'
plan(multiprocess, workers = 28) # check this value and change if memory concerns appear
# needs multicore to pass globals at least on windows... this option may be a source of error to watch for

## Change dirs below to match current machine needs
dataRootDir <- out_folder # Output directory specified in run_CBREC-LCA.R
cbrec_results <- paste0(dataRootDir,"results/") # run_CBREC-LCA.R output files

# Specify time horizon at which climate metrics will be reported
th <- list(AGWP = 100,
           AGTP = 100) #year (must be an integer value between 1 and 100)

# Load Constants and Paths ------------------------------------------------

# Specify functional unit
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
ifelse(!dir.exists(climate_metric_outputs),
       dir.create(climate_metric_outputs, recursive = T), # create file path if needed
       FALSE)

# define climate metric variables used in looping
cm = list(AGWP = "AGWP",
          AGTP = "AGTP")


# Input Data --------------------------------------------------------------

#Load AGWP scenario matrices
AGWPMat <- list(
  CO2 = as.matrix(read.csv("CBREC-LCA/input/climate_metrics/AGWPScenarioCO2Mat", header=FALSE)),
  CH4 = as.matrix(read.csv("CBREC-LCA/input/climate_metrics/AGWPScenarioCH4Mat", header=FALSE)),
  N2O = as.matrix(read.csv("CBREC-LCA/input/climate_metrics/AGWPScenarioN2OMat", header=FALSE)))

#Load AGTP scenario matrices
AGTPMat <- list(
  CO2 = as.matrix(read.csv("CBREC-LCA/input/climate_metrics/AGTPScenarioCO2Mat", header=FALSE)),
  CH4 = as.matrix(read.csv("CBREC-LCA/input/climate_metrics/AGTPScenarioCH4Mat", header=FALSE)),
  N2O = as.matrix(read.csv("CBREC-LCA/input/climate_metrics/AGTPScenarioN2OMat", header=FALSE)))


# Define the Scenarios ----------------------------------------------------
scenario_lookup <- as.data.table(read.csv("CBREC-LCA/input/case_definitions/scenario-case-pairings.csv"))

# Find All Data -------------------------------

# find all paths
poly_list <- dir(cbrec_results, full.names = T)

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
