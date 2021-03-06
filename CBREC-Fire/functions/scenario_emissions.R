################################################################################
# This script uses the scenario and tile number to calculate wildfire emissions
# at 25 year timesteps over a 100 year period. This is part of the C-BREC Fire Module.
#
# tile_number: numeric tile number. Must be one of the actual tile numbers
#
# Author: Micah Wright, Humboldt State University
################################################################################

# source function that loads and merges FCCS fuelbed, biomass residue, and 
# source function that adds scattered residue to FCCS fuelbeds
source("CBREC-Fire/functions/add_residue.R")

# source function for adding rx residues back to recovered fuelbed
source("CBREC-Fire/functions/add_rx_residue.R")

# source wrapper function for consumption and emissions functions
source("CBREC-Fire/functions/burn_residue.R")

# source decay functions
source("CBREC-Fire/functions/decay_residue.R")

# location attribute data 
source("CBREC-Fire/functions/load_data.R")

# source function that assigns appropriate residue into piled mass
source("CBREC-Fire/functions/pile_residue.R")

# source function for saving model output
source("CBREC-Fire/functions/save_output.R")

# source function that corrects midflame windspeed
source("CBREC-Fire/functions/wind_correction.R")

# load parallel processing package, wrapper for os agnostic future package
library(future.apply)
# data table
library(data.table)

scenario_emissions <- function(tile_number) {
        
        # create output tile folders if missing
        em_path <- "CBREC-Fire/output/emissions/"
        res_path <- "CBREC-Fire/output/residual_fuels/"
        
        lapply(c(em_path, res_path), function(x) {
                
                if(!dir.exists(paste0(x, tile_number))) {
                        dir.create(paste0(x, tile_number))
                }
        })
        
        # load scenarios
        scenarios <- fread("common-inputs/case_definitions/cases-to-run.csv",
                           verbose = FALSE)
        
        setkey(scenarios, Silvicultural_Treatment)
        
        scenarios[, Tile_Number := tile_number]
        
        setkey(scenarios, NULL)
        
        # split the scenario dt into a list
        scenario_list <- split(scenarios, by = "ID")
        
        # assign local multicore processing if available
        plan(multiprocess, workers = 26)
        
        # run fuel processing and decay/fire models on each scenario in the list
        # in parallel
        future_lapply(scenario_list, 
                      function(x) {
                              
                              # make sure each element of the list is a single-row
                              # data table
                              stopifnot(nrow(x) == 1)
                              
                              # assign scenario ids. x is a single-row data table,
                              # so assigning the first row gets the correct value
                              ID <- x[, ID]
                              Silvicultural_Treatment <- x[, Silvicultural_Treatment]
                              Fraction_Piled = x[, Fraction_Piled]
                              Fraction_Scattered = x[, Fraction_Scattered]
                              Burn_Type <- x[, Burn_Type]
                              Biomass_Collection <- x[, Biomass_Collection]
                              Pulp_Market <- x[, Pulp_Market]
                              Tile_Number <- x[, Tile_Number]
                              
                              # load data
                              # this combines residue, fuelbed, and spatial
                              # attribute data
                              fuel_df <- load_data(ID,
                                                   Silvicultural_Treatment,
                                                   Fraction_Piled,
                                                   Fraction_Scattered,
                                                   Burn_Type,
                                                   Biomass_Collection,
                                                   Pulp_Market, 
                                                   Tile_Number)
                              
                              # correct windspeed from 10m to mid-flame
                              # Note: TPA and TPI are already embedded in fuel_df. Can remove these as function inputs.
                              wind_correction(fuel_df,
                                              Wind, # I dont think this variable is used or defined. Does not cause error
                                              TPA,
                                              TPI)
                              
                              # RX burn scenarios
                              if(Burn_Type != "None") {
                                      
                                      # need to copy fuel_df or it is modified in place
                                      cpy <- copy(fuel_df)
                                      
                                      # calculate piled load
                                      cpy <- pile_residue(cpy, 0)
                                      
                                      # add the scattered residue to the fuelbed
                                      cpy <- add_residue(cpy, 0)
                                      
                                      # change fire weather value names appropriately
                                      cpy[, ':=' (Wind_corrected = Wind_corrected_rx,
                                                  Fm10  = Fm10_rx,
                                                  Fm1000 = Fm1000_rx)]
                                      
                                      # apply the rx burn
                                      rx_out <- burn_residue(cpy, Burn_Type)
                                      
                                      # save the output
                                      for (i in 1:2) {
                                              save_output(rx_out[[i]],
                                                          Silvicultural_Treatment,
                                                          ID,
                                                          Burn_Type,
                                                          Tile_Number,
                                                          Fraction_Piled,
                                                          Fraction_Scattered,
                                                          Biomass_Collection,
                                                          Pulp_Market,
                                                          secondary_burn = names(rx_out)[i],
                                                          0,
                                                          em_path,
                                                          res_path)
                                      }
                                      
                                      # create a vector from 25-100 years in 25 year bins
                                      timestep <- seq(25, 100, 25)
                                      
                                      # assign the vector names, otherwise position will be 
                                      # off by 1 from the value
                                      names(timestep) <- as.character(timestep)
                                      
                                      # calculate the remaining fuel for each timestep and
                                      # add to the fuelbed
                                      lapply(timestep, function(i) {
                                              
                                              # create post RX recovered fuelbed for year i
                                              # Note: rx_out[['first']] contains year 0 post RX burn (pre-wildfire)
                                              #       remaining residue without fuelbed. They are calculated in calc_emissions().
                                              post_rx <- add_rx_residue(rx_out[["first"]], fuel_df, i)
                                              
                                              # change fire weather value names appropriately
                                              post_rx[, ':=' (Wind_corrected = Wind_corrected_97,
                                                              Fm10  = Fm10_97,
                                                              Fm1000 = Fm1000_97)]
                                              
                                              # burn the recovered fuelbed with wildfire
                                              output_df <- burn_residue(post_rx, "None")
                                              
                                              # save the output
                                              save_output(output_df,
                                                          Silvicultural_Treatment,
                                                          ID,
                                                          Burn_Type,
                                                          Tile_Number,
                                                          Fraction_Piled,
                                                          Fraction_Scattered,
                                                          Biomass_Collection,
                                                          Pulp_Market,
                                                          secondary_burn = "first",
                                                          i,
                                                          em_path,
                                                          res_path)
                                              
                                      })
                                      # wildfire scenarios
                              } else {
                                      
                                      # create a vector from 0-100 years
                                      timestep <- seq(0, 100, 25)
                                      
                                      # assign the vector names, otherwise position will be 
                                      # off by 1 from the value
                                      names(timestep) <- as.character(timestep)
                                      
                                      # calculate the remaining fuel for each timestep,
                                      # add to the fuelbed, and burn it
                                      lapply(timestep, function(i) {
                                              
                                              # need to copy dt or it is modified 
                                              cpy <- copy(fuel_df)
                                              
                                              # calculate piled load
                                              cpy <- pile_residue(cpy, i)
                                              
                                              # add the remaining residue to the fuelbed
                                              cpy <-  add_residue(cpy, i)
                                              
                                              # change fire weather value names appropriately
                                              cpy[, ':=' (Wind_corrected = Wind_corrected_97,
                                                          Fm10  = Fm10_97,
                                                          Fm1000 = Fm1000_97)]
                                              
                                              # burn it
                                              output_df <- burn_residue(cpy, Burn_Type)
                                              
                                              # save the output
                                              save_output(output_df,
                                                          Silvicultural_Treatment,
                                                          ID,
                                                          Burn_Type,
                                                          Tile_Number,
                                                          Fraction_Piled,
                                                          Fraction_Scattered,
                                                          Biomass_Collection,
                                                          Pulp_Market,
                                                          secondary_burn = "first",
                                                          i,
                                                          em_path,
                                                          res_path)
                                              
                                      })
                                      
                              }
                              
                      })
}
