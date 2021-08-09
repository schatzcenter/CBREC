################################################################################
# This script adds the residual fuel from RX burns back onto a recovered fuelbed
# at each timestep. Includes decay. This is part of the CA Biopower Impact
# Project.
#
# dt_rx: rx burn output data table
# dt_fuel: fuelbed without additional residues added
# timestep: numeric
#
# Author: Micah Wright, Humboldt State University
################################################################################

add_rx_residue <- function(dt_rx, dt_fuel, timestep) {
        
        # Variables assigned to *_initial variables represent post RX burn (pre-wildfire)
        #       remaining residue without fuelbed. They are calculated in calc_emissions().
        # Note: one_hr and ten_hr are guaranteed zero by con_calc_activity, and
        #       pile_load is guaranteed zero by calc_emissions() 
        dt <- merge(dt_fuel,
                    dt_rx[, .(x, 
                              y,
                              duff_upper_loading_initial = duff_upper_loading,
                              litter_loading_initial = litter_loading,
                              one_hr_sound_initial = one_hr_sound,
                              ten_hr_sound_initial = ten_hr_sound,
                              hun_hr_sound_initial = hun_hr_sound,
                              oneK_hr_sound_initial = oneK_hr_sound,
                              oneK_hr_rotten_initial = oneK_hr_rotten,
                              tenK_hr_sound_initial = tenK_hr_sound,
                              tenK_hr_rotten_initial = tenK_hr_rotten,
                              tnkp_hr_sound_initial = tnkp_hr_sound,
                              tnkp_hr_rotten_initial = tnkp_hr_rotten,
                              pile_load_initial = pile_load)],
                    by = c("x", "y"))
        
        # Decay year 0 post RX burn (pre-wildfire) residue (w/o fuelbed) to the next time step
        dt[, ':=' (duff_toadd = decay_foliage(litter_loading_initial, 
                                              Foliage_K, 
                                              timestep,
                                              "duff") + 
                           to_duff(one_hr_sound_initial,
                                   FWD_K,
                                   timestep) +
                           to_duff(ten_hr_sound_initial,
                                   FWD_K,
                                   timestep) +
                           to_duff(hun_hr_sound_initial,
                                   FWD_K,
                                   timestep) +
                           to_duff(oneK_hr_sound_initial,
                                   CWD_K,
                                   timestep) +
                           to_duff(oneK_hr_rotten_initial,
                                   CWD_K,
                                   timestep) +
                           to_duff(tenK_hr_sound_initial,
                                   CWD_K,
                                   timestep) +
                           to_duff(tenK_hr_rotten_initial,
                                   CWD_K,
                                   timestep) +
                           to_duff(tnkp_hr_sound_initial,
                                   CWD_K,
                                   timestep) +
                           to_duff(tnkp_hr_rotten_initial,
                                   CWD_K,
                                   timestep) +
                           duff_upper_loading_initial,
                   litter_toadd = decay_foliage(litter_loading_initial, 
                                                Foliage_K,
                                                timestep,
                                                "foliage"),
                   one_hr_toadd = decay_fun(one_hr_sound_initial,
                                               FWD_K,
                                               timestep),
                   ten_hr_toadd = decay_fun(ten_hr_sound_initial,
                                            FWD_K,
                                            timestep),
                   hun_hr_toadd = decay_fun(hun_hr_sound_initial,
                                            FWD_K,
                                            timestep),
                   oneK_hr_sound_toadd = decay_woody(oneK_hr_sound_initial,
                                                     CWD_K,
                                                     timestep,
                                                     "sound"),
                   oneK_hr_rotten_toadd = (decay_woody(oneK_hr_sound_initial,
                                                      CWD_K,
                                                      timestep,
                                                      "rotten") +
                                                   decay_fun(oneK_hr_rotten_initial,
                                                             CWD_K,
                                                             timestep)),
                   tenK_hr_sound_toadd = decay_woody(tenK_hr_sound_initial, 
                                                     CWD_K,
                                                     timestep,
                                                     "sound"),
                   tenK_hr_rotten_toadd = (decay_woody(tenK_hr_sound_initial, 
                                                      CWD_K,
                                                      timestep,
                                                      "rotten") + 
                                                   decay_fun(tenK_hr_rotten_initial,
                                                             CWD_K,
                                                             timestep)),
                   tnkp_hr_sound_toadd = decay_woody(tnkp_hr_sound_initial, 
                                                     CWD_K,
                                                     timestep,
                                                     "sound"),
                   tnkp_hr_rotten_toadd = (decay_woody(tnkp_hr_sound_initial, 
                                                       CWD_K,
                                                       timestep,
                                                       "rotten") +
                                                   decay_fun(tnkp_hr_rotten_initial,
                                                             CWD_K,
                                                             timestep)))]
        
        # Calculate new residue fraction: residue / (fuelbed + residue)
        # Note: propfuel() is found in add_residue.R
        dt[, ':=' (duff_upper_load_pr = propfuel(duff_upper_loading,
                                                 duff_toadd,
                                                 1),
                   litter_loading_pr = propfuel(litter_loading,
                                                litter_toadd,
                                                1),
                   one_hr_sound_pr = propfuel(one_hr_sound, 
                                              one_hr_toadd,
                                              1),
                   ten_hr_sound_pr = propfuel(ten_hr_sound, 
                                              ten_hr_toadd,
                                              1),
                   hun_hr_sound_pr = propfuel(hun_hr_sound,
                                              hun_hr_toadd,
                                              1),
                   oneK_hr_sound_pr = propfuel(oneK_hr_sound,
                                               oneK_hr_sound_toadd,
                                               1),
                   oneK_hr_rotten_pr = propfuel(oneK_hr_rotten,
                                                oneK_hr_rotten_toadd,
                                                1),
                   tenK_hr_sound_pr = propfuel(tenK_hr_sound,
                                               tenK_hr_sound_toadd,
                                               1),
                   tenK_hr_rotten_pr = propfuel(tenK_hr_rotten,
                                                tenK_hr_rotten_toadd,
                                                1),
                   tnkp_hr_sound_pr = propfuel(tnkp_hr_sound,
                                               tnkp_hr_sound_toadd,
                                               1),
                   tnkp_hr_rotten_pr = propfuel(tnkp_hr_rotten,
                                                tnkp_hr_rotten_toadd,
                                                1),
                   Year = timestep)]
        
        # Add original fuelbed back with post-decay remaining residue
        dt[, ':=' (duff_upper_loading = duff_upper_loading + duff_toadd,
                   litter_loading = litter_loading + litter_toadd,
                   one_hr_sound = one_hr_sound + one_hr_toadd,
                   ten_hr_sound = ten_hr_sound + ten_hr_toadd,
                   hun_hr_sound = hun_hr_sound + hun_hr_toadd,
                   oneK_hr_sound = oneK_hr_sound + oneK_hr_sound_toadd,
                   oneK_hr_rotten = oneK_hr_rotten + oneK_hr_rotten_toadd,
                   tenK_hr_sound = tenK_hr_sound + tenK_hr_sound_toadd,
                   tenK_hr_rotten = tenK_hr_rotten + tenK_hr_rotten_toadd,
                   tnkp_hr_sound = tnkp_hr_sound + tnkp_hr_sound_toadd,
                   tnkp_hr_rotten = tnkp_hr_rotten + tnkp_hr_rotten_toadd,
                   pile_load = pile_load_initial)]
        
        # Update *_initial columns to store total exposed mass (residue + fuelbed) before passing through combustion.
        #       This is important for making the calculation of total exposed biomass for residue only
        #       in calc_emissions() correct.
        dt[,':=' (duff_upper_loading_initial = duff_upper_loading,
                  litter_loading_initial = litter_loading,
                  one_hr_sound_initial = one_hr_sound,
                  ten_hr_sound_initial = ten_hr_sound,
                  hun_hr_sound_initial = hun_hr_sound,
                  oneK_hr_sound_initial = oneK_hr_sound,
                  oneK_hr_rotten_initial = oneK_hr_rotten,
                  tenK_hr_sound_initial = tenK_hr_sound,
                  tenK_hr_rotten_initial = tenK_hr_rotten,
                  tnkp_hr_sound_initial = tnkp_hr_sound,
                  tnkp_hr_rotten_initial = tnkp_hr_rotten)]
        
        # Calculate duff and litter depth
        dt[, ':='  (duff_upper_depth = zero_div(duff_upper_loading,
                                                duff_upper_ratio),
                    litter_depth = zero_div(litter_loading,
                                            litter_ratio))]
        
        dt[, c("duff_toadd",
               "litter_toadd",
               "one_hr_toadd",
               "ten_hr_toadd",
               "hun_hr_toadd",
               "oneK_hr_sound_toadd", 
               "oneK_hr_rotten_toadd", 
               "tenK_hr_sound_toadd",
               "tenK_hr_rotten_toadd",
               "tnkp_hr_sound_toadd", 
               "tnkp_hr_rotten_toadd") := NULL]
        
        return(dt)
}
