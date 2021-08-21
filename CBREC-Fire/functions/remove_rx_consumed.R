################################################################################
# This script removes the consumed fuel following RX burns when the fuelbed is  
# scheduled to be consumed in a wildfire within a short period of time. It does 
# not consider decay. This script is part of the C-BREC Fire Module.
#
# dt: output from con_calc_activity_fast
#
# Authors: Micah Wright
#          Jerome Carman
################################################################################


remove_rx_consumed <- function(dt, burn_type) {
        
        # copy 
        pdt <- copy(dt)
        
        if (burn_type == "Pile") {
                
                # Pile load should not be zero post pile burn. Also, removed char_*_rx column. Just keep and overwrite char columns
                pdt[, ":=" (pile_load = pile_load - (flamg_pile +
                                                     smoldg_pile +
                                                     resid_pile) - pile_char)]
                
                # Update pile_load_initial. Do not update other *_initial variables. Need to retain original scattered load for wildfire
                pdt[, ":=" (pile_load_initial = pile_load)]
                
        } else {
                
                # Caclulate remaining fuel for each size class
                #   Subtracting char from exposed mass here rather. Just keep and overwrite char columns
                pdt[, ':=' (duff_upper_loading = duff_upper_loading - total_duff,
                            duff_upper_depth = duff_upper_loading * duff_upper_ratio,
                            litter_loading = litter_loading - total_litter,
                            litter_depth = litter_loading * litter_ratio,
                            one_hr_sound = 0,
                            ten_hr_sound = 0,
                            hun_hr_sound = hun_hr_sound - total_100 - char_100,
                            oneK_hr_sound = oneK_hr_sound - total_OneK_snd - char_OneK_snd,
                            oneK_hr_rotten = oneK_hr_rotten - total_OneK_rot - char_OneK_rot,
                            tenK_hr_sound = tenK_hr_sound - total_tenK_snd - char_tenK_snd,
                            tenK_hr_rotten = tenK_hr_rotten - total_tenK_rot - char_tenK_rot,
                            tnkp_hr_sound = tnkp_hr_sound - total_tnkp_snd - char_tnkp_snd,
                            tnkp_hr_rotten = tnkp_hr_rotten - total_tnkp_rot - char_tnkp_rot,
                            pile_load = pile_load - (flamg_pile +
                                                     smoldg_pile +
                                                     resid_pile) - pile_char)]
                
                #Update columns that store total exposed mass before passing through combustion.
                #  This is important for making the calculation of total exposed biomass for residue only
                #  in calc_emissions() correct.
                pdt[,':=' (duff_upper_loading_initial = duff_upper_loading,
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
                           pile_load_initial = pile_load)]
                
        }
        
        return(pdt)
}