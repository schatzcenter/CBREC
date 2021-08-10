################################################################################
# This script decays treatment residues as part of the California Biopower 
# Impact Project. 
#
# Author: Micah Wright, Humboldt State University
################################################################################
#
#Changes
#2019-06-24 jerome: Changed decay_woody function as I disagree with using Rebain et al., 2010
#                   equation as a decay constant. Modified to act just like decay_foliage,
#                   using 64% as cutoff. Same decay factor applied to both sound and rotten material.

# basic decay function 
# amount returned is mass remaining
decay_fun <- function(residue, k_val, t) {
        
        return(residue * exp(-k_val * t))
        
}

# function to add woody fuels to duff at 2% of decayed mass per year and decay  
# previously added mass
to_duff <- function(residue, k_val, t) {
        
        duff_added <- (decay_fun(residue, k_val, 0) - decay_fun(residue, k_val, t)) * 0.02
        
        net <- decay_fun(duff_added, 0.002, t)
        
        return(net)
        
}

# foliage-specific function that calculates decayed foliage and additions to duff
decay_foliage <- function(residue, k_val, t, toggle) {
        
        decayed <- decay_fun(residue, k_val, t)
        
        still_litter <- decayed >= residue * 0.5
        
        decayed_adj <- ifelse(still_litter, decayed, 0)
        
        last_year <- floor(log(0.5) / -k_val)
        
        dfa <- ifelse(still_litter, to_duff(residue, k_val, t), 
                      decay_fun(decay_fun(residue, k_val, last_year), 0.002, t - last_year))
        
        if(toggle == "foliage") {
                
                return(decayed_adj)
        }
        
        if(toggle == "duff") {
                
                return(dfa)
        }
}

# function for woody fuels with transition from sound to rotten at 64%
decay_woody <- function(residue, k_val, t, toggle) {
        
        decayed <- decay_fun(residue, k_val, t)
        
        frac_soft <- log(1 - k_val) / log(0.64) #From Rebain et al., 2010
        
        still_sound <- decayed >= residue * 0.64 #64% from Rebain et al., 2010
        
        decayed_sound <- ifelse(still_sound, decayed * (1 - frac_soft), 0)
        
        decayed_rotten <- ifelse(!still_sound, decayed, decayed * frac_soft)
        
        if(toggle == "sound") {
                
                return(decayed_sound)
        }
        
        if(toggle == "rotten") {
                
                return(decayed_rotten)
        }
        
}
