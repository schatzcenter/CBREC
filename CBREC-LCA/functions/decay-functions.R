# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
# 
#         Micah Wright (wrightmicahc@gmail.com)
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2019-02-27: Initialized Script
#   2019-03-19: Updated decay_fun to include all decay calculations
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This module contains decay functions for use in CBREC
#
# ---------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#
# ---------------------------------
# OBJECTIVE:
# This module contains decay functions for use in CBREC
# =============================================================================


# Decay - Remaining Mass
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function calculates the remaining mass after time t.  
# 
# -------------------------------------------------------------------------------------------------
# INPUTS:
# residue: The initial mass of residue. If set to 1, it just calculates the fraction of initial mass that is remaining.
# k_val: The decay rate. Unit is per time. Unit of time must equal unit of variable t.
# t: The time over which to calculate the decayed mass. Units must match unit of k_val variable.
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# Remaining mass after time t. If residue is set to 1, this is fraction of initial mass remaining.
# -------------------------------------------------------------------------------------------------
decay_mass_remaining <- function(residue, k_val, t) {
  return(residue * exp(-k_val * t))
}

# Decay - Mass Lost
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function calculates mass lost after time t.  
# 
# -------------------------------------------------------------------------------------------------
# INPUTS:
# residue: The initial mass of residue. If set to 1, it just calculates the fraction of initial mass that is lost.
# k_val: The decay rate. Unit is per time. Unit of time must equal unit of variable t.
# t: The time over which to calculate the decayed mass. Units must match unit of k_val variable.
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# Mass lost after time t. If residue is set to 1, this is fraction of initial mass that is lost.
# -------------------------------------------------------------------------------------------------
decay_mass_lost <- function(residue, k_val,t) {
  return(residue * (1 - (exp(-k_val*t))))
}

#####################################################################################
# piled_k_const: A function to calculate the decay constant for a given cell 
# incorporating pile fractions. 
#####################################################################################
# Piled k constant (Function by Max Blasdel)
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function calculates an adjusted decay constant for piled material
# 
# -------------------------------------------------------------------------------------------------
# INPUTS:
# k_const: the k value for given cell and residue size class
# per_ag: percent above ground, decimal less than 1. The proportion of a pile that receives the piled coefficient. per_ag + per_gc = 1.
# per_gc: percent ground contact, decimal less than 1. the proportion that is considered the same as scattered from decay. per_ag + per_gc = 1.
# coEf: the piled coefficient value that characterizes a reduction in k_const for suspended materials (i.e. above ground)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# Adjusted pile k constant for given cell/residue size class
# -------------------------------------------------------------------------------------------------
piled_k_const <- function(k_const, coEf = 0.721, per_ag = .892, per_gc = .108) {
  
  k_pile <- ((k_const * coEf) * per_ag) + (k_const * per_gc)
  
  return(k_pile)
}

# Main Decay Function
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function will, for a given residue disposition (scattered or piled) calculate
# the mass decayed over a time period along all residue size classes (EXCLUDING duff). The decayed
# mass is either allocated to duff or emissions; if decay has progressed enough, foliage decay 
# is moved entirely to the duff size class.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data and
#   time series of decay and char data.
# residue.disposition: Residue disposition; scattered or piled
# year.i: The year for which we want to calculate the decay and emissions from the year prior (i.e.,
# if t=3, we would calculate the decay between years 2-3.)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# -------------------------------------------------------------------------------------------------

decay_fun <- function(cbrec.dt,residue.disposition,year.i) {
  
  # calculating the time (in years) elapsed between entries in the cbrec.dt
  # year 1 always 1
  # COMMENT NO LONGER RELEVANT; ERASE IF CODE FIX FINALIZED # first year after year 1 is the time interval minus 1
  # t <- ifelse(year.i == 1, 1, time_int)
  # ARH 8/21/20 update: to fix foliage to duff error issue, first X years (up to the time_int) are run at a 1 year resolution
  t <- ifelse(year.i <= time_int, 1, time_int)
  # if(year.i == time_int & time_int != 1){t <- time_int - 1} # No longer relevant in code fix
  
  decay_emissions_profile <- data.table(CO2_tonnes=0, CO_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SO2_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  # Piled residues will have a slightly different decay rate becasue of varying ground contact. The piled k constant is dervied from the original k constant
  if(residue.disposition=='Piled') { 
    # Piled residues; use pile-adjusted k constant
    cbrec.dt[,':='(decay_CWD_mass_year_i = decay_mass_lost(get(paste(residue.disposition,"CWD_tonnesAcre",sep="_")),piled_k_const(CWD_cm),t),
                   decay_FWD_mass_year_i = decay_mass_lost(get(paste(residue.disposition,"FWD_tonnesAcre",sep="_")),piled_k_const(FWD_cm),t),
                   decay_Foliage_mass_year_i = decay_mass_lost(get(paste(residue.disposition,"Foliage_tonnesAcre",sep="_")),piled_k_const(Foliage_cm),t),
                   decay_Foliage_cumfrac = decay_mass_remaining(1,piled_k_const(Foliage_cm),year.i))]
  } else {
    # Non-piled residues, use standard k constant
    cbrec.dt[,':='(decay_CWD_mass_year_i = decay_mass_lost(get(paste(residue.disposition,"CWD_tonnesAcre",sep="_")),CWD_cm,t),
                   decay_FWD_mass_year_i = decay_mass_lost(get(paste(residue.disposition,"FWD_tonnesAcre",sep="_")),FWD_cm,t),
                   decay_Foliage_mass_year_i = decay_mass_lost(get(paste(residue.disposition,"Foliage_tonnesAcre",sep="_")),Foliage_cm,t),
                   decay_Foliage_cumfrac = decay_mass_remaining(1,Foliage_cm,year.i))]
  }
  
  # Apply the mass lost to the dynamic mass columns. Dynamic mass columns represent mass remaining.
  cbrec.dt[,(paste(residue.disposition,"CWD_tonnesAcre",sep="_")) := get(paste(residue.disposition,"CWD_tonnesAcre",sep="_")) - decay_CWD_mass_year_i]
  cbrec.dt[,(paste(residue.disposition,"FWD_tonnesAcre",sep="_")) := get(paste(residue.disposition,"FWD_tonnesAcre",sep="_")) - decay_FWD_mass_year_i]
  cbrec.dt[,(paste(residue.disposition,"Foliage_tonnesAcre",sep="_")) := get(paste(residue.disposition,"Foliage_tonnesAcre",sep="_")) - decay_Foliage_mass_year_i]
  
  # Calculate mass converted to duff, and populate the duff mass column. If the the residue segment is previously fired residues, this will go to the previously fired duff column.
  if(residue.disposition=='prev_fired') {
    cbrec.dt[,prev_fired_Duff_tonnesAcre := prev_fired_Duff_tonnesAcre + duff_decay_mass_fraction * (decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)]
    
    # Check if the foliage mass should be at least 1/2 decayed using decay_Foliage_cumfrac; if so, convert the rest of the foliage to duff. NOTE TO ANDY: if you get the brilliant idea to track decay loss, 
    # this will not work - if (theoretically) 75% of the mass is lost to wildfire before it can decay, the transition to duff would never happen. Base the switch on the expected decay.
    cbrec.dt[decay_Foliage_cumfrac <= foliage_to_duff_trigger, prev_fired_Duff_tonnesAcre := prev_fired_Duff_tonnesAcre + get(paste(residue.disposition,"Foliage_tonnesAcre",sep="_"))]
    cbrec.dt[decay_Foliage_cumfrac <= foliage_to_duff_trigger, paste(residue.disposition,"Foliage_tonnesAcre",sep="_") := 0]
    
  } else {
    cbrec.dt[,Duff_tonnesAcre := Duff_tonnesAcre + duff_decay_mass_fraction * (decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)]
    
    # Check if the foliage mass should be at least 1/2 decayed using decay_Foliage_cumfrac; if so, convert the rest of the foliage to duff. NOTE TO ANDY: if you get the brilliant idea to track decay loss, 
    # this will not work - if (theoretically) 75% of the mass is lost to wildfire before it can decay, the transition to duff would never happen. Base the switch on the expected decay.
    cbrec.dt[decay_Foliage_cumfrac <= foliage_to_duff_trigger, Duff_tonnesAcre := Duff_tonnesAcre + get(paste(residue.disposition,"Foliage_tonnesAcre",sep="_"))]
    cbrec.dt[decay_Foliage_cumfrac <= foliage_to_duff_trigger, paste(residue.disposition,"Foliage_tonnesAcre",sep="_") := 0]
  }
  
  
  # Calculate emissions - decay will generate CO2 and CH4, all other species are 0. Emissions will be based on the amount of carbon decayed.
  # CH4 
  decay_emissions_profile[,CH4_tonnes := CH4_tonnes + cbrec.dt[,sum((decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)*Carbon_frac)] * (1-duff_decay_mass_fraction) * acres_per_cell * CH4_decay_emissions_fraction * CH4_carbon_fraction]
  
  # CO2 Any carbon not emitted as CH4 will be as CO2.
  decay_emissions_profile[,CO2_tonnes := CO2_tonnes + cbrec.dt[,sum((decay_CWD_mass_year_i + decay_FWD_mass_year_i + decay_Foliage_mass_year_i)*Carbon_frac)] * (1-duff_decay_mass_fraction) * acres_per_cell * (1 - CH4_decay_emissions_fraction) * CO2_carbon_fraction]
  
  return(list(decay_emissions_profile,cbrec.dt))
}


# Duff Decay Function
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function calculates the mass decayed over a period of time. This is separate from the main
# residue size classes.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data and
#   100 years of decay and char data.
# year.i: The year for which we want to calculate the decay and emissions from the year prior (i.e.,
# if t=3, we would calculate the decay between years 2-3.)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# -------------------------------------------------------------------------------------------------

duff_decay_fun <- function(cbrec.dt,year.i) {
  
  # calculating the time (in years) elapsed between entries in the cbrec.dt
  # year 1 always 1
  # COMMENT NO LONGER RELEVANT; ERASE IF CODE FIX FINALIZED # first year after year 1 is the time interval minus 1
  # t <- ifelse(year.i == 1, 1, time_int)
  # ARH 8/21/20 update: to fix foliage to duff error issue, first X years (up to the time_int) are run at a 1 year resolution
  t <- ifelse(year.i <= time_int, 1, time_int)
  # if(year.i == time_int & time_int != 1){t <- time_int - 1} # No longer relevant in code fix
  
  decay_emissions_profile <- data.table(CO2_tonnes=0, CO_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SO2_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  
  # We should have accumulated duff from all residues prior to calling this function. Now we can decay duff, add the resulting emissions, and adjust duff mass totals
  decay_emissions_profile[,CH4_tonnes := CH4_tonnes + cbrec.dt[,sum(decay_mass_lost(Duff_tonnesAcre,duff_k_val,t) * Carbon_frac)] * acres_per_cell * CH4_decay_emissions_fraction * CH4_carbon_fraction]
  decay_emissions_profile[,CO2_tonnes := CO2_tonnes + cbrec.dt[,sum(decay_mass_lost(Duff_tonnesAcre,duff_k_val,t) * Carbon_frac)] * acres_per_cell * (1-CH4_decay_emissions_fraction) * CO2_carbon_fraction]
  
  # We need to output the decay mass lost
  duff.decay.year.i <- cbrec.dt[,sum(decay_mass_lost(Duff_tonnesAcre,duff_k_val,t))]
  
  # Now apply the mass loss
  cbrec.dt[,Duff_tonnesAcre := Duff_tonnesAcre - decay_mass_lost(Duff_tonnesAcre,duff_k_val,t)]
  
  # Repeat for previously fired duff.
  decay_emissions_profile[,CH4_tonnes := CH4_tonnes + cbrec.dt[,sum(decay_mass_lost(prev_fired_Duff_tonnesAcre,duff_k_val,t) * Carbon_frac)] * acres_per_cell * CH4_decay_emissions_fraction * CH4_carbon_fraction]
  decay_emissions_profile[,CO2_tonnes := CO2_tonnes + cbrec.dt[,sum(decay_mass_lost(prev_fired_Duff_tonnesAcre,duff_k_val,t) * Carbon_frac)] * acres_per_cell * (1-CH4_decay_emissions_fraction) * CO2_carbon_fraction]
  prev.fired.duff.decay.year.i <- cbrec.dt[,sum(decay_mass_lost(prev_fired_Duff_tonnesAcre,duff_k_val,t))]
  cbrec.dt[,prev_fired_Duff_tonnesAcre := prev_fired_Duff_tonnesAcre - decay_mass_lost(prev_fired_Duff_tonnesAcre,duff_k_val,t)]
  
  return(list(decay_emissions_profile,cbrec.dt,(duff.decay.year.i + prev.fired.duff.decay.year.i)))
}