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
#   2019-03-19: Initialized Script
#
# -------------------------------------------------------------------------

# FUNCTION: Prescribed Burn Processing Function
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This function will collect and calculate needed data from the wildfire model outputs for year 1 prescribed burning
# -------------------------------------------------------------------------------------------------
# INPUTS:
# wildfire.data.directory: The directory storing all of the wildfire data
# scenario.ID: The scenario matrix identifier string to select the correct wildfire data files
# tile.list: The wildfire tiles contained within the study area
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# wildfire.data: combined combustion fraction, char fraction, and emissions factor data from prescribed burns in 
# year 1.
# =================================================================================================
zero_div <- function(x, y) {
     return(ifelse(y == 0, 0, x / y))
}

prescribed_burn_processing_fun <- function(cbrec.dt,
                                           wildfire.data.directory,
                                           scenario.ID,
                                           tile.list) {
  
  if(debug_CBREC) {
    cat("Running prescribed_burn_processing_fun() function\n", file=log_file, append=T)
  }
  
  prescribed.burn.data <- data.table()
  
  # Cycle through tile_list, load the .rds files, and combine into a single data table.
  # This is a prescribed burn, so we use the "first" file for year 0.
  ## NOTE: we may run CBREC on a per tile basis or smaller in which case we could get rid of this loop
  for (tile in tile.list) {
    burn.emissions.directory <- paste0(wildfire.data.directory,
                                       tile,
                                       "/")
    
    # Load up fire data and trim unneeded columns.
    prescribed.burn.filename <- list.files(burn.emissions.directory)[grepl(paste(scenario.ID,tile,"0.rds",sep="-"),
                                                                           list.files(burn.emissions.directory))]
    new.tile <- readRDS(paste0(burn.emissions.directory,
                              prescribed.burn.filename))
    
    prescribed.burn.data <- rbind(prescribed.burn.data, new.tile)
    
    new.tile <- NULL
  }
  
  # From the fire data, we need to calculate: emissions factors, combustion fractions, char fractions, and unburned fractions.
  #   Pile burn data from the fire model is not broken out by size class, and we only have numbers for consumed, not exposed residues.
  #   In pile burn cases, emissions factors, combustion rates, and char production is equivalent for all size classes, so we only need to calculate
  #   one value. Because we only have "piled consumed" values, but a set combustion fraction (pile.burn.combustion.frac variable), we base our
  #   emissions factor on (consumed / pile.burn.combustion.frac).
  prescribed.burn.data[,':='(
    # Combustion and char fractions. Scattered residues (piled are fixed); foliage and duff do not char
    CWD_Scattered_CombustionFrac = zero_div(total_cwd_consumed, total_cwd_exposed),
    FWD_Scattered_CombustionFrac = zero_div(total_fwd_consumed, total_fwd_exposed),
    Foliage_Scattered_CombustionFrac = zero_div(total_foliage_consumed, total_foliage_exposed),
    Duff_Scattered_CombustionFrac = zero_div(total_duff_consumed, total_duff_exposed),
    CWD_Scattered_CharFrac = zero_div(char_cwd_residue, total_cwd_exposed),
    FWD_Scattered_CharFrac = zero_div(char_fwd_residue, total_fwd_exposed),
    # Emissions factors - CH4
    Duff_Scattered_CH4_EmFac = zero_div(total_duff_residue_CH4, total_duff_exposed),
    Foliage_Scattered_CH4_EmFac = zero_div(total_foliage_residue_CH4, total_foliage_exposed),
    FWD_Scattered_CH4_EmFac = zero_div(total_fwd_residue_CH4, total_fwd_exposed),
    CWD_Scattered_CH4_EmFac = zero_div(total_cwd_residue_CH4, total_cwd_exposed),
    Piled_CH4_EmFac = zero_div(total_pile_CH4, (total_pile_consumed/pile.burn.combustion.frac)),
    # CO
    Duff_Scattered_CO_EmFac = zero_div(total_duff_residue_CO, total_duff_exposed),
    Foliage_Scattered_CO_EmFac = zero_div(total_foliage_residue_CO, total_foliage_exposed),
    FWD_Scattered_CO_EmFac = zero_div(total_fwd_residue_CO, total_fwd_exposed),
    CWD_Scattered_CO_EmFac = zero_div(total_cwd_residue_CO, total_cwd_exposed),
    Piled_CO_EmFac = zero_div(total_pile_CO, (total_pile_consumed/pile.burn.combustion.frac)),
    # NOx
    Duff_Scattered_NOx_EmFac = zero_div(total_duff_residue_NOx, total_duff_exposed),
    Foliage_Scattered_NOx_EmFac = zero_div(total_foliage_residue_NOx, total_foliage_exposed),
    FWD_Scattered_NOx_EmFac = zero_div(total_fwd_residue_NOx, total_fwd_exposed),
    CWD_Scattered_NOx_EmFac = zero_div(total_cwd_residue_NOx, total_cwd_exposed),
    Piled_NOx_EmFac = zero_div(total_pile_NOx, (total_pile_consumed/pile.burn.combustion.frac)),
    # PM10
    Duff_Scattered_PM10_EmFac = zero_div(total_duff_residue_PM10, total_duff_exposed),
    Foliage_Scattered_PM10_EmFac = zero_div(total_foliage_residue_PM10, total_foliage_exposed),
    FWD_Scattered_PM10_EmFac = zero_div(total_fwd_residue_PM10, total_fwd_exposed),
    CWD_Scattered_PM10_EmFac = zero_div(total_cwd_residue_PM10, total_cwd_exposed),
    Piled_PM10_EmFac = zero_div((total_pile_clean_PM10 + total_pile_vdirty_PM10), (total_pile_consumed/pile.burn.combustion.frac)),
    # PM2.5
    Duff_Scattered_PM2.5_EmFac = zero_div(total_duff_residue_PM2.5, total_duff_exposed),
    Foliage_Scattered_PM2.5_EmFac = zero_div(total_foliage_residue_PM2.5, total_foliage_exposed),
    FWD_Scattered_PM2.5_EmFac = zero_div(total_fwd_residue_PM2.5, total_fwd_exposed),
    CWD_Scattered_PM2.5_EmFac = zero_div(total_cwd_residue_PM2.5, total_cwd_exposed),
    Piled_PM2.5_EmFac = zero_div((total_pile_clean_PM2.5 + total_pile_vdirty_PM2.5), (total_pile_consumed/pile.burn.combustion.frac)),
    # SO2
    Duff_Scattered_SO2_EmFac = zero_div(total_duff_residue_SO2, total_duff_exposed),
    Foliage_Scattered_SO2_EmFac = zero_div(total_foliage_residue_SO2, total_foliage_exposed),
    FWD_Scattered_SO2_EmFac = zero_div(total_fwd_residue_SO2, total_fwd_exposed),
    CWD_Scattered_SO2_EmFac = zero_div(total_cwd_residue_SO2, total_cwd_exposed),
    Piled_SO2_EmFac = zero_div(total_pile_SO2, (total_pile_consumed/pile.burn.combustion.frac)),
    # VOC
    Duff_Scattered_VOC_EmFac = zero_div(total_duff_residue_VOC, total_duff_exposed),
    Foliage_Scattered_VOC_EmFac = zero_div(total_foliage_residue_VOC, total_foliage_exposed),
    FWD_Scattered_VOC_EmFac = zero_div(total_fwd_residue_VOC, total_fwd_exposed),
    CWD_Scattered_VOC_EmFac = zero_div(total_cwd_residue_VOC, total_cwd_exposed),
    Piled_VOC_EmFac = zero_div(total_pile_VOC, (total_pile_consumed/pile.burn.combustion.frac))
  )]
  
  # Because we truncate fire outputs to the nearest gram, cells with very small levels of residue (<1 kg) may have a combustion 
  # and char fraction that add up to more than 1; this would result in negative masses. In these situations, if combustion_frac + char_frac > 1,
  # set char_frac to 1 - combustion_frac. This will need to be repeated for all wildfire data components.
  prescribed.burn.data[CWD_Scattered_CombustionFrac + CWD_Scattered_CharFrac > 1, CWD_Scattered_CharFrac := 1 - CWD_Scattered_CombustionFrac]
  prescribed.burn.data[FWD_Scattered_CombustionFrac + FWD_Scattered_CharFrac > 1, FWD_Scattered_CharFrac := 1 - FWD_Scattered_CombustionFrac]
  
  # Trim the now-un-needed columns.
  prescribed.burn.data[,':='(fuelbed_number = NULL, FCID2018 = NULL, total_pile_clean_PM10 = NULL, total_pile_vdirty_PM10 = NULL, total_pile_clean_PM2.5 = NULL, total_pile_vdirty_PM2.5 = NULL, total_pile_CH4 = NULL, total_pile_CO = NULL, total_pile_CO2 = NULL, total_pile_NOx = NULL, total_pile_SO2 = NULL, total_pile_VOC = NULL, pile_char = NULL, char_fwd_residue = NULL, char_cwd_residue = NULL, total_duff_exposed = NULL, total_foliage_exposed = NULL, total_fwd_exposed = NULL, total_cwd_exposed = NULL, total_fuel_consumed = NULL, total_pile_consumed = NULL, total_duff_consumed = NULL, total_foliage_consumed = NULL, total_fwd_consumed = NULL, total_cwd_consumed = NULL, total_duff_residue_CH4 = NULL, total_foliage_residue_CH4 = NULL, total_fwd_residue_CH4 = NULL, total_cwd_residue_CH4 = NULL, total_duff_residue_CO = NULL, total_foliage_residue_CO = NULL, total_fwd_residue_CO = NULL, total_cwd_residue_CO = NULL, total_duff_residue_CO2 = NULL, total_foliage_residue_CO2 = NULL, total_fwd_residue_CO2 = NULL, total_cwd_residue_CO2 = NULL, total_duff_residue_NOx = NULL, total_foliage_residue_NOx = NULL, total_fwd_residue_NOx = NULL, total_cwd_residue_NOx = NULL, total_duff_residue_PM10 = NULL, total_foliage_residue_PM10 = NULL, total_fwd_residue_PM10 = NULL, total_cwd_residue_PM10 = NULL, total_duff_residue_PM2.5 = NULL, total_foliage_residue_PM2.5 = NULL, total_fwd_residue_PM2.5 = NULL, total_cwd_residue_PM2.5 = NULL, total_duff_residue_SO2 = NULL, total_foliage_residue_SO2 = NULL, total_fwd_residue_SO2 = NULL, total_cwd_residue_SO2 = NULL, total_duff_residue_VOC = NULL, total_foliage_residue_VOC = NULL, total_fwd_residue_VOC = NULL, total_cwd_residue_VOC = NULL)]
  
  # Round x and y coordinates for prescribed burn variables
  # prescribed.burn.data[,":="(x=round(x,digits=1),y=round(y,digits=1))] # Round the X and Y coordinates so we can merge with cbrec.dt
  
  # Merge with cbrec.dt
  setkey(cbrec.dt, x, y)
  setkey(prescribed.burn.data, x, y)
  #cbrec.dt <- merge(cbrec.dt, prescribed.burn.data)
  cbrec.dt <- prescribed.burn.data[cbrec.dt]

  # There will be cells where we have residue data but not fire data. This is due to a discrepancy between residue data and FCCS data used for fire emissions, where residue data says there are trees and FCCS disagrees. For fire modeling, 
  # we side with FCCS. This is generally a smaller number of cells, for tile 118, it was half of one percent. We have elected to remove these wildfire-less points from cbrec.dt, essentially removing them from the study.
  cbrec.dt <- cbrec.dt[!is.na(CWD_Scattered_CombustionFrac)]
  
  return(cbrec.dt)
  
}

# Prescribed burning emissions & mass reduction
# -------------------------------------------------------------------------------------------------
# OBJECTIVE:
# This function will calculate mass loss and emissions along all residue size classes due to prescribed
# burning. Unburnt mass remains in the base category, because it may be exposed to wildfire.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data and
#   100 years of decay and char data.
# residue.disposition: Residue disposition; scattered, field piled or landing piled
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# =================================================================================================

prescribed_burn_fun <- function(cbrec.dt, burn.type) {
  
  if(debug_CBREC) {
    cat(paste0("Running prescribed_burn_fun() function for burn.type:",as.character(burn.type),"\n"), file=log_file, append=T)
  }
  
  # Create an emissions output object
  prescribed_burn_emissions <- data.table(CO2_tonnes=0, CO_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SO2_tonnes=0, VOC_tonnes=0, N2O_tonnes=0)
  prescribed_burn_mass <- data.table(combusted.residue_tonnes=0, char_tonnes=0)
  
  # The prescribed burn will affect different residue segments depending on the burn type.
  if(burn.type=="Pile") {
    
    if(debug_CBREC) {
      cat("   prescribed_burn_fun() function if-statement triggered for burn.type: Pile\n", file=log_file, append=T)
    }
    
    # Burn type will affect residue segment affected AND emissions factors used. CO2 emissions will be calculated as the remainder of carbon emissions after the other emissions constituents; 
    # carbon fractions defined in constants.R
    
    # Per the fire model (as of 7/3/2019) 100% of piled residues are exposed to prescribed burns. Not all will burn to completion, but all is exposed.
    
    total.segment.carbon <- cbrec.dt[,sum(Carbon_frac * Piled_CWD_tonnesAcre, Carbon_frac * Piled_FWD_tonnesAcre, Carbon_frac * Piled_Foliage_tonnesAcre)] * acres_per_cell
    total.unburned.carbon <- total.segment.carbon * (1 - pile.burn.combustion.frac - pile.burn.char.frac)
    total.combusted.mass <- cbrec.dt[,sum(Piled_CWD_tonnesAcre, Piled_FWD_tonnesAcre, Piled_Foliage_tonnesAcre)] * pile.burn.combustion.frac * acres_per_cell
    

    # Drip Torch Pile ----------------------------------------------------

    # calc total fuel
    gal_fuel <- gal_fuel_per_acre_pile * nrow(cbrec.dt) * acres_per_cell
    
    # init drip emissions
    drip_emissions <- data.table(CO2_tonnes = 0,
                                 CH4_tonnes = 0,
                                 N2O_tonnes = 0)

    # calc drip emissions in tonnes
    drip_emissions[, ':=' (
      CO2_tonnes = (gal_fuel * CO2_diesel_grams * diesel_mix / 1000000) + (gal_fuel * CO2_gas_grams * gas_mix / 1000000),
      CH4_tonnes = (gal_fuel * CH4_diesel_grams * diesel_mix / 1000000) + (gal_fuel * CH4_gas_grams * gas_mix / 1000000),
      N2O_tonnes = (gal_fuel * N2O_diesel_grams * diesel_mix / 1000000) + (gal_fuel * N2O_gas_grams * gas_mix / 1000000)
    )]
  
    # Calculate some year.i emissions; we'll use this several times
    CO.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre) * Piled_CO_EmFac)] * acres_per_cell
    CH4.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre) * Piled_CH4_EmFac)] * acres_per_cell
    NOx.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre) * Piled_NOx_EmFac)] * acres_per_cell
    PM10.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre) * Piled_PM10_EmFac)] * acres_per_cell
    PM2.5.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre) * Piled_PM2.5_EmFac)] * acres_per_cell
    SO2.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre) * Piled_SO2_EmFac)] * acres_per_cell
    VOC.emissions.year.i <- cbrec.dt[,sum((Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre) * Piled_VOC_EmFac)] * acres_per_cell
    char.year.i <- cbrec.dt[,sum(pile.burn.char.frac * (Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre))] * acres_per_cell
    
    # Any residue left unburnt can still be exposed to wildfire, so we can leave it in the original columns. Add combustion emissions to the emissions 
    # columns, and then adjust the dynamic mass column. Repeat for char. DUFF WILL NOT BE GENERATED YET, SO WE DON'T NEED TO WORRY ABOUT IT.
    # Calculate the other carbon emissions before CO2, the remainder will be lost as CO2.
    prescribed_burn_emissions[,':='(CO_tonnes = CO_tonnes + CO.emissions.year.i,
                                    CH4_tonnes = CH4_tonnes + CH4.emissions.year.i,
                                    NOx_tonnes = NOx_tonnes + NOx.emissions.year.i,
                                    PMUnder10um_tonnes = PMUnder10um_tonnes + PM10.emissions.year.i,
                                    PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + PM2.5.emissions.year.i,
                                    SO2_tonnes = SO2_tonnes + SO2.emissions.year.i,
                                    VOC_tonnes = VOC_tonnes + VOC.emissions.year.i)]
    
    prescribed_burn_mass[,':='(combusted.residue_tonnes = total.combusted.mass,
                               char_tonnes = char.year.i)]
    
    # Any carbon not emitted as CH4, CO, PM10 (which is inclusive of PM2.5), or VOC is emitted as CO2. So we need to determine how much carbon was emitted (the total carbon absent the
    # carbon present in unburnt fuel or char), determine how much the of the emitted carbon was in non-CO2 constituents, then assume the remainder was emitted as CO2 and convert to mass
    # CO2 emissions. Carbon fraction variables are species mass / carbon mass
    non.CO2.combusted.carbon <- (CO.emissions.year.i / CO_carbon_fraction +
                                   CH4.emissions.year.i / CH4_carbon_fraction +
                                   PM10.emissions.year.i / PM10_carbon_fraction + 
                                   VOC.emissions.year.i / VOC_carbon_fraction)
    
    char.carbon <- char.year.i / char_carbon_fraction # All carbon fraction are in species mass / carbon mass
    
    total.combusted.carbon <- total.segment.carbon - total.unburned.carbon - char.carbon
    
    prescribed_burn_emissions[,CO2_tonnes := CO2_tonnes + (total.combusted.carbon - non.CO2.combusted.carbon) * CO2_carbon_fraction]
    
    # add emissions associated with the drip torch
    for (j in names(drip_emissions)) {
     set(prescribed_burn_emissions, i = NULL, j = j, value = prescribed_burn_emissions[[j]] + drip_emissions[[j]]) 
    }
    
    # Apply the mass lost to the dynamic mass columns. 
    # Any piled materials remaining after a prescribed burn are converted to CWD scattered residues, as per the fire model. This is an inconsistency (it transmutes fines and foliage to coarse); this is a version 1.2 change
    cbrec.dt[,Scattered_CWD_tonnesAcre := Scattered_CWD_tonnesAcre + (1 - pile.burn.combustion.frac - pile.burn.char.frac) * (Piled_CWD_tonnesAcre + Piled_FWD_tonnesAcre + Piled_Foliage_tonnesAcre)]
    
    cbrec.dt[,':='(Piled_CWD_tonnesAcre = 0,
                   Piled_FWD_tonnesAcre = 0,
                   Piled_Foliage_tonnesAcre = 0)]
  }
  
  if(burn.type=="Broadcast") { # Broadcast burn affects only scattered residues
    
    if(debug_CBREC) {
      cat("   prescribed_burn_fun() function if-statement triggered for burn.type: Broadcast\n", file=log_file, append=T)
    }
    
    # Burn type will affect residue segment affected AND emissions factors
    # CO2 emissions will be calculated as the remainder of carbon emissions after the other emissions constituents; so we will need carbon fractions as well.
    total.segment.carbon <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * Carbon_frac,
                                          Scattered_FWD_tonnesAcre * Carbon_frac,
                                          Scattered_Foliage_tonnesAcre * Carbon_frac)] * acres_per_cell
    
    total.unburned.carbon <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * Carbon_frac * (1 - CWD_Scattered_CombustionFrac - CWD_Scattered_CharFrac),
                                           Scattered_FWD_tonnesAcre * Carbon_frac * (1 - FWD_Scattered_CombustionFrac - FWD_Scattered_CharFrac),
                                           Scattered_Foliage_tonnesAcre * Carbon_frac * (1 - Foliage_Scattered_CombustionFrac))] * acres_per_cell
    
    total.combusted.mass <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_CombustionFrac,
                                          Scattered_FWD_tonnesAcre * FWD_Scattered_CombustionFrac,
                                          Scattered_Foliage_tonnesAcre * Foliage_Scattered_CombustionFrac)] * acres_per_cell
    
    # Drip Torch Broadcast ----------------------------------------------------

    # calc total fuel
    gal_fuel <- gal_fuel_per_acre_broad * nrow(cbrec.dt) * acres_per_cell
    
    # init drip emissions
    drip_emissions <- data.table(CO2_tonnes = 0,
                                 CH4_tonnes = 0,
                                 N2O_tonnes = 0)
    
    # calc drip emissions in tonnes
    drip_emissions[, ':=' (
      CO2_tonnes = (gal_fuel * CO2_diesel_grams * diesel_mix / 1000000) + (gal_fuel * CO2_gas_grams * gas_mix / 1000000),
      CH4_tonnes = (gal_fuel * CH4_diesel_grams * diesel_mix / 1000000) + (gal_fuel * CH4_gas_grams * gas_mix / 1000000),
      N2O_tonnes = (gal_fuel * N2O_diesel_grams * diesel_mix / 1000000) + (gal_fuel * N2O_gas_grams * gas_mix / 1000000)
    )]
    
    # Any residue left unburnt can still be exposed to wildfire, so we can leave it in the original columns. Add combustion emissions to the emissions 
    # columns, and then adjust the dynamic mass column. Repeat for char. DUFF WILL NOT BE GENERATED YET, SO WE DON'T NEED TO WORRY ABOUT IT.
    # Calculate the other carbon emissions before CO2, the remainder will be lost as CO2.
    CO.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_CO_EmFac + Scattered_FWD_tonnesAcre * FWD_Scattered_CO_EmFac + Scattered_Foliage_tonnesAcre * Foliage_Scattered_CO_EmFac)] * acres_per_cell
    CH4.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_CH4_EmFac + Scattered_FWD_tonnesAcre * FWD_Scattered_CH4_EmFac + Scattered_Foliage_tonnesAcre * Foliage_Scattered_CH4_EmFac)] * acres_per_cell
    NOx.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_NOx_EmFac + Scattered_FWD_tonnesAcre * FWD_Scattered_NOx_EmFac + Scattered_Foliage_tonnesAcre * Foliage_Scattered_NOx_EmFac)] * acres_per_cell
    PM10.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_PM10_EmFac + Scattered_FWD_tonnesAcre * FWD_Scattered_PM10_EmFac + Scattered_Foliage_tonnesAcre * Foliage_Scattered_PM10_EmFac)] * acres_per_cell
    PM2.5.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_PM2.5_EmFac + Scattered_FWD_tonnesAcre * FWD_Scattered_PM2.5_EmFac + Scattered_Foliage_tonnesAcre * Foliage_Scattered_PM2.5_EmFac)] * acres_per_cell
    SO2.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_SO2_EmFac + Scattered_FWD_tonnesAcre * FWD_Scattered_SO2_EmFac + Scattered_Foliage_tonnesAcre * Foliage_Scattered_SO2_EmFac)] * acres_per_cell
    VOC.emissions.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_VOC_EmFac + Scattered_FWD_tonnesAcre * FWD_Scattered_VOC_EmFac + Scattered_Foliage_tonnesAcre * Foliage_Scattered_VOC_EmFac)] * acres_per_cell
    char.year.i <- cbrec.dt[,sum(Scattered_CWD_tonnesAcre * CWD_Scattered_CharFrac + Scattered_FWD_tonnesAcre * FWD_Scattered_CharFrac)] * acres_per_cell # No foliage char
    
    prescribed_burn_emissions[,':='(CO_tonnes = CO_tonnes + CO.emissions.year.i,
                                    CH4_tonnes = CH4_tonnes + CH4.emissions.year.i,
                                    NOx_tonnes = NOx_tonnes + NOx.emissions.year.i,
                                    PMUnder10um_tonnes = PMUnder10um_tonnes + PM10.emissions.year.i,
                                    PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + PM2.5.emissions.year.i,
                                    SO2_tonnes = SO2_tonnes + SO2.emissions.year.i,
                                    VOC_tonnes = VOC_tonnes + VOC.emissions.year.i)]
    
    prescribed_burn_mass[,':='(combusted.residue_tonnes = total.combusted.mass,
                               char_tonnes = char.year.i)]
    
    # Any carbon not emitted as CH4, CO, PM10 (which is inclusive of PM2.5), or VOC is emitted as CO2. So we need to determine how much carbon was emitted (the total carbon absent the
    # carbon present in unburnt fuel or char), determine how much the of the emitted carbon was in non-CO2 constituents, then assume the remainder was emitted as CO2 and convert to mass
    # CO2 emissions. Carbon fraction variables are species mass / carbon mass
    non.CO2.combusted.carbon <- (CO.emissions.year.i / CO_carbon_fraction +
                                   CH4.emissions.year.i / CH4_carbon_fraction +
                                   PM10.emissions.year.i / PM10_carbon_fraction +
                                   VOC.emissions.year.i / VOC_carbon_fraction)
    
    char.carbon <- char.year.i / char_carbon_fraction # All carbon fraction are in species mass / carbon mass
    
    prescribed_burn_emissions[,CO2_tonnes := CO2_tonnes + (total.segment.carbon - total.unburned.carbon - char.carbon - non.CO2.combusted.carbon) * CO2_carbon_fraction]
    
    # add emissions associated with the drip torch
    for (j in names(drip_emissions)) {
      set(prescribed_burn_emissions, i = NULL, j = j, value = prescribed_burn_emissions[[j]] + drip_emissions[[j]]) 
    }
    
    # Apply the mass lost to the dynamic mass columns. 
    cbrec.dt[,':='(Scattered_CWD_tonnesAcre = (1 - CWD_Scattered_CombustionFrac - CWD_Scattered_CharFrac) * Scattered_CWD_tonnesAcre,
                   Scattered_FWD_tonnesAcre = (1 - FWD_Scattered_CombustionFrac - FWD_Scattered_CharFrac) * Scattered_FWD_tonnesAcre,
                   Scattered_Foliage_tonnesAcre = (1 - Foliage_Scattered_CombustionFrac) * Scattered_Foliage_tonnesAcre
    )]
  }
  
  return(list(prescribed_burn_emissions,prescribed_burn_mass,cbrec.dt))
}
