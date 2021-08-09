# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2019-04-22: Initialized Script
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#
# ---------------------------------
# INPUTS:
#   power.plant: this is the nearest power plant, as defined in project_FCIDs variable in run-cbrec.R
# ---------------------------------
# OBJECTIVE:
#
#
# ---------------------------------
# OUTPUTS:
#
# =============================================================================

# Source files


# Required Libraries

power_plant_fun <- function(cbrec.dt,
                            power.emissions.factors,
                            power.plant) {
  
  if(debug_CBREC) {
    cat("Running power_plant_fun() function\n", file=log_file, append=T)
  }

  # If power.plant is/are not in the emissions list, choose Current Gen Combustion Plant Default
  if(any(power.plant %notin% power.emissions.factors[,Power.Plant.Code])) {
    warning(paste0("Power plant ",as.character(power.plant)," identified in case.data could not be found in power_emission_factors global variable. Overriding to CurrentGenCombustionPlantDefault!"))
    power.plant[power.plant %notin% power.emissions.factors[, Power.Plant.Code]] <- "CurrentGenCombustionPlantDefault"
    cbrec.dt[plant_location %notin% power.emissions.factors[, Power.Plant.Code], plant_location := "CurrentGenCombustionPlantDefault"]
  }
  
  # Create list of each power plant of interest
  power_plant_list = c("Nearest", # nearest is variable
                       "CurrentGenCombustionPlantDefault",
                       "CurrentGenIG/CombustionPlantDefault",
                       "NextGenThermochemicalPlantDefault",
                       "LessThan1MWPlant")
  
  # Run each power plant --------------------------------------------------------------------------------------------------------------
  plant_specific_list <- lapply(power_plant_list, function(pp) {
    
    if(debug_CBREC) {
      cat(paste0("   Calculating power plant emissions for ",pp,"\n"), file=log_file, append=T)
    }
    # ---------------------------------------------------------------------------------------------------------------------------------
    # Setup
    # ---------------------------------------------------------------------------------------------------------------------------------
    
    # power.plant is the nearest plant when passed to power_plant_fun()
    # change power plant if not "Nearest" in power_plant_list
    if (pp != "Nearest") {
      power.plant <- pp
      cbrec.dt[, plant_location := pp]
    }
  
    # Trim the power plant emissions data table to match the selected power plant(s).
    emission.factors <- power.emissions.factors[Power.Plant.Code %in% power.plant,]
    
    # ---------------------------------------------------------------------------------------------------------------------------------
    # Calc power plant mass and energy inputs
    # ---------------------------------------------------------------------------------------------------------------------------------
    # Calculate mass loss fraction after storage decay (fraction of pp_residue.delivered_tonnes)
    #   Emissions from storage decay are allocated to the first year. Variable avg_storage_time_months defined in constants.R as global variable
    storage_mass_lost_fraction <- round(2.1469127002*log(days_per_month * avg_storage_time_months) - 1.5039981419, 1) / 100 # From dry matter loss (DML) research
    
    # Add columns to cbrec.dt
    plant.specific.sums <- 
      cbrec.dt[,.(pp_residue.delivered_tonnes = sum(mass_to_plant_tonnesAcre) * acres_per_cell, # tonnes to power plant gate prior to any decay from storage
                  pp_residue.decayed_carbon.tonnes = sum(mass_to_plant_tonnesAcre * Carbon_frac) * storage_mass_lost_fraction * acres_per_cell, # 
                  pp_residue.burned_tonnes = sum(mass_to_plant_tonnesAcre) * (1 - storage_mass_lost_fraction) * acres_per_cell, # Mass fed to power plant after storage decay
                  pp_residue.burned_carbon.tonnes = sum(mass_to_plant_tonnesAcre * Carbon_frac) * (1 - storage_mass_lost_fraction) * acres_per_cell, # carbon fraction tracking
                  pp_residue.burned_MWh.heat = sum(mass_to_plant_tonnesAcre * HHV) * (1 - storage_mass_lost_fraction) * acres_per_cell * kg_per_tonne / MJ_per_MWh, # HHV is units of MJ / kg
                  pp_residue.burned_BTU = sum(mass_to_plant_tonnesAcre * HHV) * (1 - storage_mass_lost_fraction) * acres_per_cell * kg_per_tonne * BTU_per_MJ, # total BTUs delivered to power plant
                  pp_waste.flyash_ash.tonnes = sum(mass_to_plant_tonnesAcre * Ash_frac) * (1 - storage_mass_lost_fraction) * acres_per_cell), # ash fraction
               by = plant_location] # for each power plant (cbrec.dt has multiple rows, each of which can have a different "Nearest" power plant)
    
    # ---------------------------------------------------------------------------------------------------------------------------------
    # Calculate emissions from decay of storage piles
    # ---------------------------------------------------------------------------------------------------------------------------------
    plant.specific.sums[,pp_storage.CH4_tonnes := pp_residue.decayed_carbon.tonnes * CH4_decay_emissions_fraction_storage * CH4_carbon_fraction]
    plant.specific.sums[,pp_storage.CO2_tonnes := pp_residue.decayed_carbon.tonnes * (1 - CH4_decay_emissions_fraction_storage) * CO2_carbon_fraction]
    
    # ---------------------------------------------------------------------------------------------------------------------------------
    # Calculate power plant mass and energy outputs: electricity and cogen heat, emissions, ash, char
    # ---------------------------------------------------------------------------------------------------------------------------------
    
    # Always Run cogen for each default power plant. Cogen results are stored separately, so can choose to use or not.
    if(any(power.plant %in% c("CurrentGenCombustionPlantDefault",
                              "CurrentGenIG/CombustionPlantDefault",
                              "NextGenThermochemicalPlantDefault",
                              "LessThan1MWPlant"))){
      emission.factors[, Cogen := 1]
    }
    
    # Merge in emissions factors for the power plant
    setkey(plant.specific.sums, plant_location)
    setkey(emission.factors, Power.Plant.Code)
    plant.specific.sums <- plant.specific.sums[emission.factors]
    
    # Combine the plant.specfic.sums with the biomass_plant_unburnt_fuel data table to get the fractions of fuel remaining.
    setkey(plant.specific.sums,Power.Plant.Type)
    setkey(biomass_plant_unburnt_fuel,plant.type)
    plant.specific.sums <- biomass_plant_unburnt_fuel[plant.specific.sums]
    if(nrow(plant.specific.sums[is.na(unburned.fuel.frac)])>0){
      warning("One or more plant technology types do not match the technology types available:Biomass stoker,Fluidized bed combustor,Cyclone combustor,Gasifier,Integrated gasification and combustion")
      stop()
    }
    
    # Calculate electricity and cogen heat output
    plant.specific.sums[, pp_energy.production_MWh := pp_residue.burned_MWh.heat * Efficiency.Net.Output]
    plant.specific.sums[, pp_energy.production_CogenMMBtu := Cogen * cogen_eff * pp_residue.burned_BTU * (1 - Efficiency.Net.Output) / 1000000]
    plant.specific.sums[, pp_residue.burned.to.cogen.heat_tonnes := Cogen * pp_residue.burned_tonnes * (1 - Efficiency.Net.Output)]
    
    # calculate natural gas offsets
    plant.specific.sums <- cbind.data.frame(plant.specific.sums,
                                            naturalGasEquivalant(plant.specific.sums[, pp_energy.production_CogenMMBtu]))
    
    # Calculate char output
    #   Unburnt fuel is composed of char(which has carbon) and ash (which does not). The total unburnt fuel can be found with the unburned.fuel.frac; 
    #     using the ash content, we can calculate the mass of char, and then the carbon fraction. 
    #     If the ash content is greater than the unburnt fuel, then there is no char (all unburnt fuel is ash)
    #     This currently never happens with any FCID.
    plant.specific.sums[, pp_waste.flyash_char.tonnes := (pp_residue.burned_tonnes * unburned.fuel.frac) - pp_waste.flyash_ash.tonnes]
    plant.specific.sums[, pp_waste.flyash_char.tonnes := ifelse(pp_waste.flyash_char.tonnes < 0, 0, pp_waste.flyash_char.tonnes)] # if char.mass is negative, set to 0
    
    # Calculate plant-specific emissions and electricty outputs
    # As with other burning emissions, CO2 is calculated as a balance of carbon.
    plant.specific.sums[,':='(
      pp_electricity.CO_tonnes = pp_residue.burned_MWh.heat * Efficiency.Net.Output * kgCO.MWhe / kg_per_tonne,
      pp_electricity.N2O_tonnes = pp_residue.burned_MWh.heat * Efficiency.Net.Output * kgN2O.MWhe / kg_per_tonne,
      pp_electricity.CH4_tonnes = pp_residue.burned_MWh.heat * Efficiency.Net.Output * kgCH4.MWhe / kg_per_tonne,
      pp_electricity.VOC_tonnes = pp_residue.burned_MWh.heat * Efficiency.Net.Output * kgVOC.MWhe / kg_per_tonne,
      pp_electricity.NOx_tonnes = pp_residue.burned_MWh.heat * Efficiency.Net.Output * kgNOx.MWhe / kg_per_tonne,
      pp_electricity.SO2_tonnes = pp_residue.burned_MWh.heat * Efficiency.Net.Output * kgSOx.MWhe * plant_SO2_SOx_fraction / kg_per_tonne,
      pp_electricity.PMUnder10um_tonnes = pp_residue.burned_MWh.heat * Efficiency.Net.Output * kgPM10.MWhe / kg_per_tonne,
      pp_electricity.PMUnder2.5um_tonnes = pp_residue.burned_MWh.heat * Efficiency.Net.Output * kgPM2.5.MWhe / kg_per_tonne 
    )]
    
    # Calculate CO2 from the total carbon sent to plant, absent the charred carbon and the other carbon emissions species
    plant.specific.sums[,pp_electricity.CO2_tonnes :=
                          CO2_carbon_fraction * 
                          (pp_residue.burned_carbon.tonnes -
                             pp_waste.flyash_char.tonnes / char_carbon_fraction -
                             pp_electricity.CO_tonnes / CO_carbon_fraction -
                             pp_electricity.CH4_tonnes / CH4_carbon_fraction -
                             pp_electricity.VOC_tonnes / VOC_carbon_fraction -
                             pp_electricity.PMUnder10um_tonnes / PM10_carbon_fraction)

    ]
    
    # drop unneeded columns
    plant.specific.sums[, c(
      "unburned.fuel.frac",
      "Name",
      "Cogen",
      "pp_residue.burned_carbon.tonnes",
      'kgCO2.MWhe',
      'kgCO.MWhe',
      'kgN2O.MWhe',
      'kgCH4.MWhe',
      'kgVOC.MWhe',
      'kgNOx.MWhe',
      'kgSOx.MWhe',
      'kgPM10.MWhe',
      'kgPM2.5.MWhe',
      "pp_residue.burned_BTU",
      "Secondary.Fuel.Use.MMBtu.MWhe.net",
      "Efficiency.Net.Output",
      "pp_residue.burned_MWh.heat",
      "pp_residue.decayed_carbon.tonnes"
      ) :=  NULL]
  
  }) # end of lapply loop 

  names(plant_specific_list) <- power_plant_list
  
  # output a single list of each power plant scenario
  return(plant_specific_list)
  
} # end of function
