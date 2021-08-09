# =============================================================================
# AUTHOR: Andrew Harris (andrew.harris@humboldt.edu)
#         Schatz Energy Research Center
#         Humboldt State University
#
# -------------------------------------------------------------------------
# VERSION:
#
#   2019-04-02: Deleted old, unworkable code, developed initialized Script
#
# -------------------------------------------------------------------------
# ACTION ITEMS:
# Index   Description                                                      Status
# -----   --------------------------------------------------------------   ------
#
# ---------------------------------
# INPUTS:
#
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

harvest_processing_fun <- function(cbrec.dt,
                                   equipment_emissions,
                                   moisture,
                                   comminution,
                                   harvest_vol,
                                   harvest_trans_point,
                                   case,
                                   ind_harvests) {
  
  if(debug_CBREC) {
    cat(paste0("Running harvest_processing_fun() function for ",
               as.character(moisture),":",
               as.character(comminution),":",
               as.character(harvest_vol),"\n"), 
        file=log_file, append=T)
  }
  

# Define Constants --------------------------------------------------------

  # Calculate number of crew for crew commute emissions
  crew_count <- cbrec.dt[,sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell / harvest_tonnes_per_person_day
  
  # Initialize harvest processing emissions; this will regurgitate one row to add to year 1 emissions.
  harvest.processing.emissions <- data.table(CO2_kg=0, CO_kg=0, N2O_kg=0, CH4_kg=0, NOx_kg=0, PMUnder10um_kg=0, PMUnder2.5um_kg=0, SOx_kg=0, VOC_kg=0,char_kg=0)
  
  # Do the same for on-road and off-road transportation emissions.
  transportation.emissions.onroad <- data.table(CO2_kg=0, CO_kg=0, N2O_kg=0, CH4_kg=0, NOx_kg=0, PMUnder10um_kg=0, PMUnder2.5um_kg=0, SOx_kg=0, VOC_kg=0,char_kg=0, median.nearestPP.distance_km=0)
  transportation.emissions.offroad <- data.table(CO2_kg=0, CO_kg=0, N2O_kg=0, CH4_kg=0, NOx_kg=0, PMUnder10um_kg=0, PMUnder2.5um_kg=0, SOx_kg=0, VOC_kg=0,char_kg=0)
  
  ##########################
  # Harvest type decision
  ##########################
  if(harvest_vol == "high") {  
    ##### HIGH VOLUME HARVEST #####
    # Next, we differentiate based on residue collection.
    if(case$biomass.collection == 'Piles Only') {
      # "Dry/Green" will only affect the high volume equipment
      if(moisture == 'Green') {
        # Within this category, treatment type has no effect.
        Chip_LessThan10 <- equipment_emissions[Equipment_code=='C.1' | Equipment_code=='L.1',] # Slope less than 10
        Chip_10To35 <- equipment_emissions[Equipment_code=='C.1' | Equipment_code=='L.1',] # Slope between 10 and 35
        Chip_Over35 <- equipment_emissions[Equipment_code=='C.1' | Equipment_code=='L.1',] # Slope > 35

        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.2',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.2',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.2',]

        Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans1_10To35 <- equipment_emissions[Equipment_code=='H.6',]
        Trans1_Over35 <- equipment_emissions[Equipment_code=='H.9',]
        
        # If the user has specified that there will be a secondary harvest transfer point, apply appropriate emissions.
        # If not, they are the same as transport1
        if(harvest_trans_point) {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
          
          # Add an extra distance to CellToRoad to get to the trasfer point, and subtract that distance from the RoadToPlant distance
          CellToRoadMod <- transfer_point_distance
          RoadToPlantMod <- -1*transfer_point_distance
        } else {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.6',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.9',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
          
          # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
          CellToRoadMod <- 0
          RoadToPlantMod <- 0
        }

      } else if(moisture == 'Dry') {
        # Within this category, treatment type has no effect. Chipping is not applied to these residues.
        if(comminution=='Chip') {
          cat("\nWARNING: Dry residues cannot be chipped; emissions for grinding equipment substituted\n",file=log_file,append=T)
        }
        
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='G.2' | Equipment_code=='L.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='G.2' | Equipment_code=='L.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='G.2' | Equipment_code=='L.1',]

        Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans1_10To35 <- equipment_emissions[Equipment_code=='H.1',]
        Trans1_Over35 <- equipment_emissions[Equipment_code=='H.8',]
        
        # If the user has specifiedc that there will be a secondary harvest transfer point, apply appropriate emissions.
        # If not, they are the same as transport1
        if(harvest_trans_point) {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
          
          # Add an extra distance to CellToRoad to get to the trasfer point, and subtract that distance from the RoadToPlant distance
          CellToRoadMod <- transfer_point_distance
          RoadToPlantMod <- -1*transfer_point_distance
        } else {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.1',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.8',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
          
          # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
          CellToRoadMod <- 0
          RoadToPlantMod <- 0
        }
        
      } else {
        stop("Residue moisture in input file must be 'Green' or 'Dry'") # Shoot an error message if the variable is wrong
      }
    } else if(case$biomass.collection == 'All Tech Recoverable') {
      # "Dry/Green" will only affect the high volume equipment
      if(moisture == 'Green') {
        if(case$treatment == 'Clearcut') {

          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          
          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='G.2' | Equipment_code=='L.1',]

        }
        if(case$treatment == '20_Thin_From_Above' | case$treatment == '20_Thin_From_Below' | case$treatment == '20_Proportional_Thin') {

          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          
          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='G.2' | Equipment_code=='L.1',]

        }
        if(case$treatment == '40_Thin_From_Above' | case$treatment == '40_Thin_From_Below' | case$treatment == '40_Proportional_Thin') {
          
          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          
          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='G.2' | Equipment_code=='L.1',]

        }
        if(case$treatment == '60_Thin_From_Above' | case$treatment == '60_Thin_From_Below' | case$treatment == '60_Proportional_Thin') {

          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='C.1',]

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='G.2' | Equipment_code=='L.1',]

        }
        if(case$treatment == '80_Thin_From_Above' | case$treatment == '80_Thin_From_Below' | case$treatment == '80_Proportional_Thin') {
          Chip_LessThan10 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_10To35 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          Chip_Over35 <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='C.1',]
          
          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='G.2' | Equipment_code=='L.1',]
        }
        
        Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans1_10To35 <- equipment_emissions[Equipment_code=='H.6',]
        Trans1_Over35 <- equipment_emissions[Equipment_code=='H.9',]
        
        # If the user has specified that there will be a secondary harvest transfer point, apply appropriate emissions.
        if(harvest_trans_point) {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
          
          # Add an extra distance to CellToRoad to get to the trasfer point, and subtract that distance from the RoadToPlant distance
          CellToRoadMod <- transfer_point_distance
          RoadToPlantMod <- -1*transfer_point_distance
        } else {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.6',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.9',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
          
          # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
          CellToRoadMod <- 0
          RoadToPlantMod <- 0
        }
        
      } else if(moisture=='Dry') {
        # Chipping is not an option.
        if(comminution=='Chip') {
          warning("Dry residues cannot be chipped; emissions for grinding equipment substituted")
        }
        if(case$treatment == 'Clearcut') {

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          
        }
        if(case$treatment == '20_Thin_From_Above' | case$treatment == '20_Thin_From_Below' | case$treatment == '20_Proportional_Thin') {

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          
        }
        if(case$treatment == '40_Thin_From_Above' | case$treatment == '40_Thin_From_Below' | case$treatment == '40_Proportional_Thin') {

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          
        }
        if(case$treatment == '60_Thin_From_Above' | case$treatment == '60_Thin_From_Below' | case$treatment == '60_Proportional_Thin') {

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          
        }
        if(case$treatment == '80_Thin_From_Above' | case$treatment == '80_Thin_From_Below' | case$treatment == '80_Proportional_Thin') {

          Grind_LessThan10 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_10To35 <- equipment_emissions[Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]
          Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.2',]

        }
        
        Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans1_10To35 <- equipment_emissions[Equipment_code=='H.1',]
        Trans1_Over35 <- equipment_emissions[Equipment_code=='H.8',]
        
        # If the user has specified that there will be a secondary harvest transfer point, apply appropriate emissions.
        if(harvest_trans_point) {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',] 
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
          
          # Add an extra distance to CellToRoad to get to the trasfer point, and subtract that distance from the RoadToPlant distance
          CellToRoadMod <- transfer_point_distance
          RoadToPlantMod <- -1*transfer_point_distance
        } else {
          Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
          Trans2_10To35 <- equipment_emissions[Equipment_code=='H.1',]
          Trans2_Over35 <- equipment_emissions[Equipment_code=='H.8',]
          
          TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
          TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
          
          # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
          CellToRoadMod <- 0
          RoadToPlantMod <- 0
        }
        
        
      } else {
        stop("Residue moisture in input file must be 'Green' or 'Dry'") # Shoot an error message if the variable is wrong
      }
    } else {
      stop("Biomass collection in input file must be 'Piles Only' or 'All Tech Recoverable'") # Shoot an error message if the variable is wrong
    }
    
    if(moisture=='Green') { 
      if(comminution=='Chip') {
        # Processing emissions - Chipping
        # including fixed emissions from mobilization of equip and crew, round trip equipment emissions and commute distance one way
        harvest.processing.emissions[,':='(
          CO2_kg = 
            cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(CO2_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(CO2_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(CO2_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CO2_kg * 2 * ind_harvests] +
            equipment_emissions[Equipment_code == "PT.1", CO2_kg * crew_count * crew_commute_dist * 2],
          
          CO_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(CO_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(CO_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(CO_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CO_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", CO_kg * crew_count * crew_commute_dist * 2], 
          
          N2O_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(N2O_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(N2O_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(N2O_kg)]+
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * N2O_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", N2O_kg * crew_count * crew_commute_dist * 2], 
          
          CH4_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(CH4_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(CH4_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(CH4_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CH4_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", CH4_kg * crew_count * crew_commute_dist * 2],
          
          NOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(NOx_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(NOx_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(NOx_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * NOx_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", NOx_kg * crew_count * crew_commute_dist * 2], 
          
          PMUnder10um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(PMUnder10um_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(PMUnder10um_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(PMUnder10um_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * PMUnder10um_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", PMUnder10um_kg * crew_count * crew_commute_dist * 2], 
          
          PMUnder2.5um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(PMUnder2.5um_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(PMUnder2.5um_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(PMUnder2.5um_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * PMUnder2.5um_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", PMUnder2.5um_kg * crew_count * crew_commute_dist * 2],
          
          SOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(SOx_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(SOx_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(SOx_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * SOx_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", SOx_kg * crew_count * crew_commute_dist * 2], 
          
          VOC_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_LessThan10[,sum(VOC_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_10To35[,sum(VOC_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Chip_Over35[,sum(VOC_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * VOC_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", VOC_kg * crew_count * crew_commute_dist * 2]
        )]
        
      }
      # Processing emissions - Grinding
      # including fixed emissions from mobilization of equip and crew
      if(comminution=='Grind') {
        harvest.processing.emissions[,':='(
          CO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CO2_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CO2_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CO2_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CO2_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", CO2_kg * crew_count * crew_commute_dist * 2],
          
          CO_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CO_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CO_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CO_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CO_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", CO_kg * crew_count * crew_commute_dist * 2],
          
          N2O_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(N2O_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(N2O_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(N2O_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * N2O_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", N2O_kg * crew_count * crew_commute_dist * 2],
          
          CH4_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CH4_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CH4_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CH4_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CH4_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", CH4_kg * crew_count * crew_commute_dist * 2],
          
          NOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(NOx_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(NOx_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(NOx_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * NOx_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", NOx_kg * crew_count * crew_commute_dist * 2],
          
          PMUnder10um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(PMUnder10um_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(PMUnder10um_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(PMUnder10um_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * PMUnder10um_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", PMUnder10um_kg * crew_count * crew_commute_dist * 2],
          
          PMUnder2.5um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(PMUnder2.5um_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(PMUnder2.5um_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(PMUnder2.5um_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * PMUnder2.5um_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", PMUnder2.5um_kg * crew_count * crew_commute_dist * 2],
          
          SOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(SOx_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(SOx_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(SOx_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * SOx_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", SOx_kg * crew_count * crew_commute_dist * 2],
          
          VOC_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(VOC_kg)] +
            cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(VOC_kg)] +
            cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(VOC_kg)] +
            equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * VOC_kg * 2 * ind_harvests]+
            equipment_emissions[Equipment_code == "PT.1", VOC_kg * crew_count * crew_commute_dist * 2]
        )]
        
      }
    } else { # Dry residues; if they were not "Green" or "Dry", the module would stop and an error message thrown before now. 
      # including fixed emissions from mobilization of equip and crew
      # Likewise, a warning will have been thrown if someone selected "chipped" Grinding values have been substituted.
      harvest.processing.emissions[,':='(
        CO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CO2_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CO2_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CO2_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CO2_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", CO2_kg * crew_count * crew_commute_dist * 2],

        CO_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CO_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CO_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CO_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CO_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", CO_kg * crew_count * crew_commute_dist * 2],
        
        N2O_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(N2O_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(N2O_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(N2O_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * N2O_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", N2O_kg * crew_count * crew_commute_dist * 2],

        CH4_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CH4_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CH4_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CH4_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CH4_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", CH4_kg * crew_count * crew_commute_dist * 2],

        NOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(NOx_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(NOx_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(NOx_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * NOx_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", NOx_kg * crew_count * crew_commute_dist * 2],

        PMUnder10um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(PMUnder10um_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(PMUnder10um_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(PMUnder10um_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * PMUnder10um_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", PMUnder10um_kg * crew_count * crew_commute_dist * 2],

        PMUnder2.5um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(PMUnder2.5um_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(PMUnder2.5um_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * PMUnder2.5um_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", PMUnder2.5um_kg * crew_count * crew_commute_dist * 2],

        SOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(SOx_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(SOx_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(SOx_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * SOx_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", SOx_kg * crew_count * crew_commute_dist * 2],

        VOC_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(VOC_kg)] +
          cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(VOC_kg)] +
          cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(VOC_kg)] +
          equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * VOC_kg * 2 * ind_harvests]+
          equipment_emissions[Equipment_code == "PT.1", VOC_kg * crew_count * crew_commute_dist * 2]
      )]
    }
  } else { 
    ##### LOW VOLUME HARVEST #####
    # After the high/low volume determination, options will vary based upon: residue collection type, treatment type and slope.
    # Unlike high volume harvests, low volume harvests do not vary based upon resdidue moisture, and the only comminution option is grinding. Because these are user variables,
    # shoot a warning message to show what up.
    # warning("Study area calculated to have a low-volume harvest. Moisture/dirt content does not affect harvest, and the only comminution option is grinding - if you selected 'Chip', that option has been overriden.")
    
    if(case$biomass.collection == 'Piles Only') {
      if(case$treatment == 'Clearcut') { # 100% thin
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(case$treatment == '20_Thin_From_Above' | case$treatment == '20_Thin_From_Below' | case$treatment == '20_Proportional_Thin') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(case$treatment == '40_Thin_From_Above' | case$treatment == '40_Thin_From_Below' | case$treatment == '40_Proportional_Thin') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(case$treatment == '60_Thin_From_Above' | case$treatment == '60_Thin_From_Below' | case$treatment == '60_Proportional_Thin') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(case$treatment == '80_Thin_From_Above' | case$treatment == '80_Thin_From_Below' | case$treatment == '80_Proportional_Thin') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='L.1' | Equipment_code=='G.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }

      Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
      Trans1_10To35 <- equipment_emissions[Equipment_code=='H.1',]
      Trans1_Over35 <- equipment_emissions[Equipment_code=='H.8',]
      
      # If the user has specifiedc that there will be a secondary harvest transfer point, apply appropriate emissions.
      # If not, they are the same as transport1
      if(harvest_trans_point) {
        Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
        Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]

        TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
        TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
        
        # Add an extra distance to CellToRoad to get to the trasfer point, and subtract that distance from the RoadToPlant distance
        CellToRoadMod <- transfer_point_distance
        RoadToPlantMod <- -1*transfer_point_distance
      } else {
        Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans2_10To35 <- equipment_emissions[Equipment_code=='H.1',]
        Trans2_Over35 <- equipment_emissions[Equipment_code=='H.8',]

        TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
        
        # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
        CellToRoadMod <- 0
        RoadToPlantMod <- 0
      }
        
    } else if(case$biomass.collection == 'All Tech Recoverable') {
      # "Dry/Green" will only affect the high volume equipment
      if(case$treatment == 'Clearcut') { # 100% thin
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT' | Equipment_code=='L.3' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1' | Equipment_code=='T.1' | Equipment_code=='L.3' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(case$treatment == '20_Thin_From_Above' | case$treatment == '20_Thin_From_Below' | case$treatment == '20_Proportional_Thin') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT.20' | Equipment_code=='L.3.20' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.20' | Equipment_code=='T.1.20' | Equipment_code=='L.3.20' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(case$treatment == '40_Thin_From_Above' | case$treatment == '40_Thin_From_Below' | case$treatment == '40_Proportional_Thin') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT.40' | Equipment_code=='L.3.40' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.40' | Equipment_code=='T.1.40' | Equipment_code=='L.3.40' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(case$treatment == '60_Thin_From_Above' | case$treatment == '60_Thin_From_Below' | case$treatment == '60_Proportional_Thin') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT.60' | Equipment_code=='L.3.60' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.60' | Equipment_code=='T.1.60' | Equipment_code=='L.3.60' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      if(case$treatment == '80_Thin_From_Above' | case$treatment == '80_Thin_From_Below' | case$treatment == '80_Proportional_Thin') { # 20% thing
        Grind_LessThan10 <- equipment_emissions[Equipment_code=='SS.2.WT.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_10To35 <- equipment_emissions[Equipment_code=='SS.2.WT.80' | Equipment_code=='L.3.80' | Equipment_code=='L.1' | Equipment_code=='G.1' | Equipment_code=='CS.1',]
        Grind_Over35 <- equipment_emissions[Equipment_code=='CY.1.80' | Equipment_code=='T.1.80' | Equipment_code=='L.3.80' | Equipment_code=='G.1' | Equipment_code=='CS.1' | Equipment_code=='L.1',]
        # In this case, the loader (L.1) is used twice, incurring emissions each time. To get the same effect, we multiply the L.1 emissions by 2.
        Grind_LessThan10[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_10To35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
        Grind_Over35[Equipment_code=='L.1',':='(CO2_kg = CO2_kg * 2, CO_kg = CO_kg * 2, N2O_kg = N2O_kg * 2, CH4_kg = CH4_kg * 2, NOx_kg = NOx_kg * 2, PMUnder10um_kg = PMUnder10um_kg * 2, PMUnder2.5um_kg = PMUnder2.5um_kg * 2, SOx_kg = SOx_kg * 2, VOC_kg = VOC_kg * 2)]
      }
      Trans1_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
      Trans1_10To35 <- equipment_emissions[Equipment_code=='H.1',]
      Trans1_Over35 <- equipment_emissions[Equipment_code=='H.8',]

      # If the user has specified that there will be a secondary harvest transfer point, apply appropriate emissions.
      if(harvest_trans_point) {
        Trans2_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        Trans2_10To35 <- equipment_emissions[Equipment_code=='H.5',]
        Trans2_Over35 <- equipment_emissions[Equipment_code=='H.5',]

        TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_10To35 <- equipment_emissions[Equipment_code=='L.1',]
        TransLoad_Over35 <- equipment_emissions[Equipment_code=='L.1',]
        
        # Add an extra distance to CellToRoad to get to the trasfer point, and subtract that distance from the RoadToPlant distance
        CellToRoadMod <- transfer_point_distance
        RoadToPlantMod <- -1*transfer_point_distance
      } else {
        Trans2_LessThan10 <- equipment_emissions[Equipment_code=='H.5',]
        Trans2_10To35 <- equipment_emissions[Equipment_code=='H.1',]
        Trans2_Over35 <- equipment_emissions[Equipment_code=='H.8',]

        TransLoad_LessThan10 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_10To35 <- equipment_emissions[Equipment_code=='NONE',]
        TransLoad_Over35 <- equipment_emissions[Equipment_code=='NONE',]
        
        # There is no trasfer point, so we do not need to modify CellToRoad and RoadToPlant
        CellToRoadMod <- 0
        RoadToPlantMod <- 0
      }
    } else {
      stop("Biomass collection in input file must be 'Piles Only' or 'All Tech Recoverable'") # Shoot an error message if the variable is wrong
    }
    # Processing emissions - for small harvest, grind only
    # including fixed emissions from mobilization of equip and crew
    harvest.processing.emissions[,':='(
      CO2_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CO2_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CO2_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CO2_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CO2_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", CO2_kg * crew_count * crew_commute_dist * 2],

      CO_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CO_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CO_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CO_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CO_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", CO_kg * crew_count * crew_commute_dist * 2],
      
      N2O_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(N2O_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(N2O_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(N2O_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * N2O_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", N2O_kg * crew_count * crew_commute_dist * 2],

      CH4_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(CH4_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(CH4_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(CH4_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * CH4_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", CH4_kg * crew_count * crew_commute_dist * 2],

      NOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(NOx_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(NOx_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(NOx_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * NOx_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", NOx_kg * crew_count * crew_commute_dist * 2],

      PMUnder10um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(PMUnder10um_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(PMUnder10um_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(PMUnder10um_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * PMUnder10um_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", PMUnder10um_kg * crew_count * crew_commute_dist * 2],

      PMUnder2.5um_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(PMUnder2.5um_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(PMUnder2.5um_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(PMUnder2.5um_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * PMUnder2.5um_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", PMUnder2.5um_kg * crew_count * crew_commute_dist * 2],

      SOx_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(SOx_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(SOx_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(SOx_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * SOx_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", SOx_kg * crew_count * crew_commute_dist * 2],

      VOC_kg = cbrec.dt[cell_slope<10, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_LessThan10[,sum(VOC_kg)] +
        cbrec.dt[cell_slope>=10 & cell_slope<35, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_10To35[,sum(VOC_kg)] +
        cbrec.dt[cell_slope>=35 & cell_slope<80, sum(Recovered_CWD_tonnesAcre, Recovered_FWD_tonnesAcre, Recovered_Foliage_tonnesAcre)] * acres_per_cell * Grind_Over35[,sum(VOC_kg)] +
        equipment_emissions[Equipment_code == "ET.1", equip_commute_dist / equip_km_per_hour * VOC_kg *2]+
        equipment_emissions[Equipment_code == "PT.1", VOC_kg * crew_count * crew_commute_dist * 2]
      )]
    
  }

  # Transportation Emissions
  #   On-road transportation captures distance to power plant. Transfer point distance is subtracted from this distance if applicable.
  #   Off-road transportation captures cell-to-road distance, and transfer point distance and loaders if applicable.

  transportation.emissions.onroad[,':='(
    CO2_kg = CO2_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))),

    CO_kg = CO_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))),
    
    N2O_kg = N2O_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))),
    
    CH4_kg = CH4_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))),

    NOx_kg = NOx_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))),

    PMUnder10um_kg = PMUnder10um_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))),

    PMUnder2.5um_kg = PMUnder2.5um_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))),

    SOx_kg = SOx_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))),

    VOC_kg = VOC_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is Road to Plant.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_LessThan10[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_10To35[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Nearest_Plant * Trans1_Over35[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; Road to Plant minus transfer point distance (i.e. + RoadToPlantMod)
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_LessThan10[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_10To35[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Nearest_Plant + RoadToPlantMod) * Trans2_Over35[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))),
    
    median.nearestPP.distance_km = median.nearestPP.distance_km + 
      cbrec.dt[,median(Nearest_Plant)]
  )]
  
  transportation.emissions.offroad[,':='(
    CO2_kg = CO2_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(CO2_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(CO2_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(CO2_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(CO2_kg)])],
    
    CO_kg = CO_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad.
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(CO_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(CO_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(CO_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(CO_kg)])],
    
    N2O_kg = N2O_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(N2O_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(N2O_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(N2O_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(N2O_kg)])],
    
    CH4_kg = CH4_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(CH4_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(CH4_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(CH4_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(CH4_kg)])],
    
    NOx_kg = NOx_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(NOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(NOx_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(NOx_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(NOx_kg)])],
    
    PMUnder10um_kg = PMUnder10um_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(PMUnder10um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(PMUnder10um_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(PMUnder10um_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(PMUnder10um_kg)])],
    
    PMUnder2.5um_kg = PMUnder2.5um_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(PMUnder2.5um_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(PMUnder2.5um_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(PMUnder2.5um_kg)])],
    
    SOx_kg = SOx_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(SOx_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(SOx_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(SOx_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(SOx_kg)])],
    
    VOC_kg = VOC_kg +
      # Emissions from cells with less than transfer_point_distance between road point to plant; this will only utilize Transportation 1, and the distance is the CelltoRoad
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_LessThan10[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_10To35[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant <= transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * Cell_To_Road * Trans1_Over35[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Emissions from cells with more than transfer_point_distance between road and plant; cell-to-road + transfer point distance
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_LessThan10[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_10To35[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      (cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * (Cell_To_Road + CellToRoadMod) * Trans1_Over35[,sum(VOC_kg)])] * (1+(1-unloaded_truck_eff_improve))) +
      # Additional loader associated with transfer point (note loaders don't have return trips, so emissions aren't doubled)
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope<10,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_LessThan10[,sum(VOC_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=10 & cell_slope<35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_10To35[,sum(VOC_kg)])] +
      cbrec.dt[Nearest_Plant > transfer_point_distance & cell_slope>=35,sum((Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre) * acres_per_cell * TransLoad_Over35[,sum(VOC_kg)])]
  )]
  
  # transportation.emissions represents a 1-way travel distance. Account for return trip and adjust emissions assuming increased efficiency for un-loaded trips

  # Processing mass loss and mass transfer to power plant
  # In the high volume scenario, the only residues that are NOT processed and NOT sent to the power plant are those with the merge column "Do_Not_Harvest"
  # Step 1, populate the mass_to_plant_tonnesAcre from the recovered columns.
  # cbrec.dt[,mass_to_plant_tonnesAcre := Recovered_CWD_tonnesAcre + Recovered_FWD_tonnesAcre + Recovered_Foliage_tonnesAcre]
  # 
  # # Step 2, remove processed materials from the recovered materials. Processing and transfer losses are accounted for in the scenario matrix, so this is just zeroing the recovered mass columns.
  # cbrec.dt[,':='(Recovered_CWD_tonnesAcre = 0,
  #                Recovered_FWD_tonnesAcre = 0,
  #                Recovered_Foliage_tonnesAcre = 0)]
  
  return(list(collection.processing = harvest.processing.emissions,
              transportation.onroad = transportation.emissions.onroad,
              transportation.offroad = transportation.emissions.offroad
              ))
}
