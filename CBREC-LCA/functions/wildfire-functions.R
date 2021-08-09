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
#   2019-05-10: Added initial data processing function
# -------------------------------------------------------------------------
#
# FUNCTION: Wildfire Processing Function
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This function will collect and calculate needed data from the wildfire model outputs
# -------------------------------------------------------------------------------------------------
# INPUTS:
# wildfire.data.directory: The directory storing all of the wildfire data
# scenario.ID: The scenario matrix identifier string to select the correct wildfire data files
# tile.list: The wildfire tiles contained within the study area
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# wildfire.data: combined combustion fraction, char fractionm, and emissions factor data from years
#   0, 25, 50 ,75, and 100.
# =================================================================================================

# To avoid errors when dividing by zero we could use this function instead of adding 1 to each value -Max

# function to allow dividing by 0
# zero_div <- function(x, y) {
#   return(ifelse(y == 0, 0, x / y))
# }

wildfire_processing_fun <- function(wildfire.data.directory,
                                    scenario.ID, 
                                    prescribed.burn, 
                                    tile.list,
                                    poly_num) {
  
  if(debug_CBREC) {
    cat("Running wildfire_processing_fun() function\n", file=log_file, append=T)
  }

  wildfire_data_year0 <- data.table()
  wildfire_data_year25 <- data.table()
  wildfire_data_year50 <- data.table()
  wildfire_data_year75 <- data.table()
  wildfire_data_year100 <- data.table()
  
  # Load the wildfire .rds files, and combine into a single data table.
  #######################################################################################
  # Wildfire data will be stored as integer values, and all mass is in grams. However, as
  # all of our calculations are mass ratios, and R apparently assumes floating point
  # division unless specifically instructed otherwise, no conversions to tonnes or flaot
  # is needed.
  #######################################################################################
  
  # If there was a prescribed burn, then we will use the "second" file for year 0 emissions. Otherwise, there will only be a "first" file for year 0.
  if(prescribed.burn == 'None') {
    # For each year (0, 25, 50, 75, or 100), we'll need to load up wildfire data, trim unneeded columns and rename the non-ID columns to include the year. 
    year.0.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-0.rds",sep=""),tile.list,tile.list)
    year.25.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-25.rds",sep=""),tile.list,tile.list)
    year.50.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-50.rds",sep=""),tile.list,tile.list)
    year.75.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-75.rds",sep=""),tile.list,tile.list)
    year.100.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-100.rds",sep=""),tile.list,tile.list)
    
    # add descriptive warning if data is not present
    if(all(!file.exists(year.0.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 0", " poly ", poly_num))}
    if(all(!file.exists(year.25.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 25", " poly ", poly_num))}
    if(all(!file.exists(year.50.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 50", " poly ", poly_num))}
    if(all(!file.exists(year.75.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 75", " poly ", poly_num))}
    if(all(!file.exists(year.100.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 100", " poly ", poly_num))}
    
    # pull file data for each year and bind together
    wildfire_data_year0 <- rbindlist(lapply(year.0.filepaths, readRDS))
    wildfire_data_year25 <- rbindlist(lapply(year.25.filepaths, readRDS))
    wildfire_data_year50 <- rbindlist(lapply(year.50.filepaths, readRDS))
    wildfire_data_year75 <- rbindlist(lapply(year.75.filepaths, readRDS))
    wildfire_data_year100 <- rbindlist(lapply(year.100.filepaths, readRDS))
    
    # Trim unneeded files. This used to unclude many more columns; the .rds files have since been reduced in size.
    wildfire_data_year0[,':='(fuelbed_number=NULL, FCID2018=NULL)]
    wildfire_data_year25[,':='(fuelbed_number=NULL, FCID2018=NULL)]
    wildfire_data_year50[,':='(fuelbed_number=NULL, FCID2018=NULL)]
    wildfire_data_year75[,':='(fuelbed_number=NULL, FCID2018=NULL)]
    wildfire_data_year100[,':='(fuelbed_number=NULL, FCID2018=NULL)]
  } else { # A prescribed burn took place
    # For each year (0, 25, 50, 75, or 100), we'll need to load up wildfire data, trim unneeded columns and rename the non-ID columns to include the year. 
    # There were prescribed burns, so for year 0 we use the "second" burn files.
    year.0.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",gsub("first", "second", scenario.ID),"-%d-0.rds",sep=""),tile.list,tile.list)
    year.25.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-25.rds",sep=""),tile.list,tile.list)
    year.50.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-50.rds",sep=""),tile.list,tile.list)
    year.75.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-75.rds",sep=""),tile.list,tile.list)
    year.100.filepaths <- sprintf(paste(wildfire.data.directory,"%d/",scenario.ID,"-%d-100.rds",sep=""),tile.list,tile.list)

    # add descriptive warning if data is not present
    if(all(!file.exists(year.0.filepaths))){warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 0", " poly ", poly_num))}
    if(all(!file.exists(year.25.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 25", " poly ", poly_num))}
    if(all(!file.exists(year.50.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 50", " poly ", poly_num))}
    if(all(!file.exists(year.75.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 75", " poly ", poly_num))}
    if(all(!file.exists(year.100.filepaths))) {warning(paste("data not available for ", scenario.ID, " tile ", tile.list, " year 100", " poly ", poly_num))}
    
    wildfire_data_year0 <- rbindlist(lapply(year.0.filepaths, readRDS))
    wildfire_data_year25 <- rbindlist(lapply(year.25.filepaths, readRDS))
    wildfire_data_year50 <- rbindlist(lapply(year.50.filepaths, readRDS))
    wildfire_data_year75 <- rbindlist(lapply(year.75.filepaths, readRDS))
    wildfire_data_year100 <- rbindlist(lapply(year.100.filepaths, readRDS))

    wildfire_data_year0[,':='(fuelbed_number=NULL, FCID2018=NULL)]
    wildfire_data_year25[,':='(fuelbed_number=NULL, FCID2018=NULL)]
    wildfire_data_year50[,':='(fuelbed_number=NULL, FCID2018=NULL)]
    wildfire_data_year75[,':='(fuelbed_number=NULL, FCID2018=NULL)]
    wildfire_data_year100[,':='(fuelbed_number=NULL, FCID2018=NULL)]
  }
   
  
  # From the wildfire data, we need to calculate: emissions factors, combustion fractions, char fractions, and unburned fractions. Because we have wildfire data for only
  # 5 years (0, 25, 50, 75, 100), these values will be interpolated between years, and will change from year to year, but some of the equation terms can be calculated in advance.
  # All of these values will have the residue_exposed as the denominator, so we need to be sure that we don't have a div/0 situation. So, when the value of a residue_exposed
  # column is 0, add a very small number (everything is in grams, so let's make it 1) to residue_exposed.
  
  # Fun fact: R thinks "1" is a double., so if I try to add 1 to an integer column, I get a warning for each row. I could call as.integer(1) as many times as I want,
  # but that is needlessly converions. So we make a temporary "one.int" integer, and say mean things about R behind its back.
  # So many mean things.
  one.int <- as.integer(1)
  
  wildfire_data_year0[total_duff_exposed==0, total_duff_exposed := total_duff_exposed + one.int]
  wildfire_data_year0[total_foliage_exposed==0, total_foliage_exposed := total_foliage_exposed + one.int]
  wildfire_data_year0[total_fwd_exposed==0, total_fwd_exposed := total_fwd_exposed + one.int]
  wildfire_data_year0[total_cwd_exposed==0, total_cwd_exposed := total_cwd_exposed + one.int]
  wildfire_data_year0[total_pile_consumed==0, total_pile_consumed := total_pile_consumed + one.int]

  wildfire_data_year25[total_duff_exposed==0, total_duff_exposed := total_duff_exposed + one.int]
  wildfire_data_year25[total_foliage_exposed==0, total_foliage_exposed := total_foliage_exposed + one.int]
  wildfire_data_year25[total_fwd_exposed==0, total_fwd_exposed := total_fwd_exposed + one.int]
  wildfire_data_year25[total_cwd_exposed==0, total_cwd_exposed := total_cwd_exposed + one.int]
  wildfire_data_year25[total_pile_consumed==0, total_pile_consumed := total_pile_consumed + one.int]
  
  wildfire_data_year50[total_duff_exposed==0, total_duff_exposed := total_duff_exposed + one.int]
  wildfire_data_year50[total_foliage_exposed==0, total_foliage_exposed := total_foliage_exposed + one.int]
  wildfire_data_year50[total_fwd_exposed==0, total_fwd_exposed := total_fwd_exposed + one.int]
  wildfire_data_year50[total_cwd_exposed==0, total_cwd_exposed := total_cwd_exposed + one.int]
  wildfire_data_year50[total_pile_consumed==0, total_pile_consumed := total_pile_consumed + one.int]
  
  wildfire_data_year75[total_duff_exposed==0, total_duff_exposed := total_duff_exposed + one.int]
  wildfire_data_year75[total_foliage_exposed==0, total_foliage_exposed := total_foliage_exposed + one.int]
  wildfire_data_year75[total_fwd_exposed==0, total_fwd_exposed := total_fwd_exposed + one.int]
  wildfire_data_year75[total_cwd_exposed==0, total_cwd_exposed := total_cwd_exposed + one.int]
  wildfire_data_year75[total_pile_consumed==0, total_pile_consumed := total_pile_consumed + one.int]
  
  wildfire_data_year100[total_duff_exposed==0, total_duff_exposed := total_duff_exposed + one.int]
  wildfire_data_year100[total_foliage_exposed==0, total_foliage_exposed := total_foliage_exposed + one.int]
  wildfire_data_year100[total_fwd_exposed==0, total_fwd_exposed := total_fwd_exposed + one.int]
  wildfire_data_year100[total_cwd_exposed==0, total_cwd_exposed := total_cwd_exposed + one.int]
  wildfire_data_year100[total_pile_consumed==0, total_pile_consumed := total_pile_consumed + one.int]
  
  wildfire_data_year0[,':='(
    # Combustion and char fractions; foliage and duff do not char in scattered applications
    CWD_year0_Scattered_CombustionFrac = total_cwd_consumed / total_cwd_exposed,
    FWD_year0_Scattered_CombustionFrac = total_fwd_consumed / total_fwd_exposed,
    Foliage_year0_Scattered_CombustionFrac = total_foliage_consumed / total_foliage_exposed,
    Duff_year0_Scattered_CombustionFrac = total_duff_consumed / total_duff_exposed,
    CWD_year0_Scattered_CharFrac = char_cwd_residue / total_cwd_exposed,
    FWD_year0_Scattered_CharFrac = char_fwd_residue / total_fwd_exposed,
    # Emissions factors - CH4
    Duff_year0_Scattered_CH4_EmFac = total_duff_residue_CH4 / total_duff_exposed,
    Foliage_year0_Scattered_CH4_EmFac = total_foliage_residue_CH4 / total_foliage_exposed,
    FWD_year0_Scattered_CH4_EmFac = total_fwd_residue_CH4 / total_fwd_exposed,
    CWD_year0_Scattered_CH4_EmFac = total_cwd_residue_CH4 / total_cwd_exposed,
    Piled_year0_CH4_EmFac = total_pile_CH4 / (total_pile_consumed/pile.burn.combustion.frac),
    # CO
    Duff_year0_Scattered_CO_EmFac = total_duff_residue_CO / total_duff_exposed,
    Foliage_year0_Scattered_CO_EmFac = total_foliage_residue_CO / total_foliage_exposed,
    FWD_year0_Scattered_CO_EmFac = total_fwd_residue_CO / total_fwd_exposed,
    CWD_year0_Scattered_CO_EmFac = total_cwd_residue_CO / total_cwd_exposed,
    Piled_year0_CO_EmFac = total_pile_CO / (total_pile_consumed/pile.burn.combustion.frac),
    # NOx
    Duff_year0_Scattered_NOx_EmFac = total_duff_residue_NOx / total_duff_exposed,
    Foliage_year0_Scattered_NOx_EmFac = total_foliage_residue_NOx / total_foliage_exposed,
    FWD_year0_Scattered_NOx_EmFac = total_fwd_residue_NOx / total_fwd_exposed,
    CWD_year0_Scattered_NOx_EmFac = total_cwd_residue_NOx / total_cwd_exposed,
    Piled_year0_NOx_EmFac = total_pile_NOx / (total_pile_consumed/pile.burn.combustion.frac),
    # PM10
    Duff_year0_Scattered_PM10_EmFac = total_duff_residue_PM10 / total_duff_exposed,
    Foliage_year0_Scattered_PM10_EmFac = total_foliage_residue_PM10 / total_foliage_exposed,
    FWD_year0_Scattered_PM10_EmFac = total_fwd_residue_PM10 / total_fwd_exposed,
    CWD_year0_Scattered_PM10_EmFac = total_cwd_residue_PM10 / total_cwd_exposed,
    Piled_year0_PM10_EmFac = total_pile_vdirty_PM10 / (total_pile_consumed/pile.burn.combustion.frac),
    # PM2.5
    Duff_year0_Scattered_PM2.5_EmFac = total_duff_residue_PM2.5 / total_duff_exposed,
    Foliage_year0_Scattered_PM2.5_EmFac = total_foliage_residue_PM2.5 / total_foliage_exposed,
    FWD_year0_Scattered_PM2.5_EmFac = total_fwd_residue_PM2.5 / total_fwd_exposed,
    CWD_year0_Scattered_PM2.5_EmFac = total_cwd_residue_PM2.5 / total_cwd_exposed,
    Piled_year0_PM2.5_EmFac = total_pile_vdirty_PM2.5 / (total_pile_consumed/pile.burn.combustion.frac),
    # SO2
    Duff_year0_Scattered_SO2_EmFac = total_duff_residue_SO2 / total_duff_exposed,
    Foliage_year0_Scattered_SO2_EmFac = total_foliage_residue_SO2 / total_foliage_exposed,
    FWD_year0_Scattered_SO2_EmFac = total_fwd_residue_SO2 / total_fwd_exposed,
    CWD_year0_Scattered_SO2_EmFac = total_cwd_residue_SO2 / total_cwd_exposed,
    Piled_year0_SO2_EmFac = total_pile_SO2 / (total_pile_consumed/pile.burn.combustion.frac),
    # VOC
    Duff_year0_Scattered_VOC_EmFac = total_duff_residue_VOC / total_duff_exposed,
    Foliage_year0_Scattered_VOC_EmFac = total_foliage_residue_VOC / total_foliage_exposed,
    FWD_year0_Scattered_VOC_EmFac = total_fwd_residue_VOC / total_fwd_exposed,
    CWD_year0_Scattered_VOC_EmFac = total_cwd_residue_VOC / total_cwd_exposed,
    Piled_year0_VOC_EmFac = total_pile_VOC / (total_pile_consumed/pile.burn.combustion.frac)
  )]
  
  # Because we truncate wildfire outputs to the nearest gram, cells with very small levels of residue (<1 kg) may have a combustion 
  # and char fraction that add up to more than 1; this would result in negative masses. In these situations, if combustion_frac + char_frac > 1,
  # set char_frac to 1 - combustion_frac. This will need to be repeated for all wildfire data components.
  wildfire_data_year0[CWD_year0_Scattered_CombustionFrac + CWD_year0_Scattered_CharFrac > 1, CWD_year0_Scattered_CharFrac := 1 - CWD_year0_Scattered_CombustionFrac]
  wildfire_data_year0[FWD_year0_Scattered_CombustionFrac + FWD_year0_Scattered_CharFrac > 1, FWD_year0_Scattered_CharFrac := 1 - FWD_year0_Scattered_CombustionFrac]
  
  # Trim the now-un-needed columns
  wildfire_data_year0[,':='(total_pile_clean_PM10 = NULL, total_pile_vdirty_PM10 = NULL, total_pile_clean_PM2.5 = NULL, total_pile_vdirty_PM2.5 = NULL, total_pile_CH4 = NULL, total_pile_CO = NULL, total_pile_CO2 = NULL, total_pile_NOx = NULL, total_pile_SO2 = NULL, total_pile_VOC = NULL, pile_char = NULL, char_fwd_residue = NULL, char_cwd_residue = NULL, total_duff_exposed = NULL, total_foliage_exposed = NULL, total_fwd_exposed = NULL, total_cwd_exposed = NULL, total_pile_consumed = NULL, total_duff_consumed = NULL, total_foliage_consumed = NULL, total_fwd_consumed = NULL, total_cwd_consumed = NULL, total_fuel_consumed=NULL, total_duff_residue_CH4 = NULL, total_foliage_residue_CH4 = NULL, total_fwd_residue_CH4 = NULL, total_cwd_residue_CH4 = NULL, total_duff_residue_CO = NULL, total_foliage_residue_CO = NULL, total_fwd_residue_CO = NULL, total_cwd_residue_CO = NULL, total_duff_residue_CO2 = NULL, total_foliage_residue_CO2 = NULL, total_fwd_residue_CO2 = NULL, total_cwd_residue_CO2 = NULL, total_duff_residue_NOx = NULL, total_foliage_residue_NOx = NULL, total_fwd_residue_NOx = NULL, total_cwd_residue_NOx = NULL, total_duff_residue_PM10 = NULL, total_foliage_residue_PM10 = NULL, total_fwd_residue_PM10 = NULL, total_cwd_residue_PM10 = NULL, total_duff_residue_PM2.5 = NULL, total_foliage_residue_PM2.5 = NULL, total_fwd_residue_PM2.5 = NULL, total_cwd_residue_PM2.5 = NULL, total_duff_residue_SO2 = NULL, total_foliage_residue_SO2 = NULL, total_fwd_residue_SO2 = NULL, total_cwd_residue_SO2 = NULL, total_duff_residue_VOC = NULL, total_foliage_residue_VOC = NULL, total_fwd_residue_VOC = NULL, total_cwd_residue_VOC = NULL)]
  
  wildfire_data_year25[,':='(
    # Combustion and char fractions; foliage and duff do not char
    CWD_year25_Scattered_CombustionFrac = total_cwd_consumed / total_cwd_exposed,
    FWD_year25_Scattered_CombustionFrac = total_fwd_consumed / total_fwd_exposed,
    Foliage_year25_Scattered_CombustionFrac = total_foliage_consumed / total_foliage_exposed,
    Duff_year25_Scattered_CombustionFrac = total_duff_consumed / total_duff_exposed,
    CWD_year25_Scattered_CharFrac = char_cwd_residue / total_cwd_exposed,
    FWD_year25_Scattered_CharFrac = char_fwd_residue / total_fwd_exposed,
    # Emissions factors - CH4
    Duff_year25_Scattered_CH4_EmFac = total_duff_residue_CH4 / total_duff_exposed,
    Foliage_year25_Scattered_CH4_EmFac = total_foliage_residue_CH4 / total_foliage_exposed,
    FWD_year25_Scattered_CH4_EmFac = total_fwd_residue_CH4 / total_fwd_exposed,
    CWD_year25_Scattered_CH4_EmFac = total_cwd_residue_CH4 / total_cwd_exposed,
    Piled_year25_CH4_EmFac = total_pile_CH4 / (total_pile_consumed/pile.burn.combustion.frac),
    # CO
    Duff_year25_Scattered_CO_EmFac = total_duff_residue_CO / total_duff_exposed,
    Foliage_year25_Scattered_CO_EmFac = total_foliage_residue_CO / total_foliage_exposed,
    FWD_year25_Scattered_CO_EmFac = total_fwd_residue_CO / total_fwd_exposed,
    CWD_year25_Scattered_CO_EmFac = total_cwd_residue_CO / total_cwd_exposed,
    Piled_year25_CO_EmFac = total_pile_CO / (total_pile_consumed/pile.burn.combustion.frac),
    # NOx
    Duff_year25_Scattered_NOx_EmFac = total_duff_residue_NOx / total_duff_exposed,
    Foliage_year25_Scattered_NOx_EmFac = total_foliage_residue_NOx / total_foliage_exposed,
    FWD_year25_Scattered_NOx_EmFac = total_fwd_residue_NOx / total_fwd_exposed,
    CWD_year25_Scattered_NOx_EmFac = total_cwd_residue_NOx / total_cwd_exposed,
    Piled_year25_NOx_EmFac = total_pile_NOx / (total_pile_consumed/pile.burn.combustion.frac),
    # PM10
    Duff_year25_Scattered_PM10_EmFac = total_duff_residue_PM10 / total_duff_exposed,
    Foliage_year25_Scattered_PM10_EmFac = total_foliage_residue_PM10 / total_foliage_exposed,
    FWD_year25_Scattered_PM10_EmFac = total_fwd_residue_PM10 / total_fwd_exposed,
    CWD_year25_Scattered_PM10_EmFac = total_cwd_residue_PM10 / total_cwd_exposed,
    Piled_year25_PM10_EmFac = total_pile_vdirty_PM10 / (total_pile_consumed/pile.burn.combustion.frac),
    # PM2.5
    Duff_year25_Scattered_PM2.5_EmFac = total_duff_residue_PM2.5 / total_duff_exposed,
    Foliage_year25_Scattered_PM2.5_EmFac = total_foliage_residue_PM2.5 / total_foliage_exposed,
    FWD_year25_Scattered_PM2.5_EmFac = total_fwd_residue_PM2.5 / total_fwd_exposed,
    CWD_year25_Scattered_PM2.5_EmFac = total_cwd_residue_PM2.5 / total_cwd_exposed,
    Piled_year25_PM2.5_EmFac = total_pile_vdirty_PM2.5 / (total_pile_consumed/pile.burn.combustion.frac),
    # SO2
    Duff_year25_Scattered_SO2_EmFac = total_duff_residue_SO2 / total_duff_exposed,
    Foliage_year25_Scattered_SO2_EmFac = total_foliage_residue_SO2 / total_foliage_exposed,
    FWD_year25_Scattered_SO2_EmFac = total_fwd_residue_SO2 / total_fwd_exposed,
    CWD_year25_Scattered_SO2_EmFac = total_cwd_residue_SO2 / total_cwd_exposed,
    Piled_year25_SO2_EmFac = total_pile_SO2 / (total_pile_consumed/pile.burn.combustion.frac),
    # VOC
    Duff_year25_Scattered_VOC_EmFac = total_duff_residue_VOC / total_duff_exposed,
    Foliage_year25_Scattered_VOC_EmFac = total_foliage_residue_VOC / total_foliage_exposed,
    FWD_year25_Scattered_VOC_EmFac = total_fwd_residue_VOC / total_fwd_exposed,
    CWD_year25_Scattered_VOC_EmFac = total_cwd_residue_VOC / total_cwd_exposed,
    Piled_year25_VOC_EmFac = total_pile_VOC / (total_pile_consumed/pile.burn.combustion.frac)
  )]
  
  # Correct for char + combustion fractions greater than 1
  wildfire_data_year25[CWD_year25_Scattered_CombustionFrac + CWD_year25_Scattered_CharFrac > 1, CWD_year25_Scattered_CharFrac := 1 - CWD_year25_Scattered_CombustionFrac]
  wildfire_data_year25[FWD_year25_Scattered_CombustionFrac + FWD_year25_Scattered_CharFrac > 1, FWD_year25_Scattered_CharFrac := 1 - FWD_year25_Scattered_CombustionFrac]
  
  # Trim the now-un-needed columns
  wildfire_data_year25[,':='(total_pile_clean_PM10 = NULL, total_pile_vdirty_PM10 = NULL, total_pile_clean_PM2.5 = NULL, total_pile_vdirty_PM2.5 = NULL, total_pile_CH4 = NULL, total_pile_CO = NULL, total_pile_CO2 = NULL, total_pile_NOx = NULL, total_pile_SO2 = NULL, total_pile_VOC = NULL, pile_char = NULL, char_fwd_residue = NULL, char_cwd_residue = NULL, total_duff_exposed = NULL, total_foliage_exposed = NULL, total_fwd_exposed = NULL, total_cwd_exposed = NULL, total_pile_consumed = NULL, total_duff_consumed = NULL, total_foliage_consumed = NULL, total_fwd_consumed = NULL, total_cwd_consumed = NULL, total_fuel_consumed=NULL, total_duff_residue_CH4 = NULL, total_foliage_residue_CH4 = NULL, total_fwd_residue_CH4 = NULL, total_cwd_residue_CH4 = NULL, total_duff_residue_CO = NULL, total_foliage_residue_CO = NULL, total_fwd_residue_CO = NULL, total_cwd_residue_CO = NULL, total_duff_residue_CO2 = NULL, total_foliage_residue_CO2 = NULL, total_fwd_residue_CO2 = NULL, total_cwd_residue_CO2 = NULL, total_duff_residue_NOx = NULL, total_foliage_residue_NOx = NULL, total_fwd_residue_NOx = NULL, total_cwd_residue_NOx = NULL, total_duff_residue_PM10 = NULL, total_foliage_residue_PM10 = NULL, total_fwd_residue_PM10 = NULL, total_cwd_residue_PM10 = NULL, total_duff_residue_PM2.5 = NULL, total_foliage_residue_PM2.5 = NULL, total_fwd_residue_PM2.5 = NULL, total_cwd_residue_PM2.5 = NULL, total_duff_residue_SO2 = NULL, total_foliage_residue_SO2 = NULL, total_fwd_residue_SO2 = NULL, total_cwd_residue_SO2 = NULL, total_duff_residue_VOC = NULL, total_foliage_residue_VOC = NULL, total_fwd_residue_VOC = NULL, total_cwd_residue_VOC = NULL)]
  
  wildfire_data_year50[,':='(
    # Combustion and char fractions; foliage and duff do not char
    CWD_year50_Scattered_CombustionFrac = total_cwd_consumed / total_cwd_exposed,
    FWD_year50_Scattered_CombustionFrac = total_fwd_consumed / total_fwd_exposed,
    Foliage_year50_Scattered_CombustionFrac = total_foliage_consumed / total_foliage_exposed,
    Duff_year50_Scattered_CombustionFrac = total_duff_consumed / total_duff_exposed,
    CWD_year50_Scattered_CharFrac = char_cwd_residue / total_cwd_exposed,
    FWD_year50_Scattered_CharFrac = char_fwd_residue / total_fwd_exposed,
    # Emissions factors - CH4
    Duff_year50_Scattered_CH4_EmFac = total_duff_residue_CH4 / total_duff_exposed,
    Foliage_year50_Scattered_CH4_EmFac = total_foliage_residue_CH4 / total_foliage_exposed,
    FWD_year50_Scattered_CH4_EmFac = total_fwd_residue_CH4 / total_fwd_exposed,
    CWD_year50_Scattered_CH4_EmFac = total_cwd_residue_CH4 / total_cwd_exposed,
    Piled_year50_CH4_EmFac = total_pile_CH4 / (total_pile_consumed/pile.burn.combustion.frac),
    # CO
    Duff_year50_Scattered_CO_EmFac = total_duff_residue_CO / total_duff_exposed,
    Foliage_year50_Scattered_CO_EmFac = total_foliage_residue_CO / total_foliage_exposed,
    FWD_year50_Scattered_CO_EmFac = total_fwd_residue_CO / total_fwd_exposed,
    CWD_year50_Scattered_CO_EmFac = total_cwd_residue_CO / total_cwd_exposed,
    Piled_year50_CO_EmFac = total_pile_CO / (total_pile_consumed/pile.burn.combustion.frac),
    # NOx
    Duff_year50_Scattered_NOx_EmFac = total_duff_residue_NOx / total_duff_exposed,
    Foliage_year50_Scattered_NOx_EmFac = total_foliage_residue_NOx / total_foliage_exposed,
    FWD_year50_Scattered_NOx_EmFac = total_fwd_residue_NOx / total_fwd_exposed,
    CWD_year50_Scattered_NOx_EmFac = total_cwd_residue_NOx / total_cwd_exposed,
    Piled_year50_NOx_EmFac = total_pile_NOx / (total_pile_consumed/pile.burn.combustion.frac),
    # PM10
    Duff_year50_Scattered_PM10_EmFac = total_duff_residue_PM10 / total_duff_exposed,
    Foliage_year50_Scattered_PM10_EmFac = total_foliage_residue_PM10 / total_foliage_exposed,
    FWD_year50_Scattered_PM10_EmFac = total_fwd_residue_PM10 / total_fwd_exposed,
    CWD_year50_Scattered_PM10_EmFac = total_cwd_residue_PM10 / total_cwd_exposed,
    Piled_year50_PM10_EmFac = total_pile_vdirty_PM10 / (total_pile_consumed/pile.burn.combustion.frac),
    # PM2.5
    Duff_year50_Scattered_PM2.5_EmFac = total_duff_residue_PM2.5 / total_duff_exposed,
    Foliage_year50_Scattered_PM2.5_EmFac = total_foliage_residue_PM2.5 / total_foliage_exposed,
    FWD_year50_Scattered_PM2.5_EmFac = total_fwd_residue_PM2.5 / total_fwd_exposed,
    CWD_year50_Scattered_PM2.5_EmFac = total_cwd_residue_PM2.5 / total_cwd_exposed,
    Piled_year50_PM2.5_EmFac = total_pile_vdirty_PM2.5 / (total_pile_consumed/pile.burn.combustion.frac),
    # SO2
    Duff_year50_Scattered_SO2_EmFac = total_duff_residue_SO2 / total_duff_exposed,
    Foliage_year50_Scattered_SO2_EmFac = total_foliage_residue_SO2 / total_foliage_exposed,
    FWD_year50_Scattered_SO2_EmFac = total_fwd_residue_SO2 / total_fwd_exposed,
    CWD_year50_Scattered_SO2_EmFac = total_cwd_residue_SO2 / total_cwd_exposed,
    Piled_year50_SO2_EmFac = total_pile_SO2 / (total_pile_consumed/pile.burn.combustion.frac),
    # VOC
    Duff_year50_Scattered_VOC_EmFac = total_duff_residue_VOC / total_duff_exposed,
    Foliage_year50_Scattered_VOC_EmFac = total_foliage_residue_VOC / total_foliage_exposed,
    FWD_year50_Scattered_VOC_EmFac = total_fwd_residue_VOC / total_fwd_exposed,
    CWD_year50_Scattered_VOC_EmFac = total_cwd_residue_VOC / total_cwd_exposed,
    Piled_year50_VOC_EmFac = total_pile_VOC / (total_pile_consumed/pile.burn.combustion.frac)
  )]
  
  # Correct for char + combustion fractions greater than 1
  wildfire_data_year50[CWD_year50_Scattered_CombustionFrac + CWD_year50_Scattered_CharFrac > 1, CWD_year50_Scattered_CharFrac := 1 - CWD_year50_Scattered_CombustionFrac]
  wildfire_data_year50[FWD_year50_Scattered_CombustionFrac + FWD_year50_Scattered_CharFrac > 1, FWD_year50_Scattered_CharFrac := 1 - FWD_year50_Scattered_CombustionFrac]

  # Trim the now-un-needed columns
  wildfire_data_year50[,':='(total_pile_clean_PM10 = NULL, total_pile_vdirty_PM10 = NULL, total_pile_clean_PM2.5 = NULL, total_pile_vdirty_PM2.5 = NULL, total_pile_CH4 = NULL, total_pile_CO = NULL, total_pile_CO2 = NULL, total_pile_NOx = NULL, total_pile_SO2 = NULL, total_pile_VOC = NULL, pile_char = NULL, char_fwd_residue = NULL, char_cwd_residue = NULL, total_duff_exposed = NULL, total_foliage_exposed = NULL, total_fwd_exposed = NULL, total_cwd_exposed = NULL, total_pile_consumed = NULL, total_duff_consumed = NULL, total_foliage_consumed = NULL, total_fwd_consumed = NULL, total_cwd_consumed = NULL, total_fuel_consumed=NULL, total_duff_residue_CH4 = NULL, total_foliage_residue_CH4 = NULL, total_fwd_residue_CH4 = NULL, total_cwd_residue_CH4 = NULL, total_duff_residue_CO = NULL, total_foliage_residue_CO = NULL, total_fwd_residue_CO = NULL, total_cwd_residue_CO = NULL, total_duff_residue_CO2 = NULL, total_foliage_residue_CO2 = NULL, total_fwd_residue_CO2 = NULL, total_cwd_residue_CO2 = NULL, total_duff_residue_NOx = NULL, total_foliage_residue_NOx = NULL, total_fwd_residue_NOx = NULL, total_cwd_residue_NOx = NULL, total_duff_residue_PM10 = NULL, total_foliage_residue_PM10 = NULL, total_fwd_residue_PM10 = NULL, total_cwd_residue_PM10 = NULL, total_duff_residue_PM2.5 = NULL, total_foliage_residue_PM2.5 = NULL, total_fwd_residue_PM2.5 = NULL, total_cwd_residue_PM2.5 = NULL, total_duff_residue_SO2 = NULL, total_foliage_residue_SO2 = NULL, total_fwd_residue_SO2 = NULL, total_cwd_residue_SO2 = NULL, total_duff_residue_VOC = NULL, total_foliage_residue_VOC = NULL, total_fwd_residue_VOC = NULL, total_cwd_residue_VOC = NULL)]
  
  wildfire_data_year75[,':='(
    # Combustion and char fractions; foliage and duff do not char
    CWD_year75_Scattered_CombustionFrac = total_cwd_consumed / total_cwd_exposed,
    FWD_year75_Scattered_CombustionFrac = total_fwd_consumed / total_fwd_exposed,
    Foliage_year75_Scattered_CombustionFrac = total_foliage_consumed / total_foliage_exposed,
    Duff_year75_Scattered_CombustionFrac = total_duff_consumed / total_duff_exposed,
    CWD_year75_Scattered_CharFrac = char_cwd_residue / total_cwd_exposed,
    FWD_year75_Scattered_CharFrac = char_fwd_residue / total_fwd_exposed,
    # Emissions factors - CH4
    Duff_year75_Scattered_CH4_EmFac = total_duff_residue_CH4 / total_duff_exposed,
    Foliage_year75_Scattered_CH4_EmFac = total_foliage_residue_CH4 / total_foliage_exposed,
    FWD_year75_Scattered_CH4_EmFac = total_fwd_residue_CH4 / total_fwd_exposed,
    CWD_year75_Scattered_CH4_EmFac = total_cwd_residue_CH4 / total_cwd_exposed,
    Piled_year75_CH4_EmFac = total_pile_CH4 / (total_pile_consumed/pile.burn.combustion.frac),
    # CO
    Duff_year75_Scattered_CO_EmFac = total_duff_residue_CO / total_duff_exposed,
    Foliage_year75_Scattered_CO_EmFac = total_foliage_residue_CO / total_foliage_exposed,
    FWD_year75_Scattered_CO_EmFac = total_fwd_residue_CO / total_fwd_exposed,
    CWD_year75_Scattered_CO_EmFac = total_cwd_residue_CO / total_cwd_exposed,
    Piled_year75_CO_EmFac = total_pile_CO / (total_pile_consumed/pile.burn.combustion.frac),
    # NOx
    Duff_year75_Scattered_NOx_EmFac = total_duff_residue_NOx / total_duff_exposed,
    Foliage_year75_Scattered_NOx_EmFac = total_foliage_residue_NOx / total_foliage_exposed,
    FWD_year75_Scattered_NOx_EmFac = total_fwd_residue_NOx / total_fwd_exposed,
    CWD_year75_Scattered_NOx_EmFac = total_cwd_residue_NOx / total_cwd_exposed,
    Piled_year75_NOx_EmFac = total_pile_NOx / (total_pile_consumed/pile.burn.combustion.frac),
    # PM10
    Duff_year75_Scattered_PM10_EmFac = total_duff_residue_PM10 / total_duff_exposed,
    Foliage_year75_Scattered_PM10_EmFac = total_foliage_residue_PM10 / total_foliage_exposed,
    FWD_year75_Scattered_PM10_EmFac = total_fwd_residue_PM10 / total_fwd_exposed,
    CWD_year75_Scattered_PM10_EmFac = total_cwd_residue_PM10 / total_cwd_exposed,
    Piled_year75_PM10_EmFac = total_pile_vdirty_PM10 / (total_pile_consumed/pile.burn.combustion.frac),
    # PM2.5
    Duff_year75_Scattered_PM2.5_EmFac = total_duff_residue_PM2.5 / total_duff_exposed,
    Foliage_year75_Scattered_PM2.5_EmFac = total_foliage_residue_PM2.5 / total_foliage_exposed,
    FWD_year75_Scattered_PM2.5_EmFac = total_fwd_residue_PM2.5 / total_fwd_exposed,
    CWD_year75_Scattered_PM2.5_EmFac = total_cwd_residue_PM2.5 / total_cwd_exposed,
    Piled_year75_PM2.5_EmFac = total_pile_vdirty_PM2.5 / (total_pile_consumed/pile.burn.combustion.frac),
    # SO2
    Duff_year75_Scattered_SO2_EmFac = total_duff_residue_SO2 / total_duff_exposed,
    Foliage_year75_Scattered_SO2_EmFac = total_foliage_residue_SO2 / total_foliage_exposed,
    FWD_year75_Scattered_SO2_EmFac = total_fwd_residue_SO2 / total_fwd_exposed,
    CWD_year75_Scattered_SO2_EmFac = total_cwd_residue_SO2 / total_cwd_exposed,
    Piled_year75_SO2_EmFac = total_pile_SO2 / (total_pile_consumed/pile.burn.combustion.frac),
    # VOC
    Duff_year75_Scattered_VOC_EmFac = total_duff_residue_VOC / total_duff_exposed,
    Foliage_year75_Scattered_VOC_EmFac = total_foliage_residue_VOC / total_foliage_exposed,
    FWD_year75_Scattered_VOC_EmFac = total_fwd_residue_VOC / total_fwd_exposed,
    CWD_year75_Scattered_VOC_EmFac = total_cwd_residue_VOC / total_cwd_exposed,
    Piled_year75_VOC_EmFac = total_pile_VOC / (total_pile_consumed/pile.burn.combustion.frac)
  )]
  
  # Correct for char + combustion fractions greater than 1
  wildfire_data_year75[CWD_year75_Scattered_CombustionFrac + CWD_year75_Scattered_CharFrac > 1, CWD_year75_Scattered_CharFrac := 1 - CWD_year75_Scattered_CombustionFrac]
  wildfire_data_year75[FWD_year75_Scattered_CombustionFrac + FWD_year75_Scattered_CharFrac > 1, FWD_year75_Scattered_CharFrac := 1 - FWD_year75_Scattered_CombustionFrac]
  
  # Trim the now-un-needed columns
  wildfire_data_year75[,':='(total_pile_clean_PM10 = NULL, total_pile_vdirty_PM10 = NULL, total_pile_clean_PM2.5 = NULL, total_pile_vdirty_PM2.5 = NULL, total_pile_CH4 = NULL, total_pile_CO = NULL, total_pile_CO2 = NULL, total_pile_NOx = NULL, total_pile_SO2 = NULL, total_pile_VOC = NULL, pile_char = NULL, char_fwd_residue = NULL, char_cwd_residue = NULL, total_duff_exposed = NULL, total_foliage_exposed = NULL, total_fwd_exposed = NULL, total_cwd_exposed = NULL, total_pile_consumed = NULL, total_duff_consumed = NULL, total_foliage_consumed = NULL, total_fwd_consumed = NULL, total_cwd_consumed = NULL, total_fuel_consumed=NULL, total_duff_residue_CH4 = NULL, total_foliage_residue_CH4 = NULL, total_fwd_residue_CH4 = NULL, total_cwd_residue_CH4 = NULL, total_duff_residue_CO = NULL, total_foliage_residue_CO = NULL, total_fwd_residue_CO = NULL, total_cwd_residue_CO = NULL, total_duff_residue_CO2 = NULL, total_foliage_residue_CO2 = NULL, total_fwd_residue_CO2 = NULL, total_cwd_residue_CO2 = NULL, total_duff_residue_NOx = NULL, total_foliage_residue_NOx = NULL, total_fwd_residue_NOx = NULL, total_cwd_residue_NOx = NULL, total_duff_residue_PM10 = NULL, total_foliage_residue_PM10 = NULL, total_fwd_residue_PM10 = NULL, total_cwd_residue_PM10 = NULL, total_duff_residue_PM2.5 = NULL, total_foliage_residue_PM2.5 = NULL, total_fwd_residue_PM2.5 = NULL, total_cwd_residue_PM2.5 = NULL, total_duff_residue_SO2 = NULL, total_foliage_residue_SO2 = NULL, total_fwd_residue_SO2 = NULL, total_cwd_residue_SO2 = NULL, total_duff_residue_VOC = NULL, total_foliage_residue_VOC = NULL, total_fwd_residue_VOC = NULL, total_cwd_residue_VOC = NULL)]
  
  wildfire_data_year100[,':='(
    # Combustion and char fractions; foliage and duff do not char
    CWD_year100_Scattered_CombustionFrac = total_cwd_consumed / total_cwd_exposed,
    FWD_year100_Scattered_CombustionFrac = total_fwd_consumed / total_fwd_exposed,
    Foliage_year100_Scattered_CombustionFrac = total_foliage_consumed / total_foliage_exposed,
    Duff_year100_Scattered_CombustionFrac = total_duff_consumed / total_duff_exposed,
    CWD_year100_Scattered_CharFrac = char_cwd_residue / total_cwd_exposed,
    FWD_year100_Scattered_CharFrac = char_fwd_residue / total_fwd_exposed,
    # Emissions factors - CH4
    Duff_year100_Scattered_CH4_EmFac = total_duff_residue_CH4 / total_duff_exposed,
    Foliage_year100_Scattered_CH4_EmFac = total_foliage_residue_CH4 / total_foliage_exposed,
    FWD_year100_Scattered_CH4_EmFac = total_fwd_residue_CH4 / total_fwd_exposed,
    CWD_year100_Scattered_CH4_EmFac = total_cwd_residue_CH4 / total_cwd_exposed,
    Piled_year100_CH4_EmFac = total_pile_CH4 / (total_pile_consumed/pile.burn.combustion.frac),
    # CO
    Duff_year100_Scattered_CO_EmFac = total_duff_residue_CO / total_duff_exposed,
    Foliage_year100_Scattered_CO_EmFac = total_foliage_residue_CO / total_foliage_exposed,
    FWD_year100_Scattered_CO_EmFac = total_fwd_residue_CO / total_fwd_exposed,
    CWD_year100_Scattered_CO_EmFac = total_cwd_residue_CO / total_cwd_exposed,
    Piled_year100_CO_EmFac = total_pile_CO / (total_pile_consumed/pile.burn.combustion.frac),
    # NOx
    Duff_year100_Scattered_NOx_EmFac = total_duff_residue_NOx / total_duff_exposed,
    Foliage_year100_Scattered_NOx_EmFac = total_foliage_residue_NOx / total_foliage_exposed,
    FWD_year100_Scattered_NOx_EmFac = total_fwd_residue_NOx / total_fwd_exposed,
    CWD_year100_Scattered_NOx_EmFac = total_cwd_residue_NOx / total_cwd_exposed,
    Piled_year100_NOx_EmFac = total_pile_NOx / (total_pile_consumed/pile.burn.combustion.frac),
    # PM10
    Duff_year100_Scattered_PM10_EmFac = total_duff_residue_PM10 / total_duff_exposed,
    Foliage_year100_Scattered_PM10_EmFac = total_foliage_residue_PM10 / total_foliage_exposed,
    FWD_year100_Scattered_PM10_EmFac = total_fwd_residue_PM10 / total_fwd_exposed,
    CWD_year100_Scattered_PM10_EmFac = total_cwd_residue_PM10 / total_cwd_exposed,
    Piled_year100_PM10_EmFac = total_pile_vdirty_PM10 / (total_pile_consumed/pile.burn.combustion.frac),
    # PM2.5
    Duff_year100_Scattered_PM2.5_EmFac = total_duff_residue_PM2.5 / total_duff_exposed,
    Foliage_year100_Scattered_PM2.5_EmFac = total_foliage_residue_PM2.5 / total_foliage_exposed,
    FWD_year100_Scattered_PM2.5_EmFac = total_fwd_residue_PM2.5 / total_fwd_exposed,
    CWD_year100_Scattered_PM2.5_EmFac = total_cwd_residue_PM2.5 / total_cwd_exposed,
    Piled_year100_PM2.5_EmFac = total_pile_vdirty_PM2.5 / (total_pile_consumed/pile.burn.combustion.frac),
    # SO2
    Duff_year100_Scattered_SO2_EmFac = total_duff_residue_SO2 / total_duff_exposed,
    Foliage_year100_Scattered_SO2_EmFac = total_foliage_residue_SO2 / total_foliage_exposed,
    FWD_year100_Scattered_SO2_EmFac = total_fwd_residue_SO2 / total_fwd_exposed,
    CWD_year100_Scattered_SO2_EmFac = total_cwd_residue_SO2 / total_cwd_exposed,
    Piled_year100_SO2_EmFac = total_pile_SO2 / (total_pile_consumed/pile.burn.combustion.frac),
    # VOC
    Duff_year100_Scattered_VOC_EmFac = total_duff_residue_VOC / total_duff_exposed,
    Foliage_year100_Scattered_VOC_EmFac = total_foliage_residue_VOC / total_foliage_exposed,
    FWD_year100_Scattered_VOC_EmFac = total_fwd_residue_VOC / total_fwd_exposed,
    CWD_year100_Scattered_VOC_EmFac = total_cwd_residue_VOC / total_cwd_exposed,
    Piled_year100_VOC_EmFac = total_pile_VOC / (total_pile_consumed/pile.burn.combustion.frac)
  )]
  
  # Correct for char + combustion fractions greater than 1
  wildfire_data_year100[CWD_year100_Scattered_CombustionFrac + CWD_year100_Scattered_CharFrac > 1, CWD_year100_Scattered_CharFrac := 1 - CWD_year100_Scattered_CombustionFrac]
  wildfire_data_year100[FWD_year100_Scattered_CombustionFrac + FWD_year100_Scattered_CharFrac > 1, FWD_year100_Scattered_CharFrac := 1 - FWD_year100_Scattered_CombustionFrac]
  
  # Trim the now-un-needed columns
  wildfire_data_year100[,':='(total_pile_clean_PM10 = NULL, total_pile_vdirty_PM10 = NULL, total_pile_clean_PM2.5 = NULL, total_pile_vdirty_PM2.5 = NULL, total_pile_CH4 = NULL, total_pile_CO = NULL, total_pile_CO2 = NULL, total_pile_NOx = NULL, total_pile_SO2 = NULL, total_pile_VOC = NULL, pile_char = NULL, char_fwd_residue = NULL, char_cwd_residue = NULL, total_duff_exposed = NULL, total_foliage_exposed = NULL, total_fwd_exposed = NULL, total_cwd_exposed = NULL, total_pile_consumed = NULL, total_duff_consumed = NULL, total_foliage_consumed = NULL, total_fwd_consumed = NULL, total_cwd_consumed = NULL, total_fuel_consumed=NULL, total_duff_residue_CH4 = NULL, total_foliage_residue_CH4 = NULL, total_fwd_residue_CH4 = NULL, total_cwd_residue_CH4 = NULL, total_duff_residue_CO = NULL, total_foliage_residue_CO = NULL, total_fwd_residue_CO = NULL, total_cwd_residue_CO = NULL, total_duff_residue_CO2 = NULL, total_foliage_residue_CO2 = NULL, total_fwd_residue_CO2 = NULL, total_cwd_residue_CO2 = NULL, total_duff_residue_NOx = NULL, total_foliage_residue_NOx = NULL, total_fwd_residue_NOx = NULL, total_cwd_residue_NOx = NULL, total_duff_residue_PM10 = NULL, total_foliage_residue_PM10 = NULL, total_fwd_residue_PM10 = NULL, total_cwd_residue_PM10 = NULL, total_duff_residue_PM2.5 = NULL, total_foliage_residue_PM2.5 = NULL, total_fwd_residue_PM2.5 = NULL, total_cwd_residue_PM2.5 = NULL, total_duff_residue_SO2 = NULL, total_foliage_residue_SO2 = NULL, total_fwd_residue_SO2 = NULL, total_cwd_residue_SO2 = NULL, total_duff_residue_VOC = NULL, total_foliage_residue_VOC = NULL, total_fwd_residue_VOC = NULL, total_cwd_residue_VOC = NULL)]
  
  # Combine all years of wildfire data and combine into a signle data set
  setkey(wildfire_data_year0,x,y)
  setkey(wildfire_data_year25,x,y)
  setkey(wildfire_data_year50,x,y)
  setkey(wildfire_data_year75,x,y)
  setkey(wildfire_data_year100,x,y)
  
  wildfire.data <- wildfire_data_year0[wildfire_data_year25[wildfire_data_year50[wildfire_data_year75[wildfire_data_year100]]]]
}

# -------------------------------------------------------------------------
#
# FUNCTION: Annual Wildfire Characteristics
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This function will, for a given year, calculate the emissions factors, combustion fractions, and char fractions.
# The CBREC-Fire model calculates these data in 25-year intervals. This function either pulls these data directly if
# year.i falls directly on one of the 25-year intervals, or linearly interpolates for the particular year.i.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data and
# year.i: The year for which we want to calculate the char and emissions from the year prior (i.e.,
#   if t=3, we would calculate the emissions between years 2-3.)
# wildfire.data: A data table containing the emissions factors and combustion/char fractions for 
#   years 0, 25, 50, 75, and 100
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with the emissions factors and co,bustion/char fractions for year.i
# =================================================================================================
# study_area_FCID <- annual_fire_char_fun(study_area_FCID,year.i,wildfire_data) {}
annual_fire_detail_fun <- function(cbrec.dt, 
                                   year.i,
                                   wildfire.data
                                   #wildfire.probability.directory,
                                   #study.area.raster,
                                   #poly_num
                                   ) {

  # The key wildfire values - 8 different emission factors, and combustion and char fractions for all residue size classes - will differ from year to year. Carrying all 192 columns of wildfire data along with
  # study_area_FCID will slow this down way too much. So, we will add the columns for emission factors for each species, as well as combustion/char fractions for each size class, and update the columns each year.
  # We could write a generic function that applies the same code to every year, but I think we are going to be calling this every year. I think a series of "if" statements on year.i will be quicker.
  
  if(year.i<25){
    wildfire.data[,':='(CWD_Scattered_CombustionFrac = (CWD_year25_Scattered_CombustionFrac - CWD_year0_Scattered_CombustionFrac)/25 * (year.i) + CWD_year0_Scattered_CombustionFrac,
                        FWD_Scattered_CombustionFrac = (FWD_year25_Scattered_CombustionFrac - FWD_year0_Scattered_CombustionFrac)/25 * (year.i) + FWD_year0_Scattered_CombustionFrac, 
                        Foliage_Scattered_CombustionFrac = (Foliage_year25_Scattered_CombustionFrac - Foliage_year0_Scattered_CombustionFrac)/25 * (year.i) + Foliage_year0_Scattered_CombustionFrac, 
                        Duff_Scattered_CombustionFrac = (Duff_year25_Scattered_CombustionFrac - Duff_year0_Scattered_CombustionFrac)/25 * (year.i) + Duff_year0_Scattered_CombustionFrac,
                        CWD_Scattered_CharFrac = (CWD_year25_Scattered_CharFrac - CWD_year0_Scattered_CharFrac)/25 * (year.i) + CWD_year0_Scattered_CharFrac, 
                        FWD_Scattered_CharFrac = (FWD_year25_Scattered_CharFrac - FWD_year0_Scattered_CharFrac)/25 * (year.i) + FWD_year0_Scattered_CharFrac,
                        Duff_Scattered_CH4_EmFac = (Duff_year25_Scattered_CH4_EmFac - Duff_year0_Scattered_CH4_EmFac)/25 * (year.i) + Duff_year0_Scattered_CH4_EmFac, 
                        Foliage_Scattered_CH4_EmFac = (Foliage_year25_Scattered_CH4_EmFac - Foliage_year0_Scattered_CH4_EmFac)/25 * (year.i) + Foliage_year0_Scattered_CH4_EmFac, 
                        FWD_Scattered_CH4_EmFac = (FWD_year25_Scattered_CH4_EmFac - FWD_year0_Scattered_CH4_EmFac)/25 * (year.i) + FWD_year0_Scattered_CH4_EmFac, 
                        CWD_Scattered_CH4_EmFac = (CWD_year25_Scattered_CH4_EmFac - CWD_year0_Scattered_CH4_EmFac)/25 * (year.i) + CWD_year0_Scattered_CH4_EmFac,
                        Piled_CH4_EmFac = (Piled_year25_CH4_EmFac - Piled_year0_CH4_EmFac)/25 * (year.i) + Piled_year0_CH4_EmFac,
                        
                        Duff_Scattered_CO_EmFac = (Duff_year25_Scattered_CO_EmFac - Duff_year0_Scattered_CO_EmFac)/25 * (year.i) + Duff_year0_Scattered_CO_EmFac, 
                        Foliage_Scattered_CO_EmFac = (Foliage_year25_Scattered_CO_EmFac - Foliage_year0_Scattered_CO_EmFac)/25 * (year.i) + Foliage_year0_Scattered_CO_EmFac, 
                        FWD_Scattered_CO_EmFac = (FWD_year25_Scattered_CO_EmFac - FWD_year0_Scattered_CO_EmFac)/25 * (year.i) + FWD_year0_Scattered_CO_EmFac, 
                        CWD_Scattered_CO_EmFac = (CWD_year25_Scattered_CO_EmFac - CWD_year0_Scattered_CO_EmFac)/25 * (year.i) + CWD_year0_Scattered_CO_EmFac,
                        Piled_CO_EmFac = (Piled_year25_CO_EmFac - Piled_year0_CO_EmFac)/25 * (year.i) + Piled_year0_CO_EmFac,
                        
                        Duff_Scattered_NOx_EmFac = (Duff_year25_Scattered_NOx_EmFac - Duff_year0_Scattered_NOx_EmFac)/25 * (year.i) + Duff_year0_Scattered_NOx_EmFac, 
                        Foliage_Scattered_NOx_EmFac = (Foliage_year25_Scattered_NOx_EmFac - Foliage_year0_Scattered_NOx_EmFac)/25 * (year.i) + Foliage_year0_Scattered_NOx_EmFac, 
                        FWD_Scattered_NOx_EmFac = (FWD_year25_Scattered_NOx_EmFac - FWD_year0_Scattered_NOx_EmFac)/25 * (year.i) + FWD_year0_Scattered_NOx_EmFac, 
                        CWD_Scattered_NOx_EmFac = (CWD_year25_Scattered_NOx_EmFac - CWD_year0_Scattered_NOx_EmFac)/25 * (year.i) + CWD_year0_Scattered_NOx_EmFac,
                        Piled_NOx_EmFac = (Piled_year25_NOx_EmFac - Piled_year0_NOx_EmFac)/25 * (year.i) + Piled_year0_NOx_EmFac,
                        
                        Duff_Scattered_PM10_EmFac = (Duff_year25_Scattered_PM10_EmFac - Duff_year0_Scattered_PM10_EmFac)/25 * (year.i) + Duff_year0_Scattered_PM10_EmFac, 
                        Foliage_Scattered_PM10_EmFac = (Foliage_year25_Scattered_PM10_EmFac - Foliage_year0_Scattered_PM10_EmFac)/25 * (year.i) + Foliage_year0_Scattered_PM10_EmFac, 
                        FWD_Scattered_PM10_EmFac = (FWD_year25_Scattered_PM10_EmFac - FWD_year0_Scattered_PM10_EmFac)/25 * (year.i) + FWD_year0_Scattered_PM10_EmFac, 
                        CWD_Scattered_PM10_EmFac = (CWD_year25_Scattered_PM10_EmFac - CWD_year0_Scattered_PM10_EmFac)/25 * (year.i) + CWD_year0_Scattered_PM10_EmFac,
                        Piled_PM10_EmFac = (Piled_year25_PM10_EmFac - Piled_year0_PM10_EmFac)/25 * (year.i) + Piled_year0_PM10_EmFac,
                        
                        Duff_Scattered_PM2.5_EmFac = (Duff_year25_Scattered_PM2.5_EmFac - Duff_year0_Scattered_PM2.5_EmFac)/25 * (year.i) + Duff_year0_Scattered_PM2.5_EmFac, 
                        Foliage_Scattered_PM2.5_EmFac = (Foliage_year25_Scattered_PM2.5_EmFac - Foliage_year0_Scattered_PM2.5_EmFac)/25 * (year.i) + Foliage_year0_Scattered_PM2.5_EmFac, 
                        FWD_Scattered_PM2.5_EmFac = (FWD_year25_Scattered_PM2.5_EmFac - FWD_year0_Scattered_PM2.5_EmFac)/25 * (year.i) + FWD_year0_Scattered_PM2.5_EmFac, 
                        CWD_Scattered_PM2.5_EmFac = (CWD_year25_Scattered_PM2.5_EmFac - CWD_year0_Scattered_PM2.5_EmFac)/25 * (year.i) + CWD_year0_Scattered_PM2.5_EmFac,
                        Piled_PM2.5_EmFac = (Piled_year25_PM2.5_EmFac - Piled_year0_PM2.5_EmFac)/25 * (year.i) + Piled_year0_PM2.5_EmFac,
                        
                        Duff_Scattered_SO2_EmFac = (Duff_year25_Scattered_SO2_EmFac - Duff_year0_Scattered_SO2_EmFac)/25 * (year.i) + Duff_year0_Scattered_SO2_EmFac, 
                        Foliage_Scattered_SO2_EmFac = (Foliage_year25_Scattered_SO2_EmFac - Foliage_year0_Scattered_SO2_EmFac)/25 * (year.i) + Foliage_year0_Scattered_SO2_EmFac, 
                        FWD_Scattered_SO2_EmFac = (FWD_year25_Scattered_SO2_EmFac - FWD_year0_Scattered_SO2_EmFac)/25 * (year.i) + FWD_year0_Scattered_SO2_EmFac, 
                        CWD_Scattered_SO2_EmFac = (CWD_year25_Scattered_SO2_EmFac - CWD_year0_Scattered_SO2_EmFac)/25 * (year.i) + CWD_year0_Scattered_SO2_EmFac,
                        Piled_SO2_EmFac = (Piled_year25_SO2_EmFac - Piled_year0_SO2_EmFac)/25 * (year.i) + Piled_year0_SO2_EmFac,
                        
                        Duff_Scattered_VOC_EmFac = (Duff_year25_Scattered_VOC_EmFac - Duff_year0_Scattered_VOC_EmFac)/25 * (year.i) + Duff_year0_Scattered_VOC_EmFac, 
                        Foliage_Scattered_VOC_EmFac = (Foliage_year25_Scattered_VOC_EmFac - Foliage_year0_Scattered_VOC_EmFac)/25 * (year.i) + Foliage_year0_Scattered_VOC_EmFac, 
                        FWD_Scattered_VOC_EmFac = (FWD_year25_Scattered_VOC_EmFac - FWD_year0_Scattered_VOC_EmFac)/25 * (year.i) + FWD_year0_Scattered_VOC_EmFac, 
                        CWD_Scattered_VOC_EmFac = (CWD_year25_Scattered_VOC_EmFac - CWD_year0_Scattered_VOC_EmFac)/25 * (year.i) + CWD_year0_Scattered_VOC_EmFac,
                        Piled_VOC_EmFac = (Piled_year25_VOC_EmFac - Piled_year0_VOC_EmFac)/25 * (year.i) + Piled_year0_VOC_EmFac
                        )]
  }
  if(year.i==25) {
    wildfire.data[,':='(CWD_Scattered_CombustionFrac = CWD_year25_Scattered_CombustionFrac, FWD_Scattered_CombustionFrac = FWD_year25_Scattered_CombustionFrac, Foliage_Scattered_CombustionFrac = Foliage_year25_Scattered_CombustionFrac, Duff_Scattered_CombustionFrac = Duff_year25_Scattered_CombustionFrac,
                        CWD_Scattered_CharFrac = CWD_year25_Scattered_CharFrac, FWD_Scattered_CharFrac = FWD_year25_Scattered_CharFrac,
                        Duff_Scattered_CH4_EmFac = Duff_year25_Scattered_CH4_EmFac, Foliage_Scattered_CH4_EmFac = Foliage_year25_Scattered_CH4_EmFac, FWD_Scattered_CH4_EmFac = FWD_year25_Scattered_CH4_EmFac, CWD_Scattered_CH4_EmFac = CWD_year25_Scattered_CH4_EmFac, Piled_CH4_EmFac = Piled_year25_CH4_EmFac,
                        Duff_Scattered_CO_EmFac = Duff_year25_Scattered_CO_EmFac, Foliage_Scattered_CO_EmFac = Foliage_year25_Scattered_CO_EmFac, FWD_Scattered_CO_EmFac = FWD_year25_Scattered_CO_EmFac, CWD_Scattered_CO_EmFac = CWD_year25_Scattered_CO_EmFac, Piled_CO_EmFac = Piled_year25_CO_EmFac,
                        Duff_Scattered_NOx_EmFac = Duff_year25_Scattered_NOx_EmFac, Foliage_Scattered_NOx_EmFac = Foliage_year25_Scattered_NOx_EmFac, FWD_Scattered_NOx_EmFac = FWD_year25_Scattered_NOx_EmFac, CWD_Scattered_NOx_EmFac = CWD_year25_Scattered_NOx_EmFac, Piled_NOx_EmFac = Piled_year25_NOx_EmFac,
                        Duff_Scattered_PM10_EmFac = Duff_year25_Scattered_PM10_EmFac, Foliage_Scattered_PM10_EmFac = Foliage_year25_Scattered_PM10_EmFac, FWD_Scattered_PM10_EmFac = FWD_year25_Scattered_PM10_EmFac, CWD_Scattered_PM10_EmFac = CWD_year25_Scattered_PM10_EmFac, Piled_PM10_EmFac = Piled_year25_PM10_EmFac,
                        Duff_Scattered_PM2.5_EmFac = Duff_year25_Scattered_PM2.5_EmFac, Foliage_Scattered_PM2.5_EmFac = Foliage_year25_Scattered_PM2.5_EmFac, FWD_year25_Scattered_PM2.5_EmFac = FWD_year25_Scattered_PM2.5_EmFac, CWD_Scattered_PM2.5_EmFac = CWD_year25_Scattered_PM2.5_EmFac, Piled_PM2.5_EmFac = Piled_year25_PM2.5_EmFac,
                        Duff_Scattered_SO2_EmFac = Duff_year25_Scattered_SO2_EmFac, Foliage_Scattered_SO2_EmFac = Foliage_year25_Scattered_SO2_EmFac, FWD_Scattered_SO2_EmFac = FWD_year25_Scattered_SO2_EmFac, CWD_Scattered_SO2_EmFac = CWD_year25_Scattered_SO2_EmFac, Piled_SO2_EmFac = Piled_year25_SO2_EmFac,
                        Duff_Scattered_VOC_EmFac = Duff_year25_Scattered_VOC_EmFac, Foliage_Scattered_VOC_EmFac = Foliage_year25_Scattered_VOC_EmFac, FWD_Scattered_VOC_EmFac = FWD_year25_Scattered_VOC_EmFac, CWD_Scattered_VOC_EmFac = CWD_year25_Scattered_VOC_EmFac, Piled_VOC_EmFac = Piled_year25_VOC_EmFac)]
  }
  if(year.i>25&year.i<50){
    wildfire.data[,':='(CWD_Scattered_CombustionFrac = (CWD_year50_Scattered_CombustionFrac - CWD_year25_Scattered_CombustionFrac)/25 * (year.i-25) + CWD_year25_Scattered_CombustionFrac,
                        FWD_Scattered_CombustionFrac = (FWD_year50_Scattered_CombustionFrac - FWD_year25_Scattered_CombustionFrac)/25 * (year.i-25) + FWD_year25_Scattered_CombustionFrac, 
                        Foliage_Scattered_CombustionFrac = (Foliage_year50_Scattered_CombustionFrac - Foliage_year25_Scattered_CombustionFrac)/25 * (year.i-25) + Foliage_year25_Scattered_CombustionFrac, 
                        Duff_Scattered_CombustionFrac = (Duff_year50_Scattered_CombustionFrac - Duff_year25_Scattered_CombustionFrac)/25 * (year.i-25) + Duff_year25_Scattered_CombustionFrac,
                        CWD_Scattered_CharFrac = (CWD_year50_Scattered_CharFrac - CWD_year25_Scattered_CharFrac)/25 * (year.i-25) + CWD_year25_Scattered_CharFrac, 
                        FWD_Scattered_CharFrac = (FWD_year50_Scattered_CharFrac - FWD_year25_Scattered_CharFrac)/25 * (year.i-25) + FWD_year25_Scattered_CharFrac,
                        
                        Duff_Scattered_CH4_EmFac = (Duff_year50_Scattered_CH4_EmFac - Duff_year25_Scattered_CH4_EmFac)/25 * (year.i-25) + Duff_year25_Scattered_CH4_EmFac, 
                        Foliage_Scattered_CH4_EmFac = (Foliage_year50_Scattered_CH4_EmFac - Foliage_year25_Scattered_CH4_EmFac)/25 * (year.i-25) + Foliage_year25_Scattered_CH4_EmFac, 
                        FWD_Scattered_CH4_EmFac = (FWD_year50_Scattered_CH4_EmFac - FWD_year25_Scattered_CH4_EmFac)/25 * (year.i-25) + FWD_year25_Scattered_CH4_EmFac, 
                        CWD_Scattered_CH4_EmFac = (CWD_year50_Scattered_CH4_EmFac - CWD_year25_Scattered_CH4_EmFac)/25 * (year.i-25) + CWD_year25_Scattered_CH4_EmFac,
                        Piled_CH4_EmFac = (Piled_year50_CH4_EmFac - Piled_year25_CH4_EmFac)/25 * (year.i-25) + Piled_year25_CH4_EmFac,
                        
                        Duff_Scattered_CO_EmFac = (Duff_year50_Scattered_CO_EmFac - Duff_year25_Scattered_CO_EmFac)/25 * (year.i-25) + Duff_year25_Scattered_CO_EmFac, 
                        Foliage_Scattered_CO_EmFac = (Foliage_year50_Scattered_CO_EmFac - Foliage_year25_Scattered_CO_EmFac)/25 * (year.i-25) + Foliage_year25_Scattered_CO_EmFac, 
                        FWD_Scattered_CO_EmFac = (FWD_year50_Scattered_CO_EmFac - FWD_year25_Scattered_CO_EmFac)/25 * (year.i-25) + FWD_year25_Scattered_CO_EmFac, 
                        CWD_Scattered_CO_EmFac = (CWD_year50_Scattered_CO_EmFac - CWD_year25_Scattered_CO_EmFac)/25 * (year.i-25) + CWD_year25_Scattered_CO_EmFac,
                        Piled_CO_EmFac = (Piled_year50_CO_EmFac - Piled_year25_CO_EmFac)/25 * (year.i-25) + Piled_year25_CO_EmFac,
                        
                        Duff_Scattered_NOx_EmFac = (Duff_year50_Scattered_NOx_EmFac - Duff_year25_Scattered_NOx_EmFac)/25 * (year.i-25) + Duff_year25_Scattered_NOx_EmFac, 
                        Foliage_Scattered_NOx_EmFac = (Foliage_year50_Scattered_NOx_EmFac - Foliage_year25_Scattered_NOx_EmFac)/25 * (year.i-25) + Foliage_year25_Scattered_NOx_EmFac, 
                        FWD_Scattered_NOx_EmFac = (FWD_year50_Scattered_NOx_EmFac - FWD_year25_Scattered_NOx_EmFac)/25 * (year.i-25) + FWD_year25_Scattered_NOx_EmFac, 
                        CWD_Scattered_NOx_EmFac = (CWD_year50_Scattered_NOx_EmFac - CWD_year25_Scattered_NOx_EmFac)/25 * (year.i-25) + CWD_year25_Scattered_NOx_EmFac,
                        Piled_NOx_EmFac = (Piled_year50_NOx_EmFac - Piled_year25_NOx_EmFac)/25 * (year.i-25) + Piled_year25_NOx_EmFac,
                        
                        Duff_Scattered_PM10_EmFac = (Duff_year50_Scattered_PM10_EmFac - Duff_year25_Scattered_PM10_EmFac)/25 * (year.i-25) + Duff_year25_Scattered_PM10_EmFac, 
                        Foliage_Scattered_PM10_EmFac = (Foliage_year50_Scattered_PM10_EmFac - Foliage_year25_Scattered_PM10_EmFac)/25 * (year.i-25) + Foliage_year25_Scattered_PM10_EmFac, 
                        FWD_Scattered_PM10_EmFac = (FWD_year50_Scattered_PM10_EmFac - FWD_year25_Scattered_PM10_EmFac)/25 * (year.i-25) + FWD_year25_Scattered_PM10_EmFac, 
                        CWD_Scattered_PM10_EmFac = (CWD_year50_Scattered_PM10_EmFac - CWD_year25_Scattered_PM10_EmFac)/25 * (year.i-25) + CWD_year25_Scattered_PM10_EmFac,
                        Piled_PM10_EmFac = (Piled_year50_PM10_EmFac - Piled_year25_PM10_EmFac)/25 * (year.i-25) + Piled_year25_PM10_EmFac,
                        
                        Duff_Scattered_PM2.5_EmFac = (Duff_year50_Scattered_PM2.5_EmFac - Duff_year25_Scattered_PM2.5_EmFac)/25 * (year.i-25) + Duff_year25_Scattered_PM2.5_EmFac, 
                        Foliage_Scattered_PM2.5_EmFac = (Foliage_year50_Scattered_PM2.5_EmFac - Foliage_year25_Scattered_PM2.5_EmFac)/25 * (year.i-25) + Foliage_year25_Scattered_PM2.5_EmFac, 
                        FWD_Scattered_PM2.5_EmFac = (FWD_year50_Scattered_PM2.5_EmFac - FWD_year25_Scattered_PM2.5_EmFac)/25 * (year.i-25) + FWD_year25_Scattered_PM2.5_EmFac, 
                        CWD_Scattered_PM2.5_EmFac = (CWD_year50_Scattered_PM2.5_EmFac - CWD_year25_Scattered_PM2.5_EmFac)/25 * (year.i-25) + CWD_year25_Scattered_PM2.5_EmFac,
                        Piled_PM2.5_EmFac = (Piled_year50_PM2.5_EmFac - Piled_year25_PM2.5_EmFac)/25 * (year.i-25) + Piled_year25_PM2.5_EmFac,
                        
                        Duff_Scattered_SO2_EmFac = (Duff_year50_Scattered_SO2_EmFac - Duff_year25_Scattered_SO2_EmFac)/25 * (year.i-25) + Duff_year25_Scattered_SO2_EmFac, 
                        Foliage_Scattered_SO2_EmFac = (Foliage_year50_Scattered_SO2_EmFac - Foliage_year25_Scattered_SO2_EmFac)/25 * (year.i-25) + Foliage_year25_Scattered_SO2_EmFac, 
                        FWD_Scattered_SO2_EmFac = (FWD_year50_Scattered_SO2_EmFac - FWD_year25_Scattered_SO2_EmFac)/25 * (year.i-25) + FWD_year25_Scattered_SO2_EmFac, 
                        CWD_Scattered_SO2_EmFac = (CWD_year50_Scattered_SO2_EmFac - CWD_year25_Scattered_SO2_EmFac)/25 * (year.i-25) + CWD_year25_Scattered_SO2_EmFac,
                        Piled_SO2_EmFac = (Piled_year50_SO2_EmFac - Piled_year25_SO2_EmFac)/25 * (year.i-25) + Piled_year25_SO2_EmFac,
                        
                        Duff_Scattered_VOC_EmFac = (Duff_year50_Scattered_VOC_EmFac - Duff_year25_Scattered_VOC_EmFac)/25 * (year.i-25) + Duff_year25_Scattered_VOC_EmFac, 
                        Foliage_Scattered_VOC_EmFac = (Foliage_year50_Scattered_VOC_EmFac - Foliage_year25_Scattered_VOC_EmFac)/25 * (year.i-25) + Foliage_year25_Scattered_VOC_EmFac, 
                        FWD_Scattered_VOC_EmFac = (FWD_year50_Scattered_VOC_EmFac - FWD_year25_Scattered_VOC_EmFac)/25 * (year.i-25) + FWD_year25_Scattered_VOC_EmFac, 
                        CWD_Scattered_VOC_EmFac = (CWD_year50_Scattered_VOC_EmFac - CWD_year25_Scattered_VOC_EmFac)/25 * (year.i-25) + CWD_year25_Scattered_VOC_EmFac,
                        Piled_VOC_EmFac = (Piled_year50_VOC_EmFac - Piled_year25_VOC_EmFac)/25 * (year.i-25) + Piled_year25_VOC_EmFac
                        )]
  }
  if(year.i==50) {
    wildfire.data[,':='(CWD_Scattered_CombustionFrac = CWD_year50_Scattered_CombustionFrac, FWD_Scattered_CombustionFrac = FWD_year50_Scattered_CombustionFrac, Foliage_Scattered_CombustionFrac = Foliage_year50_Scattered_CombustionFrac, Duff_Scattered_CombustionFrac = Duff_year50_Scattered_CombustionFrac,
                        CWD_Scattered_CharFrac = CWD_year50_Scattered_CharFrac, FWD_Scattered_CharFrac = FWD_year50_Scattered_CharFrac,
                        Duff_Scattered_CH4_EmFac = Duff_year50_Scattered_CH4_EmFac, Foliage_Scattered_CH4_EmFac = Foliage_year50_Scattered_CH4_EmFac, FWD_Scattered_CH4_EmFac = FWD_year50_Scattered_CH4_EmFac, CWD_Scattered_CH4_EmFac = CWD_year50_Scattered_CH4_EmFac, Piled_CH4_EmFac = Piled_year50_CH4_EmFac,
                        Duff_Scattered_CO_EmFac = Duff_year50_Scattered_CO_EmFac, Foliage_Scattered_CO_EmFac = Foliage_year50_Scattered_CO_EmFac, FWD_Scattered_CO_EmFac = FWD_year50_Scattered_CO_EmFac, CWD_Scattered_CO_EmFac = CWD_year50_Scattered_CO_EmFac, Piled_CO_EmFac = Piled_year50_CO_EmFac,
                        Duff_Scattered_NOx_EmFac = Duff_year50_Scattered_NOx_EmFac, Foliage_Scattered_NOx_EmFac = Foliage_year50_Scattered_NOx_EmFac, FWD_Scattered_NOx_EmFac = FWD_year50_Scattered_NOx_EmFac, CWD_Scattered_NOx_EmFac = CWD_year50_Scattered_NOx_EmFac, Piled_NOx_EmFac = Piled_year50_NOx_EmFac,
                        Duff_Scattered_PM10_EmFac = Duff_year50_Scattered_PM10_EmFac, Foliage_Scattered_PM10_EmFac = Foliage_year50_Scattered_PM10_EmFac, FWD_Scattered_PM10_EmFac = FWD_year50_Scattered_PM10_EmFac, CWD_Scattered_PM10_EmFac = CWD_year50_Scattered_PM10_EmFac, Piled_PM10_EmFac = Piled_year50_PM10_EmFac,
                        Duff_Scattered_PM2.5_EmFac = Duff_year50_Scattered_PM2.5_EmFac, Foliage_Scattered_PM2.5_EmFac = Foliage_year50_Scattered_PM2.5_EmFac, FWD_year50_Scattered_PM2.5_EmFac = FWD_year50_Scattered_PM2.5_EmFac, CWD_Scattered_PM2.5_EmFac = CWD_year50_Scattered_PM2.5_EmFac, Piled_PM2.5_EmFac = Piled_year50_PM2.5_EmFac,
                        Duff_Scattered_SO2_EmFac = Duff_year50_Scattered_SO2_EmFac, Foliage_Scattered_SO2_EmFac = Foliage_year50_Scattered_SO2_EmFac, FWD_Scattered_SO2_EmFac = FWD_year50_Scattered_SO2_EmFac, CWD_Scattered_SO2_EmFac = CWD_year50_Scattered_SO2_EmFac, Piled_SO2_EmFac = Piled_year50_SO2_EmFac,
                        Duff_Scattered_VOC_EmFac = Duff_year50_Scattered_VOC_EmFac, Foliage_Scattered_VOC_EmFac = Foliage_year50_Scattered_VOC_EmFac, FWD_Scattered_VOC_EmFac = FWD_year50_Scattered_VOC_EmFac, CWD_Scattered_VOC_EmFac = CWD_year50_Scattered_VOC_EmFac, Piled_VOC_EmFac = Piled_year50_VOC_EmFac)]
  }
  if(year.i>50&year.i<75){
    wildfire.data[,':='(CWD_Scattered_CombustionFrac = (CWD_year75_Scattered_CombustionFrac - CWD_year50_Scattered_CombustionFrac)/25 * (year.i-50) + CWD_year50_Scattered_CombustionFrac,
                        FWD_Scattered_CombustionFrac = (FWD_year75_Scattered_CombustionFrac - FWD_year50_Scattered_CombustionFrac)/25 * (year.i-50) + FWD_year50_Scattered_CombustionFrac, 
                        Foliage_Scattered_CombustionFrac = (Foliage_year75_Scattered_CombustionFrac - Foliage_year50_Scattered_CombustionFrac)/25 * (year.i-50) + Foliage_year50_Scattered_CombustionFrac, 
                        Duff_Scattered_CombustionFrac = (Duff_year75_Scattered_CombustionFrac - Duff_year50_Scattered_CombustionFrac)/25 * (year.i-50) + Duff_year50_Scattered_CombustionFrac,
                        CWD_Scattered_CharFrac = (CWD_year75_Scattered_CharFrac - CWD_year50_Scattered_CharFrac)/25 * (year.i-50) + CWD_year50_Scattered_CharFrac, 
                        FWD_Scattered_CharFrac = (FWD_year75_Scattered_CharFrac - FWD_year50_Scattered_CharFrac)/25 * (year.i-50) + FWD_year50_Scattered_CharFrac,
                        
                        Duff_Scattered_CH4_EmFac = (Duff_year75_Scattered_CH4_EmFac - Duff_year50_Scattered_CH4_EmFac)/25 * (year.i-50) + Duff_year50_Scattered_CH4_EmFac, 
                        Foliage_Scattered_CH4_EmFac = (Foliage_year75_Scattered_CH4_EmFac - Foliage_year50_Scattered_CH4_EmFac)/25 * (year.i-50) + Foliage_year50_Scattered_CH4_EmFac, 
                        FWD_Scattered_CH4_EmFac = (FWD_year75_Scattered_CH4_EmFac - FWD_year50_Scattered_CH4_EmFac)/25 * (year.i-50) + FWD_year50_Scattered_CH4_EmFac, 
                        CWD_Scattered_CH4_EmFac = (CWD_year75_Scattered_CH4_EmFac - CWD_year50_Scattered_CH4_EmFac)/25 * (year.i-50) + CWD_year50_Scattered_CH4_EmFac,
                        Piled_CH4_EmFac = (Piled_year75_CH4_EmFac - Piled_year50_CH4_EmFac)/25 * (year.i-50) + Piled_year50_CH4_EmFac,
                        
                        Duff_Scattered_CO_EmFac = (Duff_year75_Scattered_CO_EmFac - Duff_year50_Scattered_CO_EmFac)/25 * (year.i-50) + Duff_year50_Scattered_CO_EmFac, 
                        Foliage_Scattered_CO_EmFac = (Foliage_year75_Scattered_CO_EmFac - Foliage_year50_Scattered_CO_EmFac)/25 * (year.i-50) + Foliage_year50_Scattered_CO_EmFac, 
                        FWD_Scattered_CO_EmFac = (FWD_year75_Scattered_CO_EmFac - FWD_year50_Scattered_CO_EmFac)/25 * (year.i-50) + FWD_year50_Scattered_CO_EmFac, 
                        CWD_Scattered_CO_EmFac = (CWD_year75_Scattered_CO_EmFac - CWD_year50_Scattered_CO_EmFac)/25 * (year.i-50) + CWD_year50_Scattered_CO_EmFac,
                        Piled_CO_EmFac = (Piled_year75_CO_EmFac - Piled_year50_CO_EmFac)/25 * (year.i-50) + Piled_year50_CO_EmFac,
                        
                        Duff_Scattered_NOx_EmFac = (Duff_year75_Scattered_NOx_EmFac - Duff_year50_Scattered_NOx_EmFac)/25 * (year.i-50) + Duff_year50_Scattered_NOx_EmFac, 
                        Foliage_Scattered_NOx_EmFac = (Foliage_year75_Scattered_NOx_EmFac - Foliage_year50_Scattered_NOx_EmFac)/25 * (year.i-50) + Foliage_year50_Scattered_NOx_EmFac, 
                        FWD_Scattered_NOx_EmFac = (FWD_year75_Scattered_NOx_EmFac - FWD_year50_Scattered_NOx_EmFac)/25 * (year.i-50) + FWD_year50_Scattered_NOx_EmFac, 
                        CWD_Scattered_NOx_EmFac = (CWD_year75_Scattered_NOx_EmFac - CWD_year50_Scattered_NOx_EmFac)/25 * (year.i-50) + CWD_year50_Scattered_NOx_EmFac,
                        Piled_NOx_EmFac = (Piled_year75_NOx_EmFac - Piled_year50_NOx_EmFac)/25 * (year.i-50) + Piled_year50_NOx_EmFac,
                        
                        Duff_Scattered_PM10_EmFac = (Duff_year75_Scattered_PM10_EmFac - Duff_year50_Scattered_PM10_EmFac)/25 * (year.i-50) + Duff_year50_Scattered_PM10_EmFac, 
                        Foliage_Scattered_PM10_EmFac = (Foliage_year75_Scattered_PM10_EmFac - Foliage_year50_Scattered_PM10_EmFac)/25 * (year.i-50) + Foliage_year50_Scattered_PM10_EmFac, 
                        FWD_Scattered_PM10_EmFac = (FWD_year75_Scattered_PM10_EmFac - FWD_year50_Scattered_PM10_EmFac)/25 * (year.i-50) + FWD_year50_Scattered_PM10_EmFac, 
                        CWD_Scattered_PM10_EmFac = (CWD_year75_Scattered_PM10_EmFac - CWD_year50_Scattered_PM10_EmFac)/25 * (year.i-50) + CWD_year50_Scattered_PM10_EmFac,
                        Piled_PM10_EmFac = (Piled_year75_PM10_EmFac - Piled_year50_PM10_EmFac)/25 * (year.i-50) + Piled_year50_PM10_EmFac,
                        
                        Duff_Scattered_PM2.5_EmFac = (Duff_year75_Scattered_PM2.5_EmFac - Duff_year50_Scattered_PM2.5_EmFac)/25 * (year.i-50) + Duff_year50_Scattered_PM2.5_EmFac, 
                        Foliage_Scattered_PM2.5_EmFac = (Foliage_year75_Scattered_PM2.5_EmFac - Foliage_year50_Scattered_PM2.5_EmFac)/25 * (year.i-50) + Foliage_year50_Scattered_PM2.5_EmFac, 
                        FWD_Scattered_PM2.5_EmFac = (FWD_year75_Scattered_PM2.5_EmFac - FWD_year50_Scattered_PM2.5_EmFac)/25 * (year.i-50) + FWD_year50_Scattered_PM2.5_EmFac, 
                        CWD_Scattered_PM2.5_EmFac = (CWD_year75_Scattered_PM2.5_EmFac - CWD_year50_Scattered_PM2.5_EmFac)/25 * (year.i-50) + CWD_year50_Scattered_PM2.5_EmFac,
                        Piled_PM2.5_EmFac = (Piled_year75_PM2.5_EmFac - Piled_year50_PM2.5_EmFac)/25 * (year.i-50) + Piled_year50_PM2.5_EmFac,
                        
                        Duff_Scattered_SO2_EmFac = (Duff_year75_Scattered_SO2_EmFac - Duff_year50_Scattered_SO2_EmFac)/25 * (year.i-50) + Duff_year50_Scattered_SO2_EmFac, 
                        Foliage_Scattered_SO2_EmFac = (Foliage_year75_Scattered_SO2_EmFac - Foliage_year50_Scattered_SO2_EmFac)/25 * (year.i-50) + Foliage_year50_Scattered_SO2_EmFac, 
                        FWD_Scattered_SO2_EmFac = (FWD_year75_Scattered_SO2_EmFac - FWD_year50_Scattered_SO2_EmFac)/25 * (year.i-50) + FWD_year50_Scattered_SO2_EmFac, 
                        CWD_Scattered_SO2_EmFac = (CWD_year75_Scattered_SO2_EmFac - CWD_year50_Scattered_SO2_EmFac)/25 * (year.i-50) + CWD_year50_Scattered_SO2_EmFac,
                        Piled_SO2_EmFac = (Piled_year75_SO2_EmFac - Piled_year50_SO2_EmFac)/25 * (year.i-50) + Piled_year50_SO2_EmFac,
                        
                        Duff_Scattered_VOC_EmFac = (Duff_year75_Scattered_VOC_EmFac - Duff_year50_Scattered_VOC_EmFac)/25 * (year.i-50) + Duff_year50_Scattered_VOC_EmFac, 
                        Foliage_Scattered_VOC_EmFac = (Foliage_year75_Scattered_VOC_EmFac - Foliage_year50_Scattered_VOC_EmFac)/25 * (year.i-50) + Foliage_year50_Scattered_VOC_EmFac, 
                        FWD_Scattered_VOC_EmFac = (FWD_year75_Scattered_VOC_EmFac - FWD_year50_Scattered_VOC_EmFac)/25 * (year.i-50) + FWD_year50_Scattered_VOC_EmFac, 
                        CWD_Scattered_VOC_EmFac = (CWD_year75_Scattered_VOC_EmFac - CWD_year50_Scattered_VOC_EmFac)/25 * (year.i-50) + CWD_year50_Scattered_VOC_EmFac,
                        Piled_VOC_EmFac = (Piled_year75_VOC_EmFac - Piled_year50_VOC_EmFac)/25 * (year.i-50) + Piled_year50_VOC_EmFac
                        )]
  }
  if(year.i==75) {
    wildfire.data[,':='(CWD_Scattered_CombustionFrac = CWD_year75_Scattered_CombustionFrac, FWD_Scattered_CombustionFrac = FWD_year75_Scattered_CombustionFrac, Foliage_Scattered_CombustionFrac = Foliage_year75_Scattered_CombustionFrac, Duff_Scattered_CombustionFrac = Duff_year75_Scattered_CombustionFrac,
                        CWD_Scattered_CharFrac = CWD_year75_Scattered_CharFrac, FWD_Scattered_CharFrac = FWD_year75_Scattered_CharFrac,
                        Duff_Scattered_CH4_EmFac = Duff_year75_Scattered_CH4_EmFac, Foliage_Scattered_CH4_EmFac = Foliage_year75_Scattered_CH4_EmFac, FWD_Scattered_CH4_EmFac = FWD_year75_Scattered_CH4_EmFac, CWD_Scattered_CH4_EmFac = CWD_year75_Scattered_CH4_EmFac, Piled_CH4_EmFac = Piled_year75_CH4_EmFac,
                        Duff_Scattered_CO_EmFac = Duff_year75_Scattered_CO_EmFac, Foliage_Scattered_CO_EmFac = Foliage_year75_Scattered_CO_EmFac, FWD_Scattered_CO_EmFac = FWD_year75_Scattered_CO_EmFac, CWD_Scattered_CO_EmFac = CWD_year75_Scattered_CO_EmFac, Piled_CO_EmFac = Piled_year75_CO_EmFac,
                        Duff_Scattered_NOx_EmFac = Duff_year75_Scattered_NOx_EmFac, Foliage_Scattered_NOx_EmFac = Foliage_year75_Scattered_NOx_EmFac, FWD_Scattered_NOx_EmFac = FWD_year75_Scattered_NOx_EmFac, CWD_Scattered_NOx_EmFac = CWD_year75_Scattered_NOx_EmFac, Piled_NOx_EmFac = Piled_year75_NOx_EmFac,
                        Duff_Scattered_PM10_EmFac = Duff_year75_Scattered_PM10_EmFac, Foliage_Scattered_PM10_EmFac = Foliage_year75_Scattered_PM10_EmFac, FWD_Scattered_PM10_EmFac = FWD_year75_Scattered_PM10_EmFac, CWD_Scattered_PM10_EmFac = CWD_year75_Scattered_PM10_EmFac, Piled_PM10_EmFac = Piled_year75_PM10_EmFac,
                        Duff_Scattered_PM2.5_EmFac = Duff_year75_Scattered_PM2.5_EmFac, Foliage_Scattered_PM2.5_EmFac = Foliage_year75_Scattered_PM2.5_EmFac, FWD_year75_Scattered_PM2.5_EmFac = FWD_year75_Scattered_PM2.5_EmFac, CWD_Scattered_PM2.5_EmFac = CWD_year75_Scattered_PM2.5_EmFac, Piled_PM2.5_EmFac = Piled_year75_PM2.5_EmFac,
                        Duff_Scattered_SO2_EmFac = Duff_year75_Scattered_SO2_EmFac, Foliage_Scattered_SO2_EmFac = Foliage_year75_Scattered_SO2_EmFac, FWD_Scattered_SO2_EmFac = FWD_year75_Scattered_SO2_EmFac, CWD_Scattered_SO2_EmFac = CWD_year75_Scattered_SO2_EmFac, Piled_SO2_EmFac = Piled_year75_SO2_EmFac,
                        Duff_Scattered_VOC_EmFac = Duff_year75_Scattered_VOC_EmFac, Foliage_Scattered_VOC_EmFac = Foliage_year75_Scattered_VOC_EmFac, FWD_Scattered_VOC_EmFac = FWD_year75_Scattered_VOC_EmFac, CWD_Scattered_VOC_EmFac = CWD_year75_Scattered_VOC_EmFac, Piled_VOC_EmFac = Piled_year75_VOC_EmFac)]
  }
  if(year.i>75&year.i<100){
    wildfire.data[,':='(CWD_Scattered_CombustionFrac = (CWD_year100_Scattered_CombustionFrac - CWD_year75_Scattered_CombustionFrac)/25 * (year.i-75) + CWD_year75_Scattered_CombustionFrac,
                        FWD_Scattered_CombustionFrac = (FWD_year100_Scattered_CombustionFrac - FWD_year75_Scattered_CombustionFrac)/25 * (year.i-75) + FWD_year75_Scattered_CombustionFrac, 
                        Foliage_Scattered_CombustionFrac = (Foliage_year100_Scattered_CombustionFrac - Foliage_year75_Scattered_CombustionFrac)/25 * (year.i-75) + Foliage_year75_Scattered_CombustionFrac, 
                        Duff_Scattered_CombustionFrac = (Duff_year100_Scattered_CombustionFrac - Duff_year75_Scattered_CombustionFrac)/25 * (year.i-75) + Duff_year75_Scattered_CombustionFrac,
                        CWD_Scattered_CharFrac = (CWD_year100_Scattered_CharFrac - CWD_year75_Scattered_CharFrac)/25 * (year.i-75) + CWD_year75_Scattered_CharFrac, 
                        FWD_Scattered_CharFrac = (FWD_year100_Scattered_CharFrac - FWD_year75_Scattered_CharFrac)/25 * (year.i-75) + FWD_year75_Scattered_CharFrac,
                        
                        Duff_Scattered_CH4_EmFac = (Duff_year100_Scattered_CH4_EmFac - Duff_year75_Scattered_CH4_EmFac)/25 * (year.i-75) + Duff_year75_Scattered_CH4_EmFac, 
                        Foliage_Scattered_CH4_EmFac = (Foliage_year100_Scattered_CH4_EmFac - Foliage_year75_Scattered_CH4_EmFac)/25 * (year.i-75) + Foliage_year75_Scattered_CH4_EmFac, 
                        FWD_Scattered_CH4_EmFac = (FWD_year100_Scattered_CH4_EmFac - FWD_year75_Scattered_CH4_EmFac)/25 * (year.i-75) + FWD_year75_Scattered_CH4_EmFac, 
                        CWD_Scattered_CH4_EmFac = (CWD_year100_Scattered_CH4_EmFac - CWD_year75_Scattered_CH4_EmFac)/25 * (year.i-75) + CWD_year75_Scattered_CH4_EmFac,
                        Piled_CH4_EmFac = (Piled_year100_CH4_EmFac - Piled_year75_CH4_EmFac)/25 * (year.i-75) + Piled_year75_CH4_EmFac,
                        
                        Duff_Scattered_CO_EmFac = (Duff_year100_Scattered_CO_EmFac - Duff_year75_Scattered_CO_EmFac)/25 * (year.i-75) + Duff_year75_Scattered_CO_EmFac, 
                        Foliage_Scattered_CO_EmFac = (Foliage_year100_Scattered_CO_EmFac - Foliage_year75_Scattered_CO_EmFac)/25 * (year.i-75) + Foliage_year75_Scattered_CO_EmFac, 
                        FWD_Scattered_CO_EmFac = (FWD_year100_Scattered_CO_EmFac - FWD_year75_Scattered_CO_EmFac)/25 * (year.i-75) + FWD_year75_Scattered_CO_EmFac, 
                        CWD_Scattered_CO_EmFac = (CWD_year100_Scattered_CO_EmFac - CWD_year75_Scattered_CO_EmFac)/25 * (year.i-75) + CWD_year75_Scattered_CO_EmFac,
                        Piled_CO_EmFac = (Piled_year100_CO_EmFac - Piled_year75_CO_EmFac)/25 * (year.i-75) + Piled_year75_CO_EmFac,
                        
                        Duff_Scattered_NOx_EmFac = (Duff_year100_Scattered_NOx_EmFac - Duff_year75_Scattered_NOx_EmFac)/25 * (year.i-75) + Duff_year75_Scattered_NOx_EmFac, 
                        Foliage_Scattered_NOx_EmFac = (Foliage_year100_Scattered_NOx_EmFac - Foliage_year75_Scattered_NOx_EmFac)/25 * (year.i-75) + Foliage_year75_Scattered_NOx_EmFac, 
                        FWD_Scattered_NOx_EmFac = (FWD_year100_Scattered_NOx_EmFac - FWD_year75_Scattered_NOx_EmFac)/25 * (year.i-75) + FWD_year75_Scattered_NOx_EmFac, 
                        CWD_Scattered_NOx_EmFac = (CWD_year100_Scattered_NOx_EmFac - CWD_year75_Scattered_NOx_EmFac)/25 * (year.i-75) + CWD_year75_Scattered_NOx_EmFac,
                        Piled_NOx_EmFac = (Piled_year100_NOx_EmFac - Piled_year75_NOx_EmFac)/25 * (year.i-75) + Piled_year75_NOx_EmFac,
                        
                        Duff_Scattered_PM10_EmFac = (Duff_year100_Scattered_PM10_EmFac - Duff_year75_Scattered_PM10_EmFac)/25 * (year.i-75) + Duff_year75_Scattered_PM10_EmFac, 
                        Foliage_Scattered_PM10_EmFac = (Foliage_year100_Scattered_PM10_EmFac - Foliage_year75_Scattered_PM10_EmFac)/25 * (year.i-75) + Foliage_year75_Scattered_PM10_EmFac, 
                        FWD_Scattered_PM10_EmFac = (FWD_year100_Scattered_PM10_EmFac - FWD_year75_Scattered_PM10_EmFac)/25 * (year.i-75) + FWD_year75_Scattered_PM10_EmFac, 
                        CWD_Scattered_PM10_EmFac = (CWD_year100_Scattered_PM10_EmFac - CWD_year75_Scattered_PM10_EmFac)/25 * (year.i-75) + CWD_year75_Scattered_PM10_EmFac,
                        Piled_PM10_EmFac = (Piled_year100_PM10_EmFac - Piled_year75_PM10_EmFac)/25 * (year.i-75) + Piled_year75_PM10_EmFac,
                        
                        Duff_Scattered_PM2.5_EmFac = (Duff_year100_Scattered_PM2.5_EmFac - Duff_year75_Scattered_PM2.5_EmFac)/25 * (year.i-75) + Duff_year75_Scattered_PM2.5_EmFac, 
                        Foliage_Scattered_PM2.5_EmFac = (Foliage_year100_Scattered_PM2.5_EmFac - Foliage_year75_Scattered_PM2.5_EmFac)/25 * (year.i-75) + Foliage_year75_Scattered_PM2.5_EmFac, 
                        FWD_Scattered_PM2.5_EmFac = (FWD_year100_Scattered_PM2.5_EmFac - FWD_year75_Scattered_PM2.5_EmFac)/25 * (year.i-75) + FWD_year75_Scattered_PM2.5_EmFac, 
                        CWD_Scattered_PM2.5_EmFac = (CWD_year100_Scattered_PM2.5_EmFac - CWD_year75_Scattered_PM2.5_EmFac)/25 * (year.i-75) + CWD_year75_Scattered_PM2.5_EmFac,
                        Piled_PM2.5_EmFac = (Piled_year100_PM2.5_EmFac - Piled_year75_PM2.5_EmFac)/25 * (year.i-75) + Piled_year75_PM2.5_EmFac,
                        
                        Duff_Scattered_SO2_EmFac = (Duff_year100_Scattered_SO2_EmFac - Duff_year75_Scattered_SO2_EmFac)/25 * (year.i-75) + Duff_year75_Scattered_SO2_EmFac, 
                        Foliage_Scattered_SO2_EmFac = (Foliage_year100_Scattered_SO2_EmFac - Foliage_year75_Scattered_SO2_EmFac)/25 * (year.i-75) + Foliage_year75_Scattered_SO2_EmFac, 
                        FWD_Scattered_SO2_EmFac = (FWD_year100_Scattered_SO2_EmFac - FWD_year75_Scattered_SO2_EmFac)/25 * (year.i-75) + FWD_year75_Scattered_SO2_EmFac, 
                        CWD_Scattered_SO2_EmFac = (CWD_year100_Scattered_SO2_EmFac - CWD_year75_Scattered_SO2_EmFac)/25 * (year.i-75) + CWD_year75_Scattered_SO2_EmFac,
                        Piled_SO2_EmFac = (Piled_year100_SO2_EmFac - Piled_year75_SO2_EmFac)/25 * (year.i-75) + Piled_year75_SO2_EmFac,
                        
                        Duff_Scattered_VOC_EmFac = (Duff_year100_Scattered_VOC_EmFac - Duff_year75_Scattered_VOC_EmFac)/25 * (year.i-75) + Duff_year75_Scattered_VOC_EmFac, 
                        Foliage_Scattered_VOC_EmFac = (Foliage_year100_Scattered_VOC_EmFac - Foliage_year75_Scattered_VOC_EmFac)/25 * (year.i-75) + Foliage_year75_Scattered_VOC_EmFac, 
                        FWD_Scattered_VOC_EmFac = (FWD_year100_Scattered_VOC_EmFac - FWD_year75_Scattered_VOC_EmFac)/25 * (year.i-75) + FWD_year75_Scattered_VOC_EmFac, 
                        CWD_Scattered_VOC_EmFac = (CWD_year100_Scattered_VOC_EmFac - CWD_year75_Scattered_VOC_EmFac)/25 * (year.i-75) + CWD_year75_Scattered_VOC_EmFac,
                        Piled_VOC_EmFac = (Piled_year100_VOC_EmFac - Piled_year75_VOC_EmFac)/25 * (year.i-75) + Piled_year75_VOC_EmFac)]
  }
  if(year.i==100) {
    wildfire.data[,':='(CWD_Scattered_CombustionFrac = CWD_year100_Scattered_CombustionFrac, FWD_Scattered_CombustionFrac = FWD_year100_Scattered_CombustionFrac, Foliage_Scattered_CombustionFrac = Foliage_year100_Scattered_CombustionFrac, Duff_Scattered_CombustionFrac = Duff_year100_Scattered_CombustionFrac,
                        CWD_Scattered_CharFrac = CWD_year100_Scattered_CharFrac, FWD_Scattered_CharFrac = FWD_year100_Scattered_CharFrac,
                        Duff_Scattered_CH4_EmFac = Duff_year100_Scattered_CH4_EmFac, Foliage_Scattered_CH4_EmFac = Foliage_year100_Scattered_CH4_EmFac, FWD_Scattered_CH4_EmFac = FWD_year100_Scattered_CH4_EmFac, CWD_Scattered_CH4_EmFac = CWD_year100_Scattered_CH4_EmFac, Piled_CH4_EmFac = Piled_year100_CH4_EmFac,
                        Duff_Scattered_CO_EmFac = Duff_year100_Scattered_CO_EmFac, Foliage_Scattered_CO_EmFac = Foliage_year100_Scattered_CO_EmFac, FWD_Scattered_CO_EmFac = FWD_year100_Scattered_CO_EmFac, CWD_Scattered_CO_EmFac = CWD_year100_Scattered_CO_EmFac, Piled_CO_EmFac = Piled_year100_CO_EmFac,
                        Duff_Scattered_NOx_EmFac = Duff_year100_Scattered_NOx_EmFac, Foliage_Scattered_NOx_EmFac = Foliage_year100_Scattered_NOx_EmFac, FWD_Scattered_NOx_EmFac = FWD_year100_Scattered_NOx_EmFac, CWD_Scattered_NOx_EmFac = CWD_year100_Scattered_NOx_EmFac, Piled_NOx_EmFac = Piled_year100_NOx_EmFac,
                        Duff_Scattered_PM10_EmFac = Duff_year100_Scattered_PM10_EmFac, Foliage_Scattered_PM10_EmFac = Foliage_year100_Scattered_PM10_EmFac, FWD_Scattered_PM10_EmFac = FWD_year100_Scattered_PM10_EmFac, CWD_Scattered_PM10_EmFac = CWD_year100_Scattered_PM10_EmFac, Piled_PM10_EmFac = Piled_year100_PM10_EmFac,
                        Duff_Scattered_PM2.5_EmFac = Duff_year100_Scattered_PM2.5_EmFac, Foliage_Scattered_PM2.5_EmFac = Foliage_year100_Scattered_PM2.5_EmFac, FWD_year100_Scattered_PM2.5_EmFac = FWD_year100_Scattered_PM2.5_EmFac, CWD_Scattered_PM2.5_EmFac = CWD_year100_Scattered_PM2.5_EmFac, Piled_PM2.5_EmFac = Piled_year100_PM2.5_EmFac,
                        Duff_Scattered_SO2_EmFac = Duff_year100_Scattered_SO2_EmFac, Foliage_Scattered_SO2_EmFac = Foliage_year100_Scattered_SO2_EmFac, FWD_Scattered_SO2_EmFac = FWD_year100_Scattered_SO2_EmFac, CWD_Scattered_SO2_EmFac = CWD_year100_Scattered_SO2_EmFac, Piled_SO2_EmFac = Piled_year100_SO2_EmFac,
                        Duff_Scattered_VOC_EmFac = Duff_year100_Scattered_VOC_EmFac, Foliage_Scattered_VOC_EmFac = Foliage_year100_Scattered_VOC_EmFac, FWD_Scattered_VOC_EmFac = FWD_year100_Scattered_VOC_EmFac, CWD_Scattered_VOC_EmFac = CWD_year100_Scattered_VOC_EmFac, Piled_VOC_EmFac = Piled_year100_VOC_EmFac)]
  }

  # Trim just what we need from wildfire.data; we will merge this with cbrec.dt and export the updated cbrec.dt.
  merge.wildfire.data <- wildfire.data[,list(x, y, CWD_Scattered_CombustionFrac, FWD_Scattered_CombustionFrac, Foliage_Scattered_CombustionFrac, Duff_Scattered_CombustionFrac, CWD_Scattered_CharFrac, FWD_Scattered_CharFrac, Duff_Scattered_CH4_EmFac, Foliage_Scattered_CH4_EmFac, FWD_Scattered_CH4_EmFac, CWD_Scattered_CH4_EmFac, Piled_CH4_EmFac, Duff_Scattered_CO_EmFac, Foliage_Scattered_CO_EmFac, FWD_Scattered_CO_EmFac, CWD_Scattered_CO_EmFac, Piled_CO_EmFac, Duff_Scattered_NOx_EmFac, Foliage_Scattered_NOx_EmFac, FWD_Scattered_NOx_EmFac, CWD_Scattered_NOx_EmFac, Piled_NOx_EmFac, Duff_Scattered_PM10_EmFac, Foliage_Scattered_PM10_EmFac, FWD_Scattered_PM10_EmFac, CWD_Scattered_PM10_EmFac, Piled_PM10_EmFac, Duff_Scattered_PM2.5_EmFac, Foliage_Scattered_PM2.5_EmFac, FWD_Scattered_PM2.5_EmFac, CWD_Scattered_PM2.5_EmFac, Piled_PM2.5_EmFac, Duff_Scattered_SO2_EmFac, Foliage_Scattered_SO2_EmFac, FWD_Scattered_SO2_EmFac, CWD_Scattered_SO2_EmFac, Piled_SO2_EmFac, Duff_Scattered_VOC_EmFac, Foliage_Scattered_VOC_EmFac, FWD_Scattered_VOC_EmFac, CWD_Scattered_VOC_EmFac, Piled_VOC_EmFac)]
  setkey(cbrec.dt,x,y)
  setkey(merge.wildfire.data,x,y)
  cbrec.dt <- merge.wildfire.data[cbrec.dt, nomatch = 0]
  
  # There will be cells where we have UW residue data but not fire data. This is likely due to a discrepancy between UW data and FCCS data for fire emissions, where UW says there are trees and FCCS disagrees. For fire modeling, 
  # we side with FCCS. This is generally a smaller number of cells, for tile 118, it was half of one percent. We have elected to remove these wildfire-less points from cbrec.dt, essentially removing them from they study.
  # cbrec.dt <- cbrec.dt[!is.na(CWD_Scattered_CombustionFrac)]

  return(cbrec.dt)
}
  
# -------------------------------------------------------------------------
#
# FUNCTION: Wildfire Combustion Function For All Size Classes Except Duff
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This function will, for a given residue segment (scattered, field piled, landing piled) calculate
# the mass burned in a one-year period along all residue size classes (excluding duff, which will 
# be called after the other residue segments.)
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data
# residue.disposition: Residue disposition; scattered, field piled or landing piled
# year.i: The year for which we want to calculate the char and emissions from the year prior (i.e.,
#   if t=3, we would calculate the emissions between years 2-3.)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# =================================================================================================

annual_wildfire_fun <- function(cbrec.dt, residue.disposition, year.i) {
  
  # scale the wildfire probability base on the time interval CBREC is running on
  # year 1 is not scaled
  # COMMENT NO LONGER RELEVANT; ERASE IF CODE FIX FINALIZED # first interval year after year 1 is scaled as interval time less 1
  # t <- ifelse(year.i == 1, 1, time_int)
  # ARH 8/21/20 update: to fix foliage to duff error issue, first X years (up to the time_int) are run at a 1 year resolution. While the bug has nothing to do with wildfire, the fix affects wildfire equations.
  t <- ifelse(year.i <= time_int, 1, time_int)
  # if(year.i == time_int & time_int != 1){t <- time_int - 1} # No longer relevant in code fix
  
  wildfire_emissions_profile <- data.table(CO2_tonnes=0, CO_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SO2_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  
  # Though wildfire will in reality strike one year and burn a large portion of residue, because wildfires are probabilistic, we model it as a series of annual,
  # small wildfires - if there was no decay, wildfire would be exposed to (initial mass * wildfire probability). Because decay does happen, the actual fire 
  # exposed mass is (initial mass * wildfire probability) * (decay by year i). So, here's how we're going to tackle it:
  # 1) Calculate cumulative wildfire probability within applicable time step
  # 2) Calculate the fire exposed mass as (<residue.disposition>_XXX_tonnesAcre_INITIAL * Wildfire_Probability)
  # 3) Adjust the fire exposed mass to account for any decay between year 0 and year i
  # 4) If the corrected fire exposed mass is greater than the available mass, adjust the fire exposed mass to equal the available mass.
  
  # Calculate cumulative wildfire probability within the applicable time step
  #   If cbrec.dt only has one row of data, use sum(), else use raster::rowSums(). Not using matrixStats::rowSums since raster package is already loaded.
  year_range <- seq(2019 + year.i - t + 1, 2019 + year.i, 1) # Wildfire probability years run from 2020 - 2120
  if(nrow(cbrec.dt)==1) {
    cbrec.dt[,cumFireProb := sum(sapply(year_range, function(x) get(paste0("Fire_Probability_", x))))] # rowSums doesn't work if there is only one row of data
  } else {
    cbrec.dt[,cumFireProb := raster::rowSums(sapply(year_range, function(x) get(paste0("Fire_Probability_", x))))] # Sum probability across year range
  }
  
  # Calculate the uncorrected fire exposed mass (units: tonnes/acre)
  cbrec.dt[,fire_exposed_CWD_mass_year_i := get(paste(residue.disposition,"CWD_tonnesAcre_INITIAL",sep="_")) * cumFireProb]
  cbrec.dt[,fire_exposed_FWD_mass_year_i := get(paste(residue.disposition,"FWD_tonnesAcre_INITIAL",sep="_")) * cumFireProb]
  cbrec.dt[,fire_exposed_Foliage_mass_year_i := get(paste(residue.disposition,"Foliage_tonnesAcre_INITIAL",sep="_")) * cumFireProb]
  cbrec.dt[,cumFireProb:=NULL] # Remove this column as it is no longer needed.
  
  # Adjust for decay (units: tonnes/acre)
  cbrec.dt[,':='(fire_exposed_CWD_mass_year_i = decay_mass_remaining(fire_exposed_CWD_mass_year_i,CWD_cm,year.i), 
                 fire_exposed_FWD_mass_year_i = decay_mass_remaining(fire_exposed_FWD_mass_year_i,FWD_cm,year.i), 
                 fire_exposed_Foliage_mass_year_i = decay_mass_remaining(fire_exposed_Foliage_mass_year_i,Foliage_cm,year.i))]
  
  # Adjust the fire_exposed_XXX_mass_year_i to equal the dynamic mass, if necessary (units: tonnes/acre)
  cbrec.dt[fire_exposed_CWD_mass_year_i > get(paste(residue.disposition,"CWD_tonnesAcre",sep="_")), fire_exposed_CWD_mass_year_i := get(paste(residue.disposition,"CWD_tonnesAcre",sep="_"))]
  cbrec.dt[fire_exposed_FWD_mass_year_i > get(paste(residue.disposition,"FWD_tonnesAcre",sep="_")), fire_exposed_FWD_mass_year_i := get(paste(residue.disposition,"FWD_tonnesAcre",sep="_"))]
  cbrec.dt[fire_exposed_Foliage_mass_year_i > get(paste(residue.disposition,"Foliage_tonnesAcre",sep="_")), fire_exposed_Foliage_mass_year_i := get(paste(residue.disposition,"Foliage_tonnesAcre",sep="_"))]
  
  # Apply the mass lost to the dynamic mass columns.(units: tonnes/acre)
  cbrec.dt[,(paste(residue.disposition,"CWD_tonnesAcre",sep="_")) := get(paste(residue.disposition,"CWD_tonnesAcre",sep="_")) - fire_exposed_CWD_mass_year_i]
  cbrec.dt[,(paste(residue.disposition,"FWD_tonnesAcre",sep="_")) := get(paste(residue.disposition,"FWD_tonnesAcre",sep="_")) - fire_exposed_FWD_mass_year_i]
  cbrec.dt[,(paste(residue.disposition,"Foliage_tonnesAcre",sep="_")) := get(paste(residue.disposition,"Foliage_tonnesAcre",sep="_")) - fire_exposed_Foliage_mass_year_i]
  
  # Calculate the total carbon exposed to wildfire. (units: tonnes)
  total.wildfire.exposed.carbon <- cbrec.dt[,sum((fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i) * Carbon_frac)] * acres_per_cell
  
  # Piled has its own, constant combustion/char fractions.
  if(residue.disposition=="Piled") {
    # In addition to needing the mass of unburnt fuel (for prev_fired, which we will address later), we need to know the amount of unburned carbon to calculate the carbon emissions. (units: tonnes)
    total.unburned.carbon <- cbrec.dt[,sum((fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i) * Carbon_frac * (1 - pile.burn.combustion.frac - pile.burn.char.frac))] * acres_per_cell
    
    # Calculate year.i emissions (units: tonnes)
    CO.emissions.year.i <- cbrec.dt[,sum(Piled_CO_EmFac * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    CH4.emissions.year.i <- cbrec.dt[,sum(Piled_CH4_EmFac * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    NOx.emissions.year.i <- cbrec.dt[,sum(Piled_NOx_EmFac * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    PM10.emissions.year.i <- cbrec.dt[,sum(Piled_PM10_EmFac * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    PM2.5.emissions.year.i <- cbrec.dt[,sum(Piled_PM2.5_EmFac * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    SO2.emissions.year.i <- cbrec.dt[,sum(Piled_SO2_EmFac * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    VOC.emissions.year.i <- cbrec.dt[,sum(Piled_VOC_EmFac * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    char.year.i <- cbrec.dt[,sum(pile.burn.char.frac * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))] * acres_per_cell # no foliage char
    
    # The fraction of biomass exposed to wildfire will never be exposed again. Once exposed, biomass is either:
    # 1) Consumed by wildfire; the mass will be transferred to emissions 
    # 2) Charred; the mass will no longer decay. (Foliage and duff will never char)
    # 3) Left unburned, to decay normally. Previously exposed/unburned material will be decayed before this, and newly exposed/unburnt added below.
    wildfire_emissions_profile[,':='(CO_tonnes = CO_tonnes + CO.emissions.year.i,
                                     NOx_tonnes = NOx_tonnes + NOx.emissions.year.i,
                                     CH4_tonnes = CH4_tonnes + CH4.emissions.year.i,
                                     PMUnder10um_tonnes = PMUnder10um_tonnes + PM10.emissions.year.i,
                                     PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + PM2.5.emissions.year.i,
                                     SO2_tonnes = SO2_tonnes + SO2.emissions.year.i,
                                     VOC_tonnes = VOC_tonnes + VOC.emissions.year.i,
                                     char_tonnes = char_tonnes + char.year.i)]
    
    # Any carbon not emitted as CH4, CO, PM10 (which is inclusive of PM2.5), or VOC is emitted as CO2. So we need to determine how much carbon was emitted (the total carbon absent the
    # carbon present in unburnt fuel or char), determine how much the of the emitted carbon was in non-CO2 constituents, then assume the remainder was emitted as CO2 and convert to mass
    # CO2 emissions. Carbon fraction variables are species mass / carbon mass
    non.CO2.combusted.carbon <- CO.emissions.year.i / CO_carbon_fraction + CH4.emissions.year.i / CH4_carbon_fraction + PM10.emissions.year.i / PM10_carbon_fraction + VOC.emissions.year.i / VOC_carbon_fraction
    
    char.carbon <- char.year.i / char_carbon_fraction # All carbon fraction are in species mass / carbon mass
    
    # Allocate emissions to CO2
    wildfire_emissions_profile[,CO2_tonnes := CO2_tonnes + (total.wildfire.exposed.carbon - total.unburned.carbon - char.carbon - non.CO2.combusted.carbon) * CO2_carbon_fraction]
    
    # And address unburned residue
    cbrec.dt[,prev_fired_CWD_tonnesAcre := prev_fired_CWD_tonnesAcre + ((1 - pile.burn.combustion.frac - pile.burn.char.frac) * (fire_exposed_CWD_mass_year_i + fire_exposed_FWD_mass_year_i + fire_exposed_Foliage_mass_year_i))]
  } else {
    # In addition to needing the mass of unburnt fuel (for prev_fired, which we will address later), we need to know the amount of unburned carbon to calculate the carbon emissions.
    total.unburned.carbon <- cbrec.dt[,sum(fire_exposed_CWD_mass_year_i * Carbon_frac * (1 - CWD_Scattered_CombustionFrac - CWD_Scattered_CharFrac),
                                           fire_exposed_FWD_mass_year_i * Carbon_frac * (1 - FWD_Scattered_CombustionFrac - FWD_Scattered_CharFrac),
                                           fire_exposed_Foliage_mass_year_i * Carbon_frac * (1 - Foliage_Scattered_CombustionFrac))] * acres_per_cell
    
    # Calculate some year.i emissions; we'll use this several times
    CO.emissions.year.i <- cbrec.dt[,sum((CWD_Scattered_CO_EmFac * fire_exposed_CWD_mass_year_i) + (FWD_Scattered_CO_EmFac * fire_exposed_FWD_mass_year_i) + (Foliage_Scattered_CO_EmFac * fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    CH4.emissions.year.i <- cbrec.dt[,sum((CWD_Scattered_CH4_EmFac * fire_exposed_CWD_mass_year_i) + (FWD_Scattered_CH4_EmFac * fire_exposed_FWD_mass_year_i) + (Foliage_Scattered_CH4_EmFac * fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    NOx.emissions.year.i <- cbrec.dt[,sum((CWD_Scattered_NOx_EmFac * fire_exposed_CWD_mass_year_i) + (FWD_Scattered_NOx_EmFac * fire_exposed_FWD_mass_year_i) + (Foliage_Scattered_NOx_EmFac * fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    PM10.emissions.year.i <- cbrec.dt[,sum((CWD_Scattered_PM10_EmFac * fire_exposed_CWD_mass_year_i) + (FWD_Scattered_PM10_EmFac * fire_exposed_FWD_mass_year_i) + (Foliage_Scattered_PM10_EmFac * fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    PM2.5.emissions.year.i <- cbrec.dt[,sum((CWD_Scattered_PM2.5_EmFac * fire_exposed_CWD_mass_year_i) + (FWD_Scattered_PM2.5_EmFac * fire_exposed_FWD_mass_year_i) + (Foliage_Scattered_PM2.5_EmFac * fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    SO2.emissions.year.i <- cbrec.dt[,sum((CWD_Scattered_SO2_EmFac * fire_exposed_CWD_mass_year_i) + (FWD_Scattered_SO2_EmFac * fire_exposed_FWD_mass_year_i) + (Foliage_Scattered_SO2_EmFac * fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    VOC.emissions.year.i <- cbrec.dt[,sum((CWD_Scattered_VOC_EmFac * fire_exposed_CWD_mass_year_i) + (FWD_Scattered_VOC_EmFac * fire_exposed_FWD_mass_year_i) + (Foliage_Scattered_VOC_EmFac * fire_exposed_Foliage_mass_year_i))] * acres_per_cell
    char.year.i <- cbrec.dt[,sum((CWD_Scattered_CharFrac * fire_exposed_CWD_mass_year_i) + (FWD_Scattered_CharFrac * fire_exposed_FWD_mass_year_i))] * acres_per_cell # no foliage char
    
    # The fraction of biomass exposed to wildfire will never be exposed again. Once exposed, biomass is either:
    # 1) Consumed by wildfire; the mass will be transferred to emissions 
    # 2) Charred; the mass will no longer decay. (Foliage and duff will never char)
    # 3) Left unburned, to decay normally. Previously exposed/unburned material will be decayed before this, and newly exposed/unburnt added below.
    wildfire_emissions_profile[,':='(CO_tonnes = CO_tonnes + CO.emissions.year.i,
                                     NOx_tonnes = NOx_tonnes + NOx.emissions.year.i,
                                     CH4_tonnes = CH4_tonnes + CH4.emissions.year.i,
                                     PMUnder10um_tonnes = PMUnder10um_tonnes + PM10.emissions.year.i,
                                     PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + PM2.5.emissions.year.i,
                                     SO2_tonnes = SO2_tonnes + SO2.emissions.year.i,
                                     VOC_tonnes = VOC_tonnes + VOC.emissions.year.i,
                                     char_tonnes = char_tonnes + char.year.i)]
    
    # Any carbon not emitted as CH4, CO, PM10 (which is inclusive of PM2.5), or VOC is emitted as CO2. So we need to determine how much carbon was emitted (the total carbon absent the
    # carbon present in unburnt fuel or char), determine how much the of the emitted carbon was in non-CO2 constituents, then assume the remainder was emitted as CO2 and convert to mass
    # CO2 emissions. Carbon fraction variables are species mass / carbon mass
    non.CO2.combusted.carbon <- CO.emissions.year.i / CO_carbon_fraction + CH4.emissions.year.i / CH4_carbon_fraction + PM10.emissions.year.i / PM10_carbon_fraction + VOC.emissions.year.i / VOC_carbon_fraction
    
    char.carbon <- char.year.i / char_carbon_fraction # All carbon fraction are in species mass / carbon mass
    
    # Allocate emissions to CO2
    wildfire_emissions_profile[,CO2_tonnes := CO2_tonnes + (total.wildfire.exposed.carbon - total.unburned.carbon - char.carbon - non.CO2.combusted.carbon) * CO2_carbon_fraction]
    
    # And address unburned residue
    cbrec.dt[,':='(prev_fired_CWD_tonnesAcre = prev_fired_CWD_tonnesAcre + ((1-CWD_Scattered_CombustionFrac-CWD_Scattered_CharFrac) * fire_exposed_CWD_mass_year_i),
                   prev_fired_FWD_tonnesAcre = prev_fired_FWD_tonnesAcre + ((1-FWD_Scattered_CombustionFrac-FWD_Scattered_CharFrac) * fire_exposed_FWD_mass_year_i),
                   prev_fired_Foliage_tonnesAcre = prev_fired_Foliage_tonnesAcre + ((1-Foliage_Scattered_CombustionFrac) * fire_exposed_Foliage_mass_year_i)
    )]
  }
 
  return(list(wildfire_emissions_profile,cbrec.dt))
}


# -------------------------------------------------------------------------
#
# FUNCTION: Wildfire Combustion Function for Duff
#
# -------------------------------------------------------------------------
# OBJECTIVE:
# This function calculates the mass of duff burned in a wildfire in a one-year period.
# -------------------------------------------------------------------------------------------------
# INPUTS:
# cbrec.dt: the main study area data table from the calling script, which contains residue data
# residue.disposition: Residue disposition; scattered, field piled or landing piled
# year.i: The year for which we want to calculate the char and emissions from the year prior (i.e.,
#   if t=3, we would calculate the emissions between years 2-3.)
# -------------------------------------------------------------------------------------------------
# OUTPUTS:
# An updated cbrec.dt, with dynamic mass and emissions data reflecting calculated changes.
# =================================================================================================
duff_annual_wildfire_fun <- function(cbrec.dt, year.i) {
  
  # scale the wildfire probability base on the time interval CBREC is running on
  # year 1 is not scaled
  # COMMENT NO LONGER RELEVANT; ERASE IF CODE FIX FINALIZED # first interval year after year 1 is scaled as interval time less 1
  # prob_scale <- ifelse(year.i == 1, 1, time_int)
  # ARH 8/21/20 update: to fix foliage to duff error issue, first X years (up to the time_int) are run at a 1 year resolution. While the bug has nothing to do with wildfire, the fix affects wildfire equations.
  prob_scale <- ifelse(year.i <= time_int, 1, time_int)
  # if(year.i == time_int & time_int!=1){prob_scale <- time_int - 1} # No longer relevant in code fix
  
  wildfire_emissions_profile <- data.table(CO2_tonnes=0, CO_tonnes=0, CH4_tonnes=0, NOx_tonnes=0, PMUnder10um_tonnes=0, PMUnder2.5um_tonnes=0, SO2_tonnes=0, VOC_tonnes=0, char_tonnes=0)
  
  # Duff will burn in the same manner as the other size classes, but we calculate it in a separate function so it does
  # not get triple-counted with the other residue segments, and I can keep the functions general.
  cbrec.dt[,fire_exposed_Duff_mass_year_i := Duff_tonnesAcre * cbrec.dt[, get(paste0("Fire_Probability_", 2019 + year.i)) ] * prob_scale]
  
  # Apply the mass lost to the dynamic mass column. 
  cbrec.dt[,Duff_tonnesAcre := Duff_tonnesAcre - fire_exposed_Duff_mass_year_i]
  
  # Calculate the total carbon exposed to wildfire. "fire_exposed_Duff_mass_year_i" is in tonnes/Acre, so be sure to convert from cells to acres
  total.wildfire.exposed.carbon <- cbrec.dt[,sum(fire_exposed_Duff_mass_year_i * Carbon_frac)] * acres_per_cell
  
  # In addition to needing the mass of unburnt fuel (for prev_fired, which we will address layer), we need to know the amount of unburned carbon to calculate the carbon emissions.
  total.unburned.carbon <- cbrec.dt[,sum(fire_exposed_Duff_mass_year_i * Carbon_frac * (1 - Duff_Scattered_CombustionFrac))] * acres_per_cell
  
  # Calculate some year.i emissions; we'll use this several times
  CO.emissions.year.i <- cbrec.dt[,sum(Duff_Scattered_CO_EmFac * fire_exposed_Duff_mass_year_i)] * acres_per_cell
  CH4.emissions.year.i <- cbrec.dt[,sum(Duff_Scattered_CH4_EmFac * fire_exposed_Duff_mass_year_i)] * acres_per_cell
  NOx.emissions.year.i <- cbrec.dt[,sum(Duff_Scattered_NOx_EmFac * fire_exposed_Duff_mass_year_i)] * acres_per_cell
  PM10.emissions.year.i <- cbrec.dt[,sum(Duff_Scattered_PM10_EmFac * fire_exposed_Duff_mass_year_i)] * acres_per_cell
  PM2.5.emissions.year.i <- cbrec.dt[,sum(Duff_Scattered_PM2.5_EmFac * fire_exposed_Duff_mass_year_i)] * acres_per_cell
  SO2.emissions.year.i <- cbrec.dt[,sum(Duff_Scattered_SO2_EmFac * fire_exposed_Duff_mass_year_i)] * acres_per_cell
  VOC.emissions.year.i <- cbrec.dt[,sum(Duff_Scattered_VOC_EmFac * fire_exposed_Duff_mass_year_i)] * acres_per_cell
  
  # Apply the mass from the fire exposed to emissions and unburned. Duff never chars.
  wildfire_emissions_profile[,':='(CO_tonnes = CO_tonnes + CO.emissions.year.i,
                                   NOx_tonnes = NOx_tonnes + NOx.emissions.year.i,
                                   CH4_tonnes = CH4_tonnes + CH4.emissions.year.i,
                                   PMUnder10um_tonnes = PMUnder10um_tonnes + PM10.emissions.year.i,
                                   PMUnder2.5um_tonnes = PMUnder2.5um_tonnes + PM2.5.emissions.year.i,
                                   SO2_tonnes = SO2_tonnes + SO2.emissions.year.i,
                                   VOC_tonnes = VOC_tonnes + VOC.emissions.year.i)]
  
  # Any carbon not emitted as CH4, CO, PM10 (which is inclusive of PM2.5), or VOC is emitted as CO2. So we need to determine how much carbon was emitted (the total carbon absent the
  # carbon present in unburnt fuel), determine how much the of the emitted carbon was in non-CO2 constituents, then assume the remainder was emitted as CO2 and convert to mass
  # CO2 emissions. Carbon fraction variables are species mass / carbon mass
  non.CO2.combusted.carbon <- CO.emissions.year.i / CO_carbon_fraction + CH4.emissions.year.i / CH4_carbon_fraction + PM10.emissions.year.i / PM10_carbon_fraction + VOC.emissions.year.i / VOC_carbon_fraction
  
  # Allocate emissions to CO2
  wildfire_emissions_profile[,CO2_tonnes := CO2_tonnes + (total.wildfire.exposed.carbon - total.unburned.carbon - non.CO2.combusted.carbon) * CO2_carbon_fraction]
  
  # And the unburned
  cbrec.dt[,prev_fired_Duff_tonnesAcre := prev_fired_Duff_tonnesAcre + (1-Duff_Scattered_CombustionFrac) * fire_exposed_Duff_mass_year_i]
  
  return(list(wildfire_emissions_profile,cbrec.dt))
}
