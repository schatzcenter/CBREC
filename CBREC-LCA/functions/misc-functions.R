# ==================================================================================
# This is a collection of additional miscellaneous functions that are too short
#   to put into their own separate file
# ==================================================================================

#####################################################################################
# %notin%: converse of "%in%" function, identifies when one item is not in a target
# object. Use: "a" %notin% c("b","c","d") will yieled TRUE.
#####################################################################################
"%notin%" <- function(x, table) match(x, table, nomatch = 0) == 0

#####################################################################################
# get_project_polygons: loads shapefiles defining the set of one or more projects to
# be modeled.
#####################################################################################
get_project_polygons <- function(user_inputs_table,fire_tile_shapefile) {
  
  if(debug_CBREC) {
    cat("Running get_project_polygons() function\n", file=log_file, append=T)
  }
  
  # Load project polygons
  all_polygons <- read_sf(user_inputs_table[variable == 'project_polygons_filepath', value])
  
  # check for an OBJECTID attribute
  if(length(grep("OBJECTID", names(all_polygons))) == 0) {
    cat("Project polygon shapefile missing OBJECTID attribute. Renaming id attribute to OBJECTID.", file=log_file, append=T)
    
    # find location of id like attribute
    name_location <- grep("id", names(all_polygons), ignore.case = T)
    
    # change to OBJECTID
    names(all_polygons)[name_location] <- "OBJECTID"
  } 
  
  # make sure projections align between fire data tiles and project polygons
  if(!identical(crs(all_polygons), crs(fire_tile_shapefile))) {
    print("transforming project polygons")
    all_polygons <- st_transform(all_polygons, crs = crs(fire_tile_shapefile))
  }
  
  # split into list; takes a while with large polygons, roughly 4m for 200acre proj_polygon
  all_polygons <- split(all_polygons, f = all_polygons[['OBJECTID']])
  
  return(all_polygons)
}

#####################################################################################
# Load initial input data
#   Does not return anything. Instead sets all variables as global.
#####################################################################################
load_initial_data <- function(user_inputs_path,tiles_without_data) {
  
  if(debug_CBREC) {
    cat("Running load_initial_data() function\n", file=log_file, append=T)
  }
  
  # Load all user input variables and values
  user_inputs <<- fread(user_inputs_path, stringsAsFactors = FALSE)
  initial_user_inputs <<- copy(user_inputs) # Store initial user inputs for safe keeping
  
  # Create list of all cases
  case_list <<- fread(user_inputs[variable == 'case_list_filepath', value])
  case_list <<- case_list[Silvicultural_Treatment != "No_Action"] # Remove "No Action" option for now as this is not dealt with by C-BREC
  setnames(case_list,
           c("ID",
             "treatment", 
             "frac.piled",
             "frac.scattered",
             "prescribed.burn.type", 
             "biomass.collection",
             "pulp.market"))
  
  # Load Emission factors/rates data from csv files
  equipment_emissions <<- fread(user_inputs[variable=='equipment_emissions_filepath',value]) # Units are kg per MT-km
  power_emission_factors <<- fread(user_inputs[variable=='power_plant_emissions_filepath',value])
  names(power_emission_factors) <<- gsub("/", ".", names(power_emission_factors)) # replace slashes with dot to avoid naming issues
  
  # Load tiles used to organize fire data
  fire_tile_full <<- read_sf(user_inputs[variable == 'fire_tiles_filepath', value])
  fire_tile_full <<- subset(fire_tile_full, fire_tile_full$ID %notin% tiles_without_data)
  
  # REMOVE AFTER CONFIRMATION THAT THIS IS NOT NEEDED HERE <<<<<<<<<<<<<<<<<<<<<<<
  # Get list of tile IDs and remove no data tiles
  # tile_list <<- subset(fire_tile_full$ID, fire_tile_full$ID %notin% tiles_without_data)
  # This is needed ^
  # The ~12,000 tiles used in the fire model contains four tiles that produced no results
  # these are the `tiles_without_data`
  # CBREC will hit an error if it tries to pull data from these tiles and therefore need to be removed
  # Creating a new shapefile without these tiles is risky since the fire model orders everything based on tile ID
  
  # Load study area raster. This is the base data that defines the available residues
  Study_Area_Raster <<- raster(user_inputs[variable=='fcid_code_filepath',value])
  
  # Read in the residue load values from a csv file. These values are indexed by FCID_code which relates to the study_area_raster
  # Note that the file loaded here post-processed raw residue to combine stem and bark into a single stem column
  fcid_table <<- fread(user_inputs[variable == 'residue_treatment_filepath', value])
  
  # Add heating value and carbon fraction.
  biomass_properties <<- fread(user_inputs[variable == 'biomass_properties_filepath', value])
  
  # Load project polygons shapefile
  all_project_polygons <<- get_project_polygons(user_inputs,fire_tile_full)
  
  # Read in power plant location ID names
  location_names <<- fread(user_inputs[variable=="power_plant_location_code_filepath",value], 
                           drop = c('Status', 'MW_Nmpl', 'Prjct_T', 'City', 'County'),
                           col.names = c("Nearest_Plant_ID", "plant_location"))
}

#####################################################################################
# Function to crop spatial data to polygon of interest
# inputs:
#   polygon of interest
#   tile number 
#   FCID raster
#####################################################################################
crop_to_polygon <- function(proj_polygon, tile_num, study_area, filepath) {
  
  if(debug_CBREC) {
    cat("Running crop_to_polygon() function\n", file=log_file, append=T)
  }
  
  # create rasters for each polygon region
  study_area_crop <- crop(study_area, extent(proj_polygon))
  study_area_masked <- mask(study_area_crop, proj_polygon)
  
  # convert to data table
  study_area_FCID <- as.data.table(study_area_masked, xy = T, na.rm = T)
  
  # Set names for clarity
  names(study_area_FCID) <- c('x','y','FCID_code')
  
  # load spatial data table
  data_dir <- lapply(tile_num, function(tile) {
    dir(filepath, pattern = paste0("\\b", tile, "\\b"), full.names = T)
  })
  
  # load data
  sp_data <- lapply(data_dir, readRDS) 
  
  # bind together
  sp_data <- rbindlist(sp_data)
  
  # key and join
  setkey(study_area_FCID, x, y)
  setkey(sp_data, x, y)
  
  study_area_FCID <- merge(study_area_FCID, sp_data)
  
  # remove slopes above 80%
  study_area_FCID <- study_area_FCID[Slope < residue_slope_cutoff]
  
  # convert distance to nearest plant from miles to kilometers
  study_area_FCID[, Nearest_Plant := Nearest_Plant / miles_per_kilometer]
  
  # convert cell to road from meters to kilometers
  study_area_FCID[, Cell_To_Road := Cell_To_Road / 1000]
  
  # change cell slope name to match CBREC code
  setnames(study_area_FCID, old = "Slope", new = "cell_slope")
  
  return(study_area_FCID)
}

#####################################################################################
# Function to load case definitions, and aggregate residue into C-BREC size classes
#####################################################################################
initialize_case_table <- function(case_to_initialize,table_of_fcid) {
  
  if(debug_CBREC) {
    cat("Running initialize_case_table() function\n", file=log_file, append=T)
  }
  
  # Copy the original study_area_FCID data into a data structure that will dynamically change.
  case_datatable <- copy(table_of_fcid)
  
  # Combine case.data with the FCID-specific residue loading, specific to scenario treatment.
  #   Note fcid_table is a global variable
  setkey(fcid_table,FCID2018)
  setkey(case_datatable,FCID_code)
  case_datatable <- fcid_table[treat.name == case_to_initialize$treatment][case_datatable]
  
  # Load relevant case definitions and merge with case.data
  case_definitions <- fread(user_inputs[variable=='case_definitions_filepath',value])
  case_definitions <- subset(case_definitions, ID == case_to_initialize$ID)
  case_definitions[, c("ID",
                       "Silvicultural Treatment",
                       'Fraction_Piled_Residues',
                       'Fraction_Scattered_Residues',
                       'Burn Type',
                       'Biomass Collection',
                       'Pulp Market'):= NULL]
  case_datatable[, Slope := ifelse(cell_slope < 40, "LT40", "GT40")] # classify slope into two categories
  setkey(case_datatable, Slope)
  setkey(case_definitions, Slope)
  case_datatable <- case_definitions[case_datatable] 
  case_definitions <- NULL 
  
  # Combine the technically available fractions with the gross resource base.
  case_datatable[,':='(Recovered_Stem9Plus_tonsAcre = Recovered_Stem9Plus * Stem9Plus_tonsAcre,
                       Recovered_Stem6to9_tonsAcre = Recovered_Stem6to9 * Stem6to9_tonsAcre,
                       Recovered_Stem4to6_tonsAcre = Recovered_Stem4to6 * Stem4to6_tonsAcre,
                       Recovered_Branch_tonsAcre = Recovered_Branch * Branch_tonsAcre,
                       Recovered_Foliage_tonsAcre = Recovered_Foliage * Foliage_tonsAcre,
                       Piled_Stem9Plus_tonsAcre = Piled_Stem9Plus * Stem9Plus_tonsAcre,
                       Piled_Stem6to9_tonsAcre = Piled_Stem6to9 * Stem6to9_tonsAcre,
                       Piled_Stem4to6_tonsAcre = Piled_Stem4to6 * Stem4to6_tonsAcre,
                       Piled_Branch_tonsAcre = Piled_Branch * Branch_tonsAcre,
                       Piled_Foliage_tonsAcre = Piled_Foliage * Foliage_tonsAcre,
                       Scattered_Stem9Plus_tonsAcre = Scattered_Stem9Plus * Stem9Plus_tonsAcre,
                       Scattered_Stem6to9_tonsAcre = Scattered_Stem6to9 * Stem6to9_tonsAcre,
                       Scattered_Stem4to6_tonsAcre = Scattered_Stem4to6 * Stem4to6_tonsAcre,
                       Scattered_Branch_tonsAcre = Scattered_Branch * Branch_tonsAcre,
                       Scattered_Foliage_tonsAcre = Scattered_Foliage * Foliage_tonsAcre
  )]
  
  # Combine relevant mass categories into CWD
  # There will be several categories with different decay rates - recovered, scattered (including merchantable breakage), field piled, and landing piled
  case_datatable[,':='(Recovered_CWD_tonsAcre = Recovered_Stem9Plus_tonsAcre + Recovered_Stem6to9_tonsAcre + Recovered_Stem4to6_tonsAcre,
                       Scattered_CWD_tonsAcre = Scattered_Stem9Plus_tonsAcre + Scattered_Stem6to9_tonsAcre + Scattered_Stem4to6_tonsAcre,
                       Piled_CWD_tonsAcre = Piled_Stem9Plus_tonsAcre + Piled_Stem6to9_tonsAcre + Piled_Stem4to6_tonsAcre
  )]
  
  # Trim out the columns we no longer need.
  #warning("This chunk of code will need to be re-written if we remove bark columns from the scenario matrix")
  case_datatable[,':='(Slope = NULL, Recovered_Stem9Plus = NULL, Stem9Plus_tonsAcre = NULL, Recovered_Stem6to9 = NULL, Stem6to9_tonsAcre = NULL, Recovered_Stem4to6 = NULL,
                       Stem4to6_tonsAcre = NULL, Recovered_Bark9Plus = NULL, Recovered_Bark6to9 = NULL, Recovered_Bark4to6 = NULL, Recovered_Branch = NULL, Branch_tonsAcre = NULL, 
                       Recovered_Foliage = NULL, Foliage_tonsAcre = NULL, Merchantable_Stem9Plus = NULL, Merchantable_Stem6to9 = NULL, Merchantable_Stem4to6 = NULL,  Merchantable_Bark9Plus = NULL,
                       Merchantable_Bark6to9 = NULL, Merchantable_Bark4to6 = NULL, Merchantable_Branch = NULL, Merchantable_Foliage = NULL, Piled_Stem9Plus = NULL, 
                       Piled_Stem6to9 = NULL, Piled_Stem4to6 = NULL, Piled_Bark9Plus = NULL, Piled_Bark6to9 = NULL, Piled_Bark4to6 = NULL, Piled_Branch = NULL, Piled_Foliage = NULL,
                       Scattered_Stem9Plus = NULL, Scattered_Stem6to9 = NULL, Scattered_Stem4to6 = NULL, Scattered_Bark9Plus = NULL, Scattered_Bark6to9 = NULL,
                       Scattered_Bark4to6 = NULL, Scattered_Branch = NULL, Scattered_Foliage = NULL, Recovered_Stem9Plus_tonsAcre = NULL, Recovered_Stem6to9_tonsAcre = NULL, Recovered_Stem4to6_tonsAcre = NULL,
                       Scattered_Stem9Plus_tonsAcre = NULL, Scattered_Stem6to9_tonsAcre = NULL, Scattered_Stem4to6_tonsAcre = NULL,
                       Piled_Stem9Plus_tonsAcre = NULL, Piled_Stem6to9_tonsAcre = NULL, Piled_Stem4to6_tonsAcre = NULL
  )]
  
  # Convert residue from imperial units to metric; 1 ton equals 0.907185 tonnes
  case_datatable[,':='(
    Recovered_Branch_tonsAcre = Recovered_Branch_tonsAcre * tonnes_per_ton,
    Recovered_Foliage_tonsAcre = Recovered_Foliage_tonsAcre * tonnes_per_ton, 
    Piled_Branch_tonsAcre = Piled_Branch_tonsAcre * tonnes_per_ton,
    Piled_Foliage_tonsAcre = Piled_Foliage_tonsAcre * tonnes_per_ton,
    Scattered_Branch_tonsAcre = Scattered_Branch_tonsAcre * tonnes_per_ton,
    Scattered_Foliage_tonsAcre = Scattered_Foliage_tonsAcre * tonnes_per_ton,
    Recovered_CWD_tonsAcre = Recovered_CWD_tonsAcre * tonnes_per_ton,
    Scattered_CWD_tonsAcre = Scattered_CWD_tonsAcre * tonnes_per_ton,
    Piled_CWD_tonsAcre = Piled_CWD_tonsAcre * tonnes_per_ton
  )]
  
  # Rename residue into decay class categories and make metric tonnes explicit
  setnames(case_datatable,
           c("Recovered_Branch_tonsAcre","Recovered_Foliage_tonsAcre","Piled_Branch_tonsAcre","Piled_Foliage_tonsAcre","Scattered_Branch_tonsAcre","Scattered_Foliage_tonsAcre","Recovered_CWD_tonsAcre","Scattered_CWD_tonsAcre","Piled_CWD_tonsAcre"),
           c("Recovered_FWD_tonnesAcre","Recovered_Foliage_tonnesAcre","Piled_FWD_tonnesAcre","Piled_Foliage_tonnesAcre","Scattered_FWD_tonnesAcre","Scattered_Foliage_tonnesAcre","Recovered_CWD_tonnesAcre","Scattered_CWD_tonnesAcre","Piled_CWD_tonnesAcre"))
  
  
  # Copy the residue columns. These will be the initial mass residues in Year 1.
  case_datatable[,':='(Recovered_CWD_tonnesAcre_INITIAL = Recovered_CWD_tonnesAcre,
                       Recovered_FWD_tonnesAcre_INITIAL = Recovered_FWD_tonnesAcre,
                       Recovered_Foliage_tonnesAcre_INITIAL = Recovered_Foliage_tonnesAcre,
                       Piled_CWD_tonnesAcre_INITIAL = Piled_CWD_tonnesAcre,
                       Piled_FWD_tonnesAcre_INITIAL = Piled_FWD_tonnesAcre,
                       Piled_Foliage_tonnesAcre_INITIAL = Piled_Foliage_tonnesAcre,
                       Scattered_CWD_tonnesAcre_INITIAL = Scattered_CWD_tonnesAcre,
                       Scattered_FWD_tonnesAcre_INITIAL = Scattered_FWD_tonnesAcre,
                       Scattered_Foliage_tonnesAcre_INITIAL = Scattered_Foliage_tonnesAcre
  )]
  
  # We will also need some blank columns to track material exposed to fire but left unburned.
  #   Make columns for duff, mass previously exposed to fire but unburnt, and year.i columns.
  case_datatable[,':='(Duff_tonnesAcre=0, decay_CWD_mass_year_i=0, decay_FWD_mass_year_i=0, decay_Foliage_mass_year_i=0,
                       fire_exposed_CWD_mass_year_i=0, fire_exposed_FWD_mass_year_i=0, fire_exposed_Foliage_mass_year_i=0, fire_exposed_Duff_mass_year_i=0,
                       prev_fired_CWD_tonnesAcre=0, prev_fired_FWD_tonnesAcre=0, prev_fired_Foliage_tonnesAcre=0, prev_fired_Duff_tonnesAcre=0)]
  
  return(case_datatable)

}

#####################################################################################
# Function to initialize data tables that will store model results
#####################################################################################
initialize_output_tables <- function() {
  
  if(debug_CBREC) {
    cat("Running initialize_output_tables() function\n", file=log_file, append=T)
  }
  
  # Enable ability to change the time resolution using time_int
  #   the total numbers of the row is 100 divided by time_int plus 1 for year 1
  #   exception is if time_int is 1, then its 100, not 101.
  # Note that time_int is a global variable
  dt_nrow <- ifelse(time_int == 1, 100, (100/time_int) + 4) # 8/21/2020: this should be 4, not 1
  
  treatment <- list(
    field.residue.removed_tonnes = 0,
    total.biomass.mobilized_tonnesAcre = 0
  )
  
  postTreatment <- data.table(
    In.field.non.char.scattered_tonnes = rep(0,dt_nrow),
    In.field.non.char.piled_tonnes = rep(0,dt_nrow),
    In.field.char.scattered_tonnes = rep(0,dt_nrow),
    In.field.char.piled_tonnes = rep(0,dt_nrow),
    wildfire.burned.residue_tonnes = rep(0,dt_nrow),
    decayed.residue_tonnes = rep(0,dt_nrow),
    wildfire.CO2_tonnes = rep(0,dt_nrow), 
    wildfire.CO_tonnes = rep(0,dt_nrow), 
    wildfire.CH4_tonnes = rep(0,dt_nrow), 
    wildfire.NOx_tonnes = rep(0,dt_nrow), 
    wildfire.N2O_tonnes = rep(0,dt_nrow), 
    wildfire.PMUnder10um_tonnes = rep(0,dt_nrow), 
    wildfire.PMUnder2.5um_tonnes = rep(0,dt_nrow), 
    wildfire.SO2_tonnes = rep(0,dt_nrow), 
    wildfire.VOC_tonnes = rep(0,dt_nrow), 
    decay.CO2_tonnes = rep(0,dt_nrow),
    decay.CH4_tonnes = rep(0,dt_nrow)
  )
  
  return(list(treatment = treatment,
              postTreatment = postTreatment
              )
  )
}


#' @description Function to kill all rsessions
#' 
#' @param none
#' 
#' @return none
#' 
killSessions <- function() {
  require(ps)
  
  # table of top output
  p <- ps::ps()
  
  # filter to rsessions
  p_k <- p[p$name == "rsession", ]
  
  # keep master session
  # sort by oldest session on top and remove
  p_k <- p_k[order(p_k$created), ]
  
  # filter out first pid
  pids_to_kill <- p_k$pid[2:length(p_k$pid)]
  
  # kill processes
  tools::pskill(pids_to_kill)
}