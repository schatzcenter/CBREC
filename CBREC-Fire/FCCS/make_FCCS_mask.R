################################################################################
# This script creates a mask layer for the cells with an FCCS identifier of 0 
# or otherwise barren. The mask will be used to select forested pixels as of the
# California Biopower Impact Project. 
#
# Author: Micah Wright, Humboldt State University
################################################################################

# load the necessary packages
library(data.table)
library(raster)
library(dplyr)

# load the fccs file with loads optimized for consume
dt_fccs <- fread("data/FCCS/tabular/LF_consume.csv", 
                 skip = 1, 
                 header = TRUE)

# load the fccs file with total loads and descriptions
fccs_descript <- fread("data/FCCS/tabular/FCCS_02102017.csv")

fccs_descript <- fccs_descript[Value != -9999, .(fuelbed_number = Value,
                                   fuelbed_name = Fuelbed_Na)]

# merge the files
dt_fccs <- merge(dt_fccs, fccs_descript, by = "fuelbed_number", all = TRUE)

###J.Kane addition 06/21/2019: Filter out all non tree FCCS fuelbed numbers.
#fread("data/FCCS/tabular/FCCS_02102017.csv") %>% 
#FCCS_02102017 %>%
#        filter(Canopy_tot==0) -> no_trees_FCCS

###### Max method by identifying areas from the FCCS proportions that have a zero woody fuels proportions 6/21/2019
fuel_prop <- fread("data/FCCS/tabular/FCCS_fuel_load_proportions.csv")

# filter areas based on lack of woody fuels
no_trees <- fuel_prop %>% 
        filter(one_hr_sound_prop == 0 & ten_hr_sound_prop == 0 & hun_hr_sound_prop == 0)
#############################################################################################################

no_trees <- list(no_trees$fuelbed_number)

no_trees <- unlist(no_trees)

# load FCCS raster
FCCS <- raster("data/FCCS/spatial/FCCS_NAD83.tif")

# get the fuelbed numbers that don't have any canopy fuels
# or are between 900 & 1000

#J.Kane change 06/21/2019
fccs_notrees <- dt_fccs[fuelbed_number %in% no_trees, .(fuelbed_number)][["fuelbed_number"]]

# Micah's original code
#fccs_notrees <- dt_fccs[fuelbed_number %in% c(0,seq(900, 999)), .(fuelbed_number)][["fuelbed_number"]]

# create a column for updated pixel value based on the FCCS ID
dt_fccs[, becomes := ifelse(fuelbed_number %in% fccs_notrees, 1, NA)]

# save the fuelbed names that will be removed
fwrite(dt_fccs[becomes == 1, .(fuelbed_name, fuelbed_number)], "data/FCCS/tabular/fuelbeds_removed.csv")

# create a reclass matrix from the dt
rcl <- as.matrix(dt_fccs[, .(fuelbed_number, becomes)])

# reclassify the FCID raster
FCCS_recl <- reclassify(FCCS, rcl)

# remove old raster to avoid overwrite issues
#file.remove("data/FCCS/spatial/FCCS_unforested.tif")

# save the output
writeRaster(FCCS_recl,
            "data/FCCS/spatial/FCCS_unforested_V3.tif", # V3 uses the proportion zero method of filtering
            format = "GTiff",
            datatype = "INT2S",
            overwrite = T)

# get the cell count summarys for both rasters and save them.
cell_counts_FCCS <- as.data.table(freq(FCCS_recl))

fwrite(cell_counts_FCCS, "data/FCCS/tabular/cell_counts_FCCS.csv")

