

# load libraries and functions
library(tidyverse)
library(raster)
source("scripts/post_process/getScenarioFiles.R")
source("scripts/post_process/sumDataFrames.R")
source("scripts/post_process/getNames.R")

# load for reference
scenarios_matrix <- read_csv("data/SERC/scenarios.csv", col_types = cols())

# path to sierra tiles numbers
files <- read.csv("data/Tiles/Sierra_Subregion_Tiles_clipped_tile-nums.csv")

# get all folders
folders <- cbrec_Dirs(path = "data/Tiles/Test_Runs/emissions/Sierra-Subregion", files$X3466)

# get all files
folders <- cbrec_ScenariosById(path = folders, id = 277, year = 0) # 273 identified as 40TFB no RX, no Collection

# bind togehter
df <- cbrec_DataBindTidy(folders)

# convert to raster, select only one variable and the coordinates
r_obj <- df %>%
        transmute(x = x,
                  y = y,
                  val = (total_foliage_residue_CO2+total_fwd_residue_CO2+total_cwd_residue_CO2) / 1000000 * 2.47105 * 0.09) # convert to megagram per hectare 

r_obj <- rasterFromXYZ(r_obj, 
              res = c(30,30),
              crs = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")


r_range <- c(round(minValue(r_obj)), maxValue(r_obj))

# check outputs
plot(r_obj,
     legend = F,
     axes = F)

plot(r_obj,
     legend.only = T,
     legend.width = 1,
     legend.shrink = .75,
     axis.args = list(at = seq(r_range[1], r_range[2], 1),
                      labels = seq(r_range[1], r_range[2], 1),
                      cex.axis = .9),
     legend.args = list(text = "CO2 Emissions (Mg/ha)", side = 4, font = 2, line = 2.5))

# write to disk
writeRaster(r_obj, filename = "data/Post_process/rasters/fwd_exposed.tif", overwrite=TRUE)
