library(tidyverse)
library(gridExtra) # for grid arrange of plots
library(ggpubr) # for shared legend
library(scales) # display y-axis with commas
library(grid) # for plotting with shared legend
library(raster)


# Load functions
source("scripts/post_process/getScenarioFiles.R")
source("scripts/post_process/sumDataFrames.R")
source("scripts/post_process/getNames.R")

# load scenario matrix
scenarios_matrix <- read_csv("data/SERC/scenarios.csv", col_types = cols())

rx_only="yes"

scens <- scenarios_matrix %>% 
        filter(Pulp_Market != "Yes") %>% # filter out pulp markets as these are assumed not to exist for the data
        filter(Fraction_Piled == 0) %>% 
        filter(Biomass_Collection == "No" & Burn_Type == "None") %>% ##take out residue treatments 
        filter(str_detect(string = Silvicultural_Treatment, pattern = "40_Thin_From_B"))

files <- read.csv(paste0(here::here(), "/data/Tiles/Sierra_Subregion_Tiles_clipped_tile-nums.csv"))

folders <- cbrec_Dirs(path = "data/Tiles/Test_Runs/emissions/Sierra-Subregion", files$X3466)        

#folders<-folders[[1]] #select one tile to conduct test runs used to assess the code before running on all pixels

dfs <- lapply(scens, function(x) {
        cbrec_ScenariosById(folders, id = x, year = 0) # currently only set up to run for one year at a time, will need to change for time series
})

        
r_objs <- lapply(dfs, function(x){
        r_obj <- x %>% 
                dplyr::select(x,y,total_fwd_exposed)%>%
                transmute(x = x,
                       y = y,
                       total_fwd = total_fwd_exposed / 1000000 * 2.47105) %>% 
        rasterFromXYZ(r_obj, 
                      res = c(30,30),
                      crs = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
})
