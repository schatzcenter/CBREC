###### Script to create multipanel graph of exposed VS. emissions for all silviculture treatments in support of the CBREC fire outptus reporting
options(scipen = 20)
# load libraries
library(tidyverse)
library(gridExtra) # for grid arrange of plots
library(ggpubr) # for shared legend
library(scales) # display y-axis with commas
library(grid) # for plotting with shared legend
library(raster)
library(leaflet) # for raster viewing in script

# Load functions
source("scripts/post_process/getScenarioFiles.R")
source("scripts/post_process/sumDataFrames.R")
source("scripts/post_process/getNames.R")

# load scenario matrix
scenarios_matrix <- read_csv("data/SERC/scenarios.csv", col_types = cols())

scens <- scenarios_matrix %>% 
        filter(Pulp_Market != "Yes") %>% # filter out pulp markets as these are assumed not to exist for the data
        filter(Fraction_Piled == 0) %>% 
        filter(Biomass_Collection == "No" & Burn_Type == "None") %>% ##take out residue treatments 
        dplyr::select(ID, Silvicultural_Treatment) 

rx_only <- "yes"

files <- read.csv(paste0(here::here(), "/data/Tiles/Sierra_Subregion_Tiles_clipped_tile-nums.csv"))

folders <- cbrec_Dirs(path = "data/Tiles/Test_Runs/emissions/Sierra-Subregion", files$X3466)

folders<-folders[[1]] #select one tile to conduct test runs used to assess the code before running on all pixels

# I want to split the scens into groups of three based on silvicultural treatment
scens <- split(scens, scens$Silvicultural_Treatment)

# will want to apply loop here
# need the scens in slightly different format

scens <- lapply(scens, function(x) {
        x$ID %>% unlist()
})

############################# Start Loop #########################

## Loop originally run to aggregate all the data and create the plots. Saving the aggregated data to file was added 
## so that other plots could more easily be made.

plots <- list()
for (i in 1:13) {
        # get all file paths
        dfs <- lapply(scens[[i]], function(x) {
                cbrec_ScenariosById(folders, id = x, year = 0) # currently only set up to run for one year at a time, will need to change for time series
        })
        
        if(rx_only == "yes") {
                dfs <- lapply(dfs, function(x) {
                        out <- cbrec_DataBindTidy(x)
                        
                        if(length(out) == 2) {
                                return(out[[1]]) # only return first dataframe with prescribed burn emissions
                        } else {
                                return(out)
                        }
                })
                print("Should only be used when comparing prescribed burn scenarios")
        }
        
        ## Define fuels of interest. This will be saved and called again to make different plots
        exp_str <- c("total_foliage_exposed",
                     "total_fwd_exposed",
                     "total_cwd_exposed")
        
        exp_str <- grep(paste(exp_str, collapse = "|"), names(dfs[[1]])) # names will be the same for all list items
        
        # bind together for plotting
        df_out <- bind_rows(dfs)
        df_out <- distinct(df_out) # important for speed
        
        # convert to Mg/ha from g/ac
        df_out <- df_out %>%
                dplyr::select(x,y,Silvicultural_Treatment,total_foliage_exposed,total_fwd_exposed,total_cwd_exposed)%>%
                mutate(total_litter = total_foliage_exposed / 1000000 * 2.47105,# convert to Mg/ha
                       total_fwd = total_fwd_exposed / 1000000 * 2.47105,
                       total_cwd = total_cwd_exposed / 1000000 * 2.47105)
        # Set naming for plots
        scenarioName <- cbrec_ScenarioName(scens[[i]][1]) # will be same for all scens
        scenarioName <- gsub("-.*", "", scenarioName)
        scenarioName <- gsub("_" ," " ,scenarioName)
        scenarioName <- ifelse(scenarioName != "Clearcut", gsub("0", "0%", scenarioName), scenarioName)

        #make a long dataframe with loading by fuel type (litter, FWD, CWD)
        df_out<-df_out%>%
                gather("FuelType","Loading",7:9)
        
        # Create bar graph
    #    plots[[i]] <- ggplot(df_out) +
    #            theme_classic() +
    #            geom_boxplot(aes(x = FuelType, y = Loading)) +
    #            labs(x = element_blank(),
    #                 y = element_blank(),
    #                 title = scenarioName)+
    #            scale_x_discrete(limits = c("total_litter","total_fwd","total_cwd"), labels = c("Litter","FWD","CWD")) +
    #            scale_y_continuous(limits = c(0, 100), labels = comma_format())
        
        ## writing outputs incase I need to change something with the graphs
        ## will be faster to load these files than run the above scripts over 
        df_out <- df_out %>% 
                mutate(name = scenarioName)
        
#        rm(df_out)
#        gc()
}
###################

# important if list has null values in it

#plots <- Filter(Negate(is.null), plots) # TODO write in dplyr format

plots[[1]] #check out an example of the generated plots

q <- grid.arrange(
        arrangeGrob(grobs=lapply(plots, function(p) p + guides(colour=FALSE)), ncol=3, 
                    bottom=textGrob("Fuel Type", gp=gpar(fontsize=15)), 
                    left=textGrob("Fuel Loading (Mg/ha)", gp=gpar(fontsize=15), rot=90)) 
)
q
ggsave("data/Post_process/charts/Fuels_Silvic.png", dpi = 400, q, width = 12, height = 12)

unique(dfs$Silvicultural_Treatment)

df_out%>%
        group_by(name, FuelType)%>%
        summarize("Loading"=mean(Loading),
                  "SE"=sd(Loading)/n(),
                  "MinLoad"=min(Loading),
                  "MaxLoad"=max(Loading))->silvloadsum

###create raster image of fine woody fuels for Sierra subregion

scenarios_matrix %>% 
        filter(Pulp_Market != "Yes") %>% # filter out pulp markets as these are assumed not to exist for the data
        filter(str_detect(string = Silvicultural_Treatment, pattern = "40_Thin_From_B"))%>% 
        filter(Fraction_Piled == 0)%>%
        filter(Biomass_Collection == "No" & Burn_Type == "None") ##take out residue treatments 

# get all file paths

r_objs <- lapply(dfs, function(x){
                r_obj <- x %>% 
                        transmute(x = x,
                                  y = y,
                                  litter = total_foliage_exposed/ 1000000 * 2.47105,# convert to Mg/ha,
                                  fwd = total_fwd_exposed/ 1000000 * 2.47105,
                                  cwd = total_cwd_exposed/ 1000000 * 2.47105)
                rasterFromXYZ(r_obj, 
                              res = c(30,30),
                              crs = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
        })

