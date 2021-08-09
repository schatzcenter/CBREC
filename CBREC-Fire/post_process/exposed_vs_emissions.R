## Max Blasdel
## July 23, 2019

###### Script to create multipanel graph of exposed VS. emissions for all silviculture treatments in support of the CBREC fire outptus reporting
options(scipen = 20)
# load libraries
library(tidyverse)
library(gridExtra) # for grid arrange of plots
library(ggpubr) # for shared legend
library(scales) # display y-axis with commas
library(grid) # for plotting with shared legend
# Load functions
source("scripts/post_process/getScenarioFiles.R")
source("scripts/post_process/sumDataFrames.R")
source("scripts/post_process/getNames.R")

## Legend function
g_legend<-function(a.gplot){
        tmp <- ggplot_gtable(ggplot_build(a.gplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend) }


# load scenario matrix
scenarios_matrix <- read_csv("data/SERC/scenarios.csv", col_types = cols())


scens <- scenarios_matrix %>% 
        filter(Pulp_Market != "Yes") %>% # filter out pulp markets as these are assumed not to exist for the data
        filter(Fraction_Piled == 50) %>% 
        filter(Biomass_Collection == "No" & Burn_Type != "Pile and Broadcast") %>% 
        dplyr::select(ID, Silvicultural_Treatment) 
        
#Running CH4 vs. Exposed for all silviculture treatments, compare pile, broadcast, wildfire for each
#Identify scenario IDs

rx_only <- "yes"

files <- read.csv(paste0(here::here(), "/data/Tiles/Sierra_Subregion_Tiles_clipped_tile-nums.csv"))

folders <- cbrec_Dirs(path = "data/Tiles/Test_Runs/emissions/Sierra-Subregion", files$X3466)


## Define emission species of interest
emissions_strs <- c('total_duff_residue_CH4', 
                    'total_foliage_residue_CH4', 
                    'total_fwd_residue_CH4',
                    'total_cwd_residue_CH4',
                    'total_pile_CH4')

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
## Below this initally loop are graphing loops for each emissions species

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
        print("Should only be used when comparing prescribed burn scenariso")
}

## Define emission species of interet. This will be saved and called again to make different plots
# ch4
ch4_strs <- c('total_duff_residue_CH4', 
                    'total_foliage_residue_CH4', 
                    'total_fwd_residue_CH4',
                    'total_cwd_residue_CH4',
                    'total_pile_CH4')
ch4_strs <- grep(paste(ch4_strs, collapse = "|"), names(dfs[[1]])) # names will be the same for all list items
# co2
co2_strs <- c('total_duff_residue_CO2', 
              'total_foliage_residue_CO2', 
              'total_fwd_residue_CO2',
              'total_cwd_residue_CO2',
              'total_pile_CO2')
co2_strs <- grep(paste(co2_strs, collapse = "|"), names(dfs[[1]])) # names will be the same for all list items
# pm2.5
pm25_strs <- c('total_duff_residue_PM2.5', 
              'total_foliage_residue_PM2.5', 
              'total_fwd_residue_PM2.5',
              'total_cwd_residue_PM2.5',
              'total_pile_PM2.5')
pm25_strs <- grep(paste(pm25_strs, collapse = "|"), names(dfs[[1]])) # names will be the same for all list items
# pm10
pm10_strs <- c('total_duff_residue_PM10', 
              'total_foliage_residue_PM10', 
              'total_fwd_residue_PM10',
              'total_cwd_residue_PM10',
              'total_pile_PM10')
pm10_strs <- grep(paste(pm10_strs, collapse = "|"), names(dfs[[1]])) # names will be the same for all list items


# total exposed
exp_str <- c("total_duff_exposed",
             "total_foliage_exposed",
             "total_fwd_exposed",
             "total_cwd_exposed")

exp_str <- grep(paste(exp_str, collapse = "|"), names(dfs[[1]])) # names will be the same for all list items


# Prepare data by calculating sums
# Differentiate lists by burn_type *this metric may change with diff implementations

df_out <- lapply(dfs, function(x) {
        x %>% 
                transmute(total_exp = base::rowSums(.[exp_str]),
                          total_ch4 = base::rowSums(.[ch4_strs]),
                          total_co2 = base::rowSums(.[co2_strs]),
                          total_pm25 = base::rowSums(.[pm25_strs]),
                          total_pm10 = base::rowSums(.[pm10_strs]),
                          scen = case_when(Burn_Type == "Broadcast" ~ "Broadcast\n Burn",
                                           Burn_Type == "Pile" ~ "Pile Burn",
                                           Burn_Type == "None" ~ "Wildfire"))
})

rm(dfs)
gc()

# bind together for plotting
df_out <- bind_rows(df_out)
df_out <- distinct(df_out) # important for speed

# convert to Mg/ha from g/ac
df_out <- df_out %>% 
        mutate(total_exp = total_exp / 1000000 * 2.47105,# convert to Mg/ha
                total_ch4 = total_ch4 / 1000000 * 2.47105,
               total_co2 = total_co2 / 1000000 * 2.47105,
               total_pm25 = total_pm25 / 1000000 * 2.47105,
               total_pm10 = total_pm10 / 1000000 * 2.47105)


# Set naming for plots
scenarioName <- cbrec_ScenarioName(scens[[i]][1]) # will be same for all scens
scenarioName <- gsub("-.*", "", scenarioName)
scenarioName <- gsub("_" ," " ,scenarioName)
scenarioName <- ifelse(scenarioName != "Clearcut", gsub("0", "0%", scenarioName), scenarioName)

# Create point or line graph
plots[[i]] <- ggplot(df_out) +
        theme_minimal() +
        geom_point(aes(x = total_exp, y = total_ch4, colour = scen)) +
        labs(x = element_blank(),
             y = element_blank(),
             title = scenarioName,
             colour = "Scenarios") +
        scale_x_continuous(limits = c(0, 250), labels = comma_format()) +
        scale_y_continuous(limits = c(0, 600), labels = comma_format())

## writing outputs incase I need to change something with the graphs
## will be faster to load these files than run the above scripts over 
df_out <- df_out %>% 
        mutate(name = scenarioName)
write_rds(df_out, paste0("data/Post_process/testing/df", i, ".rds"))
##

#rm(df_out)
#gc()
}
###################


# important if list has null values in it
plots <- Filter(Negate(is.null), plots) # TODO write in dplyr format


# Extract legend as a grob
leg = g_legend(plots[[1]])

q <- grid.arrange(
  arrangeGrob(grobs=lapply(plots, function(p) p + guides(colour=FALSE)), ncol=3, 
              bottom=textGrob("Total Surface Residues Exposed (Mg/ha)", gp=gpar(fontsize=15)), 
              left=textGrob("Total CH (Mg/ha)", gp=gpar(fontsize=15), rot=90)),
  leg, 
  widths=c(9,2)
)

ggsave("data/Post_process/charts/allTreats_ch4.png", dpi = 400, q, width = 12, height = 12)

#####################################################################################################
## create new loops from data saved to disk with emissions aggregations
files <- dir("data/Post_process/testing", pattern = ".rds$", full.names = T)

## need to reorder these around
files <- files[c(5,6,7,8,9,10,11,12,13,1,2,3,4)]

# files 4 is the clearcut scenario, naming convention is out of order due to number system
# clearcut will have the highest values and can be used to set the limits, I think this will have to be done manually since the data is being 
# read iteratively

# test <- readRDS(files[[4]])
# tibble(co2 = test$total_co2 %>% max(),
#            ch4 = test$total_ch4 %>% max(),
#            pm25 = test$total_pm25 %>% max(),
#            pm10 = test$total_pm10 %>% max()) %>% 
#         write.csv(file = "data/Post_process/testing/max_emission_numbers.csv")
# done once for reference

# TODO change legend placement, see time series script for this code
###################### CO2 ####################
plots <- list()
for (i in 1:length(files)) {
        
        df_out <- readRDS(files[[i]])

        scenarioName <- df_out$name %>% unique()
                
        # Create point or line graph
        plots[[i]] <- ggplot(df_out) +
                theme_minimal() +
                geom_point(aes(x = total_exp, y = total_co2, colour = scen)) +
                labs(x = element_blank(),
                     y = element_blank(),
                     title = scenarioName,
                     colour = "Scenarios") +
                scale_x_continuous(limits = c(0, 300), labels = comma_format()) +
                scale_y_continuous(limits = c(0, 400), labels = comma_format()) # emissions axis. Change this limit as needed

}

# Extract legend as a grob
leg = g_legend(plots[[1]])

q <- grid.arrange(
        arrangeGrob(grobs=lapply(plots, function(p) p + guides(colour=FALSE)), ncol=3, 
                    bottom=textGrob("Total Surface Residues Exposed (Mg/ha)", gp=gpar(fontsize=15)), 
                    left=textGrob(expression(Total~CO[2]~(Mg/ha)), gp=gpar(fontsize=15), rot=90)),
        leg, 
        widths=c(9,2)
)

ggsave("data/Post_process/charts/allTreats_co2.png", dpi = 400, q, width = 12, height = 12)


######################## PM2.5 #########################################
plots <- list()
for (i in 1:length(files)) {
        
        df_out <- readRDS(files[[i]])
        
        scenarioName <- df_out$name %>% unique()
        
        # Create point or line graph
        plots[[i]] <- ggplot(df_out) +
                theme_minimal() +
                geom_point(aes(x = total_exp, y = total_pm25, colour = scen)) +
                labs(x = element_blank(),
                     y = element_blank(),
                     title = scenarioName,
                     colour = "Scenarios") +
                scale_x_continuous(limits = c(0, 205000), labels = comma_format()) +
                scale_y_continuous(limits = c(0, 2162), labels = comma_format()) # emissions axis. Change this limit as needed
        
}

# Extract legend as a grob
leg = g_legend(plots[[1]])

q <- grid.arrange(
        arrangeGrob(grobs=lapply(plots, function(p) p + guides(colour=FALSE)), ncol=3, 
                    bottom=textGrob("Total Surface Residues Exposed (kg/ha)", gp=gpar(fontsize=15)), 
                    left=textGrob("Total PM 2.5 (kg/ha)", gp=gpar(fontsize=15), rot=90)),
        leg, 
        widths=c(9,2)
)

ggsave("data/Post_process/charts/allTreats_pm25.png", dpi = 400, q, width = 12, height = 12)


################################ PM 10 #########################################
plots <- list()
for (i in 1:length(files)) {
        
        df_out <- readRDS(files[[i]])
        
        scenarioName <- df_out$name %>% unique()
        
        # Create point or line graph
        plots[[i]] <- ggplot(df_out) +
                theme_minimal() +
                geom_point(aes(x = total_exp, y = total_pm10, colour = scen)) +
                labs(x = element_blank(),
                     y = element_blank(),
                     title = scenarioName,
                     colour = "Scenarios") +
                scale_x_continuous(limits = c(0, 205000), labels = comma_format()) +
                scale_y_continuous(limits = c(0, 2550), labels = comma_format()) # emissions axis. Change this limit as needed
        
}

# Extract legend as a grob
leg = g_legend(plots[[1]])

q <- grid.arrange(
        arrangeGrob(grobs=lapply(plots, function(p) p + guides(colour=FALSE)), ncol=3, 
                    bottom=textGrob("Total Surface Residues Exposed (kg/ha)", gp=gpar(fontsize=15)), 
                    left=textGrob("Total PM 10 (kg/ha)", gp=gpar(fontsize=15), rot=90)),
        leg, 
        widths=c(9,2)
)

ggsave("data/Post_process/charts/allTreats_pm10.png", dpi = 400, q, width = 12, height = 12)
