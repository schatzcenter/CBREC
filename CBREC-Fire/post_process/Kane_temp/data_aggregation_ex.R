
####### Script to show example of using functions to aggregate fire output data

## load cbrec specific functions
source("scripts/post_process/getScenarioFiles.R") # data aggregation functions
source("scripts/post_process/getNames.R") # find case names by ID number, supplemental to scenario matrix

############ First establish the path to the outputs and provide a vector of folder numbers to look into
# path to Sierra subregion
path = "data/Tiles/Test_Runs/emissions/Sierra-Subregion"

# get a list of tile numbers that define the sierra subregion
# a text file was uplaoded with all of the tile numbers listed
tiles <- read.csv("data/Tiles/Sierra_Subregion_Tiles_clipped_tile-nums.csv")

# get a list of the individual folders with the data
folders <- cbrec_Dirs(path = path, tiles$X3466) # X3466 is a default column name for the text file

############ Next get a list of all the files within those folders that match a particular case by ID.
# can take a while for large regions
# refer to scenario matrix for ID
files <- cbrec_ScenariosById(folders, id = 34, year = 0)


########### Check data using naming functions
cbrec_ScenarioName(34)
cbrec_ScenarioNameDetails(34) # used for generating plots and helping to organize

########### Read and bind all of the data together
# input the list of file locations into a data bind function and return a data.frame 
# can also take a while for large datasets
df <- cbrec_DataBind(files)

# tidyr version with time differences
# I found using dplyr has small time saving benefits

start <- Sys.time()
df <- cbrec_DataBind(files)
base <- Sys.time() - start

start <- Sys.time()
df <- cbrec_DataBindTidy(files)
tidy <- Sys.time() - start

base
## 1.9 mins
tidy
## 56.8 second



