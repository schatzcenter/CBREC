############ Functions related to data wrangling of fire model outputs ##############
## Author: Max Blasdel
## Date: 6/24/2019
#####################################################################################

## Inputs: path to top level outputs folder

## Outputs: data.frame object based on input year and scenario id

## Recommended: Scenario Matrix for reference
#####################################################################################


## Accepts path to top level folder directory of fire outputs and range of tiles of interest
## ex. "data/Tiles/output/emissions"
cbrec_Dirs <- function(path, vec) {
        # create list of tile numbers
        l <- as.vector(vec)
        
        # find corresponding folders
        f <- lapply(l, function(x) {
                dir(path,
                    pattern = paste0("^", as.character(x), "$"),
                    full.names = T)     
        }) 
        
        # return list of file paths
        return(f)
}

## runs sort of slow for large datasets, how could this be sped up?
## Accepts list of folder paths, scenario id of interest and year of interest
cbrec_ScenariosById <- function(path, id, year) {
        # get all folders within main folder that relate to year and scenario id of interest
        f <- lapply(path, function(x) {
          dir(x,
              pattern = paste("-", id, "-", "\\d+","-",year,".rds$", sep = ""),
              recursive = T,
              full.names = T)      
        })
        
        # count how man items are being removed
        countNas <- length(f[lapply(f, length) == 0])
        
        # removes characters of length 0, where there are no file paths
        f <- f[lapply(f, length) > 0]
        
        # seperate out first and second burns
        # second burns are only found in some scenarios in year 0
        first <- lapply(f, "[", 1)
        second <- lapply(f, "[", 2)
        
        # check if there was no second burn and return just first burn
        # since there are na values as missing files this file is removing all the second returns
        if(all(is.na(second)) ==  T) {
                return(list(unlist(first))) 
        } else {
        # change formating of lists and return both first and second burns
                first <- unlist(first)
                second <- unlist(second)
                
                return(list(first, second))
        }
        print(paste0("Removed ", countNas, " na(s) from list of files")) #TODO this doesnt print when used in apply loop
}

# bind data together
# two versions exist, one using base and the other using dplyr and readr from tidyverse
cbrec_DataBind <- function(data) {
        
        # check for first and second input
        if(length(data) > 1) {
                
                # read and bind seperately
                first <- lapply(data[[1]], readRDS)
                first <- do.call(rbind, first)
                
                second <- lapply(data[[2]], readRDS)
                second <- do.call(rbind, second)
                
                # notify user of output
                print("returned object is list of two dataframe")
                
                # return list of dataframes
                return(list(first, second))
                
        } else {
                # otherwise only apply to single list
                d <- lapply(data[[1]], readRDS)
                d <- do.call(rbind, d)
                
                return(d)
        }
        
}

# same functionality but with tidy functions
# performed faster in initial tests
cbrec_DataBindTidy <- function (data) {
        # check for first and second input
        if(length(data) > 1) {
                
                # read and bind seperately
                first <- dplyr::bind_rows(lapply(data[[1]], readr::read_rds))

                second <- dplyr::bind_rows(lapply(data[[2]], readr::read_rds))
                
                # notify user of output
                print("returned object is list of two dataframe")
                
                # return list of dataframes
                return(list(first, second))
                
        } else {
                # otherwise only apply to single list
                d <- dplyr::bind_rows(lapply(data[[1]], readr::read_rds))
                return(d)
        }
}
