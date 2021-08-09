library(tidyverse)
library(readr)

test<-read_rds("data/Tiles/output/emissions/1/20_Proportional_Thin-Broadcast-0-100-first-All Tech Recoverable-No-1-1-0.rds")

view(test)

test%>%
        filter(total_fwd_exposed==0)%>%
        filter(total_cwd_exposed==0)->testsum

Residue_by_treat%>%
        filter(FCID2018==357981281)->restestsum

scenarios_matrix <- read_csv("data/SERC/scenarios.csv", col_types = cols() )

path <- "data/Tiles/output/emissions"

Run1 <- seq(1, 100)

getDirs <- function(path, start, end) {
        f <- dir(path, 
                 pattern = paste0("^", as.character(seq(start, end)), "$", collapse = "|"),
                 full.names = T)
        return(f)
}

getScenariosId <- function(path, id, year) {
        
        f <- dir(path,
                 pattern =paste("first-.*-.*-",id, "-", "\\d+","-",year,".rds$", sep = ""),
                 recursive = T,
                 full.names = T)
        # account for wildfire after RX burn
        if (year == 0) {
                s <- dir(path,
                         pattern =paste("second-.*-.*-",id, "-", "\\d+","-",year,".rds$", sep = ""),
                         recursive = T,
                         full.names = T)
                f <- dir(path,
                         pattern =paste("first-.*-.*-",id, "-", "\\d+","-",year,".rds$", sep = ""),
                         recursive = T,
                         full.names = T)
                
                if (length(s) == 0) { # 'second' burn does not appear in every year zero scenario
                        return(f)
                } else {
                        message("This runs contains a second burn and I haven't figured out a way to deal with this yet.")
                        return(list(f,s)) # I want these as seperate lists     
                }
        } else {
                return(f)
        }
}

getScenarioName <- function(id) {
        if (!exists('scenarios_matrix')) {
                scenarios_matrix <- read_csv("../../data/SERC/scenarios.csv", col_types = cols())
        }
        f <- scenarios_matrix %>% 
                dplyr::filter(ID == id) %>% 
                paste(collapse = "-")
        f <- sub("\\d+.", "", f)
        return(f)
}

getDataBindAll <- function (data) {
        d <- bind_rows(lapply(data, read_rds))
        return(d)
}

getDataWrapper <- function(path, id, year, all = F) {
        
        start <- min(runN)
        end <- max(runN)
        
        run <- getDirs(path = path, start = start, end = end)
        
        t <- getScenariosId(path = run, id = id, year = year)
        
        # choose which data bind function to use
        if (all == F){
                t <- getDataBindSimple(t)        
        } else {
                t <- getDataBindAll(t)
        }
        
        n <- getScenarioName(id)
        
        return(list(t, n))
}

t <- getDirs(path, start = min(Run1), end = max(Run1))

t <- getScenariosId(path = t, id = 663, year = 0)

out <- list()
for (i in 1:length(t)) {
        out[[i]] <- read_rds(t[i]) %>% 
                transmute(gross_CO2 = acres_per_cell * (total_duff_residue_CO2 +
                                                                total_foliage_residue_CO2 +
                                                                total_fwd_residue_CO2 +
                                                                total_cwd_residue_CO2)),
        tile = str_split(t[i], pattern = "/")[[1]][7]) # get tile number from data
}
out <- do.call(rbind, out)
