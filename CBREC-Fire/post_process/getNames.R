## Max Blasdel
## July 23, 2019

## functions to help with extracting names from scenarios, used for labeling graphs and organizing data

cbrec_ScenarioNameDetails <- function(id) {
        if(id %>% class == "list") {
                lapply(id, function(x) {
                        detailedName <- scenarios_matrix %>% 
                                filter(ID == x) %>% 
                                dplyr::select(Burn_Type, Biomass_Collection, Fraction_Piled) %>% 
                                mutate(Fraction_Piled = paste0(Fraction_Piled, "% Piled"),
                                       Biomass_Collection = paste(Biomass_Collection, "Collection")) %>% 
                                unlist() %>% 
                                paste(collapse = " - ")
                        
                        return(detailedName)
                })
        } else{
                detailedName <- scenarios_matrix %>% 
                        filter(ID == id) %>% 
                        dplyr::select(Burn_Type, Biomass_Collection, Fraction_Piled) %>% 
                        mutate(Fraction_Piled = paste0(Fraction_Piled, "% Piled"),
                               Biomass_Collection = paste(Biomass_Collection, "Collection")) %>% 
                        unlist() %>% 
                        paste(collapse = " - ")
                
                return(detailedName)
        }
}

# return name of scenario given an id number
# useful for quick checks 
# automatically loads scenario matrix
cbrec_ScenarioName <- function(id) {
        if (!exists('scenarios_matrix')) {
                scenarios_matrix <<- read.csv(paste0(here::here(), "/data/SERC/scenarios.csv"), stringsAsFactors = F)
        }
        f <- scenarios_matrix %>% 
                dplyr::filter(ID == id) %>% 
                unlist() %>% 
                paste(collapse = "-")
        f <- sub("\\d+.", "", f)
        return(f)
}
