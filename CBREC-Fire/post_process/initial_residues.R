

## Purpose of this script is to use parts of the emissions model code to calculate a fuel load before any rx, wildfire, or removal occurs
library(data.table)

# emissions script
source("scripts/emissions_model/load_data.R")

# post-process script
source("scripts/post_process/getScenarioFiles.R")

# helper function
cbrec_CaseDetails <- function(df) {
        ifelse (class(df) == 'list', return(df[[1]][1,1:12]), return(df[1,1:12]))
}

initialResidue(df = x, Tile = tile)

## this function is essentially calling the entire data frame again for a case. 
## This could be combined with an existing df to pull out the technically recoverable material from that case

initialResidue <- function(df, Tile) {
        # returns variables used to run load data from existing data frame
        x <- cbrec_CaseDetails(df)
        
        df <- load_data(id = x$ID,
                        treatment = x$Silvicultural_Treatment,
                        f_piled = x$Fraction_Piled,
                        f_scattered = x$Fraction_Scattered,
                        burn_type = x$Burn_Type,
                        biomass_collection = x$Biomass_Collection,
                        pulp_market = x$Pulp_Market,
                        tile_number = tile)
        
        # read in recovered by size lookup table
        rec <- fread("data/SERC/lookup_tables/recovered_by_size.csv")
        
        # filter down to only full recoverable
        rec <- subset(rec, Biomass_Collection =="All Tech Recoverable")
        
        # merge with df, will always join all tech recoverbale scenario, varies by slope
        df <- merge(df, rec,
                    by = c(
                            "Slope_Class",
                            "Silvicultural_Treatment",
                            "Fraction_Piled",
                            "Fraction_Scattered",
                            "Burn_Type",
                            #"Biomass_Collection", # everything is assumed full recoverable
                            "Pulp_Market"), 
                    all.x = FALSE,
                    all.y = FALSE,
                    sort = TRUE,
                    allow.cartesian = T)
        
        # calculate coarse load
        df[, ':=' (tech_ge9 = Stem_ge9_tonsAcre * Stem_ge9,
                   tech_6t9 = Stem_6t9_tonsAcre * Stem_6t9,
                   tech_4t6 = Stem_4t6_tonsAcre * Stem_4t6,
                   tech_branch = Branch_tonsAcre * Branch,
                   tech_foliage = Foliage_tonsAcre * Foliage)]
        
        # drop unneeded columns
        cols <- c("x",
                  "y",
                  "tech_ge9",
                  "tech_6t9",
                  "tech_4t6",
                  "tech_branch",
                  "tech_foliage")
        
        df[, ..cols]
}


