
#' @author email {max.blasdel@humboldt.edu}
#' @references \url{https://ww3.arb.ca.gov/cc/inventory/slcp/doc/bc_inventory_tsd_20160411.pdf}
#' values come from CARB: Appendix A

#' @description calculate black carbon as a percent of PM2.5 
#' The percent varies by emissions source

#' @param dt data table of emissions tracking {data.table}
#' @param em_src emissions source used to key correct bc ratio {string}
#' 
#' @return New column of black carbon by emissions source
#' returned through data.table reference
#' 

calcBlackCarbon <- function(dt, em_src) {
  # check for correct value inputs
  if (!em_src %in% c("wildfire",
                     "rx_burn_pile",
                     "rx_burn_broadcast",
                     "orchard",
                     "straw",
                     "offroaddiesel",
                     "onroaddiesel-onroad",
                     "onroaddiesel-offroad",
                     "powerplant",
                     "ng_offset")) {
    return(cat(paste("emission sources can be only: ",
              "wildfire",
              "rx_burn_pile",
              "rx_burn_broadcast",
              "orchard",
              "straw",
              "offroaddiesel",
              "onroaddiesel-onroad",
              "onroaddiesel-offroad",
              "powerplant",
              "ng_offset", sep = '\n')))
  }
  
  # find correct value
  bc_frac <- switch (em_src,
    "wildfire" = PM2.5_BC_wildfire, # constants defined in `constants.R`
    "rx_burn_pile" = PM2.5_BC_RXburn_forest,
    "rx_burn_broadcast" = PM2.5_BC_RXburn_forest,
    "orchard" = PM2.5_BC_RXburn_orchard,
    "straw" = PM2.5_BC_RXburn_straw,
    "offroaddiesel" = PM2.5_BC_offroaddiesel,
    "onroaddiesel-onroad" = PM2.5_BC_onroaddiesel,
    "onroaddiesel-offroad" = PM2.5_BC_onroaddiesel,
    "powerplant" = PM2.5_BC_powerplant,
    "ng_offset" = PM2.5_BC_ng_offset
  )
  
  # find correct column
  col_of_int <- switch (em_src,
    "wildfire" = 'wildfire.PMUnder2.5um_tonnes',
    "rx_burn_broadcast" = 'broadcast.burn.PMUnder2.5um_tonnes',
    "rx_burn_pile" = 'pile.burn.PMUnder2.5um_tonnes',
    "orchard" = 'PMUnder2.5um_tonnes', # currently unknown
    "straw" = 'PMUnder2.5um_tonnes', # currently unknown
    "offroaddiesel" = 'collection.processing.diesel.PMUnder2.5um_tonnes',
    "onroaddiesel-onroad" = 'transportation.onroad.diesel.PMUnder2.5um_tonnes',
    "onroaddiesel-offroad" = 'transportation.offroad.diesel.PMUnder2.5um_tonnes',
    "powerplant" = 'pp.electricity.PMUnder2.5um_tonnes',
    "ng_offset" = 'ng_off_PMUnder2.5um_tonnes' # negative value
  )

  # find column position of interest
  col_position <- grep(col_of_int, colnames(dt))

  # calculate value as fraction of bc times pm2.5
  # save result as `emissions_source`.BC_tonnes
  dt[, (gsub(".PMUnder2.5um_tonnes", ".BC_tonnes", col_of_int)) := .SD * bc_frac, .SDcols = col_position]
  
  return(dt)
}


## Reference values from Chow et al. 2010
## PM, CB, and OC values are in Tons / yr
## BC_percent and OC_percent are percent mass values
## TC_percent is BC_percent plus OC_percent

## on road diesel vehicles
## Average value used as % carbon
data.frame(stringsAsFactors=FALSE,
   On.Road.Vehicle = c("LIGHT-DUTYPASSENGER", "LIGHT-DUTYTRUCKS",
                       "MEDIUMDUTYTRUCKS", "LIGHTHEAVY-DUTYGASTRUCKS",
                       "MEDIUMHEAVY-DUTYGASTRUCKS", "HEAVYHEAVY-DUTYGASTRUCKS",
                       "LIGHTHEAVY-DUTYDIESELTRUCKS", "MEDIUMHEAVY-DUTYDIESELTRUCKS",
                       "HEAVYHEAVY-DUTYDIESELTRUCKS", "MOTORCYCLES(MCY)",
                       "HEAVY-DUTYDIESELURBANBUSES", "HEAVY-DUTYGASURBANBUSES",
                       "SCHOOLBUSES", "OTHERBUSES", "MOTORHOMES", "Total"),
                PM = c(3357, 3412, 1097, 142, 21, 15, 220, 1450, 13731, 122,
                       176, 3, 167, 68, 34, 24014),
                BC = c(959, 975, 552, 40, 6, 4, 111, 729, 6907, 35, 88, 1, 48,
                       19, 10, 10483),
                OC = c(1973, 2005, 354, 83, 12, 9, 71, 469, 4439, 72, 57, 2,
                       98, 40, 20, 9703),
               BC_percent = c(0.285671731, 0.285756155, 0.50319052, 0.281690141,
                       0.285714286, 0.266666667, 0.504545455, 0.502758621,
                       0.503022358, 0.286885246, 0.5, 0.333333333, 0.28742515,
                       0.279411765, 0.294117647, 0.43653702),
               OC_percent = c(0.587727137, 0.587631887, 0.322698268, 0.584507042,
                       0.571428571, 0.6, 0.322727273, 0.323448276, 0.323283082,
                       0.590163934, 0.323863636, 0.666666667, 0.586826347,
                       0.588235294, 0.588235294, 0.404055967),
               TC_percent = c(0.873398868, 0.873388042, 0.825888788, 0.866197183,
                       0.857142857, 0.866666667, 0.827272727, 0.826206897,
                       0.82630544, 0.87704918, 0.823863636, 1, 0.874251497,
                       0.867647059, 0.882352941, 0.840592987)
)

## off-road diesel vehicles
## average values used as percent carbon
data.frame(stringsAsFactors=FALSE,
   Off.RoadVehicle = c("OFF-ROADRECREATIONALVEHICLES", "OFF-ROADEQUIPMENT",
                       "FARMEQUIPMENT"),
                PM = c(205, 10939, 2382),
                BC = c(59, 3124, 680),
                OC = c(121, 6429, 1400),
               BC_percent = c(0.287804878, 0.285583691, 0.285474391),
               OC_percent = c(0.590243902, 0.587713685, 0.587741394),
               TC_percent = c(0.87804878, 0.873297376, 0.873215785)
)

## fuel combustion
data.frame(stringsAsFactors=FALSE,
   fuel_combustion = c("ELECTRICUTILITIES", "COGENERATION",
                       "OILANDGASPRODUCTION", "PETROLEUMREFINING",
                       "MANUFACTURINGANDINDUSTRIAL", "FOODANDAGRICULTURALPROCESSING",
                       "SERVICEANDCOMMERCIAL", "OTHER", "Total"),
                PM = c(2108, 1475, 660, 1215, 2040, 1065, 1833, 1331, 11727),
                BC = c(288, 202, 99, 49, 18, 536, 16, 182, 1389),
                OC = c(576, 403, 198, 97, 150, 344, 135, 364, 2268),
        BC_percent = c(0.136622391, 0.136949153, 0.15, 0.040329218,
                       0.008823529, 0.503286385, 0.00872886, 0.136739294,
                       0.118444615),
        OC_percent = c(0.273244782, 0.273220339, 0.3, 0.079835391, 0.073529412,
                       0.323004695, 0.073649755, 0.273478588, 0.193399847),
        TC_percent = c(0.409867173, 0.410169492, 0.45, 0.120164609,
                       0.082352941, 0.82629108, 0.082378614, 0.410217881,
                       0.311844461)
)

# wildfires
data.frame(stringsAsFactors=FALSE,
       Source = c("WILDFIRES"),
           PM = c(78479),
           BC = c(15161),
           OC = c(29530),
   BC_percent = c(0.193185438),
   OC_percent = c(0.376279005),
   TC_percent = c(0.569464443)
)

# percent carbon of PM2.5 from different sources
## Composite of above sources
data.frame(
  stringsAsFactors = FALSE,
  source_PM2.5 = c(
    "wildfire",
    "RX burn forest",
    "RX burn orchard",
    "RX burn straw",
    "offroad diesel",
    "onroad diesel",
    "power plant",
    "natural gas offset"
  ),
  mass_carbon_perc = c(
    0.5694644,
    0.742554,
    0.5695,
    0.5695,
    0.8480488,
    0.8405930,
    0.9,
    0.45
  ),
  reference = c(
    "Chow et al. 2010",
    "Chow et al. 2010",
    "Chow et al. 2010",
    "Chow et al. 2010",
    "Chow et al. 2010",
    "Chow et al. 2010",
    "Chow et al. 2010",
    "Chow et al. 2010"
  ),
  reference_value = c(
    "ARB460 Grass/woodlandFires",
    "ARB463 Forest Mgmt Burning",
    "ARB463 Forest Mgmt Burning",
    "ARB463 Forest Mgmt Burning",
    "Ave. off-road",
    "Ave. on-road",
    "See Note",
    "ARB112-Fuel Combustion—Distillated"
  )
)

# No specific value for biomass power plant was found for total mass of black carbon from pm. 
# Following the assumptions made in Chow et al., the OC is set to twice the value of EC
# This sets total carbon to .9 of PM2.5 mass.
# This value is not used in the model as the carbon balance happens on residues




