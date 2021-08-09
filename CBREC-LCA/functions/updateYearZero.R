#' @author max.blasdel@humboldt.edu
#'
#' @description update treatment emissions
#'
#' @param yearZero data.table to add columns to
#' @param ... separate data.tables with collection and processing outputs
#'
#'
#' @return list of data.tables with below columns and values
#'
updateYearZero <- function(yearZero, ...) {
  # list all inputs
  d <- list(...)
  
  # update each input
  lapply(d, function(x) {
    copy(yearZero[, ':=' (
      collection.processing.diesel.CO2_tonnes = x[[1]]$CO2_kg/1000,
      collection.processing.diesel.CO_tonnes = x[[1]]$CO_kg/1000,
      collection.processing.diesel.N2O_tonnes = x[[1]]$N2O_kg/1000,
      collection.processing.diesel.CH4_tonnes = x[[1]]$CH4_kg/1000,
      collection.processing.diesel.NOx_tonnes = x[[1]]$NOx_kg/1000,
      collection.processing.diesel.PMUnder10um_tonnes = x[[1]]$PMUnder10um_kg/1000,
      collection.processing.diesel.PMUnder2.5um_tonnes = x[[1]]$PMUnder2.5um_kg/1000,
      collection.processing.diesel.BC_tonnes = 0, # calculated with calcBlackCarbon()
      collection.processing.diesel.SO2_tonnes = x[[1]]$SOx_kg * equipment_SO2_SOx_fraction / 1000,
      collection.processing.diesel.VOC_tonnes = x[[1]]$VOC_kg/1000,

      transportation.onroad.diesel.CO2_tonnes = x[[2]]$CO2_kg/1000,
      transportation.onroad.diesel.CO_tonnes = x[[2]]$CO_kg/1000,
      transportation.onroad.diesel.N2O_tonnes = x[[2]]$N2O_kg/1000,
      transportation.onroad.diesel.CH4_tonnes = x[[2]]$CH4_kg/1000,
      transportation.onroad.diesel.NOx_tonnes = x[[2]]$NOx_kg/1000,
      transportation.onroad.diesel.PMUnder10um_tonnes = x[[2]]$PMUnder10um_kg/1000,
      transportation.onroad.diesel.PMUnder2.5um_tonnes = x[[2]]$PMUnder2.5um_kg/1000,
      transportation.onroad.diesel.BC_tonnes = 0, # calculated with calcBlackCarbon()
      transportation.onroad.diesel.SO2_tonnes = x[[2]]$SOx_kg * equipment_SO2_SOx_fraction / 1000,
      transportation.onroad.diesel.VOC_tonnes = x[[2]]$VOC_kg/1000,
      transportation.onroad.diesel.distance_km = x[[2]]$median.nearestPP.distance_km,
      
      transportation.offroad.diesel.CO2_tonnes = x[[3]]$CO2_kg/1000,
      transportation.offroad.diesel.CO_tonnes = x[[3]]$CO_kg/1000,
      transportation.offroad.diesel.N2O_tonnes = x[[3]]$N2O_kg/1000,
      transportation.offroad.diesel.CH4_tonnes = x[[3]]$CH4_kg/1000,
      transportation.offroad.diesel.NOx_tonnes = x[[3]]$NOx_kg/1000,
      transportation.offroad.diesel.PMUnder10um_tonnes = x[[3]]$PMUnder10um_kg/1000,
      transportation.offroad.diesel.PMUnder2.5um_tonnes = x[[3]]$PMUnder2.5um_kg/1000,
      transportation.offroad.diesel.BC_tonnes = 0, # calculated with calcBlackCarbon()
      transportation.offroad.diesel.SO2_tonnes = x[[3]]$SOx_kg * equipment_SO2_SOx_fraction / 1000,
      transportation.offroad.diesel.VOC_tonnes = x[[3]]$VOC_kg/1000
      )]
    )
  })
}
