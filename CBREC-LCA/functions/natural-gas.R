
#' @author Max Blasdel
#' @date 9/16/2019
#' @description script to convert MMBTUs of heat to emissions of criteria pollutants and GHGs.

#' @param mmbtu btu value from burning biomass
#' @return values in metric tonnes

# calculate offset natural gas
naturalGasEquivalant <- function(mmbtu) {
  
  # update values in data.table
  table <- 
    data.table(
      'ng_off_CO2_tonnes' = -(mmbtu * ng_chp_co2) / 1000,
      'ng_off_CO_tonnes' = -(mmbtu * ng_chp_co) / 1000,
      'ng_off_CH4_tonnes' = -(mmbtu * ng_chp_ch4) / 1000,
      'ng_off_NOx_tonnes' = -(mmbtu * ng_chp_nox) / 1000,
      'ng_off_N2O_tonnes' = -(mmbtu * ng_chp_n2o) / 1000,
      'ng_off_PMUnder2.5um_tonnes' = -(mmbtu * ng_chp_pm2.5) / 1000,
      'ng_off_SO2_tonnes' = -(mmbtu * ng_chp_so2) / 1000,
      'ng_off_VOC_tonnes' = -(mmbtu * ng_chp_voc) / 1000)

  return(table)
}
