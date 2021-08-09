# Function Name: climateMetrics()
# Owner: Schatz Energy Research Center
# Original Version Author: Jerome Carman
# License: 
# Version: 1.0
# Date: July 26, 2019
# Description: 
# Required Input Variables
#'  @param use.case: a list output from run-cbrec.R. Represents the use case for the comparison being made.
#'  @param ref.case: a list output from run-cbrec.R. Represents the reference case for the comparison being made.
#'  @param AGWPMat: a 100 x 100 matrix that represents the scenario matrix for the AGWP climate metric.
#'  @param AGTPMat: a 100 x 100 matrix that represents the scenario matrix for the AGTP climate metric.
#'  @param timehorizon: list of two integer values between 1 and 100 that represents the time horizon at which AGWP and AGTP will be reported
#'  @param functional_unit: a character string defining what the functional unit will be. Must be one of the
#     following options: "MT_Recovered","MT_Delivered","kWh" {string}
#'  @param functional_unit_value: either NULL (to calculate in this function) or a float that contains the value that is desired
#'  @param ngoffset: boolean saying whether to include natural gas offset. True = include, False = don't include {boolean}
#'  @param type: either "use" or "ref" denoting the type of cases being run {string} 
# Optional Input Variables
#   
# Output Variables
#   Returns a list containing two lists, one for the use case and one for the reference case
# Version History
#   10/15/19 (CWH) updated to take the new CBREC output format
#   11/7/19 (JKC) added functional_unit and timehorizon variables
#   3/12/21 (JKC) removed functional unit and timehorizon variables

climateMetrics <- function(case,
                           AGWPMat,
                           AGTPMat,
                           ngoffset,
                           type) {

  # Get GHGs
  #   Note: functional unit no longer use. Confirm and depricate.
  pollutants <- getPollutants(case,
                              ngoffset,
                              use_case = if(type=="use") T else F)

# Calculate Climate Metrics  ---------------------------------------------------

  # Append AGWP time series
  pollutants <- calcClimateMetric(case = pollutants, # pass output of getGHG
                                  cm_mat = AGWPMat,
                                  cm = "AGWP")

  #Append AGTP time series
  pollutants <- calcClimateMetric(case = pollutants, # passed output of first calcClimateMetric function
                                  cm_mat = AGTPMat,
                                  cm = "AGTP")
  
  # Return result
  return(pollutants)
}

#' @name consolidateTreatmentEmissions()
# Owner: Schatz Energy Research Center
# Original Version Author: Jerome Carman
# Version: 1.0
# Date: March 20, 2021
#' @description Belongs to the C-BREC model. Takes a data table of use case treatment results and consolidates to a single row of data.
#'              This tackles the situation where there is more than one power plant destination for a particular project
#' @param dt: the treatment data table in the list passed to getPollutants() function in the C-BREC model. This can be safely passed by reference.
# Output Variables
#   Returns a data table with a single row of values
# Version History
#   03/20/21 (JKC): Created initial version
consolidateTreatmentEmissions <- function(dt) {
  # Get three groups of columns names on which we'll do different actions
  #   This is due to inconsistency in pattern of reporting results from C-BREC. See Issue #108
  ppNames <- names(dt)[grepl("pp_|ng_",names(dt))]
  allOtherNames <- names(dt)[names(dt) %notin% c(ppNames,"plant.type","plant_location")]
  
  # Sum power plant related data
  dtPP <- dt[, ppNames, with=F][, lapply(.SD,sum)]
  
  # Average results across all other emissions sources
  #   Because these are simply duplicated across the number of different power plant destinations (see issue #108)
  dtAllOther <- dt[, allOtherNames, with=F][, lapply(.SD,mean)]
  
  #Return result
  return(cbind(dtPP,dtAllOther))
}

#' @name getPollutants() 
# Owner: Schatz Energy Research Center
# Original Version Author: Jerome Carman
# License: 
# Version: 1.0
# Date: July 26, 2019
#' @description Belongs to the C-BREC model. Takes total 100-year emissions outputs and returns just the greenhouse gas species.
# Required Input Variables
#' @param case: the list output from climateMetrics() function in the C-BREC model. This can be safely passed by reference.
#' @param include_ngoffset: boolean that says whether or not to include the natural gas. True = include, False = don't include
# Optional Input Variables
#   
# Output Variables
#   Returns a list of three data tables, one each for CO2, CH4, and N2O. Each data table is a 100-year time series
#       of emissions by source.
# Version History
#   10/15/19 (CWH) updated to take the new CBREC output format
#   11/7/19 (JKC) added func_unit variables
#   03/12/21 (JKC) removed functional unit feature
getPollutants <- function(case,
                          include_ngoffset,
                          use_case) {
  
  # Consolidate for those cases where there are multiple power plant destinations
  #   Does not impact those cases where there is only one power plant destination
  case$treatment <- consolidateTreatmentEmissions(case$treatment)
  
  #Load year 0 data. These are all emissions that occur prior to in-field decay and wildfire. This includes
  #   all mobilization emissions, RX burns, power plant emissions
  CO2.year0 <- case[["treatment"]][, grepl("CO2", names(case[["treatment"]])), with = FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(CO2.year0))))
  colnames(fillzeros) <- colnames(CO2.year0)
  CO2.year0 <- rbind(CO2.year0, fillzeros)
  
  CH4.year0 <- case[["treatment"]][, grepl("CH4", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(CH4.year0))))
  colnames(fillzeros) <- colnames(CH4.year0)
  CH4.year0 <- rbind(CH4.year0, fillzeros)
  
  N2O.year0 <- case[["treatment"]][, grepl("N2O", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(N2O.year0))))
  colnames(fillzeros) <- colnames(N2O.year0)
  N2O.year0 <- rbind(N2O.year0, fillzeros)
  
  PM2.5.year0 <- case[["treatment"]][, grepl("PMUnder2.5um", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(PM2.5.year0))))
  colnames(fillzeros) <- colnames(PM2.5.year0)
  PM2.5.year0 <- rbind(PM2.5.year0, fillzeros)
  
  CO.year0 <- case[["treatment"]][, grepl("CO_", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(CO.year0))))
  colnames(fillzeros) <- colnames(CO.year0)
  CO.year0 <- rbind(CO.year0, fillzeros)
  
  NOx.year0 <- case[["treatment"]][, grepl("NOx", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(NOx.year0))))
  colnames(fillzeros) <- colnames(NOx.year0)
  NOx.year0 <- rbind(NOx.year0, fillzeros)
  
  PM10.year0 <- case[["treatment"]][, grepl("PMUnder10um", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(PM10.year0))))
  colnames(fillzeros) <- colnames(PM10.year0)
  PM10.year0 <- rbind(PM10.year0, fillzeros)
  
  VOC.year0 <- case[["treatment"]][, grepl("VOC", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(VOC.year0))))
  colnames(fillzeros) <- colnames(VOC.year0)
  VOC.year0 <- rbind(VOC.year0, fillzeros)
  
  SO2.year0 <- case[["treatment"]][, grepl("SO2", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(SO2.year0))))
  colnames(fillzeros) <- colnames(SO2.year0)
  SO2.year0 <- rbind(SO2.year0, fillzeros)
  
  BC.year0 <- case[["treatment"]][, grepl("BC_", names(case[["treatment"]])), with=FALSE]
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(BC.year0))))
  colnames(fillzeros) <- colnames(BC.year0)
  BC.year0 <- rbind(BC.year0, fillzeros)
  
  char.year0 <- case[["treatment"]][, grepl("pp_waste.flyash_char.tonnes", names(case[["treatment"]])), with=FALSE] # units are MT of char
  fillzeros <- as.data.table(matrix(0, 99, length(colnames(char.year0))))
  colnames(fillzeros) <- colnames(char.year0)
  char.year0 <- rbind(char.year0, fillzeros)
  
  
  # Get decay and wildfire emissions
  CO2.future <- case[["postTreatment"]][, grepl("CO2",names(case[["postTreatment"]])), with=FALSE]
  CH4.future <- case[["postTreatment"]][, grepl("CH4",names(case[["postTreatment"]])), with=FALSE]
  N2O.future <- case[["postTreatment"]][, grepl("N2O",names(case[["postTreatment"]])), with=FALSE]
  PM2.5.future <- case[["postTreatment"]][, grepl("PMUnder2.5um",names(case[["postTreatment"]])), with=FALSE]
  CO.future <- case[["postTreatment"]][, grepl("CO_",names(case[["postTreatment"]])), with=FALSE]
  NOx.future <- case[["postTreatment"]][, grepl("NOx",names(case[["postTreatment"]])), with=FALSE]
  PM10.future <- case[["postTreatment"]][, grepl("PMUnder10um",names(case[["postTreatment"]])), with=FALSE]
  VOC.future <- case[["postTreatment"]][, grepl("VOC",names(case[["postTreatment"]])), with=FALSE]
  SO2.future <- case[["postTreatment"]][, grepl("SO2",names(case[["postTreatment"]])), with=FALSE]
  BC.future <- case[["postTreatment"]][, grepl("BC_",names(case[["postTreatment"]])), with=FALSE]
  char.future <- cbind(case[["postTreatment"]][, grepl("In.field.char.scattered_tonnes", names(case[["postTreatment"]])), with=FALSE], # captures all RX and wildfire
                       case[["postTreatment"]][, grepl("In.field.char.piled_tonnes", names(case[["postTreatment"]])), with=FALSE]) # captures all RX and wildfire

  # Merge all emissions sources
  
  # create time series for each pollutant
  CO2.em <- cbind(CO2.year0, CO2.future)
  if(!include_ngoffset & use_case) {
    CO2.em[, CO2.em[, grep(pattern = "^ng_off", colnames(CO2.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  CO2.em[, total.CO2_tonnes := rowSums(.SD)]
  
  CH4.em <- cbind(CH4.year0,CH4.future)
  if(!include_ngoffset & use_case) {
    CH4.em[,CH4.em[,grep(pattern = "^ng_off", colnames(CH4.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  CH4.em[,total.CH4_tonnes := rowSums(.SD)]
  
  N2O.em <- cbind(N2O.year0,N2O.future)
  if(!include_ngoffset & use_case) {
    N2O.em[,N2O.em[,grep(pattern = "^ng_off", colnames(N2O.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  N2O.em[,total.N2O_tonnes := rowSums(.SD)]
  
  PM2.5.em <- cbind(PM2.5.year0,PM2.5.future)
  if(!include_ngoffset & use_case) {
    PM2.5.em[,PM2.5.em[,grep(pattern = "^ng_off", colnames(PM2.5.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  PM2.5.em[,total.PM2.5_tonnes := rowSums(.SD)]
  
  CO.em <- cbind(CO.year0,CO.future)
  if(!include_ngoffset & use_case) {
    CO.em[,CO.em[,grep(pattern = "^ng_off", colnames(CO.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  CO.em[,total.CO_tonnes := rowSums(.SD)]
  
  NOx.em <- cbind(NOx.year0,NOx.future)
  if(!include_ngoffset & use_case) {
    NOx.em[,NOx.em[,grep(pattern = "^ng_off", colnames(NOx.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  NOx.em[,total.NOx_tonnes := rowSums(.SD)]
  
  PM10.em <- cbind(PM10.year0,PM10.future)
  # No ngoffset for PM10
  PM10.em[,total.PM10_tonnes := rowSums(.SD)]
  
  VOC.em <- cbind(VOC.year0,VOC.future)
  if(!include_ngoffset & use_case) {
    VOC.em[,VOC.em[,grep(pattern = "^ng_off", colnames(VOC.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  VOC.em[,total.VOC_tonnes := rowSums(.SD)]
  
  SO2.em <- cbind(SO2.year0,SO2.future)
  if(!include_ngoffset & use_case) {
    SO2.em[,SO2.em[,grep(pattern = "^ng_off", colnames(SO2.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  SO2.em[,total.SO2_tonnes := rowSums(.SD)]
  
  BC.em <- cbind(BC.year0,BC.future)
  if(!include_ngoffset & use_case) {
    BC.em[,BC.em[,grep(pattern = "^ng_off", colnames(BC.em))] := NULL] #Remove all natural gas offset results before total column is created
  }
  BC.em[,total.BC_tonnes := rowSums(.SD)]
  
  char.em <- cbind(char.year0,char.future)
  # No ngoffset for PM10
  char.em[,total.char_tonnes := rowSums(.SD)]
  
  # Get mobilized residue values --------------------------------
  # Fill in zeros for reference cases
  residue_removed <-
    ifelse(is.null(case[["treatment"]]$field.residue.removed_tonnes),
           0,
           case[["treatment"]]$field.residue.removed_tonnes)
  
  # biomass per acre
  residue_removed_perAcre <- 
    ifelse(is.null(case[["treatment"]]$total.biomass.mobilized_tonnesAcre),
           0,
           case[["treatment"]]$total.biomass.mobilized_tonnesAcre)

  # get kWh generated
  electricity_generated <- 
    ifelse(is.null(case[["treatment"]]$pp_energy.production_MWh), # Will be null for reference cases
           0,
           case[["treatment"]]$pp_energy.production_MWh)
  
  # residue delivered
  residue_delivered <- 
    ifelse(is.null(case[["treatment"]]$pp_residue.delivered_tonnes), # Will be null for reference cases
           0,
           case[["treatment"]]$pp_residue.delivered_tonnes)

  # residue burned (affected by storage)
  residue_burned <- 
    ifelse(is.null(case[["treatment"]]$pp_residue.burned_tonnes),
           0,
           case[["treatment"]]$pp_residue.burned_tonnes
    )
  
  #Return results
  return(list(CO2.mass = CO2.em,
              CH4.mass = CH4.em,
              N2O.mass = N2O.em,
              PM2.5.mass = PM2.5.em,
              CO.mass = CO.em,
              NOx.mass = NOx.em,
              PM10.mass = PM10.em,
              VOC.mass = VOC.em,
              SO2.mass = SO2.em,
              BC.mass = BC.em,
              char.mass = char.em,
              MT_Residue_Mobilized = residue_removed,
              MT_Residue_Mobilized_perAcre = residue_removed_perAcre,
              MT_Residue_Delivered = residue_delivered,
              MT_Residue_Burned = residue_burned,
              MWh_Generated = electricity_generated
              ))
}

# Function Name: calcClimateMetric()
# Owner: Schatz Energy Research Center
# Original Version Author: Jerome Carman
# License: 
# Version: 1.0
# Date: July 26, 2019
# Description: Creates times series of climate metric AGTP or AGWP for the GHG gases CO2, CH4, and N2O.
# Required Input Variables
#'  @param case: the list output from the getGHG() function in the C-BREC model. This is expected to be passed by reference.
#'  @param cm_mat: a list that contains the scenario matrices for the intended climate metric for each of the GHG gases. Name of each
#'       list element  must be "CO2, "CH4, and "N2O". Each matrix must be a lower triangular 100 x 100 2D matrix of type matrix.
#'  @param cm: character string specifying the climate metric desired. Must be either "AGWP" or "AGTP".
#' Optional Input Variables
#   
# Output Variables
#' @return three data tables appended to the input list variable "case" which contain the time series climate metric results.
#' These data.tables are for AGWP or AGTP
#   Returns three data tables appended to the input list variable "case" which contain the time series climate metric results.
# Version History
#   11/7/19 (JKC) added time horizon and functional unit variables. Added CO2e calculation
#   3/12/21 (JKC) removed functional unit feature and timehorizon
calcClimateMetric <- function(case,
                              cm_mat,
                              cm) { # cm is hardcoded value for now
  
  #Identify climate metric unit
  if(cm == "AGWP") {
    cm_unit <- "W_m2"
  } else if(cm == "AGTP") {
    cm_unit <- "K"
  } else {
    stop("Incorrect climate metric unit passed to getClimateMetric()")
  }
  
  #Create 100-year times series climate metrics ------------------------------------

#' @note conditional naming defines the units of the output; is func_val is not an integer and does not equal 1, then the outputs will be AGTP per kWh
#' if the func_val is any other number the units will be W_m2 or K
  #Create climate metric for CO2
  #' @Note: %*% is the matrix multiplication function
  case[[paste0("CO2.",cm)]] <- as.data.table(case$CO2.mass[, cm_mat[["CO2"]] %*% (as.matrix(.SD) * 1000)]) #convert .SD to kg since cm_mat is per kg
  colnames(case[[paste0("CO2.",cm)]]) <- gsub("tonnes", cm_unit, colnames(case[[paste0("CO2.", cm)]]))
  
  #Create climate metric for CH4
  case[[paste0("CH4.",cm)]] <- as.data.table(case$CH4.mass[, cm_mat[["CH4"]] %*% (as.matrix(.SD) * 1000)]) #convert .SD to kg since cm_mat is per kg
  colnames(case[[paste0("CH4.", cm)]]) <- gsub("tonnes", cm_unit, colnames(case[[paste0("CH4.", cm)]]))
  
  #Create climate metric for N2O
  case[[paste0("N2O.",cm)]] <- as.data.table(case$N2O.mass[,cm_mat[["N2O"]] %*% (as.matrix(.SD) * 1000)]) #convert .SD to kg since cm_mat is per kg
  colnames(case[[paste0("N2O.",cm)]]) <- gsub("tonnes",cm_unit,colnames(case[[paste0("N2O.",cm)]]))
  
  return(case)
}


#' @author `max.blasdel@gmail.com`
#' @description calculate a CO2 equivalent number based on the sum of CO2, CH4, and N2O
#'
#' @param timehorz value between 1-100 {integer}
#' @param cm climate metric either "AGTP" or "AGWP" {string}
#' @param func_unit calculated in getFunctionalUnit(). One of three strings, "kWh", "MT Delivered", "MT Removed" {string}
#' 
calcCO2E <- function(case,
                     timehorz,
                     cm,
                     func_unit=NA) {
  
  # Identify climate metric unit
  # No longer relevent as the units change earlier 
  # if(cm == "AGWP") {
  #   cm_unit <- "W_m2"
  # } else if(cm == "AGTP") {
  #   cm_unit <- "K"
  # } else {
  #   stop("Incorrect climate metric unit passed to getClimateMetric()")
  # }
  if(cm == "AGWP") {
    timehorz <- timehorz$AGWP
    cm_mat <- AGWPMat # call global
    cm_unit <- "W_m2"
  } else {
    timehorz <- timehorz$AGTP
    cm_mat <- AGTPMat
    cm_unit <- "K"
  }
  
  #Create conversion factor for calculating CO2e
  CO2unity_kg <- c(1000, rep(0, 99)) #100-year time series of one MT CO2, in kg, all emitted in year 1
  CO2e_cf <- cm_mat[["CO2"]] %*% as.matrix(CO2unity_kg) #Create climate metric time series of a pulse of 1MT CO2 in year 1
  CO2e_cf <- CO2e_cf[timehorz] #Grab the value at the desired time horizon. This is a single float value.

  #Calculate CO2e
  if(is.na(func_unit)) {
    CO2e <- sum(case[[paste0("CO2.", cm)]][[paste0("total.CO2_", cm_unit)]][[timehorz]],
                case[[paste0("CH4.", cm)]][[paste0("total.CH4_", cm_unit)]][[timehorz]],
                case[[paste0("N2O.", cm)]][[paste0("total.N2O_", cm_unit)]][[timehorz]]) / CO2e_cf
    case[[paste0("CO2e.",cm,".",as.character(timehorz),"yr_MT")]] <- CO2e
  } else {
    CO2e <- sum(case[[paste0("CO2.", cm)]][[paste0("total.CO2_", cm_unit, "per", func_unit)]][[timehorz]],
                case[[paste0("CH4.", cm)]][[paste0("total.CH4_", cm_unit, "per", func_unit)]][[timehorz]],
                case[[paste0("N2O.", cm)]][[paste0("total.N2O_", cm_unit, "per", func_unit)]][[timehorz]]) / CO2e_cf
    case[[paste0("CO2e.",cm,".",as.character(timehorz),"yr","_MTper", func_unit)]] <- CO2e
  }
  
  
  return(case)
}


#' @description divide gross emissions numbers by the chosen function unit and rename columns
#'
#' @note This function depends on an output `functional_list` from the getFunctionalUnit()
#'
#' @param case output of climateMetrics() Emissions values in list strucutre
#' @param functional_value as returned by getFunctionalUnit() {list value}  # I think this could be improved upon for clarity
#'
#' @return case of same length as input with different units
#'
# function unit function
calcFunctionalUnit <- function(case,
                               func_val=1,
                               func_unit=NA) {
  
  # Make sure a functional unit is passed
  if(is.na(func_unit)) {
    stop("A functional unit was not passed. Stopping execution")
  }
  
  # divide by functional unit value
  CO2.mass <- case$CO2.mass[, lapply(.SD, function(x) x / func_val)] # divide by functional value
  CH4.mass <- case$CH4.mass[, lapply(.SD, function(x) x / func_val)]
  N2O.mass <- case$N2O.mass[, lapply(.SD, function(x) x / func_val)]
  PM2.5.mass <- case$PM2.5.mass[, lapply(.SD, function(x) x / func_val)]
  CO.mass = case$CO.mass[, lapply(.SD, function(x) x / func_val)]
  NOx.mass = case$NOx.mass[, lapply(.SD, function(x) x / func_val)]
  PM10.mass = case$PM10.mass[, lapply(.SD, function(x) x / func_val)]
  VOC.mass = case$VOC.mass[, lapply(.SD, function(x) x / func_val)]
  SO2.mass = case$SO2.mass[, lapply(.SD, function(x) x / func_val)]
  BC.mass = case$BC.mass[, lapply(.SD, function(x) x / func_val)]
  char.mass = case$char.mass[, lapply(.SD, function(x) x / func_val)]
  
  # rename data tables to reflect different unit
  colnames(CO2.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(CO2.mass))
  colnames(CH4.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(CH4.mass))
  colnames(N2O.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(N2O.mass))
  colnames(PM2.5.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(PM2.5.mass))
  colnames(CO.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(CO.mass))
  colnames(NOx.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(NOx.mass))
  colnames(PM10.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(PM10.mass))
  colnames(VOC.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(VOC.mass))
  colnames(SO2.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(SO2.mass))
  colnames(BC.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(BC.mass))
  colnames(char.mass) <- gsub("tonnes", paste0("MTper", func_unit), colnames(char.mass))
  
  # Divide climate metrics by functional unit and rename to reflect change
  # Select all climate metric values
  CO2.AGWP <- case$CO2.AGWP[, lapply(.SD, function(x) x / func_val)]
  CH4.AGWP <- case$CH4.AGWP[, lapply(.SD, function(x) x / func_val)]
  N2O.AGWP <- case$N2O.AGWP[, lapply(.SD, function(x) x / func_val)]
  
  CO2.AGTP <- case$CO2.AGTP[, lapply(.SD, function(x) x / func_val)]
  CH4.AGTP <- case$CH4.AGTP[, lapply(.SD, function(x) x / func_val)]
  N2O.AGTP <- case$N2O.AGTP[, lapply(.SD, function(x) x / func_val)]
  
  # rename climate metrics to reflect change in units
  colnames(CO2.AGWP) <- gsub("W_m2", paste0("W_m2per", func_unit), colnames(CO2.AGWP))  
  colnames(CH4.AGWP) <- gsub("W_m2", paste0("W_m2per", func_unit), colnames(CH4.AGWP))
  colnames(N2O.AGWP) <- gsub("W_m2", paste0("W_m2per", func_unit), colnames(N2O.AGWP))
  
  colnames(CO2.AGTP) <- gsub("K", paste0("Kper", func_unit), colnames(CO2.AGTP))
  colnames(CH4.AGTP) <- gsub("K", paste0("Kper", func_unit), colnames(CH4.AGTP))
  colnames(N2O.AGTP) <- gsub("K", paste0("Kper", func_unit), colnames(N2O.AGTP))
  
  # replace values in input data.table
  case$CO2.mass <- CO2.mass
  case$CH4.mass <- CH4.mass
  case$N2O.mass <- N2O.mass
  case$PM2.5.mass <- PM2.5.mass
  case$CO.mass <- CO.mass
  case$NOx.mass <- NOx.mass
  case$PM10.mass <- PM10.mass
  case$VOC.mass <- VOC.mass
  case$SO2.mass <- SO2.mass
  case$BC.mass <- BC.mass
  case$char.mass <- char.mass
  
  case$CO2.AGWP <- CO2.AGWP
  case$CH4.AGWP <- CH4.AGWP
  case$N2O.AGWP <- N2O.AGWP
  
  case$CO2.AGTP <- CO2.AGTP
  case$CH4.AGTP <- CH4.AGTP
  case$N2O.AGTP <- N2O.AGTP
  
  # return case with new values
  return(case)
}



#' @name getFunctionalUnit
#' @note NOT USED ANYMORE moved to different workflow, was part of climate_metrics()
#' @description extract the functional unit value based on the functional unit from the use case
#' This function is an attempt to separate out the functional unit dependency from the climate metric function
#' Allowing ref cases to be run without a use case to compare
#'
#' @param use.case A specific use case as identified in an analysis
#' @param functional_unit: a character string defining what the functional unit will be. Must be one of the
#     following options: "MT_Recovered","MT_Delivered","kWh" {string}
#' @param functional_unit_value: either NULL (to calculate in this function) or a float that contains the value that is desired
#'
#' @return list object of functional unit and functional_value. In most cases functional_unit will be the same as the input string {list}
#' 
getFunctionalUnit <- function(use.case,
                              functional_unit,
                              functional_unit_value) {
  
  #Get functional unit numeric value if it was not passed as a value (i.e. was passed as NULL)
  if(is.null(functional_unit_value)) {
    if(functional_unit == "MT_Recovered") {
      functional_unit_value <- use.case[[2]]$field.residue.removed_tonnes
      
      if(functional_unit_value < 1E-3) { #If less than 1 kg
        warning(print(paste0("Recovered residue is ",
                             as.character(functional_unit_value),
                             " metric tons. Perhaps you chose a non-mobilization case for your use case. Cannot create a per-MT emissions value. Reporting total emissions.")))
        functional_unit_value <- as.integer(1)
        functional_unit <- NA
      }
    } else if(functional_unit == "MT_Delivered") {
      
      functional_unit_value <- use.case[[2]]$residue.burned.to.electricity_tonnes
      
      if(functional_unit_value < 1E-3) { #If less than 1 kg
        warning(print(paste0("Residue delivered to the powerplant is ",
                             as.character(functional_unit_value),
                             " metric tons. Perhaps you chose a non-mobilization case for your use case. Cannot create a per-MT emissions value. Reporting total emissions.")))
        functional_unit_value <- as.integer(1)
        functional_unit <- NA
      }
    } else if(functional_unit == "kWh") {
      functional_unit_value <- use.case[[2]]$pp_energy.production_MWh * 1000 #convert to kWh
      
      if(functional_unit_value < 1 | is.null(functional_unit_value)) { #If less than 1 kWh
        warning(print(paste0("Total kWh generated is ",
                             as.character(functional_unit_value),
                             " kWh. Perhaps you chose a non-mobilization case for your use case. Cannot create a per-kWh emissions value. Reporting total emissions.")))
        functional_unit_value <- as.integer(1)
        functional_unit <- NA
      }
    } else {
      stop(print(paste0("Functional_unit variable is set to an incorrect value of ",as.character(functional_unit),". Please correct. Ending script!")))
    }
  } else if(functional_unit == "MT_Delivered") {
    
    if(functional_unit_value < 1E-3) { #If less than 1 kg
      warning(print(paste0("Residue delivered to the powerplant is ",
                           as.character(functional_unit_value),
                           " metric tons. Perhaps you chose a non-mobilization case for your use case. Cannot create a per-MT emissions value. Reporting total emissions.")))
      functional_unit_value <- as.integer(1)
      functional_unit <- NA
    }
  } else if(functional_unit == "kWh") {
    functional_unit_value <- use.case[[2]]$pp_energy.production_MWh * 1000 #convert to kWh
    # functional_unit_value <- use.case[[2]]$field.residue.removed_tonnes * 1000 #REMOVE THIS AFTER SFSS TALK ON 11/14
    
    if(functional_unit_value < 1) { #If less than 1 kWh
      warning(print(paste0("Total kWh generated is ",
                           as.character(functional_unit_value),
                           " kWh. Perhaps you chose a non-mobilization case for your use case. Cannot create a per-kWh emissions value. Reporting total emissions.")))
      functional_unit_value <- as.integer(1)
      functional_unit <- NA
    }
  } else {
    stop(print(paste0("Functional_unit variable is set to an incorrect value of ",as.character(functional_unit),". Please correct. Ending script!")))
  }
  
  return(list("functional_unit_value" = functional_unit_value,
              "functional_unit" = functional_unit))
}


#' @name scenarioExpand
#' @description Load the scenario lookup
#' If the `expand_attribute` is fraction piled, this value will be static between use and reference cases. 
#' The Silvicultural Treatment is always static between use/ref
#' If the `expand_attribute` is silvicultural, the fraction piled will vary between use and reference
#' 
#' @param matrix cbrec case matrix with all information about cases
#' @param expand_attribute column within matrix to pair on 
#' 
#' 
#' @TODO finish function
#' 
genScenarios <- function(matrix, expand_attribute) {
  # packages used in function
  require(data.table)
  require(dplyr)
  require(tidyr)
  require(praise)

  # A scenario compares two cases. This creates a lookup table that pairs all appropriate use and reference cases
  #   Label all cases with "No" collection as reference cases. All other cases are use cases.
  #   Organize into the expanded table of scenarios
  # if(scenario_expand_within == "disposition_and_presburn"){
  #   scenario_lookup <-
  #     case_matrix %>%
  #     filter(Pulp.Market == "No") %>%
  #     mutate(case = case_when(Biomass.Collection == "No" ~ "Ref",
  #                             T ~ "Use")) %>%
  #     spread(key = case, value = ID,drop = T) %>%
  #     select(Silvicultural.Treatment,Fraction_Piled_Residues,Burn.Type,Biomass.Collection,Ref,Use) %>%
  #     group_by(Silvicultural.Treatment) %>%
  #     distinct() %>% # get rid of one of the slope case, they have same ID numbers anyway
  #     group_by(Silvicultural.Treatment,Fraction_Piled_Residues,Burn.Type) %>%
  #     expand(Ref, Use) %>%
  #     filter(!is.na(Ref) , !is.na(Use)) %>%
  #     left_join(case_matrix %>% select(Use = ID,Biomass.Collection) %>% distinct() %>%filter(Biomass.Collection != "No")) %>%
  #     select(Silvicultural.Treatment,Fraction_Piled_Residues,Burn.Type,Biomass.Collection,Ref,Use) %>%
  #     setDT()
  # }
  
  # warning message
  if(!expand_attribute %in% c("fraction_piled", "silviculture")) {stop("'expand_attribute' must be one of: fraction_piled, silviculture")}
    
  # logic to pair use and reference cases
  # TODO this needs to be checked as it was written specific to the THP analysis
  if(expand_attribute == "fraction_piled"){
    scenario_lookup <-
      matrix %>%
      filter(Pulp.Market == "No") %>% # may have to change with updated matrix
      mutate(case = case_when(Biomass.Collection == "No" ~ "Ref",
                              T ~ "Use")) %>%
      spread(key = case, value = ID, drop = T) %>%
      select(Silvicultural.Treatment, Fraction_Piled_Residues, Biomass.Collection, Ref, Use) %>%
      group_by(Silvicultural.Treatment) %>%
      distinct() %>% # get rid of one of the slope case, they have same ID numbers anyway
      group_by(Silvicultural.Treatment, Fraction_Piled_Residues) %>%
      expand(Ref, Use) %>%
      filter(!is.na(Ref) , !is.na(Use)) %>% # remove rows with NAs
      left_join(matrix %>%
                  select(Use = ID, Biomass.Collection) %>%
                  distinct() %>%
                  filter(Biomass.Collection != "No")) %>%
      select(Silvicultural.Treatment,Fraction_Piled_Residues,Biomass.Collection,Ref,Use) %>%
      setDT()
  }
  
  #  Expand within each silviculture treatment
  if(expand_attribute == "silviculture"){
    scenario_lookup <-
      matrix %>%
      filter(Pulp.Market == "No") %>%
      mutate(case = case_when(Biomass.Collection == "No" ~ "Ref",
                              T ~ "Use")) %>%
      spread(key = case, value = ID,drop = T) %>%
      select(Silvicultural.Treatment,Fraction_Piled_Residues,Burn.Type,Biomass.Collection,Ref,Use) %>%
      group_by(Silvicultural.Treatment) %>%
      distinct() %>% # get rid of one of the slope case, they have same ID numbers anyway
      group_by(Silvicultural.Treatment) %>%
      expand(Ref, Use) %>%
      filter(!is.na(Ref) , !is.na(Use)) %>%
      left_join(matrix %>% select(Use = ID,Fraction_Piled_Residues) %>% distinct()) %>%
      left_join(matrix %>% select(Use = ID,Biomass.Collection) %>% distinct() %>%filter(Biomass.Collection != "No")) %>%
      select(Silvicultural.Treatment,Fraction_Piled_Residues,Biomass.Collection,Ref,Use) %>%
      setDT()
  }
  
  # Set keys for joining
  matrix <- as.data.table(matrix)
  setkey(matrix,ID)
  setkey(scenario_lookup, Ref)
 
  # join reference burn type
  scenario_lookup <- matrix[Slope=="LT40",.(ID,Burn.Type)][scenario_lookup] # add Ref ID and burn type
  setnames(scenario_lookup,c("ID","Burn.Type"),c("Ref","Ref.Burn.Type"))
  
  # key on use
  setkey(scenario_lookup, Use)
  
  # join use burn type
  scenario_lookup <- matrix[Slope=="LT40",.(ID,Burn.Type)][scenario_lookup] # add Use ID and burn type
  setnames(scenario_lookup,c("ID","Burn.Type"),c("Use","Use.Burn.Type"))
  
  # Subset based on burn type pairings
  scenario_lookup <- scenario_lookup[(Ref.Burn.Type=="No" & Use.Burn.Type=="No") |
                                       (Ref.Burn.Type=="Broadcast" & Use.Burn.Type=="Broadcast") |
                                       (Ref.Burn.Type=="Pile" & Use.Burn.Type=="None") |
                                       (Ref.Burn.Type=="Pile and Broadcast" & Use.Burn.Type=="Broadcast"),
                                     .SD]
  print(praise())
  
  return(scenario_lookup)
}

