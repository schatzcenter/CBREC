# **********************************************************************************
# This is a collection of constants used throughout CBREC-LCA.
# **********************************************************************************

# Conversion Factors -----------------------------------------------------------------------

sq_meters_per_acre <<- 4046.86 # square meters per acre
acres_per_cell <<- 900 / sq_meters_per_acre # 30 m x 30 m cell converted to acres
miles_per_meter <<- 0.000621371
miles_per_kilometer <<- miles_per_meter * 1000
tonnes_per_ton <<- 0.907185
kg_per_tonne <<- 1000 # conversion of metric tonnes to kilograms
BTU_per_MJ <<- 947.817 # 1 MJ = 947.817 BTUs
MJ_per_MWh <<- 3600
days_per_month <<- 365.25 / 12

# Carbon Fractions for Emissions Species Containing Carbon ---------------------------------

# (Mass emissions species) / (Mass carbon contained in species)
CO2_carbon_fraction <<- 44.009/12.011
CH4_carbon_fraction <<- 16.0426/12.011
CO_carbon_fraction <<- 28.010/12.011
PM10_carbon_fraction <<- 1/0.74 # Turn et al. 1997, Table 4
VOC_carbon_fraction <<- 1/0.68
char_carbon_fraction <<- 1/0.83 # Jindo et al., 2014, Table 2, value for oak at 800C

#Black Carbon Fractions for PM2.5 from Different Sources ------------------------------------------------------------

# (Mass PM2.5) / (Mass black carbon contained in PM2.5)
PM2.5_BC_wildfire <<- 0.2 # value - From text of above PDF
PM2.5_BC_RXburn_forest <<- 0.202594 # value - Forest Mananegment Burning
PM2.5_BC_RXburn_orchard <<- 0.22457
PM2.5_BC_RXburn_straw <<- 0.193183
PM2.5_BC_offroaddiesel <<- 0.610165
PM2.5_BC_onroaddiesel <<- 0.181326 # value - Heavy Duty Diesel Truck cruising
PM2.5_BC_powerplant <<- 0.3 # value - Static Internal Combustion Engine - solid Fuel
PM2.5_BC_ng_offset <<- 0.13 # ID 1120: Gas fired boilers and steam generators

# Drip Torch --------------------------------------------------------------

# fuel used (gal/acre)
# "Personal Communication, Jeremy Bailey, Fire Training and Network Coordinator at The Nature Conservancy.
gal_fuel_per_acre_pile <- 0.064
gal_fuel_per_acre_broad <- 0.15

# fuel mixture assumed to be 3:1 diesel/gasoline
diesel_mix <- .75
gas_mix <- .25

# emissions factors for diesel and gasoline come from the EPA:
# https://www.epa.gov/sites/production/files/2015-07/documents/emission-factors_2014.pdf
# Diesel assumed to be distillate fuel oil No. 2
CO2_diesel_grams = 10210
CH4_diesel_grams = 0.41
N2O_diesel_grams = 0.08

# Gasoline assumed to be motor gasoline
CO2_gas_grams = 8780
CH4_gas_grams = 0.38
N2O_gas_grams = 0.08

# Power Plants -----------------------------------------------------

cogen_eff <<- 0.80 # natural gas boiler efficiency; based on CPUC QF/CHP Settlement
# ng_MMBTU_to_MWH = 8300 # BTU to energy conversion; based on CPUC QF/CHP Settlement

# Natural Gas Equivalent
# https://www3.epa.gov/ttnchie1/ap42/ch01/ Section 1.4
# kilogram / MMBTU
ng_chp_co2 = 53.36376471
ng_chp_co = 0.037354635
ng_chp_ch4 = 0.001022805
ng_chp_nox = 0.035798192
ng_chp_n2o = 0.000284607
ng_chp_pm2.5 = 0.002534779
ng_chp_voc = 0.002445839
ng_chp_so2 = 0.000266819

# Unburnt fuel estimates for different power plant technologies
#   Biomass plant type will have a set percentage of unburned fuel. Build a reference table for that here
#   CWH: added integrated gasification and combustion and uses fluidized bed combustor value
biomass_plant_unburnt_fuel <<- data.table(plant.type = c("Biomass stoker",
                                                         "Fluidized bed combustor",
                                                         "Cyclone combustor",
                                                         "Gasifier",
                                                         "Integrated gasification and combustion"),
                                          unburned.fuel.frac = c(0.035,
                                                                 0.0025,
                                                                 0.03,
                                                                 0.0025,
                                                                 0.0025))

# N2O and SO2 Emissions Fractions ------------------------------------------------------------

#   (Mass emissions species) / (Mass NOx or SOx emitted)
plant_SO2_SOx_fraction <<- 0.97 # percent of SOx emitted as SO2, power plant
equipment_SO2_SOx_fraction <<- 0.97 # percent of SOx emitted as SO2, harvest/processing equipment
fire_N2O_NOx_fraction <<- 0.0568 # Percent of NOx emitted as N2O, prescribed burn and wildfire; based on average burn values from CARB

# Pile Burn Mass Fate Fractions --------------------------------------------------------------

# From CBREC-Fire, the combustion/char/unburned fractions for pile burns are fixed at 90% / 1% / 9%. This holds for all size classes.
pile.burn.combustion.frac <<- 0.9 # Fraction of exposed piled mass that is combusted to airborne emissions in a RX pile burn
pile.burn.char.frac <<- 0.01 # Fraction of exposed piled mass that is combusted to char remaining on the ground in a RX pile burn

# In-Field Decay ---------------------------------------------------

duff_decay_mass_fraction <<- 0.02 # (Mass as Duff) / (Mass Lost from Decay)
duff_k_val <<- 0.002 # Decay constant for duff for exponential decay function
CH4_decay_emissions_fraction <<- 1E-5 # (Mass Carbon as CH4) / (Mass Carbon Lost from Decay). Applied to scattered and piled woody material (forest and ag).
foliage_to_duff_trigger <<- 0.5 # (Mass Foliage Remaining) / (Original Mass Foliage) value below which all mass foliage remaining is moved to duff

# Decay of Power Plant Storage Piles ----------------------------------------------------

avg_storage_time_months <<- 6 # Average months residues are stored at power plant before combustion (must be <= 12 as emissions are allocated to first year)
CH4_decay_emissions_fraction_storage <<- CH4_decay_emissions_fraction # (Mass Carbon as CH4) / (Mass Carbon Lost from Decay)

# Forest treatment name convention ---------------------------------

# Create table correlating silvicultural treatment names with their associated codes in CBREC-Fire
#   CBREC-LCA refers to the silvicultural treatments by code; CBREC-Fire file names use full names.
#   This will allow us to move between the two easily.
treatment_name_lookup <<- data.table(treat.code=c("RM100",
                                                  "TFA20",
                                                  "TFA40",
                                                  "TFA60", 
                                                  "TFA80",
                                                  "TFB20",
                                                  "TFB40",
                                                  "TFB60",
                                                  "TFB80",
                                                  "TP20",
                                                  "TP40",
                                                  "TP60",
                                                  "TP80"),
                                     treat.name=c("Clearcut",
                                                  "20_Thin_From_Above",
                                                  "40_Thin_From_Above",
                                                  "60_Thin_From_Above", 
                                                  "80_Thin_From_Above", 
                                                  "20_Thin_From_Below",
                                                  "40_Thin_From_Below",
                                                  "60_Thin_From_Below",
                                                  "80_Thin_From_Below",
                                                  "20_Proportional_Thin",
                                                  "40_Proportional_Thin",
                                                  "60_Proportional_Thin",
                                                  "80_Proportional_Thin"))

# Processing and Collection ----------------------------------------

residue_slope_cutoff <<- 80 # Slope (%) above which collection of residues is not allowed
density.threshold <<- 13 # BDT/acre. Threshold at which a project is defined as "high volume" and associated equipment is applied
ave_harvest_acres <<- 25 # acres. Associated with an average "catch basin" for in-field processing equipment. This is used to determine a multiplier for equipment hauling emissions.
harvest_tonnes_per_person_day <<- 16.04 # tonnes / person-day. From Barrett, S., Bolding, M., & Munsell, J. (2017), reported 224.62 tonne per crew per week, 2.8 people per crew, and assume 5 days per week
crew_commute_dist <<- 80.5 # km. Assumption of 50 miles
equip_commute_dist <<- 80.5 # km. Assumption of 50 miles
equip_km_per_hour <<- 58 # km/hr. Equivalent to ~36 MPH
transfer_point_distance <<- 8.04672 # kilometers, equal to 5 miles. Additional hauling distance used if project includes a transfer point
unloaded_truck_eff_improve <<- 0.15 # Assume 15% fewer emissions (15% greater efficiency) on the return trip back to the field

# Associated with set() function in year 1 calculations in run-cbrec.R

firecols <<- c('CWD_Scattered_CombustionFrac',
          'FWD_Scattered_CombustionFrac',
          'Foliage_Scattered_CombustionFrac',
          'Duff_Scattered_CombustionFrac',
          'CWD_Scattered_CharFrac',
          'FWD_Scattered_CharFrac',
          'Duff_Scattered_CH4_EmFac',
          'Foliage_Scattered_CH4_EmFac',
          'FWD_Scattered_CH4_EmFac',
          'CWD_Scattered_CH4_EmFac',
          'Piled_CH4_EmFac',
          'Duff_Scattered_CO_EmFac',
          'Foliage_Scattered_CO_EmFac',
          'FWD_Scattered_CO_EmFac',
          'CWD_Scattered_CO_EmFac',
          'Piled_CO_EmFac',
          'Duff_Scattered_NOx_EmFac',
          'Foliage_Scattered_NOx_EmFac',
          'FWD_Scattered_NOx_EmFac',
          'CWD_Scattered_NOx_EmFac',
          'Piled_NOx_EmFac',
          'Duff_Scattered_PM10_EmFac',
          'Foliage_Scattered_PM10_EmFac',
          'FWD_Scattered_PM10_EmFac',
          'CWD_Scattered_PM10_EmFac',
          'Piled_PM10_EmFac',
          'Duff_Scattered_PM2.5_EmFac',
          'Foliage_Scattered_PM2.5_EmFac',
          'FWD_Scattered_PM2.5_EmFac',
          'CWD_Scattered_PM2.5_EmFac',
          'Piled_PM2.5_EmFac',
          'Duff_Scattered_SO2_EmFac',
          'Foliage_Scattered_SO2_EmFac',
          'FWD_Scattered_SO2_EmFac',
          'CWD_Scattered_SO2_EmFac',
          'Piled_SO2_EmFac',
          'Duff_Scattered_VOC_EmFac',
          'Foliage_Scattered_VOC_EmFac',
          'FWD_Scattered_VOC_EmFac',
          'CWD_Scattered_VOC_EmFac',
          'Piled_VOC_EmFac')
