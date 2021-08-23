**California Biomass Residue Emissions Characterization (C-BREC) Model**
===================================================================

## Life Cycle Assessment Module

This module is part of the [California Biomass Residue Emissions Characterization (C-BREC) Model](https://schatzcenter.org/cbrec). Additional methodological documentation can be found on the C-BREC Model website. The C-BREC model was originally developed as part of the [California Biopower Impact Project](https://schatzcenter.org/cbip/). The remote repository for the model can be found at [github.com/SchatzCenter/CBREC](https://github.com/SchatzCenter/CBREC).

This module calculates life cycle gross and net:

* Greenhouse gas emissions
* Climate impact metrics: global warming potential and global temperature potential
* Criteria air pollutants

under different silvacultural treatments and biomass utilization scenarios. This module allows specification of the following key characteristics at the individual project level:

* Location of residue generation
* Type of forest treatment or harvest activity being conducted
* Baseline residue disposition (piled or scattered)
* Location of residue utilization
* Reference fate of unremoved biomass (prescribed burn or left in place)
* Supply chain characteristics

### Installation

Download or fork the repository from [github.com/SchatzCenter/CBREC](https://github.com/SchatzCenter/CBREC). 

### File Structure

The file structure is shown in the following tree. 

```
CBREC-LCA                          # Main module directory
+-- CBREC-LCA_input_filepaths.csv  # Contains file paths to all required inputs for the
|                                  # `run_CBREC-LCA.R` script. Note that some inputs point to the
|                                  # C-BREC Fire Module file structure.
+-- functions                      # Multiple R scripts that contain functions and constants
+-- input                          # Contains nearly all of the inputs to the module, with the
|                                  # exception of reference to the C-BREC Fire Module (see
|                                  # CBREC-LCA_input_filepaths.csv).
|   +-- case_definitions           # Directory containing .csv files that contain definitions
|                                  # of cases, list of cases to run, and allowed pairings of cases
|                                  # which specify use/reference scenarios.
|   +-- equip_pp_emissions         # Directory containing .csv files of equipment and power
|                                  # plant emissions factors
|   +-- pp_decay_wfprob            # Directory containing .rds files for each tile ID in the tile
|                                  # shape file (see the fire_tiles_filepath variable in
|                                  # CBREC-LCA_input_filepaths.csv). Each .rds file contains hauling
|                                  # distance to and ID of the nearest power plant, decay constants,
|                                  # and annual wildfire probability for each raster cell ID (see
|                                  # residue_resource directory).
|   +-- project_polygons           # Directory containing shapefile of polygons that define
|                                  # the project or projects that will be run
|   +-- residue_resource           # Directory containing a spatial raster of cell ID numbers for the
|                                  # State of Calfornia, and .csv files containing biomass properties
|                                  # and residue amounts for each raster cell.
+-- output                         # Directory for holding output from both the main program script
|                                  # and the post-processing script.
+-- README.md                      # This readme
+-- run_CBREC-LCA.R                # main C-BREC LCA program script
+-- run_post-processing.R          # calculates net emissions and climate impact metrics
```

### Prerequisites

This project was written in R using the Rstudio project framework.

#### Software requirements

The following is required:

* A current version of R
* The following R packages and their dependencies:
        - data.table
        - future.apply
        - optparse
        - raster
        - sf
        - tictoc
* The CBREC project folder, including CBREC-Fire, CBREC-LCA, and associated inputs to each.
* The following minimum recommended resources for flexibility in spatial resolution and extent of the intended run. Note that required resources are dictated by the spatial resolution (the size of the individual project(s) being run) and the spatial extent (the fraction of the area of the State of California) chosen by the user. The following are based on running a large number of projects (10^4 or more) or a statewide run at low spatial resolution (50+ regions).
  * At least 24 logical CPU cores at >2GHz each. This results in a roughly 1 month runtime.
  * At least 160GB RAM, more is required for higher CPU core count.
  * At least 4TB storage to support both the total size of input data and total size of output data. This is not thoroughly tested. More storage is better.
        
Packages can be installed as follows:

```
install.packages("data.table")
```

#### Input Data

All static input data required to run both the C-BREC Fire Module and the C-BREC LCA Module can be downloaded via Zenodo. See main repository [README.md](https://github.com/schatzcenter/CBREC/tree/master#input-data) for the link. The download preserves the required directory structure. It also includes the code associated with the release correlated to it. If downloading the input data for use in a later release, do not use the R scripts included in the data download.

Note that output from the [C-BREC Fire Module](https://github.com/schatzcenter/CBREC/tree/master/CBREC-Fire) is also a required input. Hence, the user must first run the C-BREC Fire Module prior to running the C-BREC LCA module. The relative file path for the outputs of the C-BREC Fire Module is specified in the CBREC-LCA_input_filepaths.csv file. See the Usage section below for more details.

### Usage

Unlike the C-BREC Fire Module, the C-BREC LCA Module is project-based. The C-BREC LCA Module can be run repeatedly on single projects and/or batches of projects. Each run is assumed to have different inputs (i.e. different values in the input file) and different output directory paths specified.

There are two steps to running the C-BREC LCA Module:
1. Execute main model script `run_CBREC-LCA.R`
2. Execute post processing script `run_post-processing.R`

All scripts are designed to be sourced relative to the main CBREC folder.

#### Executing `run_CBREC-LCA.R`

The main model script generates 100 year time series of gross emissions for each specified case, disaggregated by source and emission species. The following steps describe how to execute this script.

0. If it hasn't already been downloaded, download the necessary input data. See the [parent README.md](https://github.com/schatzcenter/CBREC) for details.

1. If it hasn't already been run, make sure to run the [C-BREC Fire Module](https://github.com/schatzcenter/CBREC/tree/master/CBREC-Fire) first. The output from the C-BREC Fire Module is a required input for this module. The C-BREC Fire Module only needs to be run once because it generates results for the entire State of California.

2. Create a polygon shapefile that contains the shapes for each project that C-BREC LCA will be run on. There is an example shapefile in the downloadable input data referenced in Step 1 above. The shapefile can contain any number of polygons, thereby allowing a batch run of multiple projects if desired. Furthermore, each shape doesn't necessarily need to represent a project per se. It simply defines a region over which residue mobilization is implemented. For example, the shapefile can contain shapes that divide up the entire State of California into different regions, such as eco-regions.

   Note that there is a script flag `useactivitycodes` (described below) that can be set if the user wishes to specify the type of primary silvacultural treatment that is conducted on each project location. If this is set to true, this requires that the polygon shapefile contain a `treat_code` attribute that contains this definition. If this flag is set to false the model will not look for the `treat_code` attribute and will run all possible silvacultural treatments for each polygon in the shapefile. The allowed values for `treat_code` are:
   
```
Clearcut              # Remove 100% of basal area
20_Thin_From_Above    # Thin from above 20% of basal area
40_Thin_From_Above    # Thin from above 40% of basal area
60_Thin_From_Above    # Thin from above 60% of basal area
80_Thin_From_Above    # Thin from above 80% of basal area
20_Thin_From_Below    # Thin from below 20% of basal area
40_Thin_From_Below    # Thin from below 40% of basal area
60_Thin_From_Below    # Thin from below 60% of basal area
80_Thin_From_Below    # Thin from below 80% of basal area
20_Proportional_Thin  # Proportionally thin 20% of basal area
40_Proportional_Thin  # Proportionally thin 40% of basal area
60_Proportional_Thin  # Proportionally thin 60% of basal area
80_Proportional_Thin  # Proportionally thin 80% of basal area
```

3. Update the CBREC-LCA_input_filepaths.csv file to reflect name and location of the polygon shapefile. Make any other file path updates if needed.

4. Choose whether to execute run_CBREC-LCA.R in an IDE environment (such as RStudio) or at the command line. It is recommended that large batch runs be executed at the command line to avoid IDE overhead consuming additional unnecessary computer resources. Command line flag options are:

```
NAME
      run_CBREC-LCA.R
SYNOPSIS
      run_CBREC-LCA.R [OPTION]...
DESCRIPTION
      Execute the C-BREC LCA Module
      Mandatory arguments to long options are mandatory for short options too
      -c, --cores             # Specify the number of CPU cores to utilize [default parallel:detectCores()/2]
      -d, --debug             # Print extra output for debugging (currently not fully implemented) [default F]
      -i, --infile            # Relative path to .csv file specifying inputs [default "CBREC-LCA/input/CBREC-LCA_input_filepaths.csv"]
      -o, --outdir            # Relative output directory path [default "Cbrec-LCA/output/default-output-dir/"]
      -u, --useactivitycodes  # If running project polygons with specified treatment activity codes, set to T [default F]
      -v, --verbose           # Print extra output [default F]
```

#### Executing `run_post-processing.R`

Once `run_CBREC-LCA.R` is finished running, gross emissions for each specified case must be processed to generate net mass of emissions and climate metrics for specified scenarios. This post processing script is designed to be run in an IDE environment.

### Output description

Outputs of the `run_CBREC-LCA.R` script will be located in the specified output directory. This will have the following directory structure:

```
+-- results     # Contains .rds files, one for each shape ID in the specified project polygon shapefile
+-- runlogs     # Contains runlogs
```

Each of the .rds files has the following nested list structure

```
+-- <case ID>
|  +-- treatment
|     +-- field.residue.removed_tonnes
|     +-- total.biomass.mobilized_tonnesAcre
|     +-- CandP
|        +-- CandP_dry_grind                                      # Collection and processing emissions for low moisture, grind comminution, large project (>= 1,000 BDT and >= 13 BDT per acre)
|           +-- collection.processing.diesel.CO2_tonnes           # single double value occurring in the first project year
|           +-- collection.processing.diesel.CO_tonnes            # single double value occurring in the first project year
|           +-- collection.processing.diesel.N2O_tonnes           # single double value occurring in the first project year
|           +-- collection.processing.diesel.CH4_tonnes           # single double value occurring in the first project year
|           +-- collection.processing.diesel.NOx_tonnes           # single double value occurring in the first project year
|           +-- collection.processing.diesel.PMUnder10um_tonnes   # single double value occurring in the first project year
|           +-- collection.processing.diesel.PMUnder2.5um_tonnes  # single double value occurring in the first project year
|           +-- collection.processing.diesel.BC_tonnes            # single double value occurring in the first project year
|           +-- collection.processing.diesel.SO2_tonnes           # single double value occurring in the first project year
|           +-- collection.processing.diesel.VOC_tonnes           # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.CO2_tonnes           # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.CO_tonnes            # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.N2O_tonnes           # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.CH4_tonnes           # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.NOx_tonnes           # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.PMUnder10um_tonnes   # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.PMUnder2.5um_tonnes  # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.BC_tonnes            # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.SO2_tonnes           # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.VOC_tonnes           # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.CO2_tonnes          # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.CO_tonnes           # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.N2O_tonnes          # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.CH4_tonnes          # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.NOx_tonnes          # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.PMUnder10um_tonnes  # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.PMUnder2.5um_tonnes # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.BC_tonnes           # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.SO2_tonnes          # single double value occurring in the first project year
|           +-- transportation.offroad.diesel.VOC_tonnes          # single double value occurring in the first project year
|           +-- transportation.onroad.diesel.distance_km          # single double value representing distance to Nearest power plant
|        +-- CandP_green_chip                                     # Collection and processing emissions for high moisture, chipping comminution, and large project (>= 1,000 BDT and >= 13 BDT per acre)
|           +-- ... identical structure as CandP_dry_grind
|        +-- CandP_green_grind                                    # Collection and processing emissions for high moisture, grind comminution, and large project (>= 1,000 BDT and >= 13 BDT per acre)
|           +-- ... identical structure as CandP_dry_grind
|        +-- CandP_low                                            # Collection and processing emissions for low moisture, grind comminution, and small project (< 1,000 BDT or < 13 BDT per acre)
|           +-- ... identical structure as CandP_dry_grind
|     +-- PP
|        +-- Nearest                                              # Emissions associated with combustion of mobilized material in the nearest powerplant
|           +-- plant.type                                        # Vector specifying power plant technology (projects over very large areas can send portion of material to different plants).
|           +-- plant_location                                    # Vector of names of existing power plants (projects over very large areas can send portion of material to different plants).
|           +-- pp_residue_delivered_tonnes                       # single double value occurring in the first project year
|           +-- pp_residue_burned_tonnes                          # single double value occurring in the first project year
|           +-- pp_waste_flyash_ash_tonnes                        # single double value occurring in the first project year
|           +-- pp_storage.CH4_tonnes                             # single double value occurring in the first project year
|           +-- pp_storage.CO2_tonnes                             # single double value occurring in the first project year
|           +-- pp_energy.production_MWh                          # single double value occurring in the first project year
|           +-- pp_energy.production_CogenMMBtu                   # single double value occurring in the first project year
|           +-- pp_residue.burned.to.cogen.heat_tonnes            # single double value occurring in the first project year
|           +-- ng_off_CO2_tonnes                                 # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- ng_off_CO_tonnes                                  # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- ng_off_CH4_tonnes                                 # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- ng_off_NOx_tonnes                                 # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- ng_off_N2O_tonnes                                 # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- ng_off_PMUnder2.5um_tonnes                        # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- ng_off_SO2_tonnes                                 # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- ng_off_VOC_tonnes                                 # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- ng_off_BC_tonnes                                  # estimated natural gas offset by Cogen. Single double value occurring in the first project year
|           +-- pp_waste_flyash_char.tonnes                       # single double value occurring in the first project year
|           +-- pp_electricity.CO_tonnes                          # single double value occurring in the first project year
|           +-- pp_electricity.N2O_tonnes                         # single double value occurring in the first project year
|           +-- pp_electricity.CH4_tonnes                         # single double value occurring in the first project year
|           +-- pp_electricity.VOC_tonnes                         # single double value occurring in the first project year
|           +-- pp_electricity.NOx_tonnes                         # single double value occurring in the first project year
|           +-- pp_electricity.SO2_tonnes                         # single double value occurring in the first project year
|           +-- pp_electricity.PMUnder10um_tonnes                 # single double value occurring in the first project year
|           +-- pp_electricity.PMUnder2.5um_tonnes                # single double value occurring in the first project year
|           +-- pp_electricity.CO2_tonnes                         # single double value occurring in the first project year
|           +-- pp_electricity.BC_tonnes                          # single double value occurring in the first project year
|        +-- CurrentGenCombustionPlantDefault                     # Emissions associated with combustion of mobilized material in a current generation generic combustion plant
|           +-- ... identical structure as Nearest
|        +-- CurrentGenIG/CombustionPlantDefault                  # Emissions associated with combustion of mobilized material in a current generation generic integrated gasification and combustion plant
|           +-- ... identical structure as Nearest
|        +-- NextGenThermochemicalPlantDefault                    # Emissions associated with combustion of mobilized material in a next generation generic thermochemical plant
|           +-- ... identical structure as Nearest
|        +-- LessThan1MWPlant                                     # Emissions associated with combustion of mobilized material in a generic small gasification plant
|           +-- ... identical structure as Nearest
|     +-- BroadcastBurn                                           # Emissions associated with a prescribed broadcast burn. Will be empty if the particular case does not allow for a prescribed broadcast burn. Broadcast burns only occur in the first year.
|           +-- broadcast.burn.CO2_tonnes                         # single double value occurring in the first project year
|           +-- broadcast.burn.CO_tonnes                          # single double value occurring in the first project year
|           +-- broadcast.burn.CH4_tonnes                         # single double value occurring in the first project year
|           +-- broadcast.burn.NOx_tonnes                         # single double value occurring in the first project year
|           +-- broadcast.burn.N2O_tonnes                         # single double value occurring in the first project year
|           +-- broadcast.burn.PMUnder10um_tonnes                 # single double value occurring in the first project year
|           +-- broadcast.burn.PMUnder2.5um_tonnes                # single double value occurring in the first project year
|           +-- broadcast.burn.SO2_tonnes                         # single double value occurring in the first project year
|           +-- broadcast.burn.VOC_tonnes                         # single double value occurring in the first project year
|           +-- broadcast.burn.BC_tonnes                          # single double value occurring in the first project year
|           +-- broadcast.burned.residue_tonnes                   # single double value of total mass of residue burned in a broadcast burn
|     +-- PileBurn                                                # Emissions associated with a prescribed pile burn. Will be empty if the particular case does not allow for a prescribed pile burn. Pile burns only occur in the first year.
|           +-- pile.burn.CO2_tonnes                              # single double value occurring in the first project year
|           +-- pile.burn.CO_tonnes                               # single double value occurring in the first project year
|           +-- pile.burn.CH4_tonnes                              # single double value occurring in the first project year
|           +-- pile.burn.NOx_tonnes                              # single double value occurring in the first project year
|           +-- pile.burn.N2O_tonnes                              # single double value occurring in the first project year
|           +-- pile.burn.PMUnder10um_tonnes                      # single double value occurring in the first project year
|           +-- pile.burn.PMUnder2.5um_tonnes                     # single double value occurring in the first project year
|           +-- pile.burn.SO2_tonnes                              # single double value occurring in the first project year
|           +-- pile.burn.VOC_tonnes                              # single double value occurring in the first project year
|           +-- pile.burn.BC_tonnes                               # single double value occurring in the first project year
|           +-- pile.burned.residue_tonnes                        # single double value of total mass of residue burned in a pile burn
|  +-- postTreatment
|     +-- In.field.non.char.scattered_tonnes                      # 100-element vector of doubles representing mass of non-char scattered material at the beginning of each year, prior to exposure to prescribed burn (first year only), decay and wildfire.
|     +-- In.field.non.char.piled_tonnes                          # 100-element vector of doubles representing mass of non-char piled material at the beginning of each year, prior to exposure to prescribed burn (first year only), decay and wildfire. May contain zeros if the case ID does not allow any piled material.
|     +-- In.field.char.scattered_tonnes                          # 100-element vector of doubles representing mass of char from combustion of scattered material after prescribed burn (first year only) and wildfire.
|     +-- In.field.char.piled_tonnes                              # 100-element vector of doubles representing mass of char from combustion of piled material after prescribed burn (first year only) and wildfire. May contain zeros if the case ID does not allow any piled material.
|     +-- wildfire.burned.residue_tonnes                          # 100-element vector of doubles representing mass of residue exposed to wildfire (pre-combustion) after prescribed burn (first year only) and exposure to decay.
|     +-- decayed.residue_tonnes                                  # 100-element vector of doubles representing mass of residue lost to decay after prescribed burn (first year only) but before exposure to wildfire.
|     +-- wildfire.CO2_tonnes                                     # 100-element vector of doubles
|     +-- wildfire.CO_tonnes                                      # 100-element vector of doubles
|     +-- wildfire.CH4_tonnes                                     # 100-element vector of doubles
|     +-- wildfire.NOx_tonnes                                     # 100-element vector of doubles
|     +-- wildfire.N2O_tonnes                                     # 100-element vector of doubles
|     +-- wildfire.PMUnder10um_tonnes                             # 100-element vector of doubles
|     +-- wildfire.PMUnder2.5um_tonnes                            # 100-element vector of doubles
|     +-- wildfire.SO2_tonnes                                     # 100-element vector of doubles
|     +-- wildfire.VOC_tonnes                                     # 100-element vector of doubles
|     +-- wildfire.BC_tonnes                                      # 100-element vector of doubles
|     +-- decay.CO2_tonnes                                        # 100-element vector of doubles
|     +-- decay.CH4_tonnes                                        # 100-element vector of doubles
+-- <case ID>
|   +-- ... above structure replicated for the next case ID
+-- ... repeat for all case IDs that are specified for the run
```

Outputs from the `run_CBREC-LCA.R` script are then used as inputs to the `run_post-processing.R` script. The output structure of the `run_post-processing.R` script is scenario-based:
```
+-- <use case ID> x <reference case ID>
|  +-- use                                              # Results associated with the use case, represented by <use case ID> in list name
|     +-- MT_Residue_Mobilized
|     +-- MT_Residue_Mobilized_perAcre
|     +-- MT_Residue_Delivered
|     +-- MT_Residue_Burned
|     +-- MWh_Generated
|     +-- CO2.mass
|        +-- pp_storage.CO2_tonnes                      # 100-element vector of doubles
|        +-- pp_electricity.CO2_tonnes                  # 100-element vector of doubles
|        +-- collection.processing.diesel.CO2_tonnes    # 100-element vector of doubles
|        +-- transportation.onroad.diesel.CO2_tonnes    # 100-element vector of doubles
|        +-- transportation.offroad.diesel.CO2_tonnes   # 100-element vector of doubles
|        +-- wildfire.CO2_tonnes                        # 100-element vector of doubles
|        +-- decay.CO2_tonnes                           # 100-element vector of doubles
|        +-- total.CO2_tonnes                           # 100-element vector of doubles
|     +-- CH4.mass
|        +-- identical structure as CO2.mass
|     +-- N2O.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- PM2.5.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- CO.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- NOx.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- PM10.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- VOC.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- SO2.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- BC.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- char.mass
|        +-- identical structure as CO2.mass, except no value for pp_storage or decay
|     +-- CO2.AGWP
|        +-- pp_storage.CO2_W_m2                        # 100-element vector of doubles
|        +-- pp_electricity.CO2_W_m2                    # 100-element vector of doubles
|        +-- collection.processing.diesel.CO2_W_m2      # 100-element vector of doubles
|        +-- transportation.onroad.diesel.CO2_W_m2      # 100-element vector of doubles
|        +-- transportation.offroad.diesel.CO2_W_m2     # 100-element vector of doubles
|        +-- wildfire.CO2_W_m2                          # 100-element vector of doubles
|        +-- decay.CO2_W_m2                             # 100-element vector of doubles
|        +-- total.CO2_W_m2                             # 100-element vector of doubles
|     +-- CH4.AGWP
|        +-- identical structure as CO2.AGWP
|     +-- N2O.AGWP
|        +-- identical structure as CO2.AGWP, except no value for pp_storage or decay
|     +-- CO2.AGTP
|        +-- pp_storage.CO2_K                           # 100-element vector of doubles
|        +-- pp_electricity.CO2_K                       # 100-element vector of doubles
|        +-- collection.processing.diesel.CO2_K         # 100-element vector of doubles
|        +-- transportation.onroad.diesel.CO2_K         # 100-element vector of doubles
|        +-- transportation.offroad.diesel.CO2_K        # 100-element vector of doubles
|        +-- wildfire.CO2_K                             # 100-element vector of doubles
|        +-- decay.CO2_K                                # 100-element vector of doubles
|        +-- total.CO2_K                                # 100-element vector of doubles
|     +-- CH4.AGTP
|        +-- identical structure as CO2.AGTP
|     +-- N2O.AGTP
|        +-- identical structure as CO2.AGTP, except no value for pp_storage or decay
|     +-- CO2e.AGWP.###yr_MT                            # single double value, where "###" is the time horizon in years specified by the user.
|     +-- CO2e.AGTP.###yr_MT                            # single double value, where "###" is the time horizon in years specified by the user.
|  +-- ref                                              # Results associated with the reference case, represented by <ref case ID> in list name
|     +-- identical structure as use
|  +-- net                                              # Results associated with the use case minus the reference case
|     +-- net.mass
|        +-- net.CO2_tonnes                             # 100-element vector of doubles
|        +-- net.CH4_tonnes                             # 100-element vector of doubles
|        +-- net.N2O_tonnes                             # 100-element vector of doubles
|        +-- net.PM2.5_tonnes                           # 100-element vector of doubles
|        +-- net.CO_tonnes                              # 100-element vector of doubles
|        +-- net.NOx_tonnes                             # 100-element vector of doubles
|        +-- net.PM10_tonnes                            # 100-element vector of doubles
|        +-- net.VOC_tonnes                             # 100-element vector of doubles
|        +-- net.SO2_tonnes                             # 100-element vector of doubles
|        +-- net.BC_tonnes                              # 100-element vector of doubles
|        +-- net.char_tonnes                            # 100-element vector of doubles
|        +-- net.CO2_W_m2                               # 100-element vector of doubles, net AGWP profile
|        +-- net.CH4_W_m2                               # 100-element vector of doubles, net AGWP profile
|        +-- net.N2O_W_m2                               # 100-element vector of doubles, net AGWP profile
|        +-- net.CO2_K                                  # 100-element vector of doubles, net AGTP profile
|        +-- net.CH4_K                                  # 100-element vector of doubles, net AGTP profile
|        +-- net.N2O_K                                  # 100-element vector of doubles, net AGTP profile
|     +-- net.CO2e.AGWP.###yr_MT                        # single double value, where "###" is the time horizon in years specified by the user.
|     +-- net.CO2e.AGTP.###yr_MT                        # single double value, where "###" is the time horizon in years specified by the user.
```

### Versioning

We use [git](https://git-scm.com/) for version control on this project. For a complete history, see the [this repository](https://github.com/SchatzCenter/CBREC).

### Authors

* [Andrew Harris](https://github.com/arharris) (Lead Developer) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Max Blasdel](https://github.com/mxblsdl) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Jerome Qiriazi](https://github.com/jqiriazi) (Corresponding Author, Project Manager) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Micah Wright](https://github.com/wrightmicahc) - [Humboldt State University Department of Forestry & Wildland Resource](https://fwr.humboldt.edu/) and [Schatz Energy Research Center](https://schatzcenter.org)
* [Chih-Wei Hsu](https://www.linkedin.com/in/chihweihsu/) - [Schatz Energy Research Center](https://schatzcenter.org)
* Kevin Fingerman (Principal Investigator) - [Humboldt State University Department of Environmental Science and Management](https://environment.humboldt.edu/)

### Acknowledgments

* The authors would like to thank the California Energy Commission's (CEC) Electric Program Investment Charge (EPIC) program for its support of this research under contract EPC-16-047, and in particular Commission Agreement Managers Katharina Gerber and David Stoms for their indispensable assistance. The C-BREC Model would not have been possible without the contributions and commitment of the following members and organizations:
  - [Schatz Energy Research Center](https://schatzcenter.org): Cassidy Barrientos, Carisse Geronimo, Sabrinna Rios-Romero, Mark Severy, and Eli Wallach
  - [Natural Resource Spatial Informatics Group](https://nrsig.org): Luke Rogers and Jeff Comnick
  - [Consortium for Research on Renewable Industrial Materials](https://corrim.org/): Elaine Oneil and Maureen Puettmann

* This research was much improved by the ongoing input and support of the members of our Technical Advisory Committee and in particular of its chair, Andrea Tuttle.

* DISCLAIMER: This source code was prepared as the result of work sponsored by the California Energy Commission (CEC). It does not necessarily represent the views of the CEC, its employees, or the State of California. The CEC, the State of California, its employees, contractors, and subcontractors make no warrant, express or implied, and assume no legal liability for the information in this report; nor does any party represent that the uses of this information will not infringe upon privately owned rights. This report has not been approved or disapproved by the California Energy Commission, nor has the California Energy Commission passed upon the accuracy or adequacy of the information in this report.
