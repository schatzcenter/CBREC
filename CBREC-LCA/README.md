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
+-- CBREC-LCA_input_filepaths.csv  # Contains file paths to all required inputs. Note that some inputs
|                                  # point to the C-BREC Fire Module file structure.
+-- functions                      # Multiple R scripts that contain functions and constants
+-- input                          # Contains nearly all of the inputs to the module, with the
|                                  # exception of reference to the C-BREC Fire Module (see
|                                  # CBREC-LCA_input_filepaths.csv).
|   +-- case_definitions           # Directory containing .csv files that contain definitions
|                                  # of cases, and list of cases to run
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
        - future.apply 
        - data.table
        - sf 
* The CBREC project folder, including CBREC-Fire, CBREC-LCA, and associated inputs to each.
* The following minimum recommended resources for flexibility in spatial resolution and extent of the intended run. Note that required resources are dictated by the spatial resolution (the size of the individual project(s) being run) and the spatial extent (the fraction of the area of the State of California) chosen by the user. The following are based on running a large number of projects (10^4 or more) or a statewide run at low spatial resolution (50+ regions).
  * At least 24 logical CPU cores at >2GHz each. This results in a roughly 1 month runtime.
  * At least 160GB RAM, more is required for higher CPU core count.
  * At least 4TB storage to support both the total size of input data and total size of output data. This is not thoroughly tested. More storage is better.
        
Packages can be installed as follows:

```
install.packages("data.table")
```

Other packages and software are required to reproduce the entire project. Packages are loaded at the beginning of every script where possible. All scripts have a description header.

### Input Data

All static input data required to run C-BREC LCA Module can be downloaded at one of the DOI links below (choose the correct DOI for the version you are running):

* v1.2.1 and later: [![DOI](https://sandbox.zenodo.org/badge/DOI/10.5072/zenodo.899028.svg)](https://doi.org/10.5072/zenodo.899028)

Note that output from the [C-BREC Fire Module](https://github.com/schatzcenter/CBREC/tree/master/CBREC-Fire) is also a required input. Hence, the user must first run the C-BREC Fire Module prior to running the C-BREC LCA module. The relative file path for the outputs of the C-BREC Fire Module is specified in the CBREC-LCA_input_filepaths.csv file which is one of the input data files that can be found via the above Zenodo DOI link. See the Usage section below for more details.

### Usage

Unlike the C-BREC Fire Module, the C-BREC LCA Module is project-based. The C-BREC LCA Module can be run repeatedly on single projects and/or batches of projects. Each run is assumed to have different inputs (i.e. different values in the input file) and different output directory paths specified.

There are two steps to running the C-BREC LCA Module:
1. Execute main model script `run_CBREC-LCA.R`
2. Execute post processing script `run_post-processing.R`

#### Executing `run_CBREC-LCA.R`

The main model script generates 100 year time series of gross emissions for each specified case, disaggregated by source and emission species. The following steps describe how to execute this script.

1. If it hasn't already been run, make sure to run the [C-BREC Fire Module](https://github.com/schatzcenter/CBREC/tree/master/CBREC-Fire) first. The output from the C-BREC Fire Module is a required input for this module. The C-BREC Fire Module only needs to be run once because it generates results for the entire State of California.

2. Create a polygon shapefile that contains the shapes for each project that C-BREC LCA will be run on. The shapefile can contain any number of polygons, thereby allowing a batch run of multiple projects if desired. Furthermore, each shape doesn't necessarily need to represent a project per se. It simply defines a region over which residue mobilization is implemented. For example, the shapefile can contain shapes that divide up the entire State of California into different regions, such as eco-regions.

   a. Note that there is a script flag `useactivitycodes` (described below) that can be set if the user wishes to specify the type of primary silvacultural treatment that is conducted on each project location. If this is set to true, this requires that the polygon shapefile contain a `treat_code` attribute that contains this definition. The allowed values for `treat_code` are:
   
      i.    RM100: Remove 100% of basal area
      ii.   TFA20: Thin from above 20% of basal area
      iii.  TFA40: Thin from above 40% of basal area
      iv.   TFA60: Thin from above 60% of basal area
      v.    TFA80: Thin from above 80% of basal area
      vi.   TFB20: Thin from below 20% of basal area
      vii.  TFB40: Thin from below 40% of basal area
      viii. TFB60: Thin from below 60% of basal area
      ix.   TFB80: Thin from below 80% of basal area
      x.    TFP20: Proportionally thin 20% of basal area
      xi.   TFP40: Proportionally thin 40% of basal area
      xii.  TFP60: Proportionally thin 60% of basal area
      xiii. TFP80: Proportionally thin 80% of basal area

3. Update the CBREC-LCA_input_filepaths.csv file to reflect name and location of the polygon shapefile.

4. Choose whether to execute run_CBREC-LCA.R in an IDE environment (such as RStudio) or at the command line. It is recommended that large batch runs be executed at the command line to avoid IDE overhead consuming additional unnecessary computer resources. Command line flag options are:

```
-c, --cores             # Specify the number of CPU cores to utilize [default parallel:detectCores()/2]
-d, --debug             # Print extra output for debugging (currently not fully implemented) [default F]
-i, --infile            # Relative path to .csv file specifying inputs [default "CBREC-LCA/input/CBREC-LCA_input_filepaths.csv"]
-o, --outdir            # Relative output directory path [default "Cbrec-LCA/output/default-output-dir/"]
-u, --useactivitycodes  # If running project polygons with specified treatment activity codes, set to T [default F]
-v, --verbose           # Print extra output [default F]
```

#### Executing `run_post-processing.R`

Once `run_CBREC-LCA.R` is finished running, gross emissions for each specified case must be processed to generate net mass of emissions and climate metrics for specified scenarios.

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
|        +-- CandP_dry_grind
|           +-- collection.processing.diesel.CO2_tonnes
|           +-- collection.processing.diesel.CO_tonnes
|           +-- collection.processing.diesel.N2O_tonnes
|           +-- collection.processing.diesel.CH4_tonnes
|           +-- collection.processing.diesel.NOx_tonnes
|           +-- collection.processing.diesel.PMUnder10um_tonnes
|           +-- collection.processing.diesel.PMUnder2.5um_tonnes
|           +-- collection.processing.diesel.BC_tonnes
|           +-- collection.processing.diesel.SO2_tonnes
|           +-- collection.processing.diesel.VOC_tonnes
|           +-- transportation.onroad.diesel.CO2_tonnes
|           +-- transportation.onroad.diesel.CO_tonnes
|           +-- transportation.onroad.diesel.N2O_tonnes
|           +-- transportation.onroad.diesel.CH4_tonnes
|           +-- transportation.onroad.diesel.NOx_tonnes
|           +-- transportation.onroad.diesel.PMUnder10um_tonnes
|           +-- transportation.onroad.diesel.PMUnder2.5um_tonnes
|           +-- transportation.onroad.diesel.BC_tonnes
|           +-- transportation.onroad.diesel.SO2_tonnes
|           +-- transportation.onroad.diesel.VOC_tonnes
|           +-- transportation.offroad.diesel.CO2_tonnes
|           +-- transportation.offroad.diesel.CO_tonnes
|           +-- transportation.offroad.diesel.N2O_tonnes
|           +-- transportation.offroad.diesel.CH4_tonnes
|           +-- transportation.offroad.diesel.NOx_tonnes
|           +-- transportation.offroad.diesel.PMUnder10um_tonnes
|           +-- transportation.offroad.diesel.PMUnder2.5um_tonnes
|           +-- transportation.offroad.diesel.BC_tonnes
|           +-- transportation.offroad.diesel.SO2_tonnes
|           +-- transportation.offroad.diesel.VOC_tonnes
|           +-- transportation.onroad.diesel.distance_km
|        +-- CandP_green_chip
|           +-- ...
|        +-- CandP_green_grind
|           +-- ...
|        +-- CandP_low
|           +-- ...
|     +-- PP
|        +-- Nearest
|           +-- plant.type
|           +-- plant_location
|           +-- pp_residue_delivered_tonnes
|           +-- pp_residue_burned_tonnes
|           +-- pp_waste_flyash_ash_tonnes
|           +-- pp_storage.CH4_tonnes
|           +-- pp_storage.CO2_tonnes
|           +-- pp_energy.production_MWh
|           +-- pp_energy.production_CogenMMBtu
|           +-- pp_residue.burned.to.cogen.heat_tonnes
|           +-- ng_off_CO2_tonnes
|           +-- ng_off_CO_tonnes
|           +-- ng_off_CH4_tonnes
|           +-- ng_off_NOx_tonnes
|           +-- ng_off_N2O_tonnes
|           +-- ng_off_PMUnder2.5um_tonnes
|           +-- ng_off_SO2_tonnes
|           +-- ng_off_VOC_tonnes
|           +-- ng_off_BC_tonnes
|           +-- pp_waste_flyash_char.tonnes
|           +-- pp_electricity.CO_tonnes
|           +-- pp_electricity.N2O_tonnes
|           +-- pp_electricity.CH4_tonnes
|           +-- pp_electricity.VOC_tonnes
|           +-- pp_electricity.NOx_tonnes
|           +-- pp_electricity.SO2_tonnes
|           +-- pp_electricity.PMUnder10um_tonnes
|           +-- pp_electricity.PMUnder2.5um_tonnes
|           +-- pp_electricity.CO2_tonnes
|           +-- pp_electricity.BC_tonnes
|        +-- CurrentGenCombustionPlantDefault
|           +-- ...
|        +-- CurrentGenIG/CombustionPlantDefault
|           +-- ...
|        +-- NextGenThermochemicalPlantDefault
|           +-- ...
|        +-- LessThan1MWPlant
|           +-- ...
|     +-- BroadcastBurn
|           +-- broadcast.burn.CO2_tonnes
|           +-- broadcast.burn.CO_tonnes
|           +-- broadcast.burn.CH4_tonnes
|           +-- broadcast.burn.NOx_tonnes
|           +-- broadcast.burn.N2O_tonnes
|           +-- broadcast.burn.PMUnder10um_tonnes
|           +-- broadcast.burn.PMUnder2.5um_tonnes
|           +-- broadcast.burn.SO2_tonnes
|           +-- broadcast.burn.VOC_tonnes
|           +-- broadcast.burn.BC_tonnes
|           +-- broadcast.burned.residue_tonnes
|     +-- PileBurn
|           +-- pile.burn.CO2_tonnes
|           +-- pile.burn.CO_tonnes
|           +-- pile.burn.CH4_tonnes
|           +-- pile.burn.NOx_tonnes
|           +-- pile.burn.N2O_tonnes
|           +-- pile.burn.PMUnder10um_tonnes
|           +-- pile.burn.PMUnder2.5um_tonnes
|           +-- pile.burn.SO2_tonnes
|           +-- pile.burn.VOC_tonnes
|           +-- pile.burn.BC_tonnes
|           +-- pile.burned.residue_tonnes
|  +-- postTreatment
|     +-- In.field.non.char.scattered_tonnes
|     +-- In.field.non.char.piled_tonnes
|     +-- In.field.char.scattered_tonnes
|     +-- In.field.char.piled_tonnes
|     +-- wildfire.burned.residue_tonnes
|     +-- decayed.residue_tonnes
|     +-- wildfire.CO2_tonnes
|     +-- wildfire.CO_tonnes
|     +-- wildfire.CH4_tonnes
|     +-- wildfire.NOx_tonnes
|     +-- wildfire.N2O_tonnes
|     +-- wildfire.PMUnder10um_tonnes
|     +-- wildfire.PMUnder2.5um_tonnes
|     +-- wildfire.SO2_tonnes
|     +-- wildfire.VOC_tonnes
|     +-- wildfire.BC_tonnes
|     +-- decay.CO2_tonnes
|     +-- decay.CH4_tonnes
```

### Versioning

We use [git](https://git-scm.com/) for version control on this project. For a complete history, see the [this repository](https://github.com/SchatzCenter/CBREC).

Code releases are automatically archived and assigned a DOI using [Zenodo](https://zenodo.org). The latest DOI badge is included on the repository page.

Release versions (on GitHub and Zenodo) will always correlate. Furthermore, the release version will always correlate with the framework documentation version this is included in this repository.

### Authors

* Andy Harris (Lead Author) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Jerome Qiriazi](https://github.com/jqiriazi) (Project Manager) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Max Blasdel](https://github.com/mxblsdl) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Micah Wright](https://github.com/wrightmicahc) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Chih-Wei Hsu](https://www.linkedin.com/in/chihweihsu/) - [Schatz Energy Research Center](https://schatzcenter.org)

### Acknowledgments

* <list acknowledgments>

* This project was funded by the California Energy Commission's (CEC) Electric Program Investment Charge (EPIC) program under contract agreement EPC-16-047. DISCLAIMER: This source code was prepared as the result of work sponsored by the California Energy Commission. It does not necessarily represent the views of the CEC, its employees, or the State of California. The CEC, the State of California, its employees, contractors, and subcontractors make no warrant, express or implied, and assume no legal liability for the information in this report; nor does any party represent that the uses of this information will not infringe upon privately owned rights. This report has not been approved or disapproved by the California Energy Commission, nor has the California Energy Commission passed upon the accuracy or adequacy of the information in this report.
