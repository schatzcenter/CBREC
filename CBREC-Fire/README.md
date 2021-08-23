**California Biomass Residue Emissions Characterization (C-BREC) Model**
===================================================================

## Wildfire and RX Burn Emissions Module

This module is part of the [California Biomass Residue Emissions Characterization (C-BREC) Model](https://schatzcenter.org/cbrec). Additional methodological documentation can be found on the C-BREC Model website. The C-BREC model was originally developed as part of the [California Biopower Impact Project](https://schatzcenter.org/cbip/). The remote repository for this model can be found at [github.com/SchatzCenter/CBREC](https://github.com/SchatzCenter/CBREC).

This module calculates wildfire and RX burn emissions under different silvacultural treatments and biomass utilization scenarios. Outputs from this module are used as inputs to the Life Cycle Assessment Module.

### Installation

Download or fork the repository from [github.com/SchatzCenter/CBREC](https://github.com/SchatzCenter/CBREC). 

### File Structure

The file structure for the Wildfire and RX burn Emissions Module is shown in the following tree. 

```
CBREC-Fire                        # main module directory
+-- functions                     # core functions
|   +-- Consume                   # R translation of subset of Consume 4.2 functions
+-- input                         # main input data directory 
|   +-- FCCS                      # FCCS fuels data
|   +-- lookup_tables             # .csv files that define, for each case, the decimal
|                                   fraction of residues that exist as scattered and piled
|   +-- residue                   # .csv files that contain the total mass of residues, in
|                                   U.S. tons per acre, for each silvacultural treatment and
|                                   FCID raster cell, disaggregated by tree component.
|   +-- tiles                     # contains shape file that spatially breaks up the state of
|                                   California into tiles. C-BREC Fire parallelizes over these,
|                                   and generates results disaggregated by tile.
|      +-- tabulated_spatial_data # Contains .rds files for each tile ID in the tile shape file.
|                                   Each .rds file contains wind correction factors, decay
|                                   constants, and fuel moisture by size class for each FCID
|                                   raster cell.
+-- output                        # stores model outputs
|   +-- emissions                 # stores emissions data used by the Life Cycle Assessment Module.
|   +-- residual_fuels            # stores mass of residuel fuels for additional results reporting.
+-- README.md                     # readme
+-- run_CBREC-Fire.R              # main program script
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
* The CBREC Rstudio project folder, including CBREC-Fire and associated input.
* At least 24 logical CPU cores at >2GHz each for a reasonable run time, more is better.
* At least 64GB RAM, more is required for higher CPU core count.
* At least 3TB storage. This has not been thoroughly tested. More storage is better.
        
Packages can be installed as follows:

```
install.packages("data.table")
```

#### Input Data

All static input data required to run both the C-BREC Fire Module and the C-BREC LCA Module can be downloaded via Zenodo. See main repository [README.md](https://github.com/schatzcenter/CBREC/tree/master#input-data) for the link. The download preserves the required directory structure. It also includes the code associated with the release correlated to it. If downloading the input data for use in a later release, do not use the R scripts included in the data download.

### Summary Overview

The C-BREC Fire Module is designed to generate prescribed burn and wildfire emissions profiles from the combustion of residues only (not including standing trees or merchantable timber, and not including the fuel bed except where explicitly labeled as such). Furthermore, this module is designed to generate results for the entire State of California. In other words, unlike the C-BREC LCA Module, this module is not project based. It is only necessary to run this module once, where as the C-BREC LCA Module is designed to be run numerous times for different inputs.

The process for calculating the consumption and emissions for a single tile over 5 timesteps (0, 25, 50, 75, and 100) is shown below. 

1. The case lookup table is loaded. The following steps are run for each case.

2. The FCCS fuelbed, residue, and spatial attributes data are loaded and joined into a single data.table.

3. Mid-flame windspeeds are estimated from Gridmet 10m windspeeds.

4. Piled residue mass is calculated for each fuel size class. If the year is one of the later timesteps (25-100), this is adjusted for decay.

5. Scattered residue mass is calculated for each fuel size class. If the timestep is one of 25-100, this is adjusted for decay.

6. The mass consumed by size class is estimated for the scenario and timestep. RX burns only occur during timestep 0, all subsequent fires for that timestep are wildfires. Wildfires in later timesteps assume that the fuelbed is recovered and that no wildfire has occurred to that point.

7. If the scenario is an RX burn, the fuelbed is adjusted so only unconsumed fuel remains, and the fuelbed is burned again in the same year as a wildfire. All subsequent timesteps

8. The emissions and residual fuels (unconsumed) for each scenario and timestep are estimated.

9. Output emissions and residual fuels are saved as .rds files, mass units are grams per acre.

### Usage

All scripts are designed to be sourced relative to the main CBREC folder. The steps to calculate emissions for a single tile are shown below. In this example, emissions and residual (unconsumed) fuel are estimated for a fixed set of scenarios over five time steps over a 100-year period for tile number 300. For ease of use when running on multiple tiles, the run_all function wraps the scenario_emissions function, which in turn wraps all other functions. For efficiency, the scenario_emissions function implements parallel processing using the future.apply package, which is platform independent. 

```
source("CBREC-Fire/run_CBREC-Fire.R")

run_all(300)
```

Tile number must match one of the ID numbers of the tiles located in "CBREC-Fire/input/tiles/cbrec-fire-tiles.shp". There are 11,990 tiles, each of which is approximately ~2669 hectares in size. The tile ID numbers can be extracted using the following snippet:

```
tiles <- sf::st_read("CBREC-Fire/input/tiles/cbrec-fire-tiles.shp")
tiles$ID
```

All outputs are saved to the "CBREC-Fire/output" folder as data.tables in .rds format. The subfolder "emissions" contains wildfire emissions estimates, the subfolder "residual_fuels" contains the residual fuels data. Each tile has case-specific outputs stored in a folder named for the tile number. The following code demonstrates how to access the results.

```
emissions <- readRDS("CBREC-Fire/output/emissions/300/20_Proportional_Thin-None-70-30-first-No-No-49-300-0.rds")

residual_fuels <- readRDS("CBREC-Fire/output/residual_fuels/300/20_Proportional_Thin-None-70-30-first-No-No-49-300-0.rds")
```

The run_all function can run any number of the tiles. The run_all function has an optional argument t_range, which is a integer vector or sequence of tile ID numbers. The default is NULL, which runs all tiles. WARNING: This takes quite a while to run, and requires a lot of disk space. There is a second argument save_runtime, which defaults to TRUE. This saves an .rds file with the runtime, "run_time.rds", in the main project directory.

```
source("CBREC-Fire/run_CBREC-Fire.R")

# runs all tiles
run_all()

# run first 100 tiles from Rstudio
run_all(t_range = 1:100)
```
### What it does

The scenario_emissions function sourced by run_all estimates fuel consumption, emissions, and residual fuels for all of the specified fixed cases. The individual cases to run are specified in "common-inputs/cases-to-run.csv". All cases are defined in "common-inputs/case-definitions.csv".

The function loads pre-processed spatial attributes that have been converted from raster to a tabular format with x-y location indicator columns. All spatial data use the California (Teale) Albers projetion. The CRS is below:

```
CRS arguments:
 +proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m
+no_defs +ellps=GRS80 +towgs84=0,0,0 
```

The existing fuelbed and treatment residue data are then appended to the spatial attribute data using FCCS and updated GNN FCID identifiers. The post-treatment fuelbed is then created by adding the treatment residue to the exisiting fuelbed using proportions assigned for each scenario. Residues are added as piled fuels or scattered fuels by fuel size class, accounting for decau. Burns are simulated on the updated fuelbeds at 25-year increments over a 100-year period for a total of five model runs in each wildfire scenario and six in the RX treatment scenarios. Both the emissions and residual fuels (treatment residue only) are saved following each simulation. Wildfire simulations assume that no previous wildfire has occurred. For scenarios that include an RX burn, the RX burn occurs at year 0, and all subsequent burns are modeled as wildfires that occur on 25-year timesteps from 0-100 years after the treatment. With the exception of the wildfire immediately following the RX treatment, these follow-up wildfires are simulated using the remaining treatment residue that has been added to a "recovered" fuelbed. Treatment residues are updated to reflect mass loss from decay prior to burning for all cases.

To estimate emissions, all consumed mass is multiplied by phase-specific (flaming, smoldering, and residual) FEPs emissons factors, which are taken from the [Bluesky modeling framework](https://github.com/pnwairfire/eflookup/blob/master/eflookup/fepsef.py) and from [Consume 4.2](https://www.fs.fed.us/pnw/fera/fft/consumemodule.shtml). Char production is also modeled as documented in the [C-BREC Model Framework](https://schatzcenter.org/cbrec). All mass converted to char is assumed to come from unconsumed fuel. The model output is then split into separate data.tables containing residual fuels and emissions estimates and is saved as .rds files.

### Output description

The scenario_emissions function saves two output files for each scenario:

1. emissions - this is the consumption and emissions estimates for each fuelbed in the tile.

2. residual_fuels - this is the remaining residue for each fuelbed in the tile.

These are saved as .rds files in folders of the same name located in "CBREC-Fire/output". File naming convention is folders for output type (emissions or residual fuels) and tile number, then silvicultural treatment, type of burn, fraction of residues piled and scattered, whether the burn was a secondary burn from wildfire following an RX treatment, whether biomass was collected for utilization, the existence of a pulp market, tile number, and year. File paths have "-" seperation. An example:

```
"CBREC-Fire/output/emissions/300/20_Proportional_Thin-None-70-30-first-No-No-49-300-0.rds"
```                                    

The specifics in the above example are described in the table below.

| Position|Value                |Attribute               | Description                                                                 |
|--------:|:--------------------|:-----------------------|:---------------------------------------------------------------------       |
|        1|20_Proportional_Thin |Silvicultural_Treatment |Silvaculture or thinning treatment                                           |
|        2|None                 |Burn_Type               |Type of burn, wildfire is "none", all others are RX                          |
|        3|70                   |Fraction_Piled          |Fraction of residues that are piled                                          |
|        4|30                   |Fraction_Scattered      |Fraction of residues that are scattered                                      |
|        5|first                |secondary_burn          |Was this a wildfire immediately following an RX treatment? No in this case   |
|        6|No                   |Biomass_Collection      |Were any of the residues removed from the fuelbed?                           |
|        7|No                   |Pulp_Market             |Was there a pulp market where smaller trees would have value?                |
|        8|49                   |ID                      |Scenario-specific ID number                                                  |
|        9|300                  |tile_number             |Tile ID number                                                               |
|       10|0                    |year                    |Year in the 100 year sequence                                                |

#### Emissions

The emissions table has the following columns. All mass outputs are in grams/acre, and are for the specified year / burn only (no values are cumulative). All masses are associated with the residues only (do not include the fuel bed), unless otherwise noted.

| Column                     | Description                                                                |
|----------------------------|----------------------------------------------------------------------------|
|x                           |x coordinate of cell.
|y                           |y coordinate of cell.
|fuelbed_number              |FCCS fuelbed number.
|FCID2018                    |UW FCID number for 2018.
|ID                          |Integer scenario ID number, unique to each scenario treatment combination.
|Silvicultural_Treatment     |Harvest or fuel treatment method applied.
|Burn_Type                   |RX burn type for the scenario, if applicable.
|Fraction_Piled              |Fraction of residues that are piled.
|Fraction_Scattered          |Fraction of residues that are scattered.
|Biomass_Collection          |Was the biomass collected for energy generation?
|Pulp_Market                 |Was there a pulp market?
|Secondary_Burn              |Either "first", the first burn of the year, or "second", a secondary wildfire following an RX burn. Burns with "second" only occur following RX burns in year 0.
|Year                        |Year in 100-year sequence.
|total_except_pile_char      |Char produced by scattered fuels in grams/acre including original fuelbed.
|total_except_pile_CH4       |CH4 produced by scattered fuels in grams/acre including original fuelbed. 
|total_except_pile_CO        |CO produced by scattered fuels in grams/acre including original fuelbed.
|total_except_pile_CO2       |CO2 produced by scattered fuels in grams/acre including original fuelbed.
|total_except_pile_NOx       |NOx produced by scattered fuels in grams/acre including original fuelbed.
|total_except_pile_PM10      |PM10 produced by scattered fuels in grams/acre including original fuelbed.
|total_except_pile_PM2.5     |PM2.5 produced by scattered fuels in grams/acre including original fuelbed.
|total_except_pile_SO2       |SO2 produced by scattered fuels in grams/acre including original fuelbed.
|total_except_pile_VOC       |VOC produced by scattered fuels in grams/acre including original fuelbed.
|total_pile_clean_PM10       |PM10 from piled fuels in grams/acre assuming clean piles.
|total_pile_vdirty_PM10      |PM10 from piled fuels in grams/acre assuming very dirty piles.
|total_pile_clean_PM2.5      |PM2.5 from piled fuels in grams/acre assuming clean piles.
|total_pile_vdirty_PM2.5     |PM2.5 from piled fuels in grams/acre assuming very dirty piles.
|total_pile_CH4              |CH4 from piled fuels in grams/acre.
|total_pile_CO               |CO from piled fuels in grams/acre.           
|total_pile_CO2              |CO2 from piled fuels in grams/acre.
|total_pile_NOx              |NOx from piled fuels in grams/acre.
|total_pile_SO2              |SO2 from piled fuels in grams/acre.
|total_pile_VOC              |VOC from piled fuels in grams/acre.
|pile_char                   |Char from piled fuels in grams/acre.
|char_fwd_residue            |Char from scattered fine woody debris (1-3") in grams/acre.
|char_cwd_residue            |Char from scattered coarse woody debris (>3") in grams/acre
|total_duff_exposed          |Duff exposed to fire that began as residue in grams/acre.
|total_foliage_exposed       |Residue foliage exposed to fire in grams/acre.
|total_fwd_exposed           |Scattered residue fine woody debris (1-3") exposed to fire in grams/acre.
|total_cwd_exposed           |Scattered residue coarse woody debris (>3") exposed to fire in grams/acre.
|total_fuel_consumed         |Total residue biomass consumed in grams/acre, including piled fuels.
|total_pile_consumed         |Total piled biomass consumed in grams/acre.
|total_duff_consumed         |Residue duff consumed in grams/acre.
|total_foliage_consumed      |Residue foliage consumed in grams/acre.
|total_fwd_consumed          |Scattered residue fine woody debris (1-3") consumed in grams/acre.
|total_cwd_consumed          |Scattered residue coarse woody debris (>3") consumed in grams/acre.
|total_duff_residue_CH4      |CH4 produced by residue duff in grams/acre.
|total_foliage_residue_CH4   |CH4 produced by residue foliage in grams/acre.
|total_fwd_residue_CH4       |CH4 produced by scattered residue fine woody debris (1-3") in grams/acre.
|total_cwd_residue_CH4       |CH4 produced by scattered residue coarse woody debris (>3") in grams/acre.
|total_duff_residue_CO       |CO produced by residue duff in grams/acre.
|total_foliage_residue_CO    |CO produced by residue foliage in grams/acre.
|total_fwd_residue_CO        |CO produced by scattered residue fine woody debris (1-3") in grams/acre.
|total_cwd_residue_CO        |CO produced by scattered residue coarse woody debris (>3") in grams/acre.
|total_duff_residue_CO2      |CO2 produced by residue duff in grams/acre.
|total_foliage_residue_CO2   |CO2 produced by residue foliage in grams/acre.
|total_fwd_residue_CO2       |CO2 produced by scattered residue fine woody debris (1-3") in grams/acre.
|total_cwd_residue_CO2       |CO2 produced by scattered residue coarse woody debris (>3") in grams/acre.
|total_duff_residue_NOx      |NOx produced by residue duff in grams/acre.
|total_foliage_residue_NOx   |NOx produced by residue foliage in grams/acre.
|total_fwd_residue_NOx       |NOx produced by scattered residue fine woody debris (1-3") in grams/acre.
|total_cwd_residue_NOx       |NOx produced by scattered residue coarse woody debris (>3") in grams/acre.
|total_duff_residue_PM10     |PM10 produced by residue duff in grams/acre.
|total_foliage_residue_PM10  |PM10 produced by residue foliage in grams/acre.
|total_fwd_residue_PM10      |PM10 produced by scattered residue fine woody debris (1-3") in grams/acre.
|total_cwd_residue_PM10      |PM10 produced by scattered residue coarse woody debris (>3") in grams/acre.
|total_duff_residue_PM2.5    |PM2.5 produced by residue duff in grams/acre.
|total_foliage_residue_PM2.5 |PM2.5 produced by residue foliage in grams/acre.
|total_fwd_residue_PM2.5     |PM2.5 produced by scattered residue fine woody debris (1-3") in grams/acre.
|total_cwd_residue_PM2.5     |PM2.5 produced by scattered residue coarse woody debris (>3") in grams/acre.
|total_duff_residue_SO2      |SO2 produced by residue duff in grams/acre.
|total_foliage_residue_SO2   |SO2 produced by residue foliage in grams/acre.
|total_fwd_residue_SO2       |SO2 produced by scattered residue fine woody debris (1-3") in grams/acre.
|total_cwd_residue_SO2       |SO2 produced by scattered residue coarse woody debris (>3") in grams/acre.
|total_duff_residue_VOC      |VOC produced by residue duff in grams/acre.
|total_foliage_residue_VOC   |VOC produced by residue foliage in grams/acre.
|total_fwd_residue_VOC       |VOC produced by scattered residue fine woody debris (1-3") in grams/acre.
|total_cwd_residue_VOC       |VOC produced by scattered residue coarse woody debris (>3") in grams/acre

#### Residual Fuels

The residual fuels table has the following columns. All residual fuels are in grams/acre.

| Column                  | Description                                                                |
|-------------------------|----------------------------------------------------------------------------|
|x                        |x coordinate of cell.
|y                        |y coordinate of cell.
|fuelbed_number           |FCCS fuelbed number.
|FCID2018                 |UW FCID number for 2018.
|ID                       |Integer scenario ID number, unique to each scenario treatment combination.
|Silvicultural_Treatment  |Harvest or fuel treatment method applied.
|Burn_Type                |RX burn type for the scenario, if applicable.
|Fraction_Piled           |Fraction of residues that are piled.
|Fraction_Scattered       |Fraction of residues that are scattered.
|Biomass_Collection       |Was the biomass collected for energy generation?
|Pulp_Market              |Was there a pulp market?
|Secondary_Burn           |Either "first", the first burn of the year, or "second", a secondary wildfire following an RX burn. Burns with "second" only occur following RX burns in year 0.
|Year                     |Year in 100-year sequence.
|Slope                    |Slope of pixel in percent.
|Fm10                     |10-hour fuel moisture in percent.
|Fm1000                   |1,000-hour fuel moisture in percent.
|Wind_corrected           |Corrected windspeed, miles per hour.
|duff_upper_loading       |Upper duff layer loading, residue only, in grams/acre.
|litter_loading           |Litter loading, residue only, in grams/acre.
|one_hr_sound             |One-hour (<=0.25 in.) fuel loading, residue only, in grams/acre.
|ten_hr_sound             |Ten-hour (0.26-1 in.) fuel loading, residue only, in grams/acre.           
|hun_hr_sound             |Hundred-hour (1.1-3 in.) fuel loading, residue only, in grams/acre.
|oneK_hr_sound            |One thousand-hour (3-9 in.) sound fuel loading, residue only, in grams/acre.
|oneK_hr_rotten           |One thousand-hour (3-9 in.) rotten fuel loading, residue only, in grams/acre.
|tenK_hr_sound            |Ten thousand-hour (9-20 in.) sound fuel loading, residue only, in grams/acre.
|tenK_hr_rotten           |Ten thousand-hour (9-20 in.) rotten fuel loading, residue only, in grams/acre.
|tnkp_hr_sound            |Greater than ten thousand-hour (>20 in.) sound fuel loading, residue only, in grams/acre.
|tnkp_hr_rotten           |Greater than ten thousand-hour (>20 in.) rotten fuel loading, residue only, in grams/acre.
|pile_field               |Field-piled residue of all size classes, in grams/acre.
|pile_landing             |Landing-piled residue of all size classes, in grams/acre.

### Versioning

We use [git](https://git-scm.com/) for version control on this project. For a complete history, see the [this repository](https://github.com/SchatzCenter/CBREC). 

## Authors

* [Micah Wright](https://github.com/wrightmicahc) (Lead Author) - [Humboldt State University Department of Forestry & Wildland Resource](https://fwr.humboldt.edu/) and [Schatz Energy Research Center](https://schatzcenter.org)
* Jeffrey Kane (Corresponding Author) - [Humboldt State University Department of Forestry & Wildland Resource](https://fwr.humboldt.edu/)
* [Andrew Harris](https://github.com/arharris) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Max Blasdel](https://github.com/mxblsdl) - [Schatz Energy Research Center](https://schatzcenter.org)
* [Jerome Qiriazi](https://github.com/jqiriazi) (Project Manager) - [Schatz Energy Research Center](https://schatzcenter.org)
* Kevin Fingerman (Principal Investigator) - [Humboldt State University Department of Environmental Science and Management](https://environment.humboldt.edu/)

### Acknowledgments

* The R consume scripts were originally translated directly into R from the original python code from [Consume 4.2](https://www.fs.fed.us/pnw/fera/fft/consumemodule.shtml), a component of [Fuel and Fire Tools](https://www.fs.fed.us/pnw/fera/fft/index.shtml).

* This project was funded by the California Energy Commission's (CEC) Electric Program Investment Charge (EPIC) program under contract agreement EPC-16-047. DISCLAIMER: This source code was prepared as the result of work sponsored by the California Energy Commission. It does not necessarily represent the views of the CEC, its employees, or the State of California. The CEC, the State of California, its employees, contractors, and subcontractors make no warrant, express or implied, and assume no legal liability for the information in this report; nor does any party represent that the uses of this information will not infringe upon privately owned rights. This report has not been approved or disapproved by the California Energy Commission, nor has the California Energy Commission passed upon the accuracy or adequacy of the information in this report.
