**California Biomass Residue Emissions Characterization (C-BREC) Tool**
===================================================================

# Wildfire and RX Burn Emissions Module

Calculate wildfire and RX burn emissions under different silvacultural treatments and biomass utilization scenarios for the California Biomass Residue Emissions Characterization (C-BREC) Tool. This tool is part of the [California Biopower Impact Project](https://schatzcenter.org/cbip/). The remote repository for this project can be found at [github.com/SchatzCenter/C-BREC_Fire](https://github.com/SchatzCenter/C-BREC_Fire).

## Installation

Download or fork the repository from [github.com/SchatzCenter/C-BREC_Fire](https://github.com/SchatzCenter/C-BREC_Fire). 

## File Structure

The file structure is shown in the following tree. 

```
CBIP                            # main project directory
+-- CBIP.Rproj                  # r-project file
+-- README.html                 # readme
+-- Consume4_2                  # original Consume source code
+-- data                        # main data directory 
|   +-- FCCS                    # FCCS fuels data
|   +-- GAP                     # Landcover class data
|   +-- GEE                     # Google Earth Engine data
|   +-- Other                   # directory for misc. data sets
|   +-- UW                      # UW data directory
|   +-- Tiles                   # Tile shapefiles, tiled input and output data
|                                 sets
|   +-- SERC                    # Scenario matrix, and associated lookup tables
|   +-- Post_process            # Output of post processing of results
+-- scripts                     # main script directory
|   +-- Charcoal                # script that generates linear fit coefficients
|                                 for char production from scattered material
|   +-- downsample              # scripts exploring potential for spatial downsampling
|   +-- Consume                 # consume, r version
|   +-- FCCS                    # existing fuelbed processing
|   +-- GAP                     # landcover classification
|   +-- GEE                     # Google earth engine data processing
|   +-- Other                   # misc. scripts
|   +-- emissions_model         # core scripts for the emissions model excluding
|                                 consumption/emissions scripts
|   +-- post_process            # scripts for post-processing results
|   +-- scenarios               # scripts for generating lookup tables from scenario matrix
|   +-- Test                    # scratch and testing scripts
|   +-- UW                      # residue data processing 
+-- figures                     # figures
```

## Prerequisites

This project was written in R using the Rstudio project framework. Occasionally, the existing Consume functions (written in Python) were used for testing, primarily through the rstudio interface with the reticulate package, though jupyter notebooks were also employed.

### Software requirements

To run the main scenario_emissions function, the following is required:

* A current version of R
* A current version of Rstudio
* The following R packages and their dependencies:
        - future.apply 
        - data.table
        - sf 
* The CBIP Rstudio project folder, including data.
* At least 2GB storage per tile. [^1]

[^1]: This has not been thoroughly tested for all possible tiles. More storage is better.
        
Packages can be installed as follows:

```
install.packages("data.table")
```

Other packages and software are required to reproduce the entire project, inlcuding python, Consume 4.2, Google Earth Engine, and many other r packages. Packages were loaded at the beginning of every script where possible. All scripts have a description header.

## Summary Overview

The process for calculating the consumption and emissions for a single tile over 5 timesteps (0, 25, 50, 75, and 100) is shown below. 

1. The scenario lookup table is loaded. The following steps are run for each scenario.

2. The FCCS fuelbed, residue, and spatial attributes data are loaded and joined into a single data.table.

3. Mid-flame windspeeds are estimated from Gridmet 10m windspeeds.

4. Piled residue mass is calculated for each fuel size class. If the year is one of the later timesteps (25-100), this is adjusted for decay.

5. Scattered residue mass is calculated for each fuel size class. If the timestep is one of 25-100, this is adjusted for decay.

6. The mass consumed by size class is estimated for the scenario and timestep. RX burns only occur during timestep 0, all subsequent fires for that timestep are wildfires. Wildfires in later timesteps assume that the fuelbed is recovered and that no wildfire has occurred to that point.

7. If the scenario is an RX burn, the fuelbed is adjusted so only unconsumed fuel remains, and the fuelbed is burned again in the same year as a wildfire. All subsequent timesteps

8. The emissions and residual fuels (unconsumed) for each scenario and timestep are estimated.

9. Output emissions and residual fuels are saved as .rds files, mass units are grams per acre.

## Usage

This project is in the Rstudio project format, so all scripts must be sourced relative to the main CBIP folder. The steps to calculate emissions for a single tile are shown below. In this example, emissions and residual (unconsumed) fuel are estimated for a fixed set of scenarios over five time steps over a 100-year period for tile number 300. For ease of use when running on multiple tiles, the run_all function wraps the scenario_emissions function, which in turn wraps all other functions. For efficiency, the scenario_emissions function impliments parrallel processing using the future.apply package, which is platform independent. 

```
source("scripts/emissions_model/run_all.R")

run_all(300)
```

Tile number must match one of the ID numbers of the tiles located in "data/Tiles/clipped_tiles/clipped_tiles.shp". There are 11,990 tiles, each of which is approximately ~2669 hectares in size. The tile ID numbers can be extracted using the following snippet:

```
tiles <- sf::st_read("data/Tiles/clipped_tiles/clipped_tiles.shp")
tiles$ID
```

All outputs are saved to the "data/Tiles/output" folder as data.tables in .rds format. The subfolder "emissions" contains wildfire emissions estimates, the subfolder "residual_fuels" contains the residual fuels data. Each tile has scenario-specific outputs stored in a folder named for the tile number. The following code demonstrates how to access the results.

```
emissions <- readRDS("data/Tiles/output/emissions/300/20_Proportional_Thin-None-70-30-first-No-No-49-300-0.rds")

residual_fuels <- readRDS("data/Tiles/output/residual_fuels/300/20_Proportional_Thin-None-70-30-first-No-No-49-300-0.rds")
```

The run_all function can run any number of the tiles. The run_all function has an optional argument t_range, which is a integer vector or sequence of tile ID numbers. The default is NULL, which runs all tiles. WARNING: This takes quite a while to run, and requires a lot of disk space. There is a second argument save_runtime, which defaults to TRUE. This saves an .rds file with the runtime, "run_time.rds", in the main project directory.

```
# runs all tiles
source("scripts/emissions_model/run_all.R")
run_all()

# run first 100 tiles from Rstudio
run_all(t_range = 1:100)
```
## What it does

The scenario_emissions function sourced by run_all estimates fuel consumption, emissions, and residual fuels for all  of the 704 fixed scenarios. The individual scenarios are stored in a .csv file which can be accessed at "data/SERC/scenarios.csv".

The function loads pre-processed spatial attributes that have been converted from raster to a tabular format with x-y location indicator columns. All spatial data use the California (Teale) Albers projetion. The CRS is below:

```
CRS arguments:
 +proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m
+no_defs +ellps=GRS80 +towgs84=0,0,0 
```

The existing fuelbed and treatment residue data are then appended to the spatial attribute data using FCCS and updated GNN FCID identifiers. The post-treatment fuelbed is then created by adding the treatment residue to the exisiting fuelbed using proportions assigned for each scenario. Residues are added as piled fuels or scattered fuels by fuel size class, accounting for decau. Burns are simulated on the updated fuelbeds at 25-year increments over a 100-year period for a total of five model runs in each wildfire scenario and six in the RX treatment scenarios. Both the emissions and residual fuels (treatment residue only) are saved following each simulation. Wildfire simulations assume that no previous wildfire has occurred. For scenarios that include an RX burn, the RX burn occurs at year 0, and all subsequent burns are modeled as wildfires that occur on 25-year timesteps from 0-100 years after the treatment. With the exception of the wildfire immediately following the RX treatment, these follow-up wildfires are simulated using the remaining treatment residue that has been added to a "recovered" fuelbed. Treatment residues are updated to reflect mass loss from decay prior to burning for all cases.

To estimate emissions, all consumed mass is multiplied by phase-specific (flaming, smoldering, and residual) FEPs emissons factors, which are taken from the [Bluesky modeling framework](https://github.com/pnwairfire/eflookup/blob/master/eflookup/fepsef.py). Char production is also modeled. All mass converted to char is assumed to come from unconsumed fuel. The model output is then split into seperate data.tables containing residual fuels and emissions estimates and is saved as .rds files.

## Output description

The scenario_emissions function saves two output files for each scenario:

1. emissions - this is the consumption and emissions estimates for each fuelbed in the tile.

2. residual_fuels - this is the remaining residue for each fuelbed in the tile.

These are saved as .rds files in folders of the same name located in data/Tiles/output. File naming convention is folders for output type (emissions or residual fuels) and tile number, then silvicultural treatment, type of burn, fraction of residues piled and scatterd, whether the burn was a secondary burn following an RX treatment, whether biomass was collected for utilization, the existence of a pulp market, tile number, and year. File paths have "-" seperation. An example:

```
"data/Tiles/output/emissions/300/20_Proportional_Thin-None-70-30-first-No-No-49-300-0.rds"
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

### Emissions

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

### Residual Fuels

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

## Versioning

We use [git](https://git-scm.com/) for version control on this project. For a complete history, see the [this repository](https://github.com/SchatzCenter/C-BREC_Fire). 

## Authors

* Micah Wright (Original Author)  - [Github](https://github.com/wrightmicahc)
* Jeff Kane
* Andy Harris
* Max Blasdel
* Jerome Carman

## Acknowledgments

* The R consume scripts were originally translated directly into R from the original python code from [Consume 4.2](https://www.fs.fed.us/pnw/fera/fft/consumemodule.shtml), a component of [Fuel and Fire Tools](https://www.fs.fed.us/pnw/fera/fft/index.shtml). 
