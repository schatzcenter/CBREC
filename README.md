## Outputs
***
### Output format

The output structure is currently a single file for each polygon. The file has nested lists for each case. Each case has two lists of time series and year0 values. Year0 is further separated by the four processing emissions scenarios and the power plant outputs. The power plant outputs are run for the nearest power plant, which will vary spatially and four generic power plants of differing technologies. 
 
```
polygon_id
+-- case_ID (as it relates to the scenario matrix)
|   +-- treatment (year 0)
|   |   -- field.residue.removed
|   |   -- total.biomass.mobilized.tonnesAcre
|   |   +-- CandP (collection and processing)
|   |   |   +-- CandP_dry_grind
|   |   |   +-- CandP_green_chip
|   |   |   +-- CandP_green_grind
|   |   |   +-- CandP_low
|   |   +-- PP (power plant scenarios)
|   |   |   +-- Nearest
|   |   |   +-- CurrentGenCombustionPlantDefault
|   |   |   +-- CurrentGenIG/CombustionPlantDefault
|   |   |   +-- NextGenThermochemicalPlantDefault
|   |   |   +-- LessThan1MWPlant
|   |   +-- BroadcastBurn
|   |   +-- PileBurn
|   +-- postTreatment (100 year time series)
+-- ...
```

### postTreatment
| Variable                           | Description                            |
| ---------------------------------- | -------------------------------------- |
| In.field.non.char.scattered_tonnes | Scattered residue mass                 |
| In.field.non.char.piled_tonnes     | Piled residue mass                     |
| In.field.char.scattered_tonnes     | Scattered charred mass                 |
| In.field.char.piled_tonnes         | Piled charred mass                     |
| wildfire.burned.residue_tonnes     | Mass lost specifically to wildfire     |
| decayed.residue_tonnes             | Mass lost specifically to decay        |
| wildfire.CO2_tonnes                | CO<sub>2</sub> emissions from wildfire |
| wildfire.CO_tonnes                 | CO emissions from wildfire             |
| wildfire.CH4_tonnes                | CH<sub>4</sub> emissions from wildfire |
| wildfire.NOx_tonnes                | NO<sub>x</sub> emissions from wildfire |
| wildfire.N2O_tonnes                | N<sub>2</sub> emissions from wildfire  |
| wildfire.PMUnder10um_tonnes        | PM10 emissions from wildfire           |
| wildfire.PMUnder2.5um_tonnes       | PM2.5 emissions from wildfire          |
| wildfire.BC_tonnes                 | Black Carbon from wildfire             |
| wildfire.SO2_tonnes                | SO<sub>2</sub> emissions from wildfire |
| wildfire.VOC_tonnes                | VOC emissions from wildfire            |
| decay.CO2_tonnes                   | CO<sub>2</sub> emissions from decay    |
| decay.CH4_tonnes                   | CH<sub>4</sub> emissions from decay    |


### CandP (collection and processing)
***
 | Variable                                         | Description                                          |
 | ------------------------------------------------ | ---------------------------------------------------- |
 | collection.processing.diesel.CO2_tonnes          | CO<sub>2</sub> emissions from the collection process |
 | collection.processing.diesel.CO_tonnes           | CO emissions from the collection process             |
 | collection.processing.diesel.CH4_tonnes          | CH<sub>4</sub> emissions from the collection process |
 | collection.processing.diesel.NOx_tonnes          | NO<sub>x</sub> emissions from the collection process |
 | collection.processing.diesel.N2O_tonnes          | N<sub>2</sub> emissions from the collection process  |
 | collection.processing.diesel.PMUnder10um_tonnes  | PM10 emissions from the collection process           |
 | collection.processing.diesel.PMUnder2.5um_tonnes | PM2.5 emissions from the collection process          |
 | collection.processing.diesel.SO2_tonnes          | SO<sub>2</sub> emissions from the collection process |
 | collection.processing.diesel.VOC_tonnes          | VOC emissions from the collection process            |
 | transportation.diesel.CO2_tonnes                 | CO<sub>2</sub> emissions from residue mobilization   |
 | transportation.diesel.CO_tonnes                  | CO emissions from residue mobilization               |
 | transportation.diesel.CH4_tonnes                 | CH<sub>4</sub> emissions from residue mobilization   |
 | transportation.diesel.NOx_tonnes                 | NO<sub>x</sub> emissions from residue mobilization   |
 | transportation.diesel.N2O_tonnes                 | N<sub>2</sub> emissions from residue mobilization    |
 | transportation.diesel.PMUnder10um_tonnes         | PM10 emissions from residue mobilization             |
 | transportation.diesel.PMUnder2.5um_tonnes        | PM2.5 emissions from residue mobilization            |
 | transportation.diesel.SO2_tonnes                 | SO<sub>2</sub> emissions from residue mobilization   |
 | transportation.diesel.VOC_tonnes                 | VOC emissions from residue mobilization              |

### pp_output
***
| Variable                             | Description                                          |
| ------------------------------------ | ---------------------------------------------------- |
| plant.type                           | Technology being used at power plant location        |
| plant.location                       | Name of power plant                                  |
| residue.burned.to.electricity_tonnes | mass of material burned for electricity              |
| residue.burned.to.heat_tonnes        | mass of material burned for cogen heat               |
| mass.to.plant_tonnes                 | mass of material brought to power plant              |
| pp_waste.flyash_ash_tonnes           | Ash by-product from power plant (no carbon content)  |
| pp_waste.flyash_char_tonnes          | Char by-product from power plant (carbon containing) |
| pp_electricity.CO2_tonnes            | CO<sub>2</sub> emissions from the power plant        |
| pp_electricity.CO_tonnes             | CO emissions from the power plant                    |
| pp_electricity.CH4_tonnes            | CH<sub>4</sub> emissions from the power plant        |
| pp_electricity.NOx_tonnes            | NO<sub>x</sub> emissions from the power plant        |
| pp_electricity.N2O_tonnes            | N<sub>2</sub> emissions from the power plant         |
| pp_electricity.PMUnder10um_tonnes    | PM10 emissions from the power plant                  |
| pp_electricity.PMUnder2.5um_tonnes   | PM2.5 emissions from the power plant                 |
| pp_electricity.BC_tonnes             | Black Carbon from power plant                        |
| pp_electricity.SO2_tonnes            | SO<sub>2</sub> emissions from the power plant        |
| pp_electricity.VOC_tonnes            | VOC emissions from the power plant                   |
| pp_energy.production_MWh             | Amount of usable MWhs                                |
| pp_energy.production_MMBtu           | Amount of usable MMBTUs                              |
| ng_off_CO2_tonnes                    | Offset CO<sub>2</sub> emissions (negative value)     |
| ng_off_CO_tonnes                     | Offset CO emissions (negative value)                 |
| ng_off_CH4_tonnes                    | Offset CH<sub>2</sub> emissions (negative value)     |
| ng_off_NOx_tonnes                    | Offset NO<sub>x</sub> emissions (negative value)     |
| ng_off_N2O_tonnes                    | Offset N<sub>2</sub>O emissions (negative value)     |
| ng_off_PMUnder2.5um_tonnes           | Offset PM2.5 emissions (negative value)              |
| ng_off_BC_tonnes                     | Offset Black Carbon emissions (negative value)       |
| ng_off_SO2_tonnes                    | Offset SO<sub>2</sub> emissions (negative value)     |
| ng_off_VOC_tonnes                    | Offset VOC emissions (negative value)                |

## BroadcastBurn
***
| Variable                           | Description                               |
| ---------------------------------- | ----------------------------------------- |
| broadcast.burn.CO2_tonnes          | CO<sub>2</sub> emissions from broadcast   |
| broadcast.burn.CO_tonnes           | CO emissions from broadcast               |
| broadcast.burn.CH4_tonnes          | CH<sub>4</sub> emissions from broadcast   |
| broadcast.burn.NOx_tonnes          | NO<sub>x</sub> emissions from broadcast   |
| broadcast.burn.N2O_tonnes          | N<sub>2</sub> emissions from broadcast    |
| broadcast.burn.PMUnder10um_tonnes  | PM10 emissions from broadcast             |
| broadcast.burn.PMUnder2.5um_tonnes | PM2.5 emissions from broadcast            |
| broadcast.burn.BC_tonnes           | Black Carbon from broadcast               |
| broadcast.burn.SO2_tonnes          | SO<sub>2</sub> emissions from broadcast   |
| broadcast.burn.VOC_tonnes          | VOC emissions from broadcast              |
| broadcast.burned.residue_tonnes    | Mass of material burned in broadcast burn |

## PileBurn
***
| Variable                      | Description                             |
| ----------------------------- | --------------------------------------- |
| pile.burn.CO2_tonnes          | CO<sub>2</sub> emissions from pile burn |
| pile.burn.CO_tonnes           | CO emissions from pile burn             |
| pile.burn.CH4_tonnes          | CH<sub>4</sub> emissions from pile burn |
| pile.burn.NOx_tonnes          | NO<sub>x</sub> emissions from pile burn |
| pile.burn.N2O_tonnes          | N<sub>2</sub> emissions from pile burn  |
| pile.burn.PMUnder10um_tonnes  | PM10 emissions from pile burn           |
| pile.burn.PMUnder2.5um_tonnes | PM2.5 emissions from pile burn          |
| pile.burn.BC_tonnes           | Black Carbon from pile burn             |
| pile.burn.SO2_tonnes          | SO<sub>2</sub> emissions from pile burn |
| pile.burn.VOC_tonnes          | VOC emissions from pile burn            |
| pile.burned.residue_tonnes    | Mass of material burned in pile burn    |
