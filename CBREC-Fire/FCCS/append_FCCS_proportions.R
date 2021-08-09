## Change the FCCS fuel proportions file to remove zeros which have the effect of removing fuels from fire model calculations
## The change will be made for 10,000 hour fuels only under the assumption that areas may have trees smaller than 10,000 h
## but over time these trees may grow larger and become 10,000 h fuels. Changing the fuel proportions allows for this temporal change.

## Max Blasdel
## 6/26/2019

library(dplyr)

# read in calculated FCCS fuel proportions
fccs_fuel_prop <- read.csv("data/FCCS/tabular/FCCS_fuel_load_proportions.csv")

# conditionally change tenK_hr_sound_prop to 1 if both tenK and tnkp are 0
fccs_fuel_prop <- fccs_fuel_prop %>% 
        mutate(tenK_hr_sound_prop = case_when(tenK_hr_sound_prop == 0 & tnkp_hr_sound_prop == 0 ~ 1,
                                              TRUE ~ tenK_hr_sound_prop))


# write out as new file for use in fire model
write.csv(fccs_fuel_prop, file = "data/FCCS/tabular/FCCS_fuel_load_proportions_V2.csv", row.names = F)
