############ Functions related to data wrangling of fire model outputs ##############
## Author: Max Blasdel
## Date: 7/8/2019
#####################################################################################

## Inputs: multiple dataframes in list format as output from the fire model for RX and wildfire burns

## Outputs: data.frame object with emissions numbers summed

#####################################################################################


## script to add emissions from rx burn and wildfire

cbrec_SumDataFrame <- function(x) {
        # isolate variables of interest from each list item
        first <- x[[1]][,14:ncol(x[[1]])]
        second <- x[[2]][,14:ncol(x[[2]])]
        
        # add data frames as matrices
        all <- data.frame(as.matrix(first) + as.matrix(second))
        
        # bind with identifier variables
        all <- cbind(x[[1]][,1:13], all)
        
        # change burn column to be more descriptive and indicate the data has changed
        all[,c("Secondary_Burn")] <- "All_Burn"
        
        return(all)
}
