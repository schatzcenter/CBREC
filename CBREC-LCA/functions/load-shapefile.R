## Author: Max Blasdel
## Date: 8/30/2019

## script to load shapefiles for cbrec 

## takes any shapefiles in a specified folder and loads them all


loadShapefile <-
  function(folder, crs) {
    
    # find all shp files in given folder
    shp <-
      dir(folder, pattern = ".shp$", full.names = T)
    
    # throw warning if there are more than 1 shapefile in the folder
    if(length(shp) > 1) {
      warning("multiple shapefiles detected")
    }
    
    # read in each shp file
    shps <-
      lapply(shp, readOGR)
    
    # make sure projection is correct and consistant
    shps <-
      lapply(shps, function(c) {
        sp::spTransform(c, CRSobj = crs)
      })
    
    # bind together , may throw an error if shp files have different crs
    shps <-
      do.call(rbind, shps)
    
    return(shps)
  }