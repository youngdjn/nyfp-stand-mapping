library(terra)
library(here)


# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)
# Convenience functions, including function datadir() to prepend data directory to a relative path
source(here("scripts/convenience_functions.R"))

dem1 = rast(datadir("dems/raw/USGS_13_n40w121_20200106.tif"))
dem2 = rast(datadir("dems/raw/USGS_13_n40w122_20210301.tif"))

dem = merge(dem1,dem2)

## what to crop to

mask = vect(datadir("study-area-masks/mask-for-dems.shp"))

dem_mask = mask(dem,dem_mask)
dem_crop = crop(dem_mask,mask)

writeRaster(dem_crop,datadir("dems/merged/usgs_dem.tif"))
