## For each DSM, converts to CHM by subtracting USGS DEM, rescales to 0.12 m, and saves

library(sf)
library(here)
library(purrr)
library(tidyverse)
library(terra)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))


#### Load project mask

project_mask = vect(datadir("study-area-masks/flight-units.gpkg"))



#### DTM

dtm = rast(datadir("dems/merged/usgs_dem.tif")) %>% project(y = "epsg:3310")
dtm_albers = dtm %>% project(y="epsg:3310")

## get DSM layers 
dsm_files = list.files(datadir("tnc-nyfp/metashape-products"),pattern=".*_ortho_dsm\\.tif", full.names=TRUE)  # to filter to ones matching a name: pattern=paste0(las_layer_name,".*\\.las")
cat(dsm_files)

crop_and_write_chm = function(dsm_file) {
  
  cat("Starting",dsm_file,"...")
  
  file_minus_extension = str_sub(dsm_file,1,-5)
  fileparts = str_split(file_minus_extension,fixed("/"))[[1]]
  filename_only = fileparts[length(fileparts)]
  filename_no_dsm = str_replace(filename_only,"_ortho_dsm","")
  
  # file to write
  filename = paste0(datadir("metashape-products-postprocessed/ortho_cropped/"),filename_only,"_ortho.tif")
  
  # skip if file aleady exists
  if(file.exists(filename)) {
    cat("Already exists:",filename,". Skipping.\n")
    return(FALSE)
  }
  
  dsm = rast(dsm_file)
  
  dsm = crop(dsm,project_mask %>% project(crs(dsm)))
  dsm = mask(dsm,project_mask %>% project(crs(dsm)))
  
  # create dir if doesn't exist, then write
  writeRaster(dsm,filename) # naming it metashape because it's just based on metashape dsm (and usgs dtm) -- to distinguish from one generated from point cloud
  
  gc()
  
  cat("finished.\n")
  
}

#plan(multiprocess,workers=3)

map(dsm_files, crop_and_write_chm)
