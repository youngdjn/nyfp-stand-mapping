## For each DSM, converts to CHM by subtracting USGS DEM, rescales to 0.12 m, and saves

library(sf)
library(here)
library(purrr)
library(furrr)
library(tidyverse)
library(terra)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))


#### Load project mask

project_mask = vect(datadir("study-area-masks/flight-units.gpkg")) %>% buffer(-50)



#### DTM

dtm = rast(datadir("dems/merged/usgs_dem.tif")) %>% project(y = "epsg:3310")
dtm_albers = dtm %>% project(y="epsg:3310")

## get DSM layers 
dsm_files = list.files(datadir("drone-data/raw/dsms"),pattern=".*_dsm\\.tif", full.names=TRUE)  # to filter to ones matching a name: pattern=paste0(las_layer_name,".*\\.las")
dsm_files = dsm_files[c(1,3,5)]

crop_and_write_chm = function(dsm_file) {
  
  cat("Starting",dsm_file,"...")
  
  file_minus_extension = str_sub(dsm_file,1,-5)
  fileparts = str_split(file_minus_extension,fixed("/"))[[1]]
  filename_only = fileparts[length(fileparts)]
  filename_no_dsm = str_replace(filename_only,"_dsm","")
  
  # file to write
  filename = paste0(datadir("drone-data/chms/"),filename_only,"_chm.tif")
  
  # skip if file aleady exists
  if(file.exists(filename)) {
    cat("Already exists:",filename,". Skipping.\n")
    return(FALSE)
  }
  
  dsm = rast(dsm_file)
  
  dsm = crop(dsm,project_mask %>% project(crs(dsm)))
  dsm = mask(dsm,project_mask %>% project(crs(dsm)))
  
  # interpolate the the DEM to the res, extent, etc of the DSM
  dtm_interp = resample(dtm %>% project(y=crs(dsm)),dsm)
  template_coarse = rast(resolution = 10,extent=ext(dsm), crs=crs(dsm)) %>% project(y="epsg:3310")
  
  
  #### Calculate canopy height model ####
  #### and save to tif
  
  # calculate canopy height model
  chm = dsm - dtm_interp
  
  ## get ground level
  # it's in the lower 0.005 to 0.1 quantile
  v = terra::values(chm)
  lwr = quantile(v,0.005,na.rm=TRUE)
  upr = quantile(v,0.25,na.rm=TRUE)
  # get the mode within that range
  v_lowish = v[which(between(v,lwr,upr))]
  ground = modal(v_lowish)
  
  chm = chm - ground
  
  # writeRaster(chm,datadir("temp/chm_origres.tif"))
  
  # downscale to 0.12 m
  template = rast(resolution=0.12, extent=ext(template_coarse), crs="epsg:3310")
  chm_proj = project(chm,y=template, method="bilinear")
  
  
  # create dir if doesn't exist, then write
  writeRaster(chm_proj,filename) # naming it metashape because it's just based on metashape dsm (and usgs dtm) -- to distinguish from one generated from point cloud
  
  gc()
  
  cat("finished.\n")
  
}

plan(multiprocess,workers=3)

map(dsm_files, crop_and_write_chm)
