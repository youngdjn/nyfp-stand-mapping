library(sf)
library(here)
library(purrr)
library(tidyverse)
library(lidR)
#library(furrr)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))



#### Project area boundary ####

focal_area = st_read("/media/djyoung/Latimer_Drone_Data/tnc-nyfp/focal-area-mask/study-area-masks/flight-units.gpkg") %>% st_transform(3310)


#### DTM

dtm = raster("/media/djyoung/Latimer_Drone_Data/tnc-nyfp/usgs_dem.tif") %>% projectRaster(crs = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs ")

## Disaggregation of DEM

# skip if file aleady exists
dem_file = "/media/djyoung/Latimer_Drone_Data/tnc-nyfp/usgs_dem_interp.tif"
if(file.exists(dem_file)) {
  cat("Already exists:",dem_file,". Loading from already created.\n")
} else {

  cat("\nstarting disaggregation\n")
  # interpolate the the DEM to the same res as the finest CHMs (0.05 m)
  dtm_interp = disaggregate(dtm,fact=ceiling(10/0.05), method="bilinear") # starting res is 10 m
  cat("completed disaggregation\n")
  
  writeRaster(dtm_interp,dem_file)
}

dtm_interp = raster(dem_file)

## get las layers from metashape outputs directory
las_files = list.files("/media/djyoung/Latimer_Drone_Data/tnc-nyfp/metashape-products",pattern=".*\\.las", full.names=TRUE)  # to filter to ones matching a name: pattern=paste0(las_layer_name,".*\\.las")

## Only process LAS files < 5 GB
#las_file_info = file.info(las_files)
#las_file_info$too_large = las_file_info$size > 5e+9
#las_files = las_files[!las_file_info$too_large]

crop_and_write_las = function(las_file) {

  cat("Starting",las_file,"...")

  file_minus_extension = str_sub(las_file,1,-5)
  fileparts = str_split(file_minus_extension,fixed("/"))[[1]]
  filename_only = fileparts[length(fileparts)]

  # file to write
  filename = paste0("/media/djyoung/Latimer_Drone_Data/tnc-nyfp/metashape-products-postprocessed/las/",filename_only,".laz")

  # skip if file aleady exists
  if(file.exists(filename)) {
    cat("Already exists:",filename,". Skipping.\n")
    return(FALSE)
  }


  ## Read and clip las
  las = readLAS(las_file)
  las = clip_roi(las,focal_area)
  las = filter_duplicates(las)
  las = decimate_points(las, homogenize(50,5))
  las = normalize_elevation(las = las, algorithm = dtm_interp, na.rm = TRUE)


  writeLAS(las,filename)

  gc()

  cat("finished.\n")

}

#plan(multiprocess,workers=3)

map(las_files %>% sample, crop_and_write_las)
