## Take CHM and detect tree tops and crown area

library(here)
library(raster)
library(terra)
library(tidyverse)
library(ForestTools)
library(tictoc)
library(sf)
library(furrr)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))



chm_files = list.files(datadir("drone-data/chms"), full.names = TRUE)
ttop_units = list()

for(j in 1:length(chm_files)) {
  
  chm_file = chm_files[j]
  chm = rast(chm_file) %>% trim
  
  cat("Starting", chm_file, "\n")
  
  perim = as.polygons(chm > -Inf)
  perim = st_as_sf(perim)
  perim = st_simplify(perim,dTolerance = 0.1)
  st_crs(perim) = crs(chm)

  
  
  ### Split into tiles to parallelize
  
  grid = st_make_grid(perim,cellsize=100)
  grid = st_intersection(grid,perim)
  st_crs(grid) = st_crs(perim)
  
  # Buffer out by 10 m
  grid_buff = st_buffer(grid,dist=10)

  # smooth the CHM using a 9 x 9 pixel filter
  weights = matrix(1,nrow=9,ncol=9)
  chm_smooth = focal(chm, weights, fun=mean)
  
  # Make a list of tiles with some overlapping edge
  tiles = lapply(grid_buff,FUN=function(x) raster(crop(chm_smooth,x)))
  
  
  
  # define vwf linear function
  lin_196 <- function(x){x^2*0 + x*0.04 + 0} # window filter function to use in next step
  
  plan(multisession,workers=10)
  ttop_tiles = future_map(tiles, vwf, winFun = lin_196, minHeight=5, maxWinDiameter=99)
  
  
  ## crop each raster back to original tile extent
  grid_sf = st_as_sf(grid)
  ttop_tiles_sf = lapply(ttop_tiles,st_as_sf)
  
  ttop_tiles_crop = list()
  for(i in 1:length(ttop_tiles)) {
    ttop_tile = ttop_tiles_sf[[i]]
    gridcell = grid[i]
    ttop_tiles_crop[[i]] = st_intersection(ttop_tile,gridcell)
  }
  
  # merge the tiles back together
  ttops = bind_rows(ttop_tiles_crop)
  
  # buffer in from the original perim
  ttops = st_intersection(ttops,perim %>% st_buffer(-10))

  ttop_units[[j]] = ttops
}

ttops_merged = bind_rows(ttop_units)




## write the detected trees

st_write(ttops_merged,datadir("drone-data/detected-trees/ttops-vwf196.gpkg"), delete_dsn=TRUE)
