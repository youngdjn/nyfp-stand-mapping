## Take CHM and detect tree tops and crown area

library(here)
library(raster)
library(terra)
library(tidyverse)
library(ForestTools)
library(tictoc)
library(sf)
library(furrr)
library(lidR)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))



chm_files = list.files(datadir("drone-data/chms"), full.names = TRUE)
ttop_units = list()
canopy_units_mcws = list()
canopy_units_silva = list()

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
  
  ttops_sp = ttops %>% as("Spatial")
  ttops_sp$treeID = 1:nrow(ttops_sp)
  
  canopies_mcws = mcws(ttops_sp,raster(chm_smooth),minHeight = 10)
  canfun = silva2016(raster(chm_smooth), ttops_sp, max_cr_factor = 0.24, exclusion = 0.1, ID = "treeID")
  canopies_silva = canfun()
  
  canopies_rast_mcws = rast(canopies_mcws)
  canopies_poly_mcws = as.polygons(canopies_rast_mcws) %>% st_as_sf
  
  canopies_rast_silva = rast(canopies_silva)
  canopies_poly_silva = as.polygons(canopies_rast_silva) %>% st_as_sf
  
  
  # buffer in from the original perim
  ttops = st_intersection(ttops,perim %>% st_buffer(-10))

  ttop_units[[j]] = ttops
  canopy_units_mcws[[j]] = canopies_poly_mcws
  canopy_units_silva[[j]] = canopies_poly_silva
}

ttops_merged = bind_rows(ttop_units)
canopies_merged_mcws = bind_rows(canopy_units_mcws)
canopies_merged_silva = bind_rows(canopy_units_silva)



## write the detected trees

st_write(ttops_merged,datadir("drone-data/detected-trees/ttops-vwf196.gpkg"), delete_dsn=TRUE)
st_write(canopies_merged_mcws,datadir("drone-data/detected-trees/canopies-vwf196_mcws.gpkg"), delete_dsn=TRUE)
st_write(canopies_merged_silva,datadir("drone-data/detected-trees/canopies-vwf196_silva.gpkg"), delete_dsn=TRUE)
