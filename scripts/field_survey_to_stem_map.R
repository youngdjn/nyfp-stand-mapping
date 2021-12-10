## For each DSM, converts to CHM by subtracting USGS DEM, rescales to 0.12 m, and saves

library(sf)
library(here)
library(tidyverse)
library(readxl)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#### Convenience functions and main functions ####

source(here("scripts/convenience_functions.R"))

plots = read_excel(datadir("ground-mapping-data/plot-raw/NorthYubaStemMapping.xlsx"), sheet=2)

#### Get the cluster ID (first letter of plot name)
plots = plots %>%
  mutate(cluster_name = str_sub(`Stem Map ID`, 1, 1))



#### Get the Garmin UTM coords
plots$notes_list = plots$Notes %>% str_split(" ")
plots$notes_list_length = lengths(plots$notes_list)

for(i in 1:nrow(plots)) {
  plot = plots[i,]
  easting = nth(plot$notes_list[[1]],plot$notes_list_length-1)
  northing = nth(plot$notes_list[[1]],plot$notes_list_length)
  plots[i,"easting"] = easting
  plots[i,"northing"] = northing
}

plots = plots %>%
  select(-notes_list,-notes_list_length)

plots_sf = st_as_sf(plots,coords=c("easting","northing"), crs=26910)

st_write(plots_sf,datadir("ground-mapping-data/plot-processed/plots-garmin.gpkg"))

plots_phone_sf = st_as_sf(plots,coords=c("x","y"),crs=4326)

st_write(plots_phone_sf,datadir("ground-mapping-data/plot-processed/plots-phone.gpkg"), delete_dsn=TRUE)

### take the centroid of all the plots in each cluster to get the center of the macroplot

plots_phone_sf = plots_phone_sf %>% st_transform(3310)
coords = st_coordinates(plots_phone_sf)
plots$phone_x = coords[,1]
plots$phone_y = coords[,2]




### Detect and remove outliers by cluster

### Get the subplot grid x and y positions
# Not that it is y, x (row, column), but column is reversed for macroplot 3
