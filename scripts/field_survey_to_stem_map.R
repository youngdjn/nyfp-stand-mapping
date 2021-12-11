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

#### Get the cluster ID (first letter of plot name) and the subplot grid x and y coords # Note that it is y, x (row, column), but column is reversed for macroplot E
plots = plots %>%
  mutate(cluster_name = str_sub(`Stem Map ID`, 1, 1),
         grid_x = str_sub(`Stem Map ID`, 3, 3),
         grid_y = str_sub(`Stem Map ID`, 2, 2))

## For macroplot E, reverse the x coords
plots[plots$cluster_name=="E",] = plots[plots$cluster_name=="E",] %>%
    mutate(grid_x = recode(grid_x, "1"="2", "2"="1"))

plots = plots %>%
  mutate(across(c(grid_x,grid_y), as.numeric))


#### Get the Garmin UTM coords
plots$notes_list = plots$Notes %>% str_split(" ")
plots$notes_list_length = lengths(plots$notes_list)

for(i in 1:nrow(plots)) {
  plot = plots[i,]
  easting = nth(plot$notes_list[[1]],plot$notes_list_length-1)
  northing = nth(plot$notes_list[[1]],plot$notes_list_length)
  plots[i,"easting"] = easting %>% as.numeric
  plots[i,"northing"] = northing %>% as.numeric
}

plots = plots %>%
  select(-notes_list,-notes_list_length)

plots_sf = st_as_sf(plots,coords=c("easting","northing"), crs=26910)

st_write(plots_sf,datadir("ground-mapping-data/plot-processed/plots-garmin.gpkg"), append=FALSE)

# plots_phone_sf = st_as_sf(plots,coords=c("x","y"),crs=4326)
# st_write(plots_phone_sf,datadir("ground-mapping-data/plot-processed/plots-phone.gpkg"), delete_dsn=TRUE)

### Get the subplot grid x and y shifts from macroplot center

plots = plots %>%
  group_by(cluster_name) %>%
  mutate(grid_x_offset = grid_x - mean(grid_x)) %>%
  mutate(grid_y_offset = -(grid_y - mean(grid_y))) %>% # reverse the sign to make it 
  ungroup()
  
### Get what a given x offset means 

# Declination is 13.4 E

deg2rad <- function(deg) {(deg * pi) / (180)}

# for a 1 m northward (y) shift:
easting = sin(deg2rad(13.4)) # 0.23 m E, 
northing = cos(deg2rad(13.4)) # 0.97 m N

# for a 1 m eastward (x) shift:
easting = sin(deg2rad(13.4+90)) # 0.97 m E, 
northing = cos(deg2rad(13.4+90)) # -0.23 m N

plots = plots %>%
  mutate(pos_offset_e = (grid_x_offset * 0.97 + grid_y_offset * 0.23) * 20) %>% # 20 because it's the distance btwn plot centers
  mutate(pos_offset_n = (grid_x_offset * -0.23 + grid_y_offset * 0.97) * 20)


### Get mean coords for each macroplot, and align the center of the macroplot ot those coords.
centers = plots %>%
  group_by(cluster_name) %>%
  summarize(mean_easting = mean(easting),
            mean_northing = mean(northing))
  
# Bring in the center coords to offset from for each subplot
plots = left_join(plots,centers)

plots = plots %>%
  mutate(grid_coord_x = mean_easting + pos_offset_e,
         grid_coord_y = mean_northing + pos_offset_n)

plots_sf = st_as_sf(plots,coords = c("grid_coord_x", "grid_coord_y"), crs = 26910)

st_write(plots_sf, datadir("ground-mapping-data/plot-processed/plots-gridded.gpkg"))
