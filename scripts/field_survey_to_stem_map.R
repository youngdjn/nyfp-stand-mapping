## Converts raw field stem map data to spatial

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

## F13 was saved twice by accident. Delete the first
plots = plots %>%
  filter(GlobalID != "9b4650fa-1b07-481b-9c48-254b9b7e5342")

#### Get the cluster ID (first letter of plot name) and the subplot grid x and y coords # Note that it is y, x (row, column), but column is reversed for macroplot E
plots = plots %>%
  mutate(cluster_name = str_sub(`Stem Map ID`, 1, 1),
         grid_x = str_sub(`Stem Map ID`, 3, 3),
         grid_y = str_sub(`Stem Map ID`, 2, 2))

## For macroplot E, reverse the x coords (crew recorded them backward)
plots[plots$cluster_name=="E",] = plots[plots$cluster_name=="E",] %>%
    mutate(grid_x = recode(grid_x, "1"="2", "2"="1"))

## For macroplot C, reverse the y coords (crew recorded them backward)
plots[plots$cluster_name=="C",] = plots[plots$cluster_name=="C",] %>%
  mutate(grid_y = recode(grid_y, "3"="4", "4"="3"))

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
  
# Bring in the center coords in order to offset from for each subplot
plots = left_join(plots,centers)

plots = plots %>%
  mutate(grid_coord_x = mean_easting + pos_offset_e,
         grid_coord_y = mean_northing + pos_offset_n)

plots_sf = st_as_sf(plots,coords = c("grid_coord_x", "grid_coord_y"), crs = 26910)

st_write(plots_sf, datadir("ground-mapping-data/plot-processed/plots-gridded.gpkg"), append=FALSE)


### Turn the plot centers into a plot footprint

plot_footprints = plots_sf %>% st_buffer(30) %>% st_union() %>% st_cast("POLYGON") %>% st_as_sf()

plot_footprints$plotname = c("B","C","E","F")

st_write(plot_footprints, datadir("ground-mapping-data/plot-processed/plot-footprints-coarse.gpkg"), append=FALSE)



#### Tree distance and azimuth to cartesian position ####

trees = read_excel(datadir("ground-mapping-data/plot-raw/NorthYubaStemMapping.xlsx"), sheet=4) %>%
  select(plot_id = "Plot ID",
         dist = "Distance (m)",
         azi = "Azimuth (deg)",
         species = "Species",
         status = "Tree Status",
         health = "Tree Health",
         dbh = "DBH (cm)",
         height = "Height (m)",
         height_acc = "Measurement of Height",
         notes = "Tree Notes",
         plot_num = "Plot Number")

# remove duplicates 

trees = unique(trees)

## compute tree x and y offset

trees = trees %>%
  mutate(azi_true = azi + 13.4) %>%
  mutate(tree_offset_x = sin(deg2rad(azi_true)),
         tree_offset_y = cos(deg2rad(azi_true)))

## pull in subplot x and y absolute position and add tree offset to get tree absolute position

trees = left_join(trees,plots %>% select("Stem Map ID","plot_coord_x" = "grid_coord_x","plot_coord_y" = "grid_coord_y"),by=c("plot_id" = "Stem Map ID"))

trees = trees %>%
  mutate(tree_coord_x = plot_coord_x + tree_offset_x * dist,
         tree_coord_y = plot_coord_y + tree_offset_y * dist)

trees_sf = st_as_sf(trees,coords = c("tree_coord_x", "tree_coord_y"), crs = 26910)

st_write(trees_sf, datadir("ground-mapping-data/tree-processed/tree-locs-raw.gpkg"), append=FALSE)

