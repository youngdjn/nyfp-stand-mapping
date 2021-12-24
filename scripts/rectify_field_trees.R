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



#### Load the raw calculated tree locs and the manually shifted trees

raw_trees = st_read(datadir("ground-mapping-data/tree-processed/tree-locs-raw.gpkg"))

shifted_trees_exceptF = st_read(datadir("/ground-mapping-data/tree-processed/tree-locs-manually-shifted-01.gpkg"))
shifted_trees_onlyF = st_read(datadir("/ground-mapping-data/tree-processed/tree-locs-manually-shifted-01_F.gpkg"))

shifted_trees_exceptF = shifted_trees_exceptF %>%
  mutate(macroplot_id = str_sub(plot_id,1,1)) %>%
  filter(macroplot_id != "F")

shifted_trees_onlyF = shifted_trees_onlyF %>%
  mutate(macroplot_id = str_sub(plot_id,1,1)) %>%
  filter(macroplot_id == "F")

manually_shifted_trees = bind_rows(shifted_trees_exceptF,shifted_trees_onlyF)

st_write(manually_shifted_trees,datadir("/ground-mapping-data/tree-processed/temp.gpkg"),delete_dsn=TRUE)


## get x and y coords for shifted trees
a = st_coordinates(raw_trees)
raw_trees$x_raw = a[,1]
raw_trees$y_raw = a[,2]
st_geometry(raw_trees) = NULL

a = st_coordinates(manually_shifted_trees)
manually_shifted_trees$x_man = a[,1]
manually_shifted_trees$y_man = a[,2]
st_geometry(manually_shifted_trees) = NULL

#### Determine what the shift was

# match the trees
trees_joined = inner_join(raw_trees,manually_shifted_trees)

trees_joined = trees_joined %>%
  mutate(man_shift_x = x_man-x_raw,
         man_shift_y = y_man-y_raw)


## pull out only the trees that were shifted (shift > 0.01)

## compute the mean shift by subplot and by macroplot




#### Shift all in macroplot the same

# shift plot centers too


#### Shift all subplots separately

# shift plot centers too