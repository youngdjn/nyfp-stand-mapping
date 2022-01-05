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

## Load the plot centers
plot_centers = st_read(datadir("ground-mapping-data/plot-processed/plots-gridded.gpkg")) %>%
  rename(macroplot_id = cluster_name)
a = st_coordinates(plot_centers)
plot_centers$x = a[,1]
plot_centers$y = a[,2]
st_geometry(plot_centers) = NULL

raw_trees = st_read(datadir("ground-mapping-data/tree-processed/tree-locs-raw.gpkg")) %>%
  mutate(macroplot_id = str_sub(plot_id,1,1))

a = st_coordinates(raw_trees)
raw_trees$tree_x = a[,1]
raw_trees$tree_y = a[,2]
st_geometry(raw_trees) = NULL

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
trees_shifted = trees_joined %>%
  filter((abs(man_shift_x) > 0.01) | (abs(man_shift_y) > 0.01))



## compute the mean shift by subplot and by macroplot

mean_shifts_subplot = trees_shifted %>%
  group_by(plot_id, macroplot_id) %>%
  summarize(across(starts_with("man_shift_"),mean))

mean_shifts_macroplot = mean_shifts_subplot %>%
  group_by(macroplot_id) %>%
  summarize(across(starts_with("man_shift_"),mean))


#### Shift all in macroplot the same

trees_rectified_macroplot = raw_trees %>%
  left_join(mean_shifts_macroplot) %>%
  mutate(tree_macroshifted_x = tree_x + man_shift_x,
         tree_macroshifted_y = tree_y + man_shift_y) %>%
  st_as_sf(coords = c("tree_macroshifted_x","tree_macroshifted_y"),crs=26910)

st_write(trees_rectified_macroplot,datadir("/ground-mapping-data/tree-processed/tree-locs-rectified-macroplot.gpkg"), delete_dsn = TRUE)

# shift plot centers too
plot_centers_rectified_macroplot = plot_centers %>%
  left_join(mean_shifts_macroplot) %>%
  mutate(x = x + man_shift_x,
         y = y + man_shift_y) %>%
  st_as_sf(coords=c("x","y"), crs=26910)
st_write(plot_centers_rectified_macroplot,datadir("ground-mapping-data/plot-processed/plot-rectified-macroplot.gpkg"), delete_dsn=TRUE)

# make circular subplot perimeters to define the macroplot
macroplots = plot_centers_rectified_macroplot %>%
  st_buffer(10)
st_write(macroplots, datadir("ground-mapping-data/plot-processed/plot-footprint-rectified-macroplot.gpkg"), delete_dsn=TRUE)


#### Shift all subplots separately

trees_rectified_subplot = raw_trees %>%
  left_join(mean_shifts_subplot) %>%
  mutate(tree_subshifted_x = tree_x + man_shift_x,
         tree_subshifted_y = tree_y + man_shift_y) %>%
  st_as_sf(coords = c("tree_subshifted_x","tree_subshifted_y"),crs=26910)

st_write(trees_rectified_subplot,datadir("/ground-mapping-data/tree-processed/tree-locs-rectified-subplot.gpkg"), delete_dsn = TRUE)

# shift plot centers too
plot_centers_rectified_subplot = plot_centers %>%
  left_join(mean_shifts_subplot) %>%
  mutate(x = x + man_shift_x,
         y = y + man_shift_y) %>%
  st_as_sf(coords=c("x","y"), crs=26910)
st_write(plot_centers_rectified_subplot,datadir("ground-mapping-data/plot-processed/plot-rectified-subplot.gpkg"), delete_dsn = TRUE)
