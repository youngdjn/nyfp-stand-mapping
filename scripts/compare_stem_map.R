## Compare a drone map(s) with a ground-truth map

library(tidyverse)
library(sf)
library(here)

#### Get data dir ####
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)

#source(here("scripts/convenience_functions.R"))
source(here("scripts/compare_stem_map_functions.R"))

#### Load and prep the TAO map ####

drone_map = st_read(datadir("drone-data/detected-trees/ttops-vwf196.gpkg"))



#### Load and clean ground map data ####
ground_map = st_read(datadir("ground-mapping-data/tree-processed/tree-locs-rectified-macroplot.gpkg")) %>% st_transform(3310)
ground_map$ground_tree_id = 1:nrow(ground_map)
ground_map$final_drone_map_match_id = NA



#### Constants ####
search_distance_fun = function(x) { 1 + 0.1 * x}
search_distance = search_distance_fun(65) # this is used for buffering in and out of the stand to look for perimeter trees
search_height_proportion = .50
smallest_size = 20 # smallest tree height to include in a size class for comparison

# #### Filter ground map data to only trees within the height search distance of the smallest size category (here hard-coded as 10 m)
# ground_map = ground_map %>%
#   filter(Height >= (smallest_size-smallest_size*search_height_proportion))


#### Run comparison ####

match_compare_single_wrapper(ground_map = ground_map, drone_map = stem_silva, drone_map_name = "silva", make_lines_between_matches = TRUE)
match_compare_single_wrapper(ground_map = ground_map, drone_map = stem_ws, drone_map_name = "ws", make_lines_between_matches = TRUE)