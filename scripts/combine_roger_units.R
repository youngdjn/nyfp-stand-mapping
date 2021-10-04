## Make a shapefile and KML of the two units, by extracting them from the shapefiles provided and combining them

library(sf)
library(tidyverse)

d1 = st_read("/home/derek/Documents/data/nyfp-stand-mapping_data/stem-map-polygons/raw/Galloway.shp")
d2 = st_read("/home/derek/Documents/data/nyfp-stand-mapping_data/stem-map-polygons/raw/Unit178.shp")

d1 = d1[d1$Unit == 150,]

d = bind_rows(d1,d2)

st_write(d,"/home/derek/Documents/data/nyfp-stand-mapping_data/stem-map-polygons/prepped/tnc_nyfp_2021.kml")
