library(sf)
library(dplyr)

pt_df = data.frame(lat = c(   39.474364,    39.478309,    39.479510,    39.498511,   39.504582,   39.505438),
                   lon = c(-121.001663, -120.998722,      -120.994875,  -120.882634, -120.877319, -120.875975),
                   name = c("A","B", "C", "D", "E", "F"))

pt = st_as_sf(pt_df, coords = c("lon","lat"),crs=4326)


points_albers = st_transform(pt,3310)


# allcorners = NA

for(i in 1:nrow(points_albers)) {
  
  point = points_albers[i,]
  stemmap_name = point$name
  
  # make a circle with 60.01 m radius, then the square that encompases it
  circle = st_buffer(point,40)
  square = st_make_grid(circle,cellsize = 161.0,what = "polygons")
  
  corners = st_make_grid(square,cellsize = 20,what = "corners")
  centers = st_make_grid(square,cellsize = 20,what = "centers")
  
  # if(is.na(allcorners)) {
    allcorners = corners
    allcenters = centers
  # } else {
  #   allcorners = c(allcorners,corners)
  #   allcenters = c(allcenters,centers)
  # }
  
  ## number them incrementally
  allcenters = st_as_sf(allcenters)
  coords = st_coordinates(allcenters)
  allcenters$xcoord = coords[,1]
  allcenters$ycoord = coords[,2]
  
  allcenters = allcenters %>%
    mutate(stemmap_id = stemmap_name) %>%
    arrange(desc(ycoord),xcoord) %>%
    mutate(xcoord_int = match(xcoord, unique(xcoord)),
           ycoord_int = match(ycoord, unique(ycoord))) %>%
    mutate(name = paste0("p",xcoord_int,"-",ycoord_int))
  
  ## number them incrementally
  allcorners = st_as_sf(allcorners)
  coords = st_coordinates(allcorners)
  allcorners$xcoord = coords[,1]
  allcorners$ycoord = coords[,2]
  
  allcorners = allcorners %>%
    mutate(stemmap_id = stemmap_name) %>%
    arrange(desc(ycoord),xcoord) %>%
    mutate(xcoord_int = match(xcoord, unique(xcoord)),
           ycoord_int = match(ycoord, unique(ycoord))) %>%
    mutate(name = paste0("v",1:nrow(allcorners)))
  
  allpoints = rbind(allcorners,allcenters)
  
  # add z dimension
  allpoints = st_zm(allpoints, drop=FALSE, what = "Z")
  allpoints = allpoints %>% st_transform(4326)
  
  st_write(allcenters,paste0("/home/derek/Documents/data/nyfp-stand-mapping_data/stem-map-layout/stemmap_",stemmap_name,"_centers.kml"),delete_dsn = TRUE)
  st_write(allcorners,paste0("/home/derek/Documents/data/nyfp-stand-mapping_data/stem-map-layout/stemmap_",stemmap_name,"_corners.kml"),delete_dsn = TRUE)
  st_write(allpoints,paste0("/home/derek/Documents/data/nyfp-stand-mapping_data/stem-map-layout/stemmap_",stemmap_name,"_grid.geojson"),delete_dsn = TRUE)
  
  # write all points as csv in emlid format
  d = st_transform(allpoints,4326)
  coords = st_coordinates(d)
  d$Easting = coords[,1]
  d$Northing = coords[,2]
  d$Elevation = 0
  d$Name = d$name
  st_geometry(d) = NULL

  d = d %>%
    select(Name,Easting,Northing,Elevation)
  
  write.csv(d,paste0("/home/derek/Documents/data/nyfp-stand-mapping_data/stem-map-layout/stemmap_",stemmap_name,"_grid.csv"),row.names=FALSE, quote=FALSE)
  
    
}




### Tangential: save plot and base locations

pt_df = data.frame(lat = c(   39.474364,    39.478309,    39.479510,    39.498511,   39.504582,   39.505438,   39.47752426,   39.48045863,   39.499687778,   39.50581512),
                   lon = c(-121.001663, -120.998722,      -120.994875,  -120.882634, -120.877319, -120.875975, -121.00110469, -120.99671472, -120.883307475, -120.87471868),
                   name = c("A","B", "C", "D", "E", "F", "Base for A and B", "Base for C", "Base for D", "Base for E and F"))

pt = st_as_sf(pt_df, coords = c("lon","lat"),crs=4326)

st_write(pt, "/home/derek/Documents/data/nyfp-stand-mapping_data/NYFP_plot_and_base_locations.kml")



