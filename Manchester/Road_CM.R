# Clear -------------------------------------------------------------------
rm(list = ls())

# Library -----------------------------------------------------------------

library(sf)        # 空间数据读写


# Cut Boundary area -------------------------------------------------------

cm_boundary <- st_read("CM_data/cm_boundary.shp")

# loading the data --------------------------------------------------------

path_roads <- "~/Desktop/Manchester_OriginalData/oproad_essh_gb/data"

layers <- st_layers(path_roads)$name
layers


road_layers <- layers[grepl("RoadLink", layers)]
road_layers


roads_list <- lapply(road_layers, function(lyr) {
  st_read(path_roads, layer = lyr, quiet = TRUE)
})

roads_gb <- do.call(rbind, roads_list)
roads_gb

# Cut Boundary area -------------------------------------------------------

roads_crop <- st_crop(roads_gb, st_bbox(cm_boundary))

roads_manchester <- st_intersection(roads_crop, cm_boundary)


# st_write ----------------------------------------------------------------

st_write(
  roads_manchester,
  "CM_data/roads_manchester.shp",
  delete_dsn = TRUE
)


plot(st_geometry(cm_boundary), col="lightblue", border=NA)
plot(st_geometry(roads_manchester), add=TRUE, col="black", lwd=0.4)

