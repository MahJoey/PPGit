# Clear -------------------------------------------------------------------
rm(list = ls())


# Library -----------------------------------------------------------------

library(sf)        # 空间数据读写
library(dplyr)     # 数据操作
library(mapview)   # 交互式地图（仅用于中小规模）
library(spatstat.geom) # 网络数据结构

# Manchester Boundary --------------------------------------------------------

boundary <- st_read(
  "~/Desktop/Manchester/LAD"
)

gm_names <- c(
  "Manchester",
  "Salford",
  "Bolton",
  "Bury",
  "Oldham",
  "Rochdale",
  "Stockport",
  "Tameside",
  "Trafford",
  "Wigan"
)

gm_boundary <- boundary[boundary$LAD21NM %in% gm_names, ]
plot(st_geometry(gm_boundary), col = "lightblue", main="Greater Manchester Boundary")

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

# check the Projection ----------------------------------------------------

st_crs(roads_gb)
st_crs(gm_boundary)

# Cut Boundary area -------------------------------------------------------

roads_crop <- st_crop(roads_gb, st_bbox(gm_boundary))

roads_manchester <- st_intersection(roads_crop, gm_boundary)

# # Plot --------------------------------------------------------------------

plot(st_geometry(gm_boundary), col="lightblue", border=NA)
plot(st_geometry(roads_manchester), add=TRUE, col="black", lwd=0.4)


# accident ----------------------------------------------------------------

acc <- read.csv("~/Desktop/Manchester_OriginalData/STATS19AccDataJan2010Dec2021forGMServers.csv")

acc_sf <- st_as_sf(
  acc,
  coords = c("Easting", "Northing"),
  crs = 27700,
  remove = FALSE
)

plot(st_geometry(gm_boundary), col="lightblue")
plot(st_geometry(roads_manchester), add=TRUE, col="black", lwd=0.4)
plot(st_geometry(acc_sf), add=TRUE, pch=20, cex=0.4, col="red")

# # Snap --------------------------------------------------------------------
# 
# # Step 1: extract only LINESTRING or MULTILINESTRING
# roads_clean <- st_collection_extract(roads_manchester, type = "LINESTRING")
# 
# # Step 2: cast MULTILINESTRING → LINESTRING
# roads_ls <- st_cast(roads_clean, "LINESTRING")
# 
# # Step 3: filter only LINESTRING
# roads_ls <- roads_ls[st_geometry_type(roads_ls) == "LINESTRING", ]
# roads_psp <- as.psp(roads_manchester)
# net <- linnet(roads_psp)
# 

