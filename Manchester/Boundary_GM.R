# Clear -------------------------------------------------------------------
rm(list = ls())

# Library -----------------------------------------------------------------

library(sf)        # 空间数据读写

# Manchester Boundary --------------------------------------------------------

boundary <- st_read(
  "~/Desktop/Manchester_OriginalData/LAD"
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


dir.create("GM_data", showWarnings = FALSE)

st_write(gm_boundary,
         "GM_data/gm_boundary.shp",
         delete_dsn = TRUE)
