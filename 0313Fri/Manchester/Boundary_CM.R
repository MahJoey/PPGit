# Clear -------------------------------------------------------------------
rm(list = ls())

# Library -----------------------------------------------------------------

library(sf)        # 空间数据读写

# Manchester Boundary --------------------------------------------------------

boundary <- st_read(
  "~/Desktop/Manchester_OriginalData/LAD"
)

gm_names <- c(
  "Manchester"
)

gm_boundary <- boundary[boundary$LAD21NM %in% gm_names, ]
plot(st_geometry(gm_boundary), col = "lightblue", main="Manchester City Boundary")

dir.create("CM_data", showWarnings = FALSE)  # City Manchester

st_write(gm_boundary,
         "CM_data/cm_boundary.shp",
         delete_dsn = TRUE)