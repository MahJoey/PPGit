# Clear -------------------------------------------------------------------
rm(list = ls())

# Library -----------------------------------------------------------------

library(sf)        # 空间数据读写
library(dplyr)     # 数据操作
library(mapview)   # 交互式地图（仅用于中小规模）
library(spatstat) # 网络数据结构

# Load Greater Manchester Data(Boundary, Roads, )-----------------------------

cm_boundary <- st_read("CM_data/cm_boundary.shp")
roads_manchester <- st_read("CM_data/roads_manchester.shp")

# accident ----------------------------------------------------------------

acc <- read.csv("~/Desktop/Manchester_OriginalData/STATS19AccDataJan2010Dec2021forGMServers.csv")

acc_sf <- st_as_sf(
  acc,
  coords = c("Easting", "Northing"),
  crs = 27700,
  remove = FALSE
)

acc2_sf <- acc_sf[acc_sf$NumberCasualties > 1, ]
# acc2_sf <- acc_sf %>% 
#   filter(NumberCasualties > 1)


# plot_preview ------------------------------------------------------------

# plot(st_geometry(gm_boundary), col="lightblue")
# plot(st_geometry(roads_manchester), add=TRUE, col="black", lwd=0.4)
# plot(st_geometry(acc2_sf), add=TRUE, pch=20, cex=0.4, col="red")


# window ---------------------------------------------------------------------

# gm_boundary: sf MULTIPOLYGON（10 个 district 的 GM）
# 转成 spatstat 的窗口对象 owin
gm_win <- as.owin(cm_boundary)

# sf 道路 → linnet -------------------------------------------------

net       <- readRDS("GM_data/gm_net.rds")
roads_psp <- readRDS("GM_data/gm_roads_psp.rds")
# gm_win    <- readRDS("GM_data/gm_win.rds")

# Snap to Road ------------------------------------------------------------

# 选择要分析的点集（全部 or 仅重伤以上）
pts_sf <- acc2_sf              # 或者 acc2_sf

# sf -> ppp（窗口必须是 gm_win）
xy <- sf::st_coordinates(pts_sf)
acc_pp <- spatstat.geom::ppp(x = xy[,1], y = xy[,2], window = gm_win, checkdup = FALSE)

# 投影到最近道路线段（确保点落在网络上）
prj <- spatstat.geom::project2segment(acc_pp, roads_psp)
acc_snap <- prj$Xproj   # 已经“贴”到道路上的 ppp

# 构造线性网络点模式
# X <- spatstat.linnet::lpp(acc_snap, L = net)
L <- net[gm_win, clip = TRUE, drop = TRUE]
X <- spatstat.linnet::lpp(acc_snap, L)
X
X_manCity <- X


dir.create("CM_data", showWarnings = FALSE)

# save
saveRDS(X_manCity, file = "CM_data/X_manCity.rds")



# K- Function -------------------------------------------------------------

### Load the package (spatstat.Knet) ---v90i01
if (!require("spatstat.Knet")) {
  # Install the latest version of the package
  install.packages("spatstat.Knet_1.11-0.tar.gz", repos = NULL, type = "source")
  if (!require("spatstat.Knet")) 
    stop("Unable to install package")
} else requireversion(spatstat.Knet, "1.11-0")


# r 取值（单位=米），上限可根据研究尺度调整
r <- seq(0, 1200, length = 100)

K_hom <- Knet(X, r = r)

# 理论曲线（网络上的 CSR：K_theo(r) = 2r）
K_theo <- 2 * r

# 作图
plot(K_hom, main = "Homogeneous K on Linear Network", legend = FALSE, lwd = 2)
lines(r, K_theo, col = "red", lty = 2, lwd = 2)
