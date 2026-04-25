if (!require("spatstat.Knet")) {
  # Install the latest version of the package
  install.packages("spatstat.Knet_1.11-0.tar.gz", repos = NULL, type = "source")
  if (!require("spatstat.Knet")) 
    stop("Unable to install package")
} else requireversion(spatstat.Knet, "1.11-0")

library(spatstat.geom)
library(spatstat.linnet)
library(spatstat)

# Plot WA -----------------------------------------------------------------
data("wacrashes", package = "spatstat.Knet")
plot(wacrashes, cols = "red", cex = 0.5, main = " ")


pol <- spatstat.geom::clickpoly(add = TRUE)     # 结束：右键/两指点击或按 Esc
Wpoly <- if (inherits(pol, "owin")) pol else owin(poly = pol)  # 兼容返回类型


# 3) 裁剪线网

Wpoly <- as.owin(Wpoly)                 # 确保是 owin，坐标系与数据一致(27700)

X_sub <- wacrashes[Wpoly, clip = TRUE, drop = TRUE]  # 点裁到多边形内，并同时裁剪底层网络
L_sub <- domain(X_sub)                                  # 取出子网络(linnet)


# 快速检查
nsegments(L_sub)  # 裁剪后应减少
plot(X_sub)

# X_sub saving ------------------------------------------------------------

dir.create("WAsub_data2", showWarnings = FALSE)

# save
saveRDS(X_sub, file = "WAsub_data2/X_sub.rds")

# (可选) 同时把子网络和多边形也存一下，便于复现
saveRDS(L_sub,  file = "WAsub_data2/L_sub.rds")   # linnet
saveRDS(Wpoly,  file = "WAsub_data2/Wpoly.rds")   # owin



# Reproduce the homogeneous K-function ------------------------------------

r_grid <- seq(0, 1000, length = 50)
Khom <- Knet(X_sub, r = r_grid)
plot(Khom, legend = FALSE, lwd = 2, main = " ")



