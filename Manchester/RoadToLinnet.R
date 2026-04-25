library(sf)
library(spatstat.geom)    # owin/ppp/psp
library(spatstat.linnet)  # linnet/lpp
library(spatstat)

# 保证 CRS 一致（OSGB36 / British National Grid）
stopifnot(st_crs(roads_manchester)$epsg == 27700,
          st_crs(gm_boundary)$epsg == 27700)

# gm_win 已在上文得到：
# gm_win <- as.owin(gm_boundary)

# ── 函数：sf 线 → linnet ───────────────────────────────────────────
sf_to_linnet <- function(sf_lines,
                         win,
                         clip = TRUE,              # 是否先按边界裁剪
                         simplify_tol = 1,         # 线简化容差（米，0~5常用）
                         merge_lines = TRUE,       # 是否尝试合并线段
                         psp_sparse = TRUE,        # as.linnet 的稀疏选项
                         node_tolerance = 1        # 节点合并容差（米）
) {
  
  # 0) 备份 CRS
  crs0 <- st_crs(sf_lines)
  
  # 1) 只保留二维有效 LINESTRING
  x <- st_make_valid(sf_lines)
  x <- st_zm(x, drop = TRUE, what = "ZM")                         # 去 Z/M
  x <- st_collection_extract(x, "LINESTRING", warn = FALSE)       # 仅线
  if (clip) {
    # 用 sf 的多边形边界裁剪（注意不是 owin）
    x <- suppressWarnings(st_intersection(x, st_geometry(gm_boundary)))
  }
  
  # 2) 简化（显著降低顶点数，节省内存；容差按米）
  if (!is.null(simplify_tol) && simplify_tol > 0) {
    x <- st_simplify(x, dTolerance = simplify_tol)
  }
  
  # 3) 合并可拼接线段，消减碎片
  if (merge_lines) {
    x <- st_line_merge(st_union(st_geometry(x)))
  }
  
  # 4) 统一为纯 LINESTRING，去空/零长
  x <- st_cast(x, "LINESTRING", do_split = TRUE)
  x <- x[st_geometry_type(x) == "LINESTRING", ]
  x <- x[!st_is_empty(x), ]
  x <- x[as.numeric(st_length(x)) > 0, ]
  
  # 5) sfc -> psp（用几何列方法；窗口用 owin）
  g <- st_geometry(x)                         # sfc_LINESTRING
  psp_obj <- as.psp(g, window = win)          # 若仍报错，说明还有“非线”几何混入
  
  # 6) psp -> linnet（合并近邻节点；27700 下单位=米）
  net <- as.linnet(psp_obj,
                   sparse    = psp_sparse,
                   tolerance = node_tolerance)
  
  # 返回网络 + 中间结果，便于复用/排错
  list(net = net, psp = psp_obj, lines_clean = x, crs = crs0)
}

# ── 调用 ───────────────────────────────────────────────────────────
conv <- sf_to_linnet(
  roads_manchester,
  win            = gm_win,
  clip           = TRUE,     # 只保留 GM 边界内道路
  simplify_tol   = 1,        # 1 m 简化；内存紧张可调到 2–3
  merge_lines    = TRUE,
  psp_sparse     = TRUE,
  node_tolerance = 1         # 合并 1 m 内重合端点
)

net       <- conv$net        # linnet
roads_psp <- conv$psp        # psp
roads_ls  <- conv$lines_clean

# 快速检查
plot(net)                    # 网络示意
nsegments(roads_psp)         # 线段数