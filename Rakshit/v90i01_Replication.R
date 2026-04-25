###########################################################################
# Replication Script for Generating the Tables and Figures in the Paper
###########################################################################

### Load the package
if (!require("spatstat.Knet")) {
  # Install the latest version of the package
  install.packages("spatstat.Knet_1.11-0.tar.gz", repos = NULL, type = "source")
  if (!require("spatstat.Knet")) 
    stop("Unable to install package")
} else requireversion(spatstat.Knet, "1.11-0")

### Plot Figure 1
data("wacrashes", package = "spatstat.Knet")
plot(wacrashes, cols = "red", cex = 0.5, main = " ")

### Plot Figure 2
plot(unmark(chicago), cols = "blue", pch = 16, main = " ")

### Function for reproducing the Table-1 in the paper.
get_table1 <- function() {
  
  require("profmem")
  require("spatstat.Knet")  # loads spatstat
  
  # make list of example datasets
  lpp_list0 <- list(X_spiders = spiders, X_chicago = chicago, X_dendrite = dendrite)
  # remove pre-computed data
  lpp_list1 <- lapply(lpp_list0, as.lpp, sparse = TRUE)
  
  dist_grids <- list(seq(0, 1000, length = 41), seq(0, 1000, length = 41), seq(0, 
    200, length = 41))
  
  # perform memory profiling
  prof_linearK <- lapply(lpp_list1, FUN = function(z) profmem(linearK(z)))
  
  prof_Knet <- vector("list", length = 3)
  for (z in seq_along(lpp_list1)) {
    prof_Knet[[z]] <- profmem(Knet(lpp_list1[[z]], r = dist_grids[[z]]))
  }
  
  # compute execution time
  time_linearK <- sapply(lpp_list1, function(z) system.time(linearK(z))[["elapsed"]])
  
  time_Knet <- vector(length = 3)
  for (z in seq_along(lpp_list1)) {
    time_Knet[z] <- system.time(Knet(lpp_list1[[z]], r = dist_grids[[z]]))[["elapsed"]]
  }
  
  result <- data.frame(DATASET = c("spiders", "chicago", "dendrite"))
  result$POINTS <- sapply(lpp_list1, npoints)
  result$LINES <- sapply(lpp_list1, function(z) nsegments(domain(z)))
  result$TIME.Matrix <- time_linearK
  result$MTOT.Matrix <- sapply(prof_linearK, total)/2^20
  result$TIME.List <- time_Knet
  result$MTOT.List <- sapply(prof_Knet, total)/2^20
  result
}

### Reproduce Table 1
get_table1()

### Function for reproducing Table 2 in the paper.
get_table2 <- function() {
  require("spatstat.Knet")  # loads spatstat
  data("wacrashes", package = "spatstat.Knet")
  L_den <- as.linnet(dendrite, sparse = FALSE)
  den_lpp <- lpp(as.ppp(dendrite), L_den)
  
  lpp_list0 <- list(X_spiders = spiders, X_chicago = chicago, X_dendrite = den_lpp)
  
  lpp_list1 <- lapply(lpp_list0, as.lpp, sparse = TRUE)
  lpp_list1[[4]] <- wacrashes
  
  full_mat <- c(sapply(lpp_list0, function(z) round(object.size(z)/2^10)), NA)
  spar_mat <- sapply(lpp_list1, function(z) round(object.size(z)/2^10))
  
  result <- data.frame(Network = c("spiders", "chicago", "dendrite", "wacrashes"), 
    `Full matrix` = full_mat, `Sparse matrix` = spar_mat)
  result
}

### Reproduce Table 2
get_table2()

### Reproduce Figure 10
X <- unmark(chicago)
plot(linearK(X))
fit <- lppm(X ~ polynom(x,y,2))
plot(linearKinhom(X, fit))

### Function for reproducing Figure-14 in the paper
plot_figure14 <- function(X) {
  rmax <- seq(from = 100, by = 100, to = 1000)
  # List to store the execution times
  elapsed_time <- vector("numeric", length = length(rmax))
  
  for (i in seq_along(rmax)) {
    elapsed_time[i] <- system.time(Knet(X = X, r = seq(0, rmax[i], length = 41)))[["elapsed"]]
  }
  
  # Plot the time vs rmax
  elapsed_time <- elapsed_time/60
  
  par(mar = c(4, 5.5, 2, 1))
  plot(rmax, elapsed_time, type = "l", main = "", xlab = substitute(r[max]), ylab = "Time (minutes)", 
    lwd = 2, cex.axis = 1.4, cex.lab = 1.7)
}

### Reproduce Figure 14
plot_figure14(wacrashes)

### Function for reproducing Figure-16(a) in the paper
plot_figure16a <- function(X, npts, r = NULL) {
  stopifnot(is.lpp(X))
  stopifnot(is.vector(npts))
  if (is.null(r)) 
    r <- seq(0, 1000, length = 101)
  
  net <- domain(X)
  # Generate random points on the network.
  lpp_list <- lapply(npts, runiflpp, L = net)
  
  # Calculate Timings
  n <- length(lpp_list)
  Knet_timings <- sapply(lpp_list, function(z) system.time(Knet(z, r = r))[["elapsed"]])
  linearK_timings <- sapply(lpp_list, function(z) system.time(linearK(z, r = r))[["elapsed"]])
  
  # Plot the timings against the number of points
  par(mar = c(4, 5.5, 2, 1))
  matplot(npts, matrix(c(Knet_timings, linearK_timings), ncol = 2), type = "l", 
    col = c("blue", "red"), lty = c(3, 2), lwd = 2, main = "", ylab = "Time (seconds)", 
    xlab = "Number of points", cex.axis = 1.4, cex.lab = 1.5)
  labels <- c("adjacency-matrix", "adjacency-list")
  legend("topleft", inset = 0.05, labels, title = "", lwd = 2, col = c("red", "blue"), 
    lty = c(2, 3), box.col = "white")
}

### Reproduce Figure 16(a)
nPts <- seq(from = 1000, to = 10000, by = 1000)
plot_figure16a(X = unmark(chicago), npts = nPts)

### Function for reproducing Figure 16(b) in the paper
plot_figure16b <- function(X, npoints, rmax) {
  stopifnot(inherits(X, "linnet"))
  stopifnot(is.vector(rmax))
  stopifnot(is.vector(npoints))
  
  ndist <- length(rmax)
  nsim <- length(npoints)
  
  retMat <- matrix(0, nrow = nsim, ncol = ndist)
  
  for (i in 1:nsim) {
    Net <- runiflpp(n = npoints[i], L = X)
    for (j in 1:ndist) {
      retMat[i, j] <- system.time(Knet(Net, r = seq(0, rmax[j], length = 101)))[["elapsed"]]
    }
  }
  par(mar = c(4, 5.5, 2, 1))
  
  matplot(nPoints, retMat, type = "l", lwd = 2, lty = 1:4, col = c("green", "blue", 
    "red", "black"), xlab = "Number of points", ylab = "Time (seconds)", cex.axis = 1.4, 
    cex.lab = 1.5)
  
  labels <- c(expression(r[max] == 100), expression(r[max] == 400), expression(r[max] == 
    700), expression(r[max] == 1000))
  legend("topleft", inset = 0.05, labels, title = "", lwd = 2, col = c("green", 
    "blue", "red", "black"), lty = c(1, 2, 3, 4), box.col = "white")
}

### Reproduce Figure 16(b)
rMax <- c(100, 400, 700, 1000)
nPoints <- seq(1000, 5000, by = 1000)
chic_net <- as.linnet(chicago, sparse = TRUE)
plot_figure16b(chic_net, nPoints, rMax)

### Function for reproducing Figure-17 in the paper
plot_figure17 <- function(L, npt_vec) {
  stopifnot(is.lpp(L))
  stopifnot(is.vector(npt_vec))
  
  npts <- npoints(L)
  stopifnot(max(npt_vec) <= npts)
  
  r_grid <- seq(0, 800, length = 101)
  
  exec_time <- sapply(npt_vec, function(n) {
    pt_ids <- sample(1:npts, n, replace = FALSE)
    net <- L[pt_ids]
    system.time(Knet(net, r = r_grid))[["elapsed"]]
  })
  exec_time <- exec_time/60
  plot(npt_vec, exec_time, type = "l", lwd = 2, main = "", xlab = "Number of points", 
    ylab = "Execution time (minutes)", xlim = c(min(npt_vec), max(npt_vec)), 
    ylim = c(0, ceiling(max(exec_time))), cex.axis = 1.4, cex.lab = 1.5)
}

### Reproduce Figure 17
n_points <- seq(from = 1000L, to = 13000L, by = 2000L)
plot_figure17(wacrashes, npt_vec = n_points)

### Reproduce the homogeneous K-function in Figure 18
r_grid <- seq(0, 800, length = 101)
Khom <- Knet(wacrashes, r = r_grid)
plot(Khom, legend = FALSE, lwd = 2, main = " ")

### Plot the intensity heatmaps in Figure 19
load("waCrashIntensity.rda")
plot(waCrashIntensity, log = TRUE, main = " ")
load("waCrashIntensityAdaptive.rda")
plot(waCrashIntensityAdaptive, log = TRUE, main = " ")

### Plot the inhomogeneous K-functions in Figure 18
inten_est <- waCrashIntensity[as.ppp(wacrashes)]
inten_est_adap <- waCrashIntensityAdaptive[as.ppp(wacrashes)]
Kin <- Knetinhom(wacrashes, lambda = inten_est, r = r_grid)
Kin_adap <- Knetinhom(wacrashes, lambda = inten_est_adap, r = r_grid)
plot(Kin, lty = c(5, 2), lwd = 2, col = c("blue", "red"), legend = FALSE)
plot(Kin_adap, est ~ r, add = TRUE, lty = 1, lwd = 2, col = "black")

