##########################
# Univariate Diagnostics.#
##########################

# Command line arguments 
# =======================
# - 1) Variable name 1.
# - 2) Variable name 2.
# - 3) Variable name 3.
# - 4) Ensemble size.
# - 5) AR order.
# - 6) MA order.
# - 7) Taper.
# - 8) Simulated ensemble size.
args = commandArgs(TRUE)
args = c("TMQ", "TS", "U10", 5, 3, 0, 10, 28)

# Load libraries.
# =======================
package_names = c("tidyverse", "parallel", "Hmisc", "RNetCDF", "plyr", "gridExtra", "abind")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/temp/temp_fun.R")
source("scripts/eda/eda_fun.R")

# Load data.
# =======================
Y = get(load(paste0("data/ALL/Y.", args[1], ".", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
load(file = paste0("data/", args[1], "/year.R"))
load(file = paste0("data/", args[1], "/lon.R"))
load(file = paste0("data/", args[1], "/lat.R"))

# Univariate Arrays.
# =======================
Y1 = Y[, , , , 1]
Y2 = Y[, , , , 2]
Y3 = Y[, , , , 3]; rm(Y)

# Variable Statistics.
# =======================
var_w = weight_array(Y1[1, , , ], lat)
cl = makeCluster(detectCores() - 1)
var_q1 = parApply(cl, Y1, 1, wtd.quantile, weights = var_w)
var_m1 = parApply(cl, Y1, 1, wtd.mean, weights = var_w)
var_q2 = parApply(cl, Y2, 1, wtd.quantile, weights = var_w)
var_m2 = parApply(cl, Y2, 1, wtd.mean, weights = var_w)
var_q3 = parApply(cl, Y3, 1, wtd.quantile, weights = var_w)
var_m3 = parApply(cl, Y3, 1, wtd.mean, weights = var_w)
stopCluster(cl)

# Variable data frame.
# =======================
print(args[1])
print(round(data.frame(Means = c(mean(var_m1), rowMeans(var_q1)),
                       SDs   = c(sd(var_m1), apply(var_q1, 1, sd))), 2))
print(args[2])
print(round(data.frame(Means = c(mean(var_m2), rowMeans(var_q2)),
                       SDs   = c(sd(var_m2), apply(var_q2, 1, sd))), 2))
print(args[3])
print(round(data.frame(Means = c(mean(var_m3), rowMeans(var_q3)),
                       SDs   = c(sd(var_m3), apply(var_q3, 1, sd))), 2))

 # Temporal and latitudinal statistics.
# =======================
temp_w = weight_array(Y1[1, 1, , ], lat)
cl = makeCluster(detectCores() - 1)
temp_m1 = parApply(cl, Y1, 1:2, wtd.mean, weights = temp_w)
lat_m1  = parApply(cl, Y1, c(1, 4), mean)
temp_m2 = parApply(cl, Y2, 1:2, wtd.mean, weights = temp_w)
lat_m2  = parApply(cl, Y2, c(1, 4), mean)
temp_m3 = parApply(cl, Y3, 1:2, wtd.mean, weights = temp_w)
lat_m3  = parApply(cl, Y3, c(1, 4), mean)
stopCluster(cl)

# Temporal and latitudinal statistics plot.
# =======================
png(paste0("plots/", args[1], "/temp_lat.", args[1], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), width = 900, height = 400)
grid.arrange(ensemble_plot(year, temp_m1, "Year", "Global Mean", seq(2010, 2100, 10)), 
             ensemble_plot(lat, lat_m1, "Latitude", "Longitudinal Mean", seq(-90, 90, 30)),
             layout_matrix = matrix(1:2, 1, 2))
dev.off()
png(paste0("plots/", args[2], "/temp_lat.", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), width = 900, height = 400)
grid.arrange(ensemble_plot(year, temp_m2 - 289.64, "Year", "Global Mean Annomoly", seq(2010, 2100, 10)), 
             ensemble_plot(lat, lat_m2 - 289.64, "Latitude", "Longitudinal Mean Annomoly", seq(-90, 90, 30)),
             layout_matrix = matrix(1:2, 1, 2))
dev.off()
png(paste0("plots/", args[3], "/temp_lat.", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), width = 900, height = 400)
grid.arrange(ensemble_plot(year, temp_m3, "Year", "Global Mean", seq(2010, 2100, 10)), 
             ensemble_plot(lat, lat_m3, "Latitude", "Longitudinal Mean", seq(-90, 90, 30)),
             layout_matrix = matrix(1:2, 1, 2))
dev.off()

# Spatial Statistics.
# ========================
cl = makeCluster(detectCores() - 1)
spat_p1 = apply(Y1[1:3, , , ], 3:4, spat_pars)
spat_p2 = apply(Y2[1:3, , , ], 3:4, spat_pars)
spat_p3 = apply(Y3[1:3, , , ], 3:4, spat_pars)
stopCluster(cl)

# Spatial NetCDF File.
# ========================
map2(alply(spat_p1, 1), c(paste0("intercept.", args[1], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                         paste0("trend.", args[1], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                         paste0("std.", args[1], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                         paste0("auto-cov.", args[1], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png")), 
     save_ncdf, lon = lon, lat = lat, args = args)
map2(alply(spat_p2, 1), c(paste0("intercept.", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                          paste0("trend.", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                          paste0("std.", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                          paste0("auto-cov.", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png")), 
     save_ncdf, lon = lon, lat = lat, args = args)
map2(alply(spat_p3, 1), c(paste0("intercept.", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                          paste0("trend.", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                          paste0("std.", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), 
                          paste0("auto-cov.", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png")), 
     save_ncdf, lon = lon, lat = lat, args = args)


# Residual arrays.
# =======================
R1 = aaply(Y1[1:3, , , ], 3:4, lm_res)
R2 = aaply(Y2[1:3, , , ], 3:4, lm_res)
R3 = aaply(Y3[1:3, , , ], 3:4, lm_res)

# Lon ensemble arrays.
# =======================
R1_lon = abind(R1, abind(R1[dim(R1)[1], , , drop = FALSE], R1[-dim(R1)[1], ,], along = 1), rev.along = 0)
R2_lon = abind(R2, abind(R2[dim(R2)[1], , , drop = FALSE], R2[-dim(R2)[1], ,], along = 1), rev.along = 0)
R3_lon = abind(R3, abind(R3[dim(R3)[1], , , drop = FALSE], R3[-dim(R3)[1], ,], along = 1), rev.along = 0)

# Lat ensemble arrays.
# =======================
R1_lat = abind(R1, abind(R1[, -1,], R1[, 1, , drop = FALSE], along = 2), rev.along = 0)
R2_lat = abind(R2, abind(R2[, -1,], R2[, 1, , drop = FALSE], along = 2), rev.along = 0)
R3_lat = abind(R3, abind(R3[, -1,], R3[, 1, , drop = FALSE], along = 2), rev.along = 0)

cross_spat_p1 = apply(R1_lon, 1:2, cross_cor)
cross_spat_p2 = apply(R2_lon, 1:2, cross_cor)
cross_spat_p3 = apply(R3_lon, 1:2, cross_cor)
cross_spat_q1 = apply(R1_lat, 1:2, cross_cor)
cross_spat_q2 = apply(R2_lat, 1:2, cross_cor)
cross_spat_q3 = apply(R3_lat, 1:2, cross_cor)

save_ncdf(cross_spat_p1, lon, lat, "lon_cor_train", args)
save_ncdf(cross_spat_p2, lon, lat, "lon_cor_train2", args)
save_ncdf(cross_spat_p3, lon, lat, "lon_cor_train3", args)
save_ncdf(cross_spat_q1, lon, lat, "lat_cor_train", args)
save_ncdf(cross_spat_q2, lon, lat, "lat_cor_train2", args)
save_ncdf(cross_spat_q3, lon, lat, "lat_cor_train3", args)


# Clear workspace.
# =======================
rm(list = ls())
