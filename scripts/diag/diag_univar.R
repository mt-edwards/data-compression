##########################
# Diagnostics.           #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
# - 5) Taper.
# - 6) Simulated ensemble size.
args = commandArgs(TRUE)

# Libraries.
# ========================
package_names = c("tidyverse", "abind", "plyr")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# ========================
source("scripts/diag/diag_fun.R")

# Load files.
# ========================
load(file = paste0("data/", args[1], "/lon.R"))
load(file = paste0("data/", args[1], "/lat.R"))
load(file = paste0("data/", args[1], "/year.R"))
Y.sim = get(load(file = paste0("data/", args[1], "/Y.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".R")))
Y = get(load(file = paste0("data/", args[1], "/Y.R")))

# Test set.
# ========================
Y = Y[(as.numeric(args[2]) + 1):dim(Y)[1], , , ]

# Longitude and latitude indices.
# ========================
lon.ind = c(1, 33, 65, 97, 129, 161, 193, 225, 257)
lat.ind = 1:(dim(Y)[4] - 1)

# Climatology arrays.
# ========================
LM = aaply(Y[, , lon.ind, lat.ind], c(1, 3, 4), temp_lm, year = year, .progress = "text")
LM.sim = aaply(Y.sim[, , lon.ind, lat.ind], c(1, 3, 4), temp_lm, year = year, .progress = "text")

# Annomoly arrays.
# ========================
R = aperm(aaply(Y, c(1, 3, 4), temp_resid, year = year, .progress = "text"), c(1, 4, 2, 3))
R.sim = aperm(aaply(Y.sim, c(1, 3, 4), temp_resid, year = year, .progress = "text"), c(1, 4, 2, 3))
R = abind(R[, , lon.ind, lat.ind], R[, , lon.ind + 1, lat.ind], 
          R[, , lon.ind, lat.ind + 1], R[, , lon.ind + 1, lat.ind + 1], rev.along = 0)
R.sim = abind(R.sim[, , lon.ind, lat.ind], R.sim[, , lon.ind + 1, lat.ind], 
              R.sim[, , lon.ind, lat.ind + 1], R.sim[, , lon.ind + 1, lat.ind + 1], rev.along = 0)

# Longitudes and latitudes.
# ========================
lon = lon[lon.ind]
lat = lat[lat.ind]

# Spatio-temporal covariances.
# ========================
TempC = aaply(R[, , , , 1], c(1, 3, 4), temp_acf, .progress = "text")
TempC.sim = aaply(R.sim[, , , , 1], c(1, 3, 4), temp_acf, .progress = "text")
LonC = aaply(R[, , , ,c(1, 2)], c(1, 3, 4), temp_ccf, .progress = "text")
LonC.sim = aaply(R.sim[, , , ,c(1, 2)], c(1, 3, 4), temp_ccf, .progress = "text")
LatC = aaply(R[, , , ,c(1, 3)], c(1, 3, 4), temp_ccf, .progress = "text")
LatC.sim = aaply(R.sim[, , , ,c(1, 3)], c(1, 3, 4), temp_ccf, .progress = "text")

# Mean (2020)
# ========================
png(paste0("plots/", args[1], "/mean.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"))
diag_plot_grid(LM[, , , 1], LM.sim[, , , 1], lat, lon, "Mean (2020)", args[1])
dev.off()

# Trend
# ========================
png(paste0("plots/", args[1], "/trend.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"))
diag_plot_grid(LM[, , , 2], LM.sim[, , , 2], lat, lon, "Trend", args[1])
dev.off()

# Variance.
# ========================
png(paste0("plots/", args[1], "/var.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"))
diag_plot_grid(TempC[, , , 1], TempC.sim[, , , 1], lat, lon, "Variance", args[1])
dev.off()

# Temporal-covariance.
# ========================
png(paste0("plots/", args[1], "/temp_cov.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"))
diag_plot_grid(TempC[, , , 2], TempC.sim[, , , 2], lat, lon, "Temporal-covariance (lag 1)", args[1])
dev.off()

# Longitudinal-covariance.
# ========================
png(paste0("plots/", args[1], "/lon_cov.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"))
diag_plot_grid(LonC[, , , 1], LonC.sim[, , , 1], lat, lon, "Longitudinal-covariance (lag 1)", args[1])
dev.off()

# Latitudinal-covariance.
# ========================
png(paste0("plots/", args[1], "/lat_cov.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"))
diag_plot_grid(LatC[, , , 1], LatC.sim[, , , 1], lat, lon, "Latitudinal-covariance (lag 1)", args[1])
dev.off()

# Clear workspace.
# ========================
rm(list = ls())
