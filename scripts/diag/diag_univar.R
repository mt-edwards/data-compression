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
D.sim = get(load(file = paste0("data/", args[1], "/D.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".R")))
D     = get(load(file = paste0("data/", args[1], "/D.R"))) 

# Test sets.
# ========================
D = D[(as.numeric(args[2]) + 1):dim(D)[1], , , ]

# Longitude and latitude indices.
# ========================
lon.ind = c(1, 33, 65, 97, 129, 161, 193, 225, 257)
lat.ind = 1:(dim(D)[4] - 1)

# Annomoly arrays.
# ========================
D = abind(D[, , lon.ind, lat.ind], D[, , lon.ind + 1, lat.ind], 
          D[, , lon.ind, lat.ind + 1], D[, , lon.ind + 1, lat.ind + 1], rev.along = 0)
D.sim = abind(D.sim[, , lon.ind, lat.ind], D.sim[, , lon.ind + 1, lat.ind], 
              D.sim[, , lon.ind, lat.ind + 1], D.sim[, , lon.ind + 1, lat.ind + 1], rev.along = 0)

# Longitudes and latitudes.
# ========================
lon = lon[lon.ind]
lat = lat[lat.ind]

# Spatio-temporal covariances.
# ========================
TempC = aaply(D[, , , , 1], c(1, 3, 4), temp_acf, .progress = "text")
TempC.sim = aaply(D.sim[, , , , 1], c(1, 3, 4), temp_acf, .progress = "text")
LonC = aaply(D[, , , ,c(1, 2)], c(1, 3, 4), temp_ccf, .progress = "text")
LonC.sim = aaply(D.sim[, , , ,c(1, 2)], c(1, 3, 4), temp_ccf, .progress = "text")
LatC = aaply(D[, , , ,c(1, 3)], c(1, 3, 4), temp_ccf, .progress = "text")
LatC.sim  = aaply(D.sim[, , , ,c(1, 3)], c(1, 3, 4), temp_ccf, .progress = "text")

# Temporal-covariance.
# ========================
png(paste0("plots/", args[1], "/temp_cov.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"), width = 800, height = 800)
diag_plot_grid(TempC[, , , 2], TempC.sim[, , , 2], lat, lon, "Temporal-correlation (lag 1)", args[1])
dev.off()

# Longitudinal-covariance.
# ========================
png(paste0("plots/", args[1], "/lon_cov.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"), width = 800, height = 800)
diag_plot_grid(LonC[, , , 1], LonC.sim[, , , 1], lat, lon, "Longitudinal-correlation (lag 1)", args[1])
dev.off()

# Latitudinal-covariance.
# ========================
png(paste0("plots/", args[1], "/lat_cov.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".png"), width = 800, height = 800)
diag_plot_grid(LatC[, , , 1], LatC.sim[, , , 1], lat, lon, "Latitudinal-correlation (lag 1)", args[1])
dev.off()

# Clear workspace.
# ========================
rm(list = ls())
