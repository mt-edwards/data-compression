##########################
# Diagnostics.           #
##########################

# Command line arguments
# =======================
# - 1) Variable name 1.
# - 2) Variable name 2.
# - 3) Ensemble size.
# - 4) AR order.
# - 5) MA order.
# - 6) Stationary.
# - 7) Taper.
# - 8) Simulated ensemble size.
args = commandArgs(TRUE)
args = c("FSNS", "FLNS", 5, 3, 0, "n", 10, 5)

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
Y1 = get(load(paste0("data/", args[1], "/Y.R")))
Y2 = get(load(paste0("data/", args[2], "/Y.R")))
Y.sim = get(load(paste0("data/ALL/Y.", args[1], ".", args[2], ".r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".t", args[7], ".s", args[8], ".R")))
Y = abind(Y1, Y2, rev.along = 0); rm(Y1, Y2)

# Longitude and latitude indices.
# ========================
lon.ind = c(1, 33, 65, 97, 129, 161, 193, 225, 257)
lat.ind = 1:(dim(Y)[4] - 1)

# Data arrays.
# ========================
Y = abind(Y[, , lon.ind, lat.ind, ], Y[, , lon.ind + 1, lat.ind, ], 
          Y[, , lon.ind, lat.ind + 1, ], Y[, , lon.ind + 1, lat.ind + 1, ], rev.along = 0)
Y.sim = abind(Y.sim[, , lon.ind, lat.ind, ], Y.sim[, , lon.ind + 1, lat.ind, ], 
              Y.sim[, , lon.ind, lat.ind + 1, ], Y.sim[, , lon.ind + 1, lat.ind + 1, ], rev.along = 0)

# Longitudes and latitudes.
# ========================
lon = lon[lon.ind]
lat = lat[lat.ind]

# Data arrays.
# ========================
LM = aaply(Y[, , , , , 1], c(1, 3, 4, 5), temp_lm, year = year, .progress = "text")
LM.sim = aaply(Y.sim[, , , , , 1], c(1, 3, 4, 5), temp_lm, year = year, .progress = "text")
AC = aaply(Y[, , , , , 1], c(1, 3, 4, 5), temp_acf, .progress = "text")
AC.sim = aaply(Y.sim[, , , , , 1], c(1, 3, 4, 5), temp_acf, .progress = "text")
VarCC = aaply(Y[, , , , , 1], c(1, 3, 4), temp_ccf, .progress = "text")
VarCC.sim = aaply(Y.sim[, , , , , 1], c(1, 3, 4), temp_ccf, .progress = "text")
LonCC = aaply(Y[, , , , ,c(1, 2)], c(1, 3, 4, 5), temp_ccf, .progress = "text")
LonCC.sim = aaply(Y.sim[, , , , ,c(1, 2)], c(1, 3, 4, 5), temp_ccf, .progress = "text")
LatCC = aaply(Y[, , , , ,c(1, 3)], c(1, 3, 4, 5), temp_ccf, .progress = "text")
LatCC.sim = aaply(Y.sim[, , , , ,c(1, 3)], c(1, 3, 4, 5), temp_ccf, .progress = "text")

# Mean (2020)
# ========================
diag_plot_grid(LM[, , , 1, 1], LM.sim[, , , 1, 1], lat, lon, "Mean (2020)", args[1])
diag_plot_grid(LM[, , , 2, 1], LM.sim[, , , 2, 1], lat, lon, "Mean (2020)", args[2])

# Trend
# ========================
diag_plot_grid(LM[, , , 1, 2], LM.sim[, , , 1, 2], lat, lon, "Trend", args[1])
diag_plot_grid(LM[, , , 2, 2], LM.sim[, , , 2, 2], lat, lon, "Trend", args[2])

# Variance.
# ========================
diag_plot_grid(AC[, , , 1, 1], AC.sim[, , , 1, 1], lat, lon, "Variance", args[1])
diag_plot_grid(AC[, , , 2, 1], AC.sim[, , , 2, 1], lat, lon, "Variance", args[2])

# Auto-covariance.
# ========================
diag_plot_grid(AC[, , , 1, 2], AC.sim[, , , 1, 2], lat, lon, "Auto-covariance (temporal lag 1)", args[1])
diag_plot_grid(AC[, , , 2, 2], AC.sim[, , , 2, 2], lat, lon, "Auto-covariance (temporal lag 1)", args[2])

# Spatial cross-covariance.
# ========================
diag_plot_grid(LonCC[, , , 1, 2], LonCC.sim[, , , 1, 2], lat, lon, "Cross-covariance (longitudinal lag 1)", args[1])
diag_plot_grid(LonCC[, , , 2, 2], LonCC.sim[, , , 2, 2], lat, lon, "Cross-covariance (longitudinal lag 1)", args[2])
diag_plot_grid(LatCC[, , , 1, 2], LatCC.sim[, , , 1, 2], lat, lon, "Cross-covariance (latitudinal lag 1)", args[1])
diag_plot_grid(LatCC[, , , 2, 2], LatCC.sim[, , , 2, 2], lat, lon, "Cross-covariance (latitudinal lag 1)", args[2])

# Multivariate cross-covariance.
# ========================
diag_plot_grid(VarCC[, , , 2], VarCC.sim[, , , 2], lat, lon, "Cross-covariance", paste(args[1:2], collapse = " - "))

# Clear workspace.
# ========================
rm(list = ls())
