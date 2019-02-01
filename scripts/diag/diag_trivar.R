##########################
# Diagnostics.           #
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

# Libraries.
# ========================
package_names = c("plyr", "tidyverse", "abind")
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

# Longitude and latitude indices.
# ========================
lon.ind = c(1, 33, 65, 97, 129, 161, 193, 225, 257)
lat.ind = 1:(length(lat) - 1)
lon = lon[lon.ind]
lat = lat[lat.ind]

# Load more files.
# ========================
Y.joint = get(load(paste0("data/ALL/Y.", args[1], ".", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))[, , lon.ind, lat.ind, ]
Y.indep1 = get(load(paste0("data/", args[1], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))[, , lon.ind, lat.ind]
Y.indep2 = get(load(paste0("data/", args[2], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))[, , lon.ind, lat.ind]
Y.indep3 = get(load(paste0("data/", args[3], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))[, , lon.ind, lat.ind]
Y.indep  = abind(Y.indep1, Y.indep2, Y.indep3, rev.along = 0); rm(Y.indep1, Y.indep2, Y.indep3)
Y1 = get(load(paste0("data/", args[1], "/Y.R")))[, , lon.ind, lat.ind]
Y2 = get(load(paste0("data/", args[2], "/Y.R")))[, , lon.ind, lat.ind]
Y3 = get(load(paste0("data/", args[3], "/Y.R")))[, , lon.ind, lat.ind]
Y  = abind(Y1, Y2, Y3, rev.along = 0); rm(Y1, Y2, Y3)

# Test sets.
# ========================
Y = Y[(as.numeric(args[4]) + 1):dim(Y)[1], , , , ]

# Detrended.
# ========================
D.joint = aperm(aaply(Y.joint, 3:5, quad_detrend, .progress = "text"), c(4, 5, 1, 2, 3))
D.indep = aperm(aaply(Y.indep, 3:5, quad_detrend, .progress = "text"), c(4, 5, 1, 2, 3))
D       = aperm(aaply(Y, 3:5, quad_detrend, .progress = "text"), c(4, 5, 1, 2, 3))

# Data arrays.
# ========================
VarC12.joint = aaply(D.joint[, , , , c(1, 2)], c(1, 3, 4), cross_cov, .progress = "text")
VarC12.indep = aaply(D.indep[, , , , c(1, 2)], c(1, 3, 4), cross_cov, .progress = "text")
VarC12 = aaply(D[, , , , c(1, 2)], c(1, 3, 4), cross_cov, .progress = "text")
VarC13.joint = aaply(D.joint[, , , , c(1, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC13.indep = aaply(D.indep[, , , , c(1, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC13 = aaply(D[, , , , c(1, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC23.joint = aaply(D.joint[, , , , c(2, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC23.indep = aaply(D.indep[, , , , c(2, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC23 = aaply(D[, , , , c(2, 3)], c(1, 3, 4), cross_cov, .progress = "text")

# Cross-covariance plots.
# ========================
png(paste0("plots/ALL/multi_cov.", args[1], ".", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), width = 900, height = 900)
cross_cov_plot_grid(VarC12, VarC12.indep, VarC12.joint, lat, lon, "Latitude", "Cross-covariance")
dev.off()
png(paste0("plots/ALL/multi_cov.", args[1], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), width = 900, height = 900)
cross_cov_plot_grid(VarC13, VarC13.indep, VarC13.joint, lat, lon, "Latitude", "Cross-covariance")
dev.off()
png(paste0("plots/ALL/multi_cov.", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"), width = 900, height = 900)
cross_cov_plot_grid(VarC23, VarC23.indep, VarC23.joint, lat, lon, "Latitude", "Cross-covariance")
dev.off()

# Clear workspace.
# ========================
rm(list = ls())
