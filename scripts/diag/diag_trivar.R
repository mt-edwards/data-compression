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
args = c("TMQ", "TREFHT", "U10", 5, 3, 0, 10, 5)

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
Y.joint = get(load(paste0("data/ALL/Y.", args[1], ".", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.indep1 = get(load(paste0("data/", args[1], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.indep2 = get(load(paste0("data/", args[2], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.indep3 = get(load(paste0("data/", args[3], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.indep  = abind(Y.indep1, Y.indep2, Y.indep3, rev.along = 0); rm(Y.indep1, Y.indep2, Y.indep3)
Y1 = get(load(paste0("data/", args[1], "/Y.R")))
Y2 = get(load(paste0("data/", args[2], "/Y.R")))
Y3 = get(load(paste0("data/", args[3], "/Y.R")))
Y  = abind(Y1, Y2, Y3, rev.along = 0); rm(Y1, Y2, Y3)

# Longitude and latitude indices.
# ========================
lon.ind = c(1, 33, 65, 97, 129, 161, 193, 225, 257)
lat.ind = 1:(dim(Y)[4] - 1)

# Test sets.
# ========================
lon = lon[lon.ind]
lat = lat[lat.ind]
Y.joint = Y.joint[, , lon.ind, lat.ind, ]
Y.indep = Y.indep[, , lon.ind, lat.ind, ]
Y = Y[(as.numeric(args[4]) + 1):dim(Y)[1], , lon.ind, lat.ind, ]

# Data arrays.
# ========================
VarC12.joint = aaply(Y.joint[, , , , c(1, 2)], c(1, 3, 4), cross_cov, .progress = "text")
VarC12.indep = aaply(Y.indep[, , , , c(1, 2)], c(1, 3, 4), cross_cov, .progress = "text")
VarC12 = aaply(Y[, , , , c(1, 2)], c(1, 3, 4), cross_cov, .progress = "text")
VarC13.joint = aaply(Y.joint[, , , , c(1, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC13.indep = aaply(Y.indep[, , , , c(1, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC13 = aaply(Y[, , , , c(1, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC23.joint = aaply(Y.joint[, , , , c(2, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC23.indep = aaply(Y.indep[, , , , c(2, 3)], c(1, 3, 4), cross_cov, .progress = "text")
VarC23 = aaply(Y[, , , , c(2, 3)], c(1, 3, 4), cross_cov, .progress = "text")

# Multivariate cross-covariance.
# ========================
png(paste0("plots/ALL/multi_cov.", args[1], ".", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC12, VarC12.indep, VarC12.joint, lat, lon, "Cross-covariance", paste(args[c(1, 2)], collapse = " vs. "))
dev.off()
png(paste0("plots/ALL/multi_cov.", args[1], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC13, VarC13.indep, VarC13.joint, lat, lon, "Cross-covariance", paste(args[c(1, 3)], collapse = " vs. "))
dev.off()
png(paste0("plots/ALL/multi_cov.", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC23, VarC23.indep, VarC23.joint, lat, lon, "Cross-covariance", paste(args[c(2, 3)], collapse = " vs. "))
dev.off()

# Clear workspace.
# ========================
rm(list = ls())
