##########################
# Bivariate EDA.         #
##########################

# Command line arguments
# =======================
# - 1) Variable 1 name.
# - 2) Variable 2 name.
args = commandArgs(TRUE)

# Load libraries.
# =======================
package_names = c("tidyverse")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/eda/eda_fun.R")

# Load data.
# =======================
Y1 = get(load(file = paste0("data/", args[1], "/Y.R")))
Y2 = get(load(file = paste0("data/", args[2], "/Y.R")))
load(file = paste0("data/", args[1], "/year.R"))
load(file = paste0("data/", args[1], "/lon.R"))
load(file = paste0("data/", args[1], "/lat.R"))

# Concatinated arrays.
# ========================
Y = abind(Y1, Y2, rev.along = 0); rm(Y1, Y2)

# Longitude and latitude indices.
# ========================
lon.ind = c(1, 33, 65, 97, 129, 161, 193, 225, 257)
lat.ind = 1:(dim(Y)[4] - 1)

# Variable subarray.
# ========================
Y = Y[, , lon.ind, lat.ind, ]
lon = lon[lon.ind]
lat = lat[lat.ind]

# Residuals.
# ========================
R = aperm(aaply(Y, c(1, 3, 4, 5), lm_res, .progress = "text"), c(1, 5, 2, 3, 4))

# Unscaled residuals.
# ========================
U = aperm(aaply(Y, c(1, 3, 4, 5), lm_unscaled_res, .progress = "text"), c(1, 5, 2, 3, 4))

# Cross-covariances.
# ========================
Y.cov = aaply(Y, c(1, 3, 4), cross_cov, .progress = "text")
R.cov = aaply(R, c(1, 3, 4), cross_cov, .progress = "text")
U.cov = aaply(U, c(1, 3, 4), cross_cov, .progress = "text")

# Cross-covariance plots.
# ========================
png(paste0("plots/ALL/multi_cov.", args[1], ".", args[2], ".png"), width = 800, height = 800)
cross_cov_plot_grid(Y.cov, R.cov, U.cov, lat, lon, "Cross-covariance", paste(args, collapse = " vs. "))
dev.off()

# Clear workspace.
# =======================
rm(list = ls())
