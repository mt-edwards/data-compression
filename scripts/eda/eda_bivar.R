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
package_names = c("tidyverse", "plyr", "abind", "RNetCDF")
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

# Temporary subsetting.
# =======================
Y1 = Y1[1:5, , , ]
Y2 = Y2[1:5, , , ]

# Concatinated arrays.
# ========================
Y = abind(Y1, Y2, rev.along = 0); rm(Y1, Y2)

# Residuals.
# ========================
R = aperm(aaply(Y, 3:5, lm_res, .progress = "text"), c(4, 5, 1, 2, 3)); rm(Y)

# Correlation.
# ========================
C = apply(aaply(R, c(1, 3, 4), cross_cov), 2:3, mean); rm(R)

# Save NetCDF file.
# ========================
save_ncdf(C, lon, lat, paste0(args[1], ".", args[2], ".cross_cor"), "ALL")

# Clear workspace.
# =======================
rm(list = ls())
