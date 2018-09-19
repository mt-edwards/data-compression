##########################
# Bivariate Diagnostics. #
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
Y.joint = get(load(paste0("data/ALL/Y.", args[1], ".", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.indep1 = get(load(paste0("data/", args[1], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.indep2 = get(load(paste0("data/", args[2], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.indep3 = get(load(paste0("data/", args[3], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.indep  = abind(Y.indep1, Y.indep2, Y.indep3, rev.along = 0); rm(Y.indep1, Y.indep2, Y.indep3, Y)
load(file = paste0("data/", args[1], "/year.R"))
load(file = paste0("data/", args[1], "/lon.R"))
load(file = paste0("data/", args[1], "/lat.R"))

# Temporary subsetting.
# =======================
Y.joint = Y.joint[1:5, , , , 1:2]
Y.indep = Y.indep[1:5, , , , 1:2]

# Residuals.
# ========================
R.joint = aperm(aaply(Y.joint, 3:5, lm_res, .progress = "text"), c(4, 5, 1, 2, 3)); rm(Y.joint)
R.indep = aperm(aaply(Y.indep, 3:5, lm_res, .progress = "text"), c(4, 5, 1, 2, 3)); rm(Y.indep)

# Correlation.
# ========================
C.joint = apply(aaply(R.joint, c(1, 3, 4), cross_cov), 2:3, mean); rm(R.joint)
C.indep = apply(aaply(R.indep, c(1, 3, 4), cross_cov), 2:3, mean); rm(R.indep)

# Save NetCDF file.
# ========================
save_ncdf(C.joint, lon, lat, paste0(args[1], ".", args[2], ".cross_cor_joint.", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R"), "ALL")
save_ncdf(C.indep, lon, lat, paste0(args[1], ".", args[2], ".cross_cor_indep.", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R"), "ALL")

# Clear workspace.
# =======================
rm(list = ls())