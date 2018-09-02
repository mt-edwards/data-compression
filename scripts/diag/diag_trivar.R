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
Y.sim = get(load(paste0("data/ALL/Y.", args[1], ".", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.ind1 = get(load(paste0("data/", args[1], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.ind2 = get(load(paste0("data/", args[2], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.ind3 = get(load(paste0("data/", args[3], "/Y.r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R")))
Y.ind  = abind(Y.ind1, Y.ind2, Y.ind3, rev.along = 0); rm(Y.ind1, Y.ind2, Y.ind3)
Y1 = get(load(paste0("data/", args[1], "/Y.R")))
Y2 = get(load(paste0("data/", args[2], "/Y.R")))
Y3 = get(load(paste0("data/", args[3], "/Y.R")))
Y = abind(Y1, Y2, Y3, rev.along = 0); rm(Y1, Y2, Y3)

# Test set.
# ========================
Y = Y[(as.numeric(args[4]) + 1):dim(Y)[1], , , , ]

# Longitude and latitude indices.
# ========================
lon.ind = c(1, 33, 65, 97, 129, 161, 193, 225, 257)
lat.ind = 1:(dim(Y)[4] - 1)

# Climatology arrays.
# ========================
LM = aaply(Y[, , lon.ind, lat.ind, ], c(1, 3, 4, 5), temp_lm, year = year, .progress = "text")
LM.sim = aaply(Y.sim[, , lon.ind, lat.ind, ], c(1, 3, 4, 5), temp_lm, year = year, .progress = "text")

# Annomoly arrays.
# ========================
R = aaply(Y, c(1, 3, 4, 5), temp_resid, year = year, .progress = "text")
R.sim = aaply(Y.sim, c(1, 3, 4, 5), temp_resid, year = year, .progress = "text")




R = abind(R[, , lon.ind, lat.ind], R[, , lon.ind + 1, lat.ind], 
          R[, , lon.ind, lat.ind + 1], R[, , lon.ind + 1, lat.ind + 1], rev.along = 0)
R.sim = abind(R.sim[, , lon.ind, lat.ind], R.sim[, , lon.ind + 1, lat.ind], 
              R.sim[, , lon.ind, lat.ind + 1], R.sim[, , lon.ind + 1, lat.ind + 1], rev.along = 0)












# Longitude and latitude indices.
# ========================
lon.ind = c(1, 33, 65, 97, 129, 161, 193, 225, 257)
lat.ind = 1:(dim(Y)[4] - 1)

# Data arrays.
# ========================
Y.sim = abind(Y.sim[, , lon.ind, lat.ind, ])
Y.ind = abind(Y.ind[, , lon.ind, lat.ind, ])
Y = abind(Y[, , lon.ind, lat.ind, ])

# Longitudes and latitudes.
# ========================
lon = lon[lon.ind]
lat = lat[lat.ind]

# Data arrays.
# ========================
VarC12.sim = aaply(Y.sim[, , , , c(1, 2)], c(1, 3, 4), temp_ccf, .progress = "text")
VarC12.ind = aaply(Y.ind[, , , , c(1, 2)], c(1, 3, 4), temp_ccf, .progress = "text")
VarC12 = aaply(Y[, , , , c(1, 2)], c(1, 3, 4), temp_ccf, .progress = "text")
VarC13.sim = aaply(Y.sim[, , , , c(1, 3)], c(1, 3, 4), temp_ccf, .progress = "text")
VarC13.ind = aaply(Y.ind[, , , , c(1, 3)], c(1, 3, 4), temp_ccf, .progress = "text")
VarC13 = aaply(Y[, , , , c(1, 3)], c(1, 3, 4), temp_ccf, .progress = "text")
VarC23.sim = aaply(Y.sim[, , , , c(2, 3)], c(1, 3, 4), temp_ccf, .progress = "text")
VarC23.ind = aaply(Y.ind[, , , , c(2, 3)], c(1, 3, 4), temp_ccf, .progress = "text")
VarC23 = aaply(Y[, , , , c(2, 3)], c(1, 3, 4), temp_ccf, .progress = "text")


# Multivariate cross-covariance.
# ========================
png(paste0("plots/ALL/multi_cov_dep.", args[1], ".", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC12[, , , 2], VarC12.sim[, , , 2], lat, lon, "Multivariate-covariance (dependent)", paste(args[c(1, 2)], collapse = " - "))
dev.off()
png(paste0("plots/ALL/multi_cov_ind.", args[1], ".", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC12[, , , 2], VarC12.ind[, , , 2], lat, lon, "Multivariate-covariance (independent)", paste(args[c(1, 2)], collapse = " - "))
dev.off()
png(paste0("plots/ALL/multi_cov_dep.", args[1], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC13[, , , 2], VarC13.sim[, , , 2], lat, lon, "Multivariate-covariance (dependent)", paste(args[c(1, 3)], collapse = " - "))
dev.off()
png(paste0("plots/ALL/multi_cov_ind.", args[1], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC13[, , , 2], VarC13.ind[, , , 2], lat, lon, "Multivariate-covariance (independent)", paste(args[c(1, 3)], collapse = " - "))
dev.off()
png(paste0("plots/ALL/multi_cov_dep.", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC23[, , , 2], VarC23.sim[, , , 2], lat, lon, "Multivariate-covariance (dependent)", paste(args[c(2, 3)], collapse = " - "))
dev.off()
png(paste0("plots/ALL/multi_cov_ind.", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".png"))
diag_plot_grid(VarC23[, , , 2], VarC23.ind[, , , 2], lat, lon, "Multivariate-covariance (independent)", paste(args[c(2, 3)], collapse = " - "))
dev.off()

# Clear workspace.
# ========================
rm(list = ls())

