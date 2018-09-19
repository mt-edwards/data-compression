##########################
# Univariate EDA.        #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
args = commandArgs(TRUE)

# Load libraries.
# =======================
package_names = c("tidyverse", "parallel", "Hmisc", "plyr", "gridExtra")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/eda/eda_fun.R")

# Load data.
# =======================
load(file = paste0("data/", args[1], "/Y.R"))
load(file = paste0("data/", args[1], "/year.R"))
load(file = paste0("data/", args[1], "/lon.R"))
load(file = paste0("data/", args[1], "/lat.R"))

# Variable Statistics.
# =======================
var_w = weight_array(Y[1, , , ], lat)
cl = makeCluster(detectCores() - 1)
var_q = parApply(cl, Y, 1, wtd.quantile, weights = var_w)
var_m = parApply(cl, Y, 1, wtd.mean, weights = var_w)
stopCluster(cl)

# Variable data frame.
# =======================
print(round(data.frame(Means = c(mean(var_m), rowMeans(var_q)),
                       SDs   = c(sd(var_m), apply(var_q, 1, sd))), 2))

# Temporal and latitudinal statistics.
# =======================
temp_w = weight_array(Y[1, 1, , ], lat)
cl = makeCluster(detectCores() - 1)
temp_m = parApply(cl, Y, 1:2, wtd.mean, weights = temp_w)
lat_m  = parApply(cl, Y, c(1, 4), mean)
stopCluster(cl)

# Temporal and latitudinal statistics plot.
# =======================
png(paste0("plots/", args, "/temp_lat.png"), width = 900, height = 900)
grid.arrange(ensemble_plot(year, temp_m, "Year", paste("Global Mean", args), seq(2010, 2100, 10)), 
             ensemble_plot(lat, lat_m, "Latitude", paste("Longitudinal Mean", args), seq(-90, 90, 30)))
dev.off()

# Spatial Statistics.
# ========================
cl = makeCluster(detectCores() - 1)
spat_p = apply(Y[1, , , ], 2:3, spat_pars)
stopCluster(cl)

# Spatial NetCDF File.
# ========================
map2(alply(spat_p, 1), c("intercept", "trend", "std", "auto_cov"), save_ncdf, lon = lon, lat = lat, args = args)

# Clear workspace.
# =======================
rm(list = ls())
