##########################
# Latitudinal Model.     #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
# - 5) Stationary.
args = commandArgs(TRUE)

# Libraries.
# ========================
package_names = c("tidyverse", "parallel")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# ========================
source("scripts/lat/lat_fun.R")

# Load files.
# ========================
load(paste0("data/", args[1], "/nspec.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Latitudinal model fitting.
# ========================
cl = makeCluster(detectCores() - 1)
clusterExport(cl, list("nspec", "lat_neg_log_like", "quad_form"))
if (args[5] == "s") lat_model = lat_fit(nspec)
if (args[5] == "n") lat_model = lapply(lat_pairs(nspec), lat_fit)
stopCluster(cl)

# Save files.
# ========================
save(lat_model, file = paste0("models/", args[1], "/lat_model.r", args[2], ".p", args[3], ".q", args[4], ".", args[5], ".R"))

# Clear workspace.
# ========================
rm(list = ls())
