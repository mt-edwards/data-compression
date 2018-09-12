############################
# Temporal Model.          #
############################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
args = commandArgs(TRUE)

# Load libraries.
# =======================
package_names = c("tidyverse", "forecast", "parallel")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/temp/temp_fun.R")

# Load data.
# =======================
load(paste0("data/", args[1], "/Y.R"))

# Load data.
# =======================
Y = Y[seq_len(args[2]), , , ]

# Temporal Model.
# =======================
cl = makeCluster(detectCores() - 1)
clusterExport(cl, list("Y", "temp_fit", "auto.arima", "args"))
temp_model = parApply(cl, Y, 3:4, temp_fit, args = args)
stopCluster(cl)

# Save files.
# =======================
save(temp_model, file = paste0("models/", args[1], "/temp_model.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Clear workspace.
# =======================
rm(list = ls())
