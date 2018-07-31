##########################
# Multivariate Data.    #
##########################

# =======================
# - 1) Variable name 1.
# - 2) Variable name 2.
# - 3) Ensemble size.
# - 4) AR order.
# - 5) MA order.
# - 6) Stationary.
# - 7) Taper.
args = commandArgs(TRUE)

# Libraries.
# ========================
package_names = c("tidyverse", "abind", "parallel", "complexplus")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# ========================
source("scripts/multi/multi_fun.R")

# Load files.
# ========================
load(paste0("data/", args[1], "/utdnspec.r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".t", args[7], ".R"))
load(paste0("models/ALL/multi_model.", args[1], ".", args[2], ".r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".t", args[7], ".R"))

# Multivariate cross-spectral mass function.
# ========================
mcsmf = csm_fun(multi_model$par, seq_len(dim(utdnspec)[3]) - 1, dim(utdnspec)[3])

# Save files.
# ========================
save(mcsmf, file = paste0("data/ALL/mcsmf.", args[1], ".", args[2], ".r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".t", args[7], ".R"))

# Clear workspace.
# ========================
rm(list = ls())
