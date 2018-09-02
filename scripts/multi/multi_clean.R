##########################
# Multivariate Clean.    #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
# - 5) Taper.
args = commandArgs(TRUE)

# Libraries.
# ========================
package_names = c("tidyverse")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# ========================
source("scripts/multi/multi_fun.R")

# Load files.
# ========================
load(paste0("data/", args[1], "/dnspec.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Tapered spectra.
# ========================
tdnspec = tapered_spec(dnspec, as.numeric(args[5]))

# Unscaled spectra.
# ========================
utdnspec = unscaled_spec(tdnspec)

# Save files.
# =======================
save(utdnspec, file = paste0("data/", args[1], "/utdnspec.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".R"))

# Clear workspace.
# =======================
rm(list = ls())
