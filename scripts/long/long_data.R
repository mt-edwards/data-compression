############################
# Long Data.               #
############################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
args = commandArgs(TRUE)

# Libraries.
# =======================
package_names = c("tidyverse", "plyr")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/long/long_fun.R")

# Load data.
# =======================
load(paste0("data/", args[1], "/smf.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("data/", args[1], "/spec.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Normalised spectrum.
# ========================
nspec = aaply(spec, 1:2, long_nspec, smf = smf)

# Save files.
# =======================
save(nspec, file = paste0("data/", args[1], "/nspec.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Clear workspace.
# =======================
rm(list = ls())
