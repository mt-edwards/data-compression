##########################
# Lat Data.              #
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
package_names = c("tidyverse", "abind", "plyr")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/lat/lat_fun.R")

# Load data.
# =======================
load(paste0("data/", args[1], "/smf.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("data/", args[1], "/spec.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("data/", args[1], "/nspec.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("models/", args[1], "/lat_model.r", args[2], ".p", args[3], ".q", args[4], ".", args[5], ".R"))

# Latitudinal model parameters.
# ========================
if (args[5] == "s") pars = lat_model$par
if (args[5] == "n") pars = lapply(lat_model, function(mod) mod$par)

# Coherence function.
# ========================
if (args[5] == "s") coh = replicate(ncol(smf) - 1, coh_fun(pars, seq_len(nrow(smf)) - 1, nrow(smf)))
if (args[5] == "n") coh = sapply(pars, function(par) coh_fun(par, seq_len(nrow(smf)) - 1, nrow(smf)))

# Cross-spectral mass function.
# ========================
csmf = lat_csmf(smf, coh)

# Detrended normalized spectrum.
# ========================
dnspec = aaply(nspec, 1:2, function(nsp) lat_detrend(coh, nsp))

# Mean cross-meriodogram.
# ========================
mcpgram = apply(apply(aaply(apply(spec, 3:4, c), 1, spec_con), c(1, 4), lat_cpgram), c(1, 3), mean)

# Save files.
# =======================
save(coh, file = paste0("data/", args[1], "/coh.r", args[2], ".p", args[3], ".q", args[4], ".", args[5], ".R"))
save(csmf, file = paste0("data/", args[1], "/csmf.r", args[2], ".p", args[3], ".q", args[4], ".", args[5], ".R"))
save(dnspec, file = paste0("data/", args[1], "/dnspec.r", args[2], ".p", args[3], ".q", args[4], ".", args[5], ".R"))
save(mcpgram, file = paste0("data/", args[1], "/mcpgram.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Clear workspace.
# =======================
rm(list = ls())
