############################
# Temp Data.               #
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
package_names = c("tidyverse", "plyr", "RNetCDF")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/temp/temp_fun.R")

# Load data.
# =======================
load(paste0("data/", args[1], "/year.R"))
load(paste0("data/", args[1], "/lon.R"))
load(paste0("data/", args[1], "/lat.R"))
load(paste0("models/", args[1], "/temp_model.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Temporal parameters.
# ========================
pars = apply(temp_model, 1:2, temp_pars)

# Save NetCDF files.
# =======================
map2(alply(pars, 1), first(dimnames(pars)), save_ncdf, lon = lon, lat = lat, args = args)

# Residuals.
# =======================
resid = aperm(aaply(temp_model, 1:2, temp_resid, args = args), c(4:3, 1:2))

# Spectrum.
# =======================
spec = aperm(apply(resid, c(1:2, 4), temp_fft), c(2:3, 1, 4))

# Periodogram.
# =======================
pgram = aperm(apply(spec, c(1:2, 4), temp_pgram), c(2:3, 1, 4))

# Mean periodogram.
# =======================
mpgram = apply(pgram, 3:4, mean)

# Save files.
# =======================
save(pars, file = paste0("models/", args[1], "/temp_pars.r", args[2], ".p", args[3], ".q", args[4], ".R"))
save(resid, file = paste0("data/", args[1], "/resid.r", args[2], ".p", args[3], ".q", args[4], ".R"))
save(spec, file = paste0("data/", args[1], "/spec.r", args[2], ".p", args[3], ".q", args[4], ".R"))
save(pgram, file = paste0("data/", args[1], "/pgram.r", args[2], ".p", args[3], ".q", args[4], ".R"))
save(mpgram, file = paste0("data/", args[1], "/mpgram.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Clear workspace.
# =======================
rm(list = ls())
