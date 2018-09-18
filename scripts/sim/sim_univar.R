##########################
# Simulation.            #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
# - 5) Taper.
# - 6) Simulated ensemble size.
args = commandArgs(TRUE)

# Libraries.
# ========================
package_names = c("tidyverse", "abind", "plyr")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# ========================
source("scripts/sim/sim_fun.R")

# Load files.
# ========================
load(paste0("data/", args[1], "/coh.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("data/", args[1], "/smf.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("models/", args[1], "/temp_model.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Temporal model vector.
# ========================
temp_model = aaply(temp_model, 1:2, mod_2_vec)

# Array of cross-spectral innovations.
# ========================
dnspec = array(rnorm(as.numeric(args[6]) * 95 * 288 * 192), dim = c(as.numeric(args[6]), 95, 288, 192))

# Trended cross-spectral innnovations.
# ========================
nspec = aperm(aaply(dnspec, 1:2, lat_trend_array2, Phi = coh, .progress = "text"), c(1, 2, 4, 3)); rm(dnspec, coh)

# Unnormalised cross-spectral innovations.
# ========================
spec = aaply(nspec, 1:2, function(X) X * sqrt(smf), .progress = "text"); rm(nspec, smf)

# Inverse Fourier transform innoations.
# ========================
resid = Re(aperm(aaply(spec, c(1, 2, 4), inverse_nfft, .progress = "text"), c(1, 2, 4, 3))); rm(spec)

# Model and residual array.
# ========================
temp_model_resid = abind(aperm(replicate(as.numeric(args[6]), temp_model), c(4, 3, 1, 2)), resid, along = 2); rm(resid, temp_model)

 # Complete data.
# ========================
Y = aperm(aaply(temp_model_resid, 3:4, temp_trend_array, p.max = as.numeric(args[3]), .progress = "text"), c(4, 3, 1, 2)); rm(temp_model_resid)

# Save simulated ensemble.
# ========================
save(Y, file = paste0("data/", args[1], "/Y.r", args[2], ".p", args[3], ".q", args[4], ".t", args[5], ".s", args[6], ".R"))

# Clear workspace.
# ========================
rm(list = ls())
