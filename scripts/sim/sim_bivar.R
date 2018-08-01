##########################
# Simulation.            #
##########################

# Command line arguments
# =======================
# - 1) Variable name 1.
# - 2) Variable name 2.
# - 3) Ensemble size.
# - 4) AR order.
# - 5) MA order.
# - 6) Stationary.
# - 7) Taper.
args = commandArgs(TRUE)
args = c("FSNS", "FLNS", 5, 3, 0, "n", 10)

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
load(paste0("data/ALL/mcsmf.", args[1], ".", args[2], ".r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".t", args[7], ".R"))
csmf1 = get(load(paste0("data/", args[1], "/csmf.r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".R")))
csmf2 = get(load(paste0("data/", args[2], "/csmf.r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".R")))
smf1 = get(load(paste0("data/", args[1], "/smf.r", args[3], ".p", args[4], ".q", args[5], ".R")))
smf2 = get(load(paste0("data/", args[2], "/smf.r", args[3], ".p", args[4], ".q", args[5], ".R")))
temp_model1 = get(load(paste0("models/", args[1], "/temp_model.r", args[3], ".p", args[4], ".q", args[5], ".R")))
temp_model2 = get(load(paste0("models/", args[1], "/temp_model.r", args[3], ".p", args[4], ".q", args[5], ".R"))); rm(temp_model)
M1 = get(load(paste0("data/", args[1], "/M.r", args[3], ".R")))
M2 = get(load(paste0("data/", args[2], "/M.r", args[3], ".R")))

# Concatinate files.
# ========================
csmf = abind(csmf1, csmf2, rev.along = 0); rm(csmf1, csmf2)
smf = abind(smf1, smf2, rev.along = 0); rm(smf1, smf2)
M = abind(M1, M2, rev.along = 0); rm(M1, M2)

# Complex squareroots.
# ========================
Rs = lapply(mcsmf, compose(cplx_sqrt, csm_matrix))

# List of cross-spectral innovations.
# ========================
dnspec_list = lapply(Rs, mvn_sim_array, dims = c(5, 95, 192))

# Array of cross-spectral innovations.
# ========================
dnspec = aperm(abind(dnspec_list, along = 0), c(3, 4, 1, 5, 2))

# Trended cross-spectral innnovations.
# ========================
nspec = aaply(dnspec, 1:2, lat_trend_array, Phi = csmf)

# Unnormalize cross-spectral innovations.
# ========================
spec = aaply(nspec, 1:2, function(X) X * sqrt(smf))

# Inverse Fourier transform innoations.
# ========================
resid = aperm(aaply(spec, c(1, 2, 4, 5), inverse_nfft), c(1, 2, 5, 3, 4))


save(resid, file = "resid.R")
load("resid.R")
