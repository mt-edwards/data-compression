##########################
# Simulation.            #
##########################

# Command line arguments
# =======================
# - 1) Variable name 1.
# - 2) Variable name 2.
# - 3) Variable name 3.
# - 4) Ensemble size.
# - 5) AR order.
# - 6) MA order.
# - 7) Taper.
# - 8) Simulated ensemble size.
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
mcsmf1 = get(load(paste0("data/ALL/mcsmf.", args[1], ".", args[2], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".R")))
mcsmf2 = get(load(paste0("data/ALL/mcsmf.", args[1], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".R")))
mcsmf3 = get(load(paste0("data/ALL/mcsmf.", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".R")))
coh1 = get(load(paste0("data/", args[1], "/coh.r", args[4], ".p", args[5], ".q", args[6], ".R")))
coh2 = get(load(paste0("data/", args[2], "/coh.r", args[4], ".p", args[5], ".q", args[6], ".R")))
coh3 = get(load(paste0("data/", args[3], "/coh.r", args[4], ".p", args[5], ".q", args[6], ".R")))
smf1 = get(load(paste0("data/", args[1], "/smf.r", args[4], ".p", args[5], ".q", args[6], ".R")))
smf2 = get(load(paste0("data/", args[2], "/smf.r", args[4], ".p", args[5], ".q", args[6], ".R")))
smf3 = get(load(paste0("data/", args[3], "/smf.r", args[4], ".p", args[5], ".q", args[6], ".R")))
temp_model1 = get(load(paste0("models/", args[1], "/temp_model.r", args[4], ".p", args[5], ".q", args[6], ".R")))
temp_model2 = get(load(paste0("models/", args[2], "/temp_model.r", args[4], ".p", args[5], ".q", args[6], ".R")))
temp_model3 = get(load(paste0("models/", args[3], "/temp_model.r", args[4], ".p", args[5], ".q", args[6], ".R")))
M1 = get(load(paste0("data/", args[1], "/M.r", args[4], ".R")))
M2 = get(load(paste0("data/", args[2], "/M.r", args[4], ".R")))
M3 = get(load(paste0("data/", args[3], "/M.r", args[4], ".R")))

# Concatinate files.
# ========================
mcsmf = cbind(mcsmf1, mcsmf2, mcsmf3); rm(mcsmf1, mcsmf2, mcsmf3)
coh = abind(coh1, coh2, coh3, rev.along = 0); rm(coh1, coh2, coh3)
smf = abind(smf1, smf2, smf3, rev.along = 0); rm(smf1, smf2, smf3)
temp_model = aperm(abind(aaply(temp_model1, 1:2, mod_2_vec, p.max = as.numeric(args[5])),
                         aaply(temp_model2, 1:2, mod_2_vec, p.max = as.numeric(args[5])),
                         aaply(temp_model3, 1:2, mod_2_vec, p.max = as.numeric(args[5])),
                         rev.along = 0), c(3, 1, 2, 4)); rm(temp_model1, temp_model2, temp_model3)
M = abind(M1, M2, M3, rev.along = 0); rm(M1, M2, M3)

# Complex squareroots.
# ========================
Rs = alply(mcsmf, 1, compose(cplx_sqrt, csm_matrix3))

# List of cross-spectral innovations.
# ========================
dnspec_list = lapply(Rs, mvn_sim_array, dims = c(as.numeric(args[8]), dim(M)[c(1, 3)]))

# Array of cross-spectral innovations.
# ========================
dnspec = aperm(aaply(abind(unname(dnspec_list), along = 0), 2:5, make_real), c(2, 3, 5, 4, 1))

# Trended cross-spectral innnovations.
# ========================
nspec = aperm(aaply(dnspec, 1:2, lat_trend_array, Phi = coh, .progress = "text"), c(1, 2, 4, 3, 5))

# Unnormalised cross-spectral innovations.
# ========================
spec = aaply(nspec, 1:2, function(X) X * sqrt(smf))

# Inverse Fourier transform innoations.
# ========================
resid = Re(aperm(aaply(spec, c(1, 2, 4, 5), inverse_nfft), c(1, 2, 5, 3, 4)))

# Trended innovations.
# ========================
temp_model_resid = unname(abind(aperm(replicate(as.numeric(args[8]), temp_model), c(5, 1, 2, 3, 4)), resid, along = 2))
D = aperm(aaply(temp_model_resid, 3:5, temp_trend_array, p.max = as.numeric(args[5]), .progress = "text"), c(4, 5, 1, 2, 3))

# Complete data.
# ========================
Y = aaply(D, 1, function(X) X + M)

# Save simulated ensemble.
# ========================
save(D, file = paste0("data/ALL/D.", args[1], ".", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R"))
save(Y, file = paste0("data/ALL/Y.", args[1], ".", args[2], ".", args[3], ".r", args[4], ".p", args[5], ".q", args[6], ".t", args[7], ".s", args[8], ".R"))

# Clear workspace.
# ========================
rm(list = ls())
