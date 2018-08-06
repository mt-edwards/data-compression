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
coh1 = get(load(paste0("data/", args[1], "/coh.r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".R")))
coh2 = get(load(paste0("data/", args[2], "/coh.r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".R")))
smf1 = get(load(paste0("data/", args[1], "/smf.r", args[3], ".p", args[4], ".q", args[5], ".R")))
smf2 = get(load(paste0("data/", args[2], "/smf.r", args[3], ".p", args[4], ".q", args[5], ".R")))
temp_model1 = get(load(paste0("models/", args[1], "/temp_model.r", args[3], ".p", args[4], ".q", args[5], ".R")))
temp_model2 = get(load(paste0("models/", args[2], "/temp_model.r", args[3], ".p", args[4], ".q", args[5], ".R"))); rm(temp_model)
M1 = get(load(paste0("data/", args[1], "/M.r", args[3], ".R")))
M2 = get(load(paste0("data/", args[2], "/M.r", args[3], ".R")))

# Concatinate files.
# ========================
coh = abind(coh1, coh2, rev.along = 0); rm(coh1, coh2)
smf = abind(smf1, smf2, rev.along = 0); rm(smf1, smf2)
temp_model = aperm(abind(aaply(temp_model1, 1:2, mod_2_vec, args = args),
                   aaply(temp_model2, 1:2, mod_2_vec, args = args), 
                   rev.along = 0), c(3, 1, 2, 4)); rm(temp_model1, temp_model2)
M = abind(M1, M2, rev.along = 0); rm(M1, M2)

# Complex squareroots.
# ========================
Rs = lapply(mcsmf, compose(cplx_sqrt, csm_matrix))

# List of cross-spectral innovations.
# ========================
dnspec_list = lapply(Rs, mvn_sim_array, dims = c(3, dim(M)[c(1, 3)]))

# Array of cross-spectral innovations.
# ========================
dnspec = aperm(aaply(abind(dnspec_list, along = 0), 2:5, make_real), c(2, 3, 5, 4, 1))

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
D = aperm(aaply(unname(abind(aperm(replicate(3, temp_model), c(5, 1, 2, 3, 4)), resid, along = 2)), 3:5, temp_trend_array, args = args, .progress = "text"), c(4, 5, 1, 2, 3))

# Complete data.
# ========================
Y = aaply(D, 1, function(d) d + M)

# Clear workspace.
# ========================
rm(list = ls())

# # Test indexes.
# r.ind = 1; t.ind = 30; n.ind = 20; m.ind = 50; v.ind = 1
# 
# # dnspec_list test.
# mcsmf[n.ind]
# mean(dnspec_list[[n.ind]][1, , , ] * Conj(dnspec_list[[n.ind]][2, , , ]))
# 
# # dnspec test.
# plot(mcsmf, type = "l")
# points(aaply(dnspec, 3, function(X) mean(X[, , , 1] * Conj(X[, , , 2]))))
# 
# # nspec test.
# plot(coh[, m.ind, v.ind], type = "l")
# points(apply(nspec, 3, function(X) mean(X[, , m.ind, v.ind] * Conj(X[, , m.ind + 1, v.ind]))))
# 
# # spec test.
# plot(smf[, m.ind, v.ind], type = "l", log = "y")
# points(apply(spec, 3, function(X) mean(X[, , m.ind, v.ind] * Conj(X[, , m.ind, v.ind]))))
# 
# # resid test.
# all.equal(c(Im(resid)), rep(0, length(resid)))
# 
# # Save and load files.
# # ========================
# save(dnspec, file = "dnspec.R")
# load("dnspec.R")
# save(nspec, file = "nspec.R")
# load("nspec.R")
# save(spec, file = "spec.R")
# load("spec.R")
# save(resid, file = "resid.R")
# load("resid.R")