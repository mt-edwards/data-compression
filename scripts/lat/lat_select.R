##########################
# Latitudinal Selection. #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
args = commandArgs(TRUE)

# Libraries.
# ========================
package_names = c("tidyverse", "parallel", "plyr")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# ========================
source("scripts/lat/lat_fun.R")

# Load files.
# ========================
lat_model_s = get(load(paste0("models/", args[1], "/lat_model.r", args[2], ".p", args[3], ".q", args[4], ".s.R")))
lat_model_n = get(load(paste0("models/", args[1], "/lat_model.r", args[2], ".p", args[3], ".q", args[4], ".n.R")))
load(paste0("data/", args[1], "/nspec.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Latitudinal model parameters.
# ========================
pars_s = lat_model_s$par
pars_n = lapply(lat_model_n, function(mod) mod$par)

# Coherence function.
# ========================
coh_s = alply(replicate(dim(nspec)[4] - 1, coh_fun(pars_s, seq_len(dim(nspec)[3]) - 1, dim(nspec)[3])), 1)
coh_n = alply(sapply(pars_n, function(par) coh_fun(par, seq_len(dim(nspec)[3]) - 1, dim(nspec)[3])), 1)

# Coherernce matrices.
# ========================
Cs_s = lapply(coh_s, ns_coh_matrix)
Cs_n = lapply(coh_n, ns_coh_matrix)

# Stationary AIC.
# ========================
cl = makeCluster(detectCores() - 1)
clusterExport(cl, list("lat_neg_log_like", "quad_form"))
aic_s = cal_AIC(Cs_s, nspec, length(pars_s))
stopCluster(cl)

# Non-stationary AIC
# ========================
cl = makeCluster(detectCores() - 1)
clusterExport(cl, list("lat_neg_log_like", "quad_form"))
aic_n = cal_AIC(Cs_n, nspec, length(unlist(pars_n)))
stopCluster(cl)

# AIC selection.
# ========================
if (aic_n < aic_s + 2) coh = laply(coh_n, identity) else coh = laply(coh_s, identity)

# Save files.
# =======================
save(coh, file = paste0("data/", args[1], "/coh.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Clear workspace.
# =======================
rm(list = ls())
