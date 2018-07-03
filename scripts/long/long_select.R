###########################
# Longitudinal Selection. #
###########################

# Command line arguments.
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
args = commandArgs(TRUE)

# Libraries.
# =======================
package_names = c("purrr", "plyr")
invisible(lapply(package_names, library, character.only = TRUE))

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/long/long_fun.R")

# File names.
# =======================
file_names = list.files(paste0("models/", args[1]), full.names = TRUE, pattern = paste0("long_model.r", args[2]))

# Temporal model list.
# =======================
long_model_list = map(file_names, ~ get(load(.x)))

# Load latitude.
# =======================
load(file = paste0("data/", args[1], "/lon.R"))
load(file = paste0("data/", args[1], "/lat.R"))

# Longitudinal model parameter list.
# =======================
pars_list = lapply(long_model_list, function(long_model) sapply(seq_along(long_model), function(m) long_model[[m]]$par))

# Spectral mass function list.
# =======================
specs_array = laply(pars_list, function(pars) apply(pars, 2, spec_mass, N = length(lon), class = cal_SMF(pars)))

# AIC matrix.
# =======================
AIC_matrix = do.call(rbind, lapply(long_model_list, function(long_model) sapply(long_model, cal_AIC)))

# Best AIC index.
# =======================
AIC_index = apply(AIC_matrix, 2, best_AIC)

# Spectral mass functions.
# =======================
smf = sapply(seq_along(lat), function(n) specs_array[AIC_index[n], , n])

# Save data.
# =======================
save(smf, file = paste0("data/", args[1], "/smf.r", args[2], ".p", args[3], ".q", args[4], ".R"))

# Clear workspace.
# =======================
rm(list = ls())
