############################
# Longitudinal Model.      #
############################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
# - 5) Spectral mass function.
args = commandArgs(TRUE)

# Libraries.
# =======================
package_names = c("parallel")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/long/long_fun.R")

# Load data.
# =======================
load(paste0("data/", args[1], "/pgram.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("data/", args[1], "/lat.R"))

# Longitudinal model fitting. 
# =======================
cl = makeCluster(detectCores() - 1)
clusterExport(cl, list("long_fit", "full_Whittle_neg_log_like", "Whittle_neg_log_like", "spec_mass", "spec_Matern", "sine2", "pyramid2", "beta_mix"))
long_model = parApply(cl, pgram, 4, long_fit, class = args[5], init = rep(0, ifelse(args[5] == "mm", 2, 3)))
stopCluster(cl)

# Longitudinal model parameters.
# =======================
long_pars = sapply(long_model, function(mod) mod$par)
long_init = apply(long_pars, 1, function(par) lowess(par, f = 1 / 10)$y)

# Longitudinal model fitting. 
# =======================
long_model = lapply(seq_along(lat), function(m) long_fit(pgram[, , , m], args[5], long_init[m, ]))

# Save longitudinal model.
# =======================
save(long_model, file = paste0("models/", args[1], "/long_model.r", args[2], ".p", args[3], ".q", args[4], ".", args[5], ".R"))

# Clear workspace.
# =======================
rm(list = ls())
