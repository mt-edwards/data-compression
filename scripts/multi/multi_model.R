##########################
# Multivariate Model.    #
##########################

# =======================
# - 1) Variable name 1.
# - 2) Variable name 2.
# - 3) Ensemble size.
# - 4) AR order.
# - 5) MA order.
# - 6) Stationary.
# - 7) Taper.
args = commandArgs(TRUE)

# Libraries.
# ========================
package_names = c("tidyverse", "abind", "parallel", "complexplus")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# ========================
source("scripts/multi/multi_fun.R")

# Load files.
# ========================
utdnspec1 = get(load(paste0("data/", args[1], "/utdnspec.r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".t", args[7], ".R")))
utdnspec2 = get(load(paste0("data/", args[2], "/utdnspec.r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".t", args[7], ".R")))

# Concatinate arrays.
# ========================
utdnspec = abind(utdnspec1, utdnspec2, rev.along = 0)

# Multivarite model fitting.
# ========================
cl = makeCluster(detectCores() - 1)
clusterExport(cl, list("multi_neg_log_like", "quad_form"))
multi_model = multi_fit(utdnspec)
stopCluster(cl)

# plot(Arg(apply(Conj(utdnspec[, , , , 1]) * utdnspec[, , , , 2], 3, mean)))
# lines(Arg(csm_fun(multi_model$par, 0:287, 288)))
# plot(Mod(apply(Conj(utdnspec[, , , , 1]) * utdnspec[, , , , 2], 3, mean)))
# lines(Mod(csm_fun(multi_model$par, 0:287, 288)))

# Clear workspace.
# ========================
rm(list = ls())
