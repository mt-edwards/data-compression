##########################
# Multivariate Model.    #
##########################

# =======================
# - 1) Variable name 1.
# - 2) Variable name 2.
# - 3) Ensemble size.
# - 4) AR order.
# - 5) MA order.
# - 6) Taper.
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
utdnspec1 = get(load(paste0("data/", args[1], "/utdnspec.r", args[3], ".p", args[4], ".q", args[5], ".t", args[6], ".R")))
utdnspec2 = get(load(paste0("data/", args[2], "/utdnspec.r", args[3], ".p", args[4], ".q", args[5], ".t", args[6], ".R")))

# Concatinate arrays.
# ========================
# utdnspec = abind(utdnspec1, utdnspec2, rev.along = 0)

# Multivarite model fitting.
# ========================
# cl = makeCluster(detectCores() - 1)
# clusterExport(cl, list("multi_neg_log_like", "quad_form"))
# multi_model = multi_fit(utdnspec)
# stopCluster(cl)

# Mean multivariate cross-periodogram.
# ========================
mcpgram = multi_pgram(utdnspec1, utdnspec2)

# Multivariate cross-spectral mass function.
# ========================
mcsmf = smooth.spline(mcpgram$modulus, df = 20)$y

# Save files.
# ========================
# save(multi_model, file = paste0("models/ALL/multi_model.", args[1], ".", args[2], ".r", args[3], ".p", args[4], ".q", args[5], ".t", args[6], ".R"))
save(mcsmf, file = paste0("data/ALL/mcsmf.", args[1], ".", args[2], ".r", args[3], ".p", args[4], ".q", args[5], ".t", args[6], ".R"))

# Clear workspace.
# ========================
rm(list = ls())
