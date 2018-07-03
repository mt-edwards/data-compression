##########################
# Bivariate EDA.         #
##########################

# Command line arguments
# =======================
# - 1) Variable 1 name.
# - 2) Variable 2 name.
args = commandArgs(TRUE)

# Load libraries.
# =======================
package_names = c("tidyverse")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/eda/eda_fun.R")

# Load data.
# =======================
Y1 = get(load(file = paste0("data/", args[1], "/Y.R")))
Y2 = get(load(file = paste0("data/", args[2], "/Y.R")))

# -----------------------

# Clear workspace.
# =======================
rm(list = ls())