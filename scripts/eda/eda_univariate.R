##########################
# Univariate EDA.        #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
args = commandArgs(TRUE)
args = "TREFHT"

# Load libraries.
# =======================
package_names = c("tidyverse", "spatstat")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/eda/eda_fun.R")

# Load data.
# =======================
load(file = paste0("data/", args[1], "/Y.R"))
load(file = paste0("data/", args[1], "/lat.R"))
load(file = paste0("data/", args[1], "/year.R"))

# Probabilities.
# =======================
ps = seq(0, 1, 1 / 4)

# Variable Statistics.
# =======================
var_w      = weight_array(Y, lat)
var_quants = weighted.quantile(Y, var_w, probs = ps)
var_mean   = weighted.mean(Y, var_w)

# Temporal Statistics.
# =======================
temp_w      = weight_array(Y[, 1, , ], lat)
temp_quants = apply(Y, 2, weighted.quantile, w = temp_w, probs = ps)
temp_mean   = apply(Y, 2, weighted.mean, w = temp_w)

# Temporal Data Frame.
# ========================
temp_df = tibble(Year = year,
                 Mean = temp_mean) %>%
  cbind(t(temp_quants))

# Temporal Plot.
# ========================
ggplot(temp_df, aes(x = Year)) +
  geom_line(aes(y = `50%`), col = "blue") +
  geom_ribbon(aes(min = `25%`, max = `75%`), fill = "blue", alpha = 0.4) +
  geom_point(aes(y = `100%`), shape = 1, col = "blue") +
  geom_point(aes(y = `0%`), shape = 1, col = "blue") +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 10) - 1) +
  ylab(args)

# Latitudinal Statistics.
# ========================
lat_quants
lat_mean

# Clear workspace.
# =======================
rm(list = ls())
