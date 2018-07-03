##########################
# Univariate EDA.        #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
args = commandArgs(TRUE)

# Load libraries.
# =======================
package_names = c("tidyverse", "spatstat", "RNetCDF")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# =======================
source("scripts/temp/temp_fun.R")
source("scripts/eda/eda_fun.R")

# Load data.
# =======================
load(file = paste0("data/", args[1], "/Y.R"))
load(file = paste0("data/", args[1], "/lon.R"))
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
  geom_point(aes(y = `100%`), col = "blue", size = 0.1) +
  geom_point(aes(y = `0%`), col = "blue", size = 0.1) +
  scale_x_continuous(breaks = seq(min(year), max(year), by = 10) - 1) +
  ylab(args)

# Save Plot.
# ========================
ggsave("stats_plot_temp.png", path = paste0("plots/", args), width = 15, height = 8, units = "cm")

# Latitudinal Statistics.
# ========================
lat_quants = apply(Y, 4, quantile, probs = ps)
lat_mean   = apply(Y, 4, mean) 

# Latitudinal Data Frame.
# ========================
lat_df = tibble(Latitude = lat,
                Mean     = lat_mean) %>%
  cbind(t(lat_quants))

# Temporal Plot.
# ========================
ggplot(lat_df, aes(x = Latitude)) +
  geom_line(aes(y = `50%`), col = "blue") +
  geom_ribbon(aes(min = `25%`, max = `75%`), fill = "blue", alpha = 0.4) +
  geom_point(aes(y = `100%`), col = "blue", size = 0.1) +
  geom_point(aes(y = `0%`), col = "blue", size = 0.1) +
  scale_x_continuous(breaks = seq(-90, 90, by = 30)) +
  ylab(args)

# Save Plot.
# ========================
ggsave("stats_plot_lat.png", path = paste0("plots/", args), width = 15, height = 8, units = "cm")

# Spatial Statistics.
# ========================
spat_quants = apply(Y, 3:4, quantile, probs = ps)
spat_mean   = apply(Y, 3:4, mean)
spat_median = spat_quants[3, , ]
spat_IQR    = spat_quants[4, , ] - spat_quants[2, , ]

# Spatial NetCDF File.
# ========================
save_ncdf(spat_median, lon, lat, "med", args)
save_ncdf(spat_IQR, lon, lat, "iqr", args)

# Clear workspace.
# =======================
rm(list = ls())
