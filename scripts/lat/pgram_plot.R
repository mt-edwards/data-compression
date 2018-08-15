##########################
# Periodogram Plots.     #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
# - 2) Ensemble size.
# - 3) AR order.
# - 4) MA order.
# - 5) Stationary.
args = commandArgs(TRUE)

# Libraries.
# ========================
package_names = c("tidyverse")
lapply(package_names, library, character.only = TRUE)

# Set Working Directory (bash scripts).
# ========================
setwd("/Users/matthewedwards/Sync/Projects/data-compression")

# Source functions.
# ========================
source("scripts/lat/lat_fun.R")

# Load files.
# ========================
load(paste0("data/", args[1], "/smf.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("data/", args[1], "/csmf.r", args[2], ".p", args[3], ".q", args[4], ".", args[5], ".R"))
load(paste0("data/", args[1], "/mpgram.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("data/", args[1], "/mcpgram.r", args[2], ".p", args[3], ".q", args[4], ".R"))
load(paste0("data/", args[1], "/lat.R"))

# Prediodogram data frame.
# ========================
indx = c(43, 86, 128, 160)
pgram_df = tibble(Periodogram  = as.vector(cbind(mpgram[, indx], mcpgram[, indx])),
                  SMF          = as.vector(cbind(smf[, indx], csmf[, indx])),
                  Type         = factor(rep(c("Auto-spectra", "Cross-spectra"), each = length(Periodogram) / 2)),
                  Location     = factor(rep(rep(pgram_title(indx, lat), each = nrow(mpgram)), 2), 
                                     levels = unique(pgram_title(indx, lat))),
                  Frequency    = rep(0:(nrow(mpgram) - 1) / nrow(mpgram), length(indx) * 2))

# Periodogram plot.
# ========================
g = ggplot(pgram_df, aes(x = Frequency)) +
  geom_point(aes(y = Mod(Periodogram)), col = "blue", shape = 1) +
  geom_line(aes(y = SMF), col = "red", lwd = 1.1) +
  facet_grid(Type ~ Location, scales = "free_y") +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(labels = fancy_scientific,
                     trans = "log10") +
  ylab("Spectrum") +
  theme_bw()

# Save plot.
# ========================
ggsave(paste0("pgram_plots.r", args[2], ".p", args[3], ".q", args[4], ".", args[5], ".png"), 
              plot = g, path = paste0("plots/", args[1]),
              width = 15, height = 8, units = "cm")

# Clear workspace.
# ========================
rm(list = ls())
