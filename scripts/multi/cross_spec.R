##########################
# Cross-spectra.         #
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

# Libraries.
# ========================
package_names = c("tidyverse")
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

# Mean multivariate cross-periodogram.
# ========================
mcpgram = multi_pgram(utdnspec1, utdnspec2)

# Mean multivariate cross-periodogram data frame.
# ========================
mcpgram_df = tibble(Frequency = rep(0:(dim(utdnspec1)[3] - 1) / dim(utdnspec1)[3], 2),
                    Value     = unlist(mcpgram),
                    Type      = rep(c("Modulus", "Argument"), each = dim(utdnspec1)[3]))

# Mean multivariate cross-periodogram plot.
# ========================
ggplot(mcpgram_df, aes(x = Frequency, y = Value)) +
  geom_point(col = "blue", shape = 1) +
  facet_grid(Type ~ ., scales = "free") +
  scale_x_continuous(limits = c(0, 0.5)) +
  labs(main = "Test") +
  ggtitle(paste(args[1], args[2], sep = " - ")) +
  theme_bw()

# Save plot.
# ========================
ggsave(paste0("mcpgram_plot.", args[1], ".", args[2], ".r", args[3], ".p", args[4], ".q", args[5], ".", args[6], ".t", args[7], ".png"), path = paste0("plots/ALL"), width = 6, height = 8, units = "cm")

# Clear workspace.
# ========================
rm(list = ls())
