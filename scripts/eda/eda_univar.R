##########################
# Univariate EDA.        #
##########################

# Command line arguments
# =======================
# - 1) Variable name.
args = commandArgs(TRUE)

# Load libraries.
# =======================
package_names = c("tidyverse", "parallel", "Hmisc", "plyr", "gridExtra", "RNetCDF", "abind")
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
load(file = paste0("data/", args[1], "/year.R"))
load(file = paste0("data/", args[1], "/lon.R"))
load(file = paste0("data/", args[1], "/lat.R"))

# Variable Statistics.
# =======================
var_w = weight_array(Y[1, , , ], lat)
cl = makeCluster(detectCores() - 1)
var_q = parApply(cl, Y, 1, wtd.quantile, weights = var_w)
var_m = parApply(cl, Y, 1, wtd.mean, weights = var_w)
stopCluster(cl)

# Variable data frame.
# =======================
print(round(data.frame(Means = c(mean(var_m), rowMeans(var_q)),
                       SDs   = c(sd(var_m), apply(var_q, 1, sd))), 2))

# Temporal and latitudinal statistics.
# =======================
temp_w = weight_array(Y[1, 1, , ], lat)
cl = makeCluster(detectCores() - 1)
temp_m = parApply(cl, Y, 1:2, wtd.mean, weights = temp_w)
lat_m  = parApply(cl, Y, c(1, 4), mean)
stopCluster(cl)

# Temporal and latitudinal statistics plot.
# =======================
png(paste0("plots/", args, "/temp_lat.png"), width = 900, height = 400)
grid.arrange(ensemble_plot(year, temp_m, "Year", "Global Mean Annomoly", seq(2010, 2100, 10)), 
             ensemble_plot(lat, lat_m, "Latitude", "Longitudinal Mean Annomoly", seq(-90, 90, 30)),
             layout_matrix = matrix(1:2, 1, 2))
dev.off()

# Spatial Statistics.
# ========================
cl = makeCluster(detectCores() - 1)
clusterExport(cl, list("auto_cor"))
spat_p = parApply(cl, Y[1:3, , , ], 3:4, spat_pars)
stopCluster(cl)

# Spatial NetCDF File.
# ========================
map2(alply(spat_p, 1), c("intercept", "trend", "std", "auto_cor"), save_ncdf, lon = lon, lat = lat, args = args)

# Residual arrays.
# =======================
R = aaply(Y, 3:4, lm_res, .progress = "text")

# Longitudinal ensemble arrays.
# =======================
R_lon = abind(R, abind(R[-1, , ], R[1, , , drop = FALSE], along = 1), rev.along = 0)
R_lat = abind(R, abind(R[, -1, ], R[, 1, , drop = FALSE], along = 2), rev.along = 0)

# Cross-spatial statistics.
# ========================
cross_spat_p1 = apply(R_lon, 1:2, cross_cor)
cross_spat_p2 = apply(R_lat, 1:2, cross_cor)

# Spatial.
# =======================
save_ncdf(cross_spat_p1, lon, lat, "lon_cor", args)
save_ncdf(cross_spat_p2, lon, lat, "lat_cor", args)

# Clear workspace.
# =======================
rm(list = ls())

sp_loc = function(year, lon, lat) {
  
  paste0("Lon: ",  round(lon, 1), 
        " E, Lat: ",  round(lat, 1), " N")
  
}

ran_year = sample(seq_along(year), 4)
ran_lon  = sample(seq_along(lon), 4)
ran_lat  = sample(seq_along(lat), 4)
df <- tibble(y = c(scale(Y[, ran_year[1], ran_lon[1], ran_lat[1]]),
                  scale(Y[, ran_year[2], ran_lon[2], ran_lat[2]]),
                  scale(Y[, ran_year[3], ran_lon[3], ran_lat[3]]),
                  scale(Y[, ran_year[4], ran_lon[4], ran_lat[4]])),
            x = rep(c(sp_loc(year[ran_year[1]], lon[ran_lon[1]], lat[ran_lat[1]]),
                      sp_loc(year[ran_year[2]], lon[ran_lon[2]], lat[ran_lat[2]]),
                      sp_loc(year[ran_year[3]], lon[ran_lon[3]], lat[ran_lat[3]]),
                      sp_loc(year[ran_year[4]], lon[ran_lon[4]], lat[ran_lat[4]])),
                    each = dim(Y)[1]))

qplot(data = df, sample = y, facets = . ~ x) +
  geom_abline(intercept = 0, slope = 1, col = "red", lwd = 1, alpha = 0.6) +
  ylab("Empirical Quantiles") +
  xlab("Theoretic Quantiles") +
  theme_bw()
ggsave(paste0(args, "-qqplot"), device = "png", width = 10, height = 3)

ran_year = sample(seq_along(year), 4)
ran_lon  = sample(seq_along(lon), 4)
ran_lat  = sample(seq_along(lat), 4)
df_acf <- tibble(y = c(acf(Y[, ran_year[1], ran_lon[1], ran_lat[1]], plot = FALSE)$acf,
                       acf(Y[, ran_year[2], ran_lon[2], ran_lat[2]], plot = FALSE)$acf,
                       acf(Y[, ran_year[3], ran_lon[3], ran_lat[3]], plot = FALSE)$acf,
                       acf(Y[, ran_year[4], ran_lon[4], ran_lat[4]], plot = FALSE)$acf),
                 x = rep(c(sp_loc(year[ran_year[1]], lon[ran_lon[1]], lat[ran_lat[1]]),
                       sp_loc(year[ran_year[2]], lon[ran_lon[2]], lat[ran_lat[2]]),
                       sp_loc(year[ran_year[3]], lon[ran_lon[3]], lat[ran_lat[3]]),
                       sp_loc(year[ran_year[4]], lon[ran_lon[4]], lat[ran_lat[4]])),
                     each = 16),
                 lag = rep(0:15, 4))

qplot(data = df_acf, x = lag, y = y, facets = . ~ x, xend = lag, yend = 0, geom = "segment") +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = -1/sqrt(33)), col = "red", lwd = 1, alpha = 0.6, lty = 2) +
  geom_hline(aes(yintercept =  1/sqrt(33)), col = "red", lwd = 1, alpha = 0.6, lty = 2) +
  scale_y_continuous(breaks = 0:1) +
  ylab("Auto-correlation") +
  xlab("Lag") +
  theme_bw()
ggsave(paste0(args, "-acfplot"), device = "png", width = 10, height = 3)

