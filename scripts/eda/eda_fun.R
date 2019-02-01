##########################
# EDA Functions.         #
##########################

# Weight array function.
# ========================
weight_array = function(Y, lat) {
  
  # Weight vector.
  w = cos(lat * pi / 180)
  
  # Return weight array.
  return(aperm(array(w, dim = rev(dim(Y))), rev(seq_along(dim(Y)))))
  
}

# Ensemble plot.
# ========================
ensemble_plot = function(x, y, x_val, y_val, breaks) {
  
  # Ensemble data frame.
  df = tibble(Dep  = colMeans(y), 
              Min  = apply(y, 2, min),
              Max  = apply(y, 2, max),
              Ind  = x)
  
  # Return Ensemble plot.
  ggplot(df) +
    geom_ribbon(aes(x = Ind, ymin = Min, ymax = Max), fill = "grey") +
    geom_line(aes(x = Ind, y = Dep), col = "blue") +
    xlab(x_val) +
    ylab(y_val) +
    scale_x_continuous(breaks = breaks) +
    theme_minimal(base_size = 14)
  
}

# Auto-correlation
# ========================
auto_cor = function(y) {
  
  # Return auto-covariance.
  mean(y[-1] * y[-length(y)])
  
}

# Cross-correlation
# ========================
cross_cor = function(y) {
  
  # Return cross-covariance.
  mean(y[, 1] * y[, 2])
  
}

# Spatial parameters.
# ========================
spat_pars = function(y) {
  
  # Linear model.
  mod = lm(c(t(y)) ~ rep(seq_len(ncol(y)), nrow(y)))
  
  # Return parameters.
  return(c(intercept = unname(mod$coefficients[1]),
           trend     = unname(mod$coefficients[2]),
           std       = summary(mod)$sigma,
           auto_cor  = auto_cor(scale(mod$residuals))))

}

# Save NetCDF file.
# =======================
save_ncdf = function(var, lon, lat, name, args) {
  
  # Create NetCDF file.
  nc = create.nc(paste0("data_ncdf/", args[1], "/", name, ".nc"))
  
  # Define dimensions.
  dim.def.nc(nc, "lon", length(lon))
  dim.def.nc(nc, "lat", length(lat))
  
  # Define variables.
  var.def.nc(nc, "lon", "NC_DOUBLE", 0)
  var.def.nc(nc, "lat", "NC_DOUBLE", 1)
  var.def.nc(nc, "var", "NC_FLOAT", c(0, 1))
  
  # Put attributes.
  att.put.nc(nc, "var", "coordinates", "NC_CHAR", "long lat")
  
  # Put variables.
  var.put.nc(nc, "lon", lon)
  var.put.nc(nc, "lat", lat)
  var.put.nc(nc, "var", var)
  
  # Close NetCDF file.
  close.nc(nc)
  
}

# Residuals.
# ========================
lm_res = function(y) {
  
  # Return residuals.
  return(scale(lm(c(t(y)) ~ rep(seq_len(ncol(y)), nrow(y)))$residuals))
  
}

# Cross-covariance plot.
# ========================
cross_cov_plot = function(A1, A2, A3, lat, lon, value, var) {
  
  plot(lat, A1[1, ], type = "l", col = "blue", ylim = range(A1, A2, A3),
       xlab = "Latitude", ylab = value, main = paste0(var, " (", lon, " E)"))
  
  for (i in seq_len(nrow(A1))) {
    lines(lat, A1[i, ], col = "grey")
    lines(lat, A2[i, ], col = "blue")
    lines(lat, A3[i, ], col = "red")
  }
  
}

# Cross-covariance plot grid.
# ========================
cross_cov_plot_grid = function(A1, A2, A3, lat, lon, value, var) {
  
  # Return grid of diagnostic plots.
  par(mfrow = c(3, 3))
  for (i in seq_len(dim(A1)[2])) {
    cross_cov_plot(A1[, i, ], A2[, i, ], A3[, i, ], lat, lon[i], value, var)
    abline(h = 1, lty = 2)
    legend("topleft", legend = c("Raw Data", "Detrended Data", "Detrended and Unscaled Data"), col = c("grey", "blue", "red"), lty = 1)
  }
  par(mfrow = c(1, 1))
  
}
