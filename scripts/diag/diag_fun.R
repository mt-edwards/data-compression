##########################
# Diagnostic functions.  #
##########################

# 2020 mean and trend function.
# ========================
mean_trend = function(vec) {
  
  # Return 2020 mean and trend.
  return(c(vec[1] + 20 * vec[2], vec[2]))
  
}

# Temporal linear model.
# ========================
temp_lm = function(y, year) {
  
  # Return linear model
  return(mean_trend(summary(lm(y ~ year))$coefficients[, 1]))
  
}

# Temporal auto-covariance.
# ========================
temp_acf = function(y) {
  
  # Return auto-covariance
  return(c(acf(y, lag.max = 1, type = "covariance", plot = FALSE)$acf))
  
}

# Temporal cross-correlation
# ========================
temp_ccf = function(y) {
  
  # Return cross-covariance.
  return(c(ccf(y[, 1], y[, 2], lag.max = 1, type = "covariance", plot = FALSE)$acf))
  
}

# Diagnostic plot.
# ========================
diag_plot = function(A, A.sim, lat, lon, value, var) {
  
  # Return diagnostic plot.
  plot(lat, A[1, ], type = "l", col = "grey", ylim = range(A, A.sim),
       xlab = "Latitude", ylab = value, main = paste0(var, " (", lon, " E)"))
  for (i in 2:dim(A)[1]) {
    lines(lat, A[i, ], col = "grey")
  }
  for (i in 1:dim(A.sim)[1]) {
    lines(lat, A.sim[i, ], col = "red")
  }
  
}

# Diagnostic plot grid.
# ========================
diag_plot_grid = function(A, A.sim, lat, lon, value, var) {
  
  # Return grid of diagnostic plots.
  par(mfrow = c(3, 3))
  for (i in seq_len(dim(A)[2])) {
    diag_plot(A[, i, ], A.sim[, i, ], lat, lon[i], value, var)
  }
  par(mfrow = c(1, 1))
  
}
