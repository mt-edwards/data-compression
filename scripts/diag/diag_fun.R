##########################
# Diagnostic functions.  #
##########################

# Cross-covariance.
# ========================
cross_cov = function(y) {
  
  # Return cross-covariance.
  return(mean((y[, 1] - mean(y[, 1])) * (y[, 2] - mean(y[, 2]))))
  
}


# Diagnostic plot.
# ========================
diag_plot = function(A, A.ind, A.dep, lat, lon, value, var) {
  
  # Return diagnostic plot.
  plot(lat, A[1, ], type = "l", col = "grey", ylim = range(A, A.ind, A.dep),
       xlab = "Latitude", ylab = value, main = paste0(var, " (", lon, " E)"))
  for (i in 2:dim(A)[1]) {
    lines(lat, A[i, ], col = "grey")
  }
  for (i in 1:dim(A.ind)[1]) {
    lines(lat, A.ind[i, ], col = "blue")
  }
  for (i in 1:dim(A.dep)[1]) {
    lines(lat, A.dep[i, ], col = "red")
  }
  
}

# Diagnostic plot grid.
# ========================
diag_plot_grid = function(A, A.ind, A.dep, lat, lon, value, var) {
  
  # Return grid of diagnostic plots.
  par(mfrow = c(3, 3))
  for (i in seq_len(dim(A)[2])) {
    diag_plot(A[, i, ], A.ind[, i, ], A.dep[, i, ], lat, lon[i], value, var)
    legend("topleft", legend = c("Original", "Independent", "Dependent"), col = c("grey", "blue", "red"), lty = 1)
  }
  par(mfrow = c(1, 1))
  
}
