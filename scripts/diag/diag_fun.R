##########################
# Diagnostic functions.  #
##########################

# Quadratic detrend.
# ========================
quad_detrend = function(y) {
  
  # Extrenal regressor matrix.
  Xreg = do.call(rbind, replicate(nrow(y), poly(seq_len(ncol(y)), 2), simplify = FALSE))
  
  # Residuals.
  res = lm(c(t(y)) ~ Xreg)$residuals
  
  # Return unscaled residuals.
  return(t(scale(matrix(res, nrow = ncol(y), ncol = nrow(y)))))
  
}

# Cross-covariance.
# ========================
cross_cov = function(y) {
  
  # Return cross-covariance.
  return(mean((y[, 1] - mean(y[, 1])) * (y[, 2] - mean(y[, 2]))))
  
}


# Diagnostic plot.
# ========================
cross_cov_plot = function(A, A.ind, A.dep, lat, lon, xlab, ylab) {
  
  # Return diagnostic plot.
  plot(lat, A[1, ], type = "l", col = "grey", ylim = c(-1, 1),
       xlab = xlab, ylab = ylab, main = paste( lon, "E"), cex.lab = 2, cex.main = 2, cex.axis = 1.8)
  for (i in 2:dim(A)[1]) {
    lines(lat, A[i, ], col = "grey")
  }
  for (i in 1:dim(A.ind)[1]) {
    lines(lat, A.ind[i, ], col = rgb(red = 132 / 255, green = 186 / 255, blue = 91 / 255))
  }
  for (i in 1:dim(A.dep)[1]) {
    lines(lat, A.dep[i, ], col = rgb(red = 225 / 255, green = 151 / 255, blue = 75 / 255))
  }
  
}

# Diagnostic plot grid.
# ========================
cross_cov_plot_grid = function(A, A.ind, A.dep, lat, lon, xlab, ylab) {
  
  # Return grid of diagnostic plots.
  par(mfrow = c(3, 3))
  for (i in seq_len(dim(A)[2])) {
    cross_cov_plot(A[, i, ], A.ind[, i, ], A.dep[, i, ], lat, lon[i], 
                   xlab = ifelse(i %in% 7:9, xlab, ""),
                   ylab = ifelse(i %% 3 == 1, ylab, ""))
    abline(h = 0, lty = 2, lwd = )
  }
  par(mfrow = c(1, 1))
  
}

# Residuals.
# ========================
lm_res = function(y) {
  
  # Return residuals.
  return(scale(lm(y ~ seq_along(y))$residuals))
  
}
