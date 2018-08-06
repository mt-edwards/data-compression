##########################
# Simulation functions. #
##########################

# Cross-spectral mass matrix.
csm_matrix = function(csm) {
  
  # Return cross-spectral mass matrix.
  return(matrix(c(1, Conj(csm), csm, 1), 2, 2))
  
}

# Complex matrix square root.
cplx_sqrt = function(C) {
  
  # Eigendecomposition.
  eig = eigen(C)
  
  # Return complex choleski factorisation.
  return(tcrossprod(eig$vectors, diag(sqrt(eig$values))))
  
}

# Multi-variate normal simulation.
mvn_sim = function(n, R) {

  # Return simulation.
  return(tcrossprod(R, matrix(rnorm(nrow(R) * n), n, nrow(R))))
  
}

# Make data real.
make_real = function(x) {
  
  # Return real complex numbers.
  return(c(x[1:(length(x) / 2 + 1)], Conj(rev(x[2:(length(x) / 2)]))))
  
}

# Multi-variate normal simulations array.
mvn_sim_array = function(dims, R) {
  
  # Return simulation.
  return(array(c(mvn_sim(prod(dims), R)), dim = c(nrow(R), dims)))
  
}

# Latitudinal trending.
lat_trend = function(phi_x) {
  
  # phi_x matrix.
  PHI_X = matrix(c(NA, phi_x), ncol = 2)
  
  # Trended data
  y = PHI_X[, 2]
  
  # Trending data
  for (i in 2:nrow(PHI_X)) {
    
    y[i] = PHI_X[i, 2] * sqrt(1 - PHI_X[i, 1] ^ 2) + PHI_X[i, 1] * y[i - 1]
    
  }
  
  # Return trended data
  return(y)
  
}

# Latitudinal array trending.
lat_trend_array = function(Phi, X) {
  
  # Return trended array data.
  return(apply(abind(Phi, X, along = 2), c(1, 3), lat_trend))
    
}

# Inverse normalised Fourier transform.
inverse_nfft = function(spec) {
  
  # Return invese normalised Fourier transform.
  return(fft(spec, inverse = TRUE) / sqrt(length(spec)))
  
}

# Temporal trending.
temp_trend = function(mod, innov) {
  
  # Return trended data.
  return(arima.sim(list(ar = mod[[1]]$coef), n = length(innov), innov = innov * sqrt(mod[[1]]$sigma2)))
  
}

# Temporal array trending.
temp_trend_array = function(mod, innov) {
  
  # Return trended array data.
  return(matrix(temp_trend(mod, c(t(innov))), nrow = nrow(innov), ncol = ncol(innov), byrow = TRUE))
  
}


# # C matrix.
# C = csm_matrix(complex(modulus = 0.8, argument = 1))
# all.equal(complex(modulus = 0.8, argument = 1), C[1, 2])
# 
# # R matrix
# R = cplx_sqrt(C)
# all.equal(R %*% Conj(t(R)), C)
# 
# # X matrix.
# X = mvn_sim(1e6, R)
# all.equal(X %*% Conj(t(X)) / ncol(X), C)
# 
# # A matrix.
# A = mvn_sim_array(rep(100, 3), R)
# all.equal(mean(A[1, , , ] * Conj(A[2, , , ])), complex(modulus = 0.8, argument = 1))
# 
# # y vector.
# y = lat_trend(c(rep(0.54, 1e5 - 1), rnorm(1e5)))
# all.equal(arima(y, order = c(1, 0, 0), include.mean = FALSE)$coef, 0.54)
