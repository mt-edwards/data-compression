##########################
# Simulation functions. #
##########################

# Convert model to vector.
mod_2_vec = function(mod, args) {
  
  # Return model as vector.
  return(unname(c(mod$sigma2, mod$coef, rep(NA, as.numeric(args[4]) - length(mod$coef)))))
  
}

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
  return(arima.sim(list(ar = na.omit(mod[-1])), n = length(innov), innov = innov * sqrt(mod[1])))
  
}

# Temporal array trending.
temp_trend_array = function(X, args) {
  
  # Matrix commponents.
  mod = X[1, 1:(as.numeric(args[4]) + 1)]
  innov = X[, (as.numeric(args[4]) + 2):ncol(X)]
  
  # Return trended array data.
  return(matrix(temp_trend(mod, c(t(innov))), nrow = nrow(innov), ncol = ncol(innov), byrow = TRUE))
  
}
