##########################
# Simulation functions. #
##########################

# Convert model to vector.
mod_2_vec = function(mod, p.max) {
  
  # Return model as vector.
  return(c(int = unname(mod$coef["intercept"]), 
           xrg = unname(mod$coef["xreg"]), 
           var = unname(mod$sigma2), 
           ar1 = unname(mod$coef["ar1"]), 
           ar2 = unname(mod$coef["ar2"]), 
           ar3 = unname(mod$coef["ar3"])))
  
}

# Cross-spectral mass matrix.
csm_matrix = function(csm) {
  
  # Return cross-spectral mass matrix.
  return(matrix(c(1, Conj(csm), csm, 1), 2, 2))
  
}

# Cross-spectral mass matrix.
csm_matrix3 = function(csms) {
  
  # Return cross-spectral mass matrix.
  return(matrix(c(1, Conj(csms[1]), Conj(csms[2]), csms[1], 1, Conj(csms[3]), csms[2], csms[3], 1), 3, 3))
  
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

# Latitudinal array trending.
lat_trend_array2 = function(Phi, X) {
  
  # Return trended array data.
  return(apply(abind(Phi, X, along = 2), 1, lat_trend))
  
}

# Inverse normalised Fourier transform.
inverse_nfft = function(spec) {
  
  # Return invese normalised Fourier transform.
  return(fft(spec, inverse = TRUE) / sqrt(length(spec)))
  
}

# Temporal trending.
temp_trend = function(mod, innov) {
  
  # Return trended data.
  return(arima.sim(list(ar = na.omit(mod[c("ar1", "ar2", "ar3")])), n = length(innov), innov = innov * sqrt(mod["var"])) + mod["int"] + seq_along(innov) * mod["xrg"])
  
}

# Temporal array trending.
temp_trend_array = function(X, p.max) {
  
  # Matrix commponents.
  mod = X[1, 1:(p.max + 3)]
  innov = X[, (p.max + 4):ncol(X)]
  
  # Return trended array data.
  return(apply(innov, 1, function(innov_vec) temp_trend(mod, innov_vec)))
  
}
