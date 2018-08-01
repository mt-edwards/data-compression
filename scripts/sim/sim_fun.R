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

# Multi-variate normal simulations array.
mvn_sim_array = function(dims, R) {
  
  # Return simulation.
  return(array(c(mvn_sim(prod(dims), R)), dim = c(nrow(R), dims)))
  
}

# Latitudinal trending.
lat_trend = function(phi_x) {

  # Create list.
  lst = alply(matrix(c(0, phi_x), ncol = 2), 1, rev)
  
  # Return trended data.
  return(sapply(accumulate(lst, ~ c(.y[1] + .y[2] * .x[1], 0)), first))
  
}

# Latitudinal array trending.
lat_trend_array = function(Phi, X) {
  
  # Return trended array data.
  return(aperm(apply(abind(Phi, X, along = 2), c(1, 3), lat_trend), c(2, 1, 3)))
    
}

# Inverse normalised Fourier transform.
inverse_nfft = function(spec) {
  
  # Return invese normalised Fourier transform.
  return(fft(spec, inverse = TRUE) / sqrt(length(spec)))
  
}

# # C matrix.
# C = csm_matrix(complex(modulus = 0.8, argument = 1))
# all.equal(complex(modulus = 0.8, argument = 4), C[1, 2])
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
