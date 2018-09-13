##########################
# Multivariate functions.#
##########################

# Shift arguments.
# ========================
shift_arguments = function(arguments, min_arg) {
  
  # Return shifted arguments.
  return(ifelse(arguments > min_arg, arguments, arguments + 2 * pi))
  
}

# Mean multivariate cross-preriodogram.
# ========================
multi_pgram = function(dnspec1, dnspec2) {
  
  # Multivariate cross-periodogram.
  mcpg = apply(Conj(dnspec1) * dnspec2, 3, mean)
  
  # Return modulus and arument.
  return(list(modulus  = Mod(mcpg),
              argument = Arg(mcpg)))
  
}

# Tapered spectra.
# ========================
tapered_spec = function(dnspec, taper) {
  
  # Tapered spectrum.
  return(dnspec[, , , seq(taper + 1, dim(dnspec)[4] - taper)])
  
}

# Unscaled spectra.
# ========================
unscaled_spec = function(dnspec) {
  
  # Modulus.
  mod2 = apply(Mod(dnspec) ^ 2, 3, mean)
  
  # Return unscaled spectrum.
  return(aperm(apply(dnspec, c(1:2, 4), function(dnsp) dnsp / sqrt(mod2)), c(2:3, 1, 4)))
  
}

# Cross-spectral mass function.
# ========================
csm_fun = function(pars, c, L){
  
  # Cross-spectral mass function.
  return(complex(modulus  = plogis(pars[1]) * (exp(2 * pars[2]) + 4 * sin(pi * c / L) ^ 2) ^ -exp(pars[3]), 
                 argument = plogis(pars[4]) * 2 * pi))
  
}

# Cross-spectral mass function.
# ========================
csm_matrix = function(pars, c, L){
  
  # Spectral mass.
  csm = csm_fun(pars, c, L)
  
  # Cross-spectral mass function.
  return(matrix(c(1, Conj(csm), csm, 1), 2, 2))
  
}

# Multivariate model fitting.
# ========================
multi_fit = function(utdnspec) {
  
  # Return latitudinal model fit.
  return(optim(rep(0, 4), fn = full_multi_neg_log_like, utdnspec = utdnspec, method = "Nelder-Mead"))
  
}

# Full multivariate negative log-likelihood.
# =======================
full_multi_neg_log_like = function(pars, utdnspec) {

  # Covariance matrices.
  Cs = lapply(seq_len(dim(utdnspec)[3]), function(c) csm_matrix(pars, c - 1, dim(utdnspec)[3]))
  
  # Inverse matrices and log-determinant.
  Is  = lapply(Cs, solve); LD = log(prod(sapply(Cs, Det)))
  
  # Cluster computation.
  return(sum(parApply(cl, utdnspec, c(1:2, 4), function(X) multi_neg_log_like(X, Is, LD))))
  
}

# Multivariate negative log-likelihood.
# =======================
multi_neg_log_like = function(X, Is, LD) {
  
  # Quadratic form.
  QF = sum(sapply(seq_along(Is), function(i) quad_form(X[i, ], Is[[i]])))
  
  # Negative log-likelhood.
  return(0.5 * LD + 0.5 * QF)
  
}

# Quadratic form.
# =======================
quad_form = function(x, I) {
  
  # Return negative log-likelihood.
  return(Re(crossprod(Conj(crossprod(Conj(I), x)), x)))
  
}
