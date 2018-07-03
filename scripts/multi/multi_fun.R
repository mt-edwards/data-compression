##########################
# Multivariate functions.#
##########################

# Mean multivariate cross-preriodogram.
# ========================
multi_pgram = function(dnspec1, dnspec2) {
  
  # Return multivariate cross-spectra.
  return(Mod(apply(dnspec1 * Conj(dnspec2), 3, mean)))
  
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
  return(exp(pars[1]) * (exp(pars[2]) + 4 * sin(pi * c / L) ^ 2) ^ -exp(pars[3]))
  
}

# Cross-spectral mass function.
# ========================
csm_matrix = function(pars, c, L){
  
  # Cross-spectral mass function.
  return(csm_fun(pars, c, L) ^ matrix(c(0, 1, 1, 0), 2, 2))
  
}

# Multivariate model fitting.
# ========================
multi_fit = function(utdnspec) {
  
  # Return latitudinal model fit.
  return(optim(par = c(0, 0.4, 0.25), fn = full_multi_neg_log_like, utdnspec = utdnspec, method = "Nelder-Mead"))
  
}

# Full multivariate negative log-likelihood.
# =======================
full_multi_neg_log_like = function(par, utdnspec) {

  # Invesre Cholesky factors.
  Rinvs  = lapply(seq_len(dim(utdnspec)[3]), function(c) solve(chol(csm_matrix(par, c - 1, dim(utdnspec)[3]))))
  
  # Log-determinant.
  LD = sum(sapply(Rinvs, function(Rinv) log(1 / prod(diag(Rinv)))))
  
  # Force evaluation.
  utdnspec = utdnspec; Rinvs = Rinvs; LD = LD
  
  # Cluster computation.
  return(sum(parApply(cl, utdnspec, c(1:2, 4), function(X) multi_neg_log_like(X, Rinvs, LD))))
  
}

# Multivariate negative log-likelihood.
# =======================
multi_neg_log_like = function(X, Rinvs, LD) {
  
  # Quadratic form.
  quad_form = sum(sapply(seq_along(Rinvs), function(i) quad_form(X[i, ], Rinvs[[i]])))
  
  # Negative log-likelhood.
  return(LD + 0.5 * quad_form)
  
}

# Quadratic form.
# =======================
quad_form = function(x, Rinv) {
  
  # Return negative log-likelihood.
  return(sum(Mod(crossprod(Rinv, x)) ^ 2))
  
}


