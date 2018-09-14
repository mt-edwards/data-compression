############################
# Longitudinal Functions.  #
############################

# Pyramid squared function.
# ========================
pyramid2 = function(f) {
  
  # Return value.
  return((2 - abs(4 * f - 2)) ^ 2)
  
}

# Sine squared function.
# ========================
sine2 = function(f) {
  
  # Return value.
  return(4 * sin(pi * f) ^ 2)
  
}

# Beta mix function.
# ========================
beta_mix = function(f, par) {
  
  # Return value.
  return(plogis(par) * sine2(f) + (1 - plogis(par)) + pyramid2(f))
  
}

# Matern spectral mass function.
# ========================
spec_Matern = function(pars, fun){
  
  # Return Matern spectral density.
  return((exp(2 * pars[1]) + 4 * fun) ^ -(exp(pars[2]) + 0.5))
  
}

# Spectral mass functions.
# ========================
spec_mass = function(N, pars, class) {
  
  # Frequencies.
  fs = 0:(N - 1) / N
  
  # Classes of spectral mass functions.
  if (class == "m")       SMF = spec_Matern(pars, pyramid2(fs))
  else if (class == "mm") SMF = spec_Matern(pars, sine2(fs))
  else if (class == "bm") SMF = spec_Matern(pars, beta_mix(fs, pars[3]))
  
  # Return spectral mass function.
  return(SMF / mean(SMF))
  
}

# Whittle negative log-likelihood.
# ========================
Whittle_neg_log_like = function(pgram, SMF) {
  
  # Return negative log-likelihood.
  return(0.5 * sum(log(SMF)) + 0.5 * sum(pgram / SMF))
  
}

# Full Whittle negative log-likelihood.
# ========================
full_Whittle_neg_log_like = function(pgram, pars, class) {
  
  # Spectral mass function.
  SMF = spec_mass(dim(pgram)[3], pars, class)
  
  # Return full Whittle negative log-likelihood.
  return(sum(apply(pgram, 1:2, Whittle_neg_log_like, SMF = SMF)))
  
}

# Longitudinal model fitting.
# ========================
long_fit = function(pgram, class) {
  
  # Number of parameters.
  npars = ifelse(class == "bm", 3, 2)
  
  # Return fitted model.
  return(optim(par = rep(0, npars), fn = full_Whittle_neg_log_like, pgram = pgram, class = class, method = "Nelder-Mead"))
  
}

# Calculate AIC.
# ========================
cal_AIC = function(mod) {
  
  # Return AIC.
  return(2 * length(mod$par) + 2 * mod$value)
  
}

# Calculate SMF.
# ========================
cal_SMF = function(pars) {
  
  # Return SMF.
  return(c("NA", "mm", "bm")[dim(pars)[1]])
  
}

# Best AIC.
# ========================
best_AIC = function(aics) {
  
  return(ifelse(aics[1] + 2 < aics[2], 1, 2))
  
}

# Calculate normalized spectrum.
# ========================
long_nspec = function(spec, smf) {
  
  # Return NFC.
  return(spec / sqrt(smf))
  
}
