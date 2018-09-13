############################
# Temp Functions.          #
############################

# Temporal selection function.
# =======================
temp_fit = function(y, args) {
  
  # Return temporal model.
  return(auto.arima(c(t(y)), stationary = TRUE,
                    max.p = as.numeric(args[3]), 
                    max.q = as.numeric(args[4]),
                    xreg = rep(seq_len(ncol(y)), nrow(y))))
  
}
                      
# Temporal parameters.
# ========================
temp_pars = function(mod, args) {

    # Temporal parameters.
    pars = c(ifelse(is.na(mod[[1]]$coef["ar1"]), 0, mod[[1]]$coef["ar1"]),
             ifelse(is.na(mod[[1]]$coef["intercept"]), 0, mod[[1]]$coef["intercept"]),
             ifelse(is.na(mod[[1]]$coef["xreg"]), 0, mod[[1]]$coef["xreg"]),
             sqrt(mod[[1]]$sigma2))
  
    # Return parameters.
    return(unname(pars))
  
}

#  Residual function.
# =======================
temp_resid = function(mod, args) {
  
  # Return residuals.
  return(matrix(scale(mod$residuals), ncol =  as.numeric(args[2])))
  
} 

# Fourier transform function.
# ========================
temp_fft = function(resid) {
  
  # Return normalised FFT.
  return(fft(resid) / sqrt(length(resid)))
  
}

# Periodogram function.
# ========================
temp_pgram = function(spec) {
  
  # Return periodogram.
  return(Mod(spec) ^ 2)
  
}
