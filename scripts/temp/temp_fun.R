############################
# Temp Functions.          #
############################

# Temporal selection function.
# =======================
temp_fit = function(y, args) {
  
  # Extrenal regressor matrix.
  Xreg = do.call(rbind, replicate(nrow(y), poly(seq_len(ncol(y)), 2), simplify = FALSE))
  colnames(Xreg) = c("xreg", "xreg2")
  
  # Temporal models.
  mod1 = auto.arima(c(t(y)), stationary = TRUE,
                    max.p = as.numeric(args[3]), 
                    max.q = as.numeric(args[4]),
                    xreg  = Xreg[, 1])
  mod2 = auto.arima(c(t(y)), stationary = TRUE,
                    max.p = as.numeric(args[3]), 
                    max.q = as.numeric(args[4]),
                    xreg  = Xreg)

  # Return temporal model.
  return(ifelse(mod2$aic + 2 < mod1$aic, mod2, mod1))
  
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
