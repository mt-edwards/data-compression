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
  if (mod2$aic + 2 < mod1$aic) return(mod2) else return(mod1)
  
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
