############################
# Temp Functions.          #
############################

# Temporal selection function.
# =======================
temp_fit = function(d, args) {
  
  # Return temporal model.
  return(auto.arima(c(t(d)), stationary = TRUE, allowmean = FALSE, ic = "aic", 
                    max.p = as.numeric(args[3]), 
                    max.q = as.numeric(args[4]),
                    start.q = as.numeric(args[4])))
  
}

# Temporal parameters.
# ========================
temp_pars = function(mod) {

    # AR coeficient.
    ar1 = unname(mod[[1]]$coef["ar1"])
  
    # Return parameters.
    return(c(ar1 = ifelse(is.na(ar1), 0, ar1), 
             std = sqrt(mod[[1]]$sigma2)))
  
}

#  Residual function.
# =======================
temp_resid = function(mod, args) {
  
  # Residuals.
  resid = mod$residuals
  
  # Return residuals.
  return(matrix(scale(resid), ncol =  as.numeric(args[2])) * sqrt(length(resid) / (length(resid) - 1)))
  
} 

# Save NetCDF file.
# =======================
save_ncdf = function(var, lon, lat, name, args) {
  
  # Create NetCDF file.
  nc = create.nc(paste0("data_ncdf/", args[1], "/", name, ".nc"))
  
  # Define dimensions.
  dim.def.nc(nc, "lon", length(lon))
  dim.def.nc(nc, "lat", length(lat))
  
  # Define variables.
  var.def.nc(nc, "lon", "NC_DOUBLE", 0)
  var.def.nc(nc, "lat", "NC_DOUBLE", 1)
  var.def.nc(nc, "var", "NC_FLOAT", c(0, 1))
  
  # Put attributes.
  att.put.nc(nc, "var", "coordinates", "NC_CHAR", "long lat")
  
  # Put variables.
  var.put.nc(nc, "lon", lon)
  var.put.nc(nc, "lat", lat)
  var.put.nc(nc, "var", var)
  
  # Close NetCDF file.
  close.nc(nc)
  
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
