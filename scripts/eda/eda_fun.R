##########################
# EDA Functions.         #
##########################

# Weight array function.
# ========================
weight_array = function(Y, lat) {
  
  # Weight vector.
  w = cos(lat * pi / 180)
  
  # Return weight array.
  return(aperm(array(w, dim = rev(dim(Y))), rev(seq_along(dim(Y)))))
  
}
