###################################################################################
# Function to convert `hsdar::speclib` object to a dataframe
# Original snippet from https://rdrr.io/github/ssdxj/G407/src/R/ulits4spc.R
# Edited by Jayan Wijesingha (jayan.wijesingha@uni-kassel.de)
# 2019-02-10
###################################################################################

spc_2df <- function(spc) {
  out <- NULL
  attri <- SI(spc)
  ref <- spectra(spc)
  
  # incase no attri
  if (ncol(attri) == 0) {
    out <- as.tibble(ref)
  } else {
    out <- as.tibble(cbind(attri, ref))
  }
  
  # handle colnames
  names(out) <- c(names(attri), hsdar::wavelength(spc))
  
  return(out)
}
