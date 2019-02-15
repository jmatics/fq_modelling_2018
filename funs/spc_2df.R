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