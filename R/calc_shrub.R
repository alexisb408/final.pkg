#' Calculate the density of a shrub
#' 
#' Use the length, width, height of an object to calculate its density.
#' Returns density as a floating point number.
#' 
#' @param len Length of the object
#' @param width Width of the object
#' @param height Height of the object
#' @return density The density of the object (numeric)
#' 
#' @export

calc_den <- function(length, width, height){ 
  volume <- calc_vol(length, width, height)
  mass <- calc_mass(length)
  density <- mass/volume
  return(density)
}