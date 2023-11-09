#' Remove NA values from a data set
#' 
#' Remove NA values from a specific column in a data set.
#' Returns a data set containing no NA values.
#' 
#' @param data Dataset
#' @param hindfoot_length Hind foot length of an organism
#' @return rna_p3 Selected column from a data set containing no NA values
#' 
#' @export
#'
remove_nas_p3 <- function(data, hindfoot_length){
  rna_p3 <- data %>% 
    na.omit(data) %>% 
    select(hindfoot_length) %>% 
    mutate(is.numeric(hindfoot_length))
  return(rna_p3)
}    