#' Removes NA values from a data set
#' 
#' Removes NA values from a specific column in a data set.
#' Returns a column from a data set containing no NA values.
#' 
#' @param data data set
#' @param column a column from a data set
#' @return clean selected column from a data set containing no NA values
#' 
#' @export
#'
remove_nas_p3 <- function(data, column){
     clean <- data %>% 
         na.omit(data) %>% 
         select(all_of(column)) 
     return(clean)
} 