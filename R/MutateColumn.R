#' Generate a new column for the data set
#' 
#' Multiplies a specific column in a data set by two. 
#' Returns a new column consisting of a previous column from the data set multiplied by two. 
#' 
#' @param df data set
#' @param variable a column from the data set
#' @return new a new column in the data set 
#' 
#' @export

multiply_variable <- function(df, variable) {
  column <- df %>% 
    na.omit() %>% 
    select({{variable}})
  if(sum(column) > 0) {
    new <- df %>% 
      na.omit() %>% 
      mutate(new_column = 2 * {{variable}}) 
    return(new)
  } else {
    print("STOP")
  }
}