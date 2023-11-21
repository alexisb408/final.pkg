#' Generate a plot of an independent variable versus dependent variable
#' 
#' Use the surveys dataset to generate a point graph of an independent variable versus a dependent variable.
#' Returns a point graph of an independent variable versus a dependent variable.
#' 
#' @param df data set
#' @param independent independent variable
#' @param dependent dependent variable 
#' @return pg graph of independent variable versus dependent variable
#' 
#' @export

point_graph <- function(df, independent, dependent) {
      column <- df %>% 
          na.omit() %>% 
          select({{independent}})
      if(sum(column) > 0) {
              print(df)
              pg <- ggplot(df, mapping = aes(x = {{independent}}, y = {{dependent}})) + geom_point()
              return(pg)
          } else {
              print("STOP!!")
              }
}