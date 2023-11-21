#' Generate a plot of an independent variable versus dependent variable
#' 
#' Use the surveys dataset to generate a bar graph of an independent variable versus a dependent variable.
#' Returns a bar graph of an independent variable versus a dependent variable.
#' 
#' @param df data set
#' @param independent independent variable
#' @return bg graph of independent variable versus dependent variable
#' 
#' @export

bar_graph <- function(df, independent) {
  column_1 <- df %>% 
    na.omit() %>% 
    select({{independent}})
  if(sum(column_1) > 0) {
    print(df)
    bg <- ggplot(df, mapping = aes(x = {{independent}})) + geom_bar()
    return(bg)
  } else {
    print("STOP!!")
  }
}