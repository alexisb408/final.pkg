#' Generate a plot of weight versus sex
#' 
#' Use the surveys dataset to generate a plot of weight versus sex.
#' Returns a plot of weight versus sex.
#' 
#' @param data Dataset
#' @param sex Sex of the species
#' @param weight Weight of the organism
#' @return gp3 Plot of weight versus sex
#' 
#' @export

graph_project_3 <- function(data, sex, weight){
  gp3 <- ggplot(data = surveys_1, mapping = aes(x = sex, y = weight)) + geom_point()
  return(gp3)
}