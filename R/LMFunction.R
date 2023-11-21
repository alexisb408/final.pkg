#' Describes the relationship between two variables
#' 
#' Generates a summary that determines how much an independent variable affects a dependent variable.
#' Returns a summary depicting the relationship between two variables.
#' 
#' @param data data set
#' @param independent independent variable
#' @param dependent dependent variable 
#' @return model summary of lm function
#' 
#' @export

lm_function <- function(data, independent, dependent) {
     clean_data <- data %>% 
         na.omit() 
     model <- clean_data %>% 
         na.omit() %>% 
         select(a = {{independent}}, {{dependent}}) %>% 
         lm(a ~ ., data = .) %>% 
         summary()
     if(sum(is.na(clean_data)) == 0){
         return(model)
     } else {
         print("STOP")
     }
 }