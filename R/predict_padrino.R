#' Generic PADRINO parameter extraction function
#' 
#' @param Package The package housing the predict method to be used
#' @param Class The class of the model to be predicted
#' @param Model A model object that contains, at a minimum, the names
#' and estimates of the response variable, fixed and random effects.
#' @param MeshPts The meshpoints of the IPM matrix to be created.
#' @param upper In the case of a model with a single fixed effect,
#' a numeric indicating the upper bound of the range of the state variable
#' in the regression. In the case of models with multiple fixed effects,
#' a named list with where each component is a numeric indicating the upper
#' bound of the effect.
#' @param lower In the case of a model with a single fixed effect,
#' a numeric indicating the lower bound of the range of the state variable
#' in the regression. In the case of models with multiple fixed effects,
#' a named list with where each component is a numeric indicating the lower
#' bound of the effect.
#' 
#' @importFrom pryr otype
#' 
#' @example
#' resp <- runif(50, 1, 50)
#' exp1 <- rnorm(50, 10, 2)
#' exp2 <- rnorm(50, 5, 1)
#' 
#' mod <- glm(resp ~ exp1 + exp2, family = gaussian())
#' 
#' lower <- list(exp1 = min(exp1), exp2 = min(exp2))
#' upper <- list(exp1 = max(exp1), exp2 = max(exp2))
#' 



predict_Padrino <- function(Package, Class, Model, MeshPts,
                            lower = NULL, upper = NULL) {
  # make sure user has the right package to generate model predictions
  check_install_package(Package, auto.install = TRUE)
  
  if(!inherits(Model, Class)) {
    stop('Unable to find method for predicting ', Class)
  }
  # Put together the function call
  FUN <- eval(parse(text = paste(Package, ':::predict.', Class, sep = '')))
  
  # this is to generate predictions. We need to extract the formula
  # # from the model object to generate 
  # if(pryr::otype(Model) == 'S3'){ EVALUATE NECESSITY
  #   newdata <- generate_new_S3_data(Model, lower, upper, nMeshPts)
  # }
  # if(pryr::otype(Model) == 'S4') { EVALUATE NECESSITY
  #   newdata <- generate_new_S4_data(Model, lower, upper, nMeshPts)
  # }
  
  
  out <- FUN(Model#, newdata = newdata)
             )
}
