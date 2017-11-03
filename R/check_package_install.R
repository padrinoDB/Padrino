#' Check for and optionally install packages
#' 
#' @description checks to make sure package is installed. If not, 
#' either auto installs or checks with user first
#' 
#' @param x The package to be checked
#' @param auto.install A logical indicating whether to 
#' automatically install missing packages or check with user first
#' 
#' @return Message indicating status of installation
#' 
#' @importFrom utils install.packages
#' 

check_install_package <- function(x, auto.install = FALSE) {
  if(!requireNamespace(x, quietly = TRUE)){
    if(!auto.install) {
      ans <- readline(prompt = paste('Package', x, 'is not installed.',
                                     'Would you like to install it? [y/n]: '))
      
      if(ans == 'y'){
        install.packages(x)
        msg <- paste0('Package ', x, ' and any dependencies have been installed and loaded')
        
      } else {
       msg <- paste0('Could not install package ', x, '. Program may not run as intended.')
       
      }
      
    } else {
      utils::install.packages(x)
      msg <- paste0('Package ', x, ' and any dependencies have been installed and loaded')
      
    }
  }
  
  requireNamespace(x, quietly = TRUE)
  message(msg)
}
