context('check_install_packages')

x <- c('dplyr', 'magrittr', 'swirl')
test_that('message outputs are as expected', {
  skip_on_cran()
  skip_on_travis()
  options(repos = c(CRAN = "http://cran.rstudio.com"))
  if(!requireNamespace(x[3])){
    expect_message(check_install_package(x[3], auto.install = TRUE),
                   'Package swirl and any dependencies have been installed and loaded')
  } else {
    expect_message(check_install_package(x[3], auto.install = TRUE),
                   '\n')
  }
  
})
