
#Setup
rm(list = ls(all = TRUE))
gc(reset=TRUE)
dir <- getwd()
setwd('../')

#Load libraries
library('devtools')
library('roxygen2')
library('testthat')

#Helper function
build_and_install <- function(x){
  roxygenize(x)
  devtools::install(x)
  library(x, character.only=TRUE)
  test_package(x)
}

#Build documentation
build_and_install('vw')
build_and_install('rgf')
build_and_install('gc')

#Back to original path
setwd(dir)
