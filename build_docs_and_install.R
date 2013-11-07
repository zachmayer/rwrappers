
#Restart R/RStudio prior to running

#Setup
gc(reset=TRUE)
setwd('~/source/rwrappers/') #May need to edit this line

#Build documentation
require('roxygen2')
roxygenize('vw')
roxygenize('rgf')
roxygenize('gc')

#Install Packages
library(devtools)
devtools::install('vw')
devtools::install('rgf')
devtools::install('gc')

#Load packages
require('vw')
require('rgf')
require('gc')

#Test Packages
require('testthat')
