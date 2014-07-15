################################################################################
# Geographically Weighted Regression
# instead of one global coefficient for each independent variable, coefficients
# are able to vary according to space, i.e. locally. The spatial variation in 
# coefficients can reveal interesting patterns which otherwise would be masked.
################################################################################

################################################################################
# many packages for GWR
# e.g.: spgwr, GWmodel, gwrr
################################################################################

library(spgwr)
library(ggplot2)
library(maptools)