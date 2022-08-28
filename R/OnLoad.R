packages <- c("optparse", "tidyverse", "investr", "minpack.lm", "sigmoid", "MALDIquant")
newPackages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0) install.packages(newPackages)
