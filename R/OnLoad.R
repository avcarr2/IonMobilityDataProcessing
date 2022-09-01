packages <- c("optparse", "tidyverse", "investr", "minpack.lm", "sigmoid", 
              "MALDIquant", "openxlsx", "R6", "openxlsx")
newPackages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0) install.packages(newPackages)
