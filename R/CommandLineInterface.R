packages <- c("optparse", "tidyverse", "investr", "minpack.lm", "sigmoid", "MALDIquant", "R6")
newPackages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(newPackages) > 0) install.packages(newPackages)

## User Interface ## 
option_list <- 
  list(
    optparse::make_option(
      c("-r", "--PathToRCode"), 
      type = "character", 
      default = getwd(), 
      help = "Local IonMobilityDataProcessing Repository"
    ),
    optparse::make_option(
      c("-i", "--inputfolder"), 
      type = "character", 
      default = getwd(), 
      help = "Input folder path [default = %default"), 
    optparse::make_option(
      c("-o", "--outputfolder"), 
      type = "character", 
      default = file.path(getwd(), "Data"), 
      help = "Output folder path [deafult = %default]"
    ), 
    optparse::make_option(
      c("-n", "--minmz"), 
      type = "numeric", 
      default = 620.0, 
      help = "min m/z for area calculation"
    ), 
    optparse::make_option(
      c("-x", "--maxmz"), 
      type = "numeric", 
      default = 630, 
      help = "maximum value"
    )
  )
optParser <- optparse::OptionParser(option_list = option_list)

opt <- optparse::parse_args(optParser)
if(is.null(opt$inputfolder) | is.null(opt$outputfolder)){
  print_help(optParser)
  stop("Must have input and output folder arguments")
}
setwd(opt$PathToRCode)
source("./R/main.R")
source("./R/CombinedSYPlot.R")
## Enter main function 
main(opt$inputfolder, 
     opt$outputfolder, 
     minMz = opt$minmz, 
     maxMz = opt$maxmz)
