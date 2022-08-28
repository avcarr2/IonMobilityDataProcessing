source("R/main.R")

## User Interface ## 
option_list <- 
  list(
    make_option(
      c("-i", "--inputfolder"), 
      type = "character", 
      default = getwd(), 
      help = "Input folder path [default = %default"), 
    make_option(
      c("-o", "--outputfolder"), 
      type = "character", 
      default = file.path(getwd(), "Data"), 
      help = "Output folder path [deafult = %default]"
    ), 
    make_option(
      c("-n", "--minmz"), 
      type = "numeric", 
      default = 620.0, 
      help = "min m/z for area calculation"
    ), 
    make_option(
      c("-x", "--maxmz"), 
      type = "numeric", 
      default = 630, 
      help = "maximum value"
    )
  )
optParser <- optparse::OptionParser(option_list = option_list)

opt <- parse_args(optParser)
if(is.null(opt$inputfolder) | is.null(opt$outputfolder)){
  print_help(optParser)
  stop("Must have input and output folder arguments")
}
## FOR TESTING PURPOSES. REMOVE FOR REAL THING ## 
opt$inputfolder <- file.path(getwd(), "Data")

## Enter main function 
main(opt$inputfolder, 
     opt$outputfolder, 
     minMz = opt$minmz, 
     maxMz = opt$maxmz)
