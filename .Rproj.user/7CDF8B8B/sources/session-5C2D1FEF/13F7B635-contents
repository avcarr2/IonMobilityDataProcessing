## User Interface ## 
option_list <- 
  list(
    make_option(
      c("-i", "--inputfolder"), 
      type = "character", 
      default = NULL, 
      help = "Input folder path [default = %default"), 
    make_option(
      c("-o", "--outputfolder"), 
      type = "character", 
      default = NULL, 
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

## Enter main function 


## Parameters: min_Mz, max_Mz, input folder, output folder

## Run workflow loop

## Return list of reports in output folder 