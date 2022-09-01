## Testing list 
library(openxlsx)
writeSy50 <- function(outputList, fileNames, outputFolderPath){
  extractedSYVals <- sapply(outputList, function(x){
    x$Sy50
  })
  names(extractedSYVals) <- fileNames
  write.csv(extractedSYVals, file = "SY50Values")
}

