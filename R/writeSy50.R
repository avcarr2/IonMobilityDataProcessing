## Testing list 
library(openxlsx)
writeSy50 <- function(outputList, fileNames, outputFolderPath){
  extractedSYVals <- sapply(testOutputList, function(x){
    x$Sy50
  })
  names(extractedSYVals) <- fileNames
  write.csv(extractedSYVals, file = "SY50Values")
}

