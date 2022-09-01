library(openxlsx)
writeSYTables <- function(outputList, fileNames, outputFolderPath){
  extractedDataFrames <- lapply(outputList, function(x){
    x$EnergyVPercent
  })
  names(extractedDataFrames) <- fileNames
  wb <- openxlsx::createWorkbook()
  for(i in 1:length(extractedDataFrames)){
    openxlsx::addWorksheet(wb, sheetName = fileNames[i])
    openxlsx::writeData(wb, 
                        sheet = fileNames[i], 
                        extractedDataFrames[[i]])
  }
  openxlsx::saveWorkbook(wb, file.path(outputFolderPath, "SYCurveExcelWB.xlsx"), overwrite = TRUE)
}

writePredTables <- function(outputList, fileNames, outputFolderPath){
  extractedDataFrames <- lapply(outputList, function(x){
    x$PredInt
  })
  names(extractedDataFrames) <- fileNames
  wb <- openxlsx::createWorkbook()
  for(i in 1:length(extractedDataFrames)){
    openxlsx::addWorksheet(wb, sheetName = fileNames[i])
    openxlsx::writeData(wb, 
                        sheet = fileNames[i], 
                        extractedDataFrames[[i]])
  }
  openxlsx::saveWorkbook(wb, file.path(outputFolderPath, "PredictedIntervalsExcelWB.xlsx"), overwrite = TRUE)
}

writeConfidTables <- function(outputList, fileNames, outputFolderPath){
  extractedDataFrames <- lapply(outputList, function(x){
    x$ConfInt
  })
  names(extractedDataFrames) <- fileNames
  wb <- openxlsx::createWorkbook()
  for(i in 1:length(extractedDataFrames)){
    openxlsx::addWorksheet(wb, sheetName = fileNames[i])
    openxlsx::writeData(wb, 
                        sheet = fileNames[i], 
                        extractedDataFrames[[i]])
  }
  openxlsx::saveWorkbook(wb, file.path(outputFolderPath, "ConfidenceIntervalsExcelWB.xlsx"), overwrite = TRUE)
}