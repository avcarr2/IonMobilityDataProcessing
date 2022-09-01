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
