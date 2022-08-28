source("R/functions.R")
source("R/OnLoad.R")
main <- function(inputFolder, outputFolder, minMz, maxMz){
  
  ## Get vector of file names
  input_file_names <- list.files(path = inputFolder, pattern= '*.csv', recursive=TRUE)
  folder_names  <- tools::file_path_sans_ext(basename(input_file_names)) 
  folder_paths <- file.path(outputFolder, folder_names)
  lapply(folder_paths, dir.create)
  
  ## loop over the file names
  
  ## initialize list to hold all output
  outputList <- list()
  
  for(i in 1:length(input_file_names)){
    ## Create output directory folder
    tempResult <- list()
    csv <- read.csv(file = file.path(inputFolder, input_file_names[i]),
                    skip = 1)
    namesCsv <- colnames(csv)[-1]
    ## initialize the list to hold all the results
    
    EnergyVals <- as.numeric(stringr::str_extract(namesCsv, pattern = "([0-9]+)"))
    PercentVals <- c()
    ## Also need to get the energy values as well

    for(j in 1:length(namesCsv)){
      
      final_path <- file.path(folder_paths[i], paste("Energy", namesCsv[j], sep = "_")) 
      if(!dir.exists(final_path)){ dir.create(final_path) }
      
      S <-  MALDIquant::createMassSpectrum(csv[,1], 
                                           csv[,1+j], 
                                           metaData=list(name="Spectrum2"))
      
      plot1 <- plotIntensityFunction(S, outputFilePath = final_path)
      plot2 <- plotSmoothingFunction(spectra1, outputFilePath = final_path)
      plot3 <- plotBaselineFunction(spectra2, outputFilePath = final_path)
      plot4 <- plotSNFunction(spectra3, outputFilePath = final_path)
      
      PercentVals[j] <- calcSY(spectra3)
      plot5 <- IntensitiesUsed(spectra3, outputFilePath = final_path)
    }
    
    outputPercentValTable <- data.frame(EnergyVals, PercentVals)
    x <- EnergyVals
    y <- PercentVals
    tempResult[[1]] <- outputPercentValTable
    

    nlslmfit <- minpack.lm::nlsLM(y ~ M_4pl(x, lower.asymp, upper.asymp, inflec, hill),
                     data = data.frame(x=x, y=y),
                     start = c(lower.asymp=min(y)+1E-10, 
                               upper.asymp=max(y)-1E-10, 
                               inflec=mean(x), 
                               hill=1),
                     control = nls.control(maxiter=1000, warnOnly=TRUE) )
    SYSum <- summary(nlslmfit)
    
    #store inflection point as SY50
    SYEstimates <- nlslmfit$m$getAllPars()
    SY50 <- SYEstimates[3]
    tempResult[[2]] <- SY50
    
    #library("investr")
    xvals=seq(min(x),max(x),length.out=200)
    predintervals = data.frame(x=xvals,
                               investr::predFit(nlslmfit, newdata=data.frame(x=xvals), 
                                                interval="prediction"))
    confintervals = data.frame(x=xvals,
                               investr::predFit(nlslmfit, newdata=data.frame(x=xvals), 
                                                interval="confidence"))
    predSY50Y <- data.frame(x=SY50,
                            investr::predFit(nlslmfit, newdata=data.frame(x=SY50), 
                                    interval="prediction"))
    SY50Y <- predSY50Y[,2]
    SY50Y
    #SYCurve
    ggplot(data=predintervals, x=x, y=fit, 
           ymin=lwr, ymax=upr, 
           geom="ribbon", fill=I("red"), alpha=I(0.2)) +
      geom_ribbon(data=confintervals, aes(x=x, ymin=lwr, ymax=upr), fill=I("blue"), alpha=I(0.2)) +
      geom_line(data=confintervals, aes(x=x, y=fit), colour=I("blue"), lwd=1) +
      geom_point(data=outputPercentValTable, aes(x=x, y=y, ymin=NULL, ymax=NULL), size=3) +
      ylab("SY Percent") + xlab("Energy") +
      geom_point(aes(SY50, SY50Y), col="red", size=3)
    ggsave(filename = file.path(folder_paths[i], "SYPlot.png"))
  }
}
