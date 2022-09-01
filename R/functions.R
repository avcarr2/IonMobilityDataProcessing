#### Functions ####
plotIntensityFunction <- function(S, outputFilePath){
  spectra1 <<- MALDIquant::transformIntensity(S, method="sqrt")
  
  png(filename = file.path(outputFilePath, "IntensityPlot.png"), 
      units = "in", width = 6.67, height = 6.67, res = 72)
  MALDIquant::plot(S, xlim=c(400,700),  ylim=c(0, 800), main="Transform Intensity")
  MALDIquant::lines(spectra1, col="blue")
  dev.off()
}
plotSmoothingFunction <- function(spectra1, outputFilePath){
  spectra2 <<- MALDIquant::smoothIntensity(spectra1, 
                               method="SavitzkyGolay",
                               halfWindowSize=20, 
                               polynomialOrder=3)
  png(filename = file.path(outputFilePath, "SmoothedPlot.png"), 
      units = "in", width = 6.67, height = 6.67, res = 72)
  MALDIquant::plot(spectra1, xlim=c(400, 620), ylim=c(0, 60), main="Smoothing")
  MALDIquant::lines(spectra2, col="blue")
  dev.off()
}
plotBaselineFunction <- function(spectra2, outputFilePath){
  baseline <- MALDIquant::estimateBaseline(spectra2, method="SNIP", iterations=50)
  spectra3 <<- MALDIquant::removeBaseline(spectra2, method="SNIP", iterations=50)
  
  png(filename = file.path(outputFilePath, "BaselinePlot.png"), 
      units = "in", width = 6.67, height = 6.67, res = 72)
  MALDIquant::plot(spectra2, xlim=c(400, 620), ylim=c(0, 60), main="Baseline Subtracting")
  MALDIquant::lines(baseline, col="red", lwd=1)
  MALDIquant::lines(spectra3, col="blue", lwd=1, lty=5)
  dev.off()
}
plotSNFunction <- function(spectra3, outputFilePath){
  noise <<- MALDIquant::estimateNoise(spectra3)
  
  png(filename = file.path(outputFilePath, "SNFunctionPlot.png"), 
      units = "in", width = 6.67, height = 6.67, res = 72)
  MALDIquant::plot(spectra3, xlim=c(400, 620), ylim=c(0, 60), main="Singal to Noise")
  MALDIquant::lines(noise, col="red")
  # *2 means plot signal to noise ratio of 2. *3 means S/N of 3
  lines(noise[,1], noise[,2]*5, col="blue")
  #plot(spectra3, xlim=c(400, 620), ylim=c(0, 60))
  #noise <- estimateNoise(spectra3)
  #lines(noise, col="red")
  ## *2 means plot signal to noise ratio of 2. *3 means S/N of 3
  #lines(noise[,1], noise[,2]*5, col="blue")
  dev.off()
}
IntensitiesUsed <- function(spectra3, outputFilePath){
  png(filename = file.path(outputFilePath, "IntensitiesUsedPlot.png"), 
      units = "in", width = 6.67, height = 6.67, res = 72)
  MALDIquant::plot(spectra3, ylim=c(0, 10), main="Intensities Used")
  MALDIquant::lines(noise[,1], noise[,2]*5, col="blue")
  MALDIquant::lines(spectraMod, col="red")
  dev.off()
}
calcSY <- function(spectra3, minMz, maxMz){
  # use spectra3 to get 2 values. 1 for total signal, then other for just defined range
  # if above noise, add, if not, ignore. maybe noise 1 or 2. but then it wont count the first few peak areas
  # then do s/n cutoff of 3. #noise[row1,column2]*s/n
  cutoff <- noise[1,2]*5
  # replace any value below s/n threshold with 0
  MALDIquant::intensity(spectra3)[MALDIquant::intensity(spectra3) <= cutoff] <- 0
  spectraMod <<- spectra3
  #intensity of all fragments
  totalfrag <- MALDIquant::totalIonCurrent(spectraMod)
  #intensity of original peak of interest
  abetacu <- MALDIquant::totalIonCurrent(MALDIquant::trim(spectraMod, c(minMz, maxMz)))
  #calculate survival yield
  SY <<- abetacu/totalfrag
  return(SY)
}
calcratio <- function(spectra3, minMz, maxMz){
  # use spectra3 to get 2 values. 1 for total signal, then other for just defined range
  # if above noise, add, if not, ignore. maybe noise 1 or 2. but then it wont count the first few peak areas
  # then do s/n cutoff of 3. #noise[row1,column2]*s/n
  cutoff <- noise[1,2]*5
  # replace any value below s/n threshold with 0
  MALDIquant::intensity(spectra3)[MALDIquant::intensity(spectra3) <= cutoff] <- 0
  spectraMod <<- spectra3
  #intensity of all fragments
  totalfrag <- MALDIquant::totalIonCurrent(spectraMod)
  #intensity of original peak of interest
  abetacu <- MALDIquant::totalIonCurrent(MALDIquant::trim(spectraMod, c(minMz,maxMz)))
  abeta <- MALDIquant::totalIonCurrent((MALDIquant::trim(spectraMod, c(597,599))))
  #calculate survival yield
  ratio <<- abeta/abetacu
  return(SY)
}
M_4pl <- function(x, lower.asymp, upper.asymp, inflec, hill){
  f <- lower.asymp + ((upper.asymp - lower.asymp)/
                        (1 + (x / inflec)^-hill))
  return(f)
}