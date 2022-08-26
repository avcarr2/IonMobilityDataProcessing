#### Functions ####
plotIntensityFunction <- function(S, outputFilePath){
  spectra1 <<- transformIntensity(S, method="sqrt")
  png(filename = file.path(outputFilePath, "IntensityPlot.png"))
  plot(S, xlim=c(400,700),  ylim=c(0, 800), main="Transform Intensity")
  lines(spectra1, col="blue")
  dev.off()
}
plotSmoothingFunction <- function(spectra1, outputFilePath){
  spectra2 <<- smoothIntensity(spectra1, method="SavitzkyGolay",halfWindowSize=20, polynomialOrder=3)
  plot(spectra1, xlim=c(400, 620), ylim=c(0, 60), main="Smoothing")
  lines(spectra2, col="blue")
}
plotBaselineFunction <- function(spectra2, outputFilePath){
  baseline <- estimateBaseline(spectra2, method="SNIP", iterations=50)
  spectra3 <<- removeBaseline(spectra2, method="SNIP", iterations=50)
  plot(spectra2, xlim=c(400, 620), ylim=c(0, 60), main="Baseline Subtracting")
  lines(baseline, col="red", lwd=1)
  lines(spectra3, col="blue", lwd=1, lty=5)
}
plotSNFunction <- function(spectra3, outputFilePath){
  noise <<- estimateNoise(spectra3)
  plot(spectra3, xlim=c(400, 620), ylim=c(0, 60), main="Singal to Noise")
  lines(noise, col="red")
  # *2 means plot signal to noise ratio of 2. *3 means S/N of 3
  lines(noise[,1], noise[,2]*5, col="blue")
  #plot(spectra3, xlim=c(400, 620), ylim=c(0, 60))
  #noise <- estimateNoise(spectra3)
  #lines(noise, col="red")
  ## *2 means plot signal to noise ratio of 2. *3 means S/N of 3
  #lines(noise[,1], noise[,2]*5, col="blue")
}
IntensitiesUsed <- function(spectra3, outputFilePath){
  plot(spectra3, ylim=c(0, 10), main="Intensities Used")
  lines(noise[,1], noise[,2]*5, col="blue")
  lines(spectraMod, col="red")
}
calcSY <- function(spectra3){
  # use spectra3 to get 2 values. 1 for total signal, then other for just defined range
  # if above noise, add, if not, ignore. maybe noise 1 or 2. but then it wont count the first few peak areas
  # then do s/n cutoff of 3. #noise[row1,column2]*s/n
  cutoff <- noise[1,2]*5
  # replace any value below s/n threshold with 0
  intensity(spectra3)[intensity(spectra3) <= cutoff] <- 0
  spectraMod <<- spectra3
  #intensity of all fragments
  totalfrag <- totalIonCurrent(spectraMod)
  #intensity of original peak of interest
  abetacu <- totalIonCurrent(trim(spectraMod, c(629,631.5)))
  #calculate survival yield
  SY <<- abetacu/totalfrag
  return(SY)
}
IntensitiesUsed <- function(spectra3, outputFilePath){
  plot(spectra3, ylim=c(0, 10), main="Intensities Used")
  lines(noise[,1], noise[,2]*5, col="blue")
  lines(spectraMod, col="red")
}
M_4pl <- function(x, lower.asymp, upper.asymp, inflec, hill){
  f <- lower.asymp + ((upper.asymp - lower.asymp)/
                        (1 + (x / inflec)^-hill))
  return(f)
}