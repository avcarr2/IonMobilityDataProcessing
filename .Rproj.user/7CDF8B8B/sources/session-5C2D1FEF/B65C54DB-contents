#### Packages ####

install.packages("sigmoid")
library("sigmoid")

if(!require(c("MALDIquant", "dplyr", "tidyverse", "minpack.lm", "investr"), character.only = TRUE)){
  
}
install.packages("MALDIquant", "dplyr", "tidyverse", "minpack.lm", "investr")
packages <- c("MALDIquant", "dplyr", "tidyverse", "minpack.lm", "investr")
lapply(packages, function(x){library(x, character.only = TRUE)})

#packages <- c("MALDIquant", "dplyr")
#if(!require(packages)){
#  install.packages(packages)
#  library(packages)
#}


#### Functions ####
plotIntensityFunction <- function(S){
  spectra1 <<- transformIntensity(S, method="sqrt")
  plot(S, xlim=c(400,700),  ylim=c(0, 800), main="Transform Intensity")
  lines(spectra1, col="blue")
}
plotSmoothingFunction <- function(spectra1){
  spectra2 <<- smoothIntensity(spectra1, method="SavitzkyGolay",halfWindowSize=20, polynomialOrder=3)
  plot(spectra1, xlim=c(400, 620), ylim=c(0, 60), main="Smoothing")
  lines(spectra2, col="blue")
}
plotBaselineFunction <- function(spectra2){
  baseline <- estimateBaseline(spectra2, method="SNIP", iterations=50)
  spectra3 <<- removeBaseline(spectra2, method="SNIP", iterations=50)
  plot(spectra2, xlim=c(400, 620), ylim=c(0, 60), main="Baseline Subtracting")
  lines(baseline, col="red", lwd=1)
  lines(spectra3, col="blue", lwd=1, lty=5)
}
plotSNFunction <- function(spectra3){
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
IntensitiesUsed <- function(spectra3){
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
IntensitiesUsed <- function(spectra3){
  plot(spectra3, ylim=c(0, 10), main="Intensities Used")
  lines(noise[,1], noise[,2]*5, col="blue")
  lines(spectraMod, col="red")
}


#### Data ####
input_files_names <- list.files(path = '.', pattern= '*.csv', recursive=TRUE)
#input_files <- lapply(input_files_names, read.csv, header = TRUE, skip = 1, sep = ",")

#extract energy value from column name, starting with column 2
#input_files_col <- mapply(input_files, colnames)
#EnergyValues <- lapply(input_files_names, colnames(read.csv()))

csv <- read.csv(file = input_files_names[[1]], header = TRUE, skip = 1, sep = ",")

S <- createMassSpectrum(csv$X.TrapCV.,csv$X10.0, metaData=list(name="Spectrum2"))

csv
#### Plots for each energy ####
plot(S, main="Original MSMS", ylim=c(0,400), xlim=c(200,1200))
plot1 <- plotIntensityFunction(S)
plot2 <- plotSmoothingFunction(spectra1)
plot3 <- plotBaselineFunction(spectra2)
plot4 <- plotSNFunction(spectra3)
calcSY(spectra3)
plot5 <- IntensitiesUsed(spectra3)
#put each set of plots in a folder(defined by file)
#put each calcSY in a table(defined by file), as y and x is energy value
#ie 10SY = 80, 12SY = 76, 14SY = 70
#SY is also percent

#### Survival Yield Plots ####
#use list for now as place holder
Energy <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)
Percent <- c(0.61778, 0.53347,	0.41726,	0.2709,	0.16812,	0.10815,	0.07447,	0.05674,	0.047144,	0.03834,	0.03298)
x <- Energy
y <- Percent
abetaCu <- data.frame (Energy, Percent)
#above lines should be done with table you will create in earlier chunk

SY_abetaCu <- ggplot(data=abetaCu) +
  geom_point(mapping = aes(x = Energy, y = Percent))
plot(SY_abetaCu)



M_4pl <- function(x, lower.asymp, upper.asymp, inflec, hill){
  f <- lower.asymp + ((upper.asymp - lower.asymp)/
                        (1 + (x / inflec)^-hill))
  return(f)
}

#library("minpack.lm")
nlslmfit = nlsLM(y ~ M_4pl(x, lower.asymp, upper.asymp, inflec, hill),
  data = data.frame(x=x, y=y),
  start = c(lower.asymp=min(y)+1E-10, upper.asymp=max(y)-1E-10, inflec=mean(x), hill=1),
  control = nls.control(maxiter=1000, warnOnly=TRUE) )
SYSum <- summary(nlslmfit)

#store inflection point as SY50
SYEstimates <- nlslmfit$m$getAllPars()
SY50 <- SYEstimates[3]
SY50

#library("investr")
xvals=seq(min(x),max(x),length.out=100)
predintervals = data.frame(x=xvals,predFit(nlslmfit, newdata=data.frame(x=xvals), interval="prediction"))
confintervals = data.frame(x=xvals,predFit(nlslmfit, newdata=data.frame(x=xvals), interval="confidence"))

#SYCurve
ggplot(data=predintervals, x=x, y=fit, ymin=lwr, ymax=upr, geom="ribbon", fill=I("red"), alpha=I(0.2)) +
  geom_ribbon(data=confintervals, aes(x=x, ymin=lwr, ymax=upr), fill=I("blue"), alpha=I(0.2)) +
  geom_line(data=confintervals, aes(x=x, y=fit), colour=I("blue"), lwd=1) +
  geom_point(data=abetaCu, aes(x=x, y=y, ymin=NULL, ymax=NULL), size=3) +
  ylab("SY Percent") + xlab("Energy") +
  geom_point(aes(SY50, 0.5))
#trying to indicate SY50 on each line

#Then store every SY50 and SY50curve plots for every file in a table


#### Ignore Everything Below ####


#detectpeaks doesnt really seem like it works well for my purpose. only returns the intensity at the m/z value
#will not do area of peak
#peaks <- detectPeaks(spectra3, method="MAD", halfWindowSize=10, SNR=5)
#plot(spectra3, xlim=c(390, 400), ylim=c(0, 60))
#points(peaks, col="red", pch=4)

#mpeaks <- monoisotopicPeaks(peaks)

#totalIonCurrent(spectra2)
#total
#modifiedlist <- createMassSpectrum(peaks@mass, peaks@intensity)
#plot(modifiedlist, xlim=c(390, 400), ylim=c(0, 60))


#spec3intensity <- intensity(spectra3)

#select spectra points number 400 to 600
#spectra3[400:600]

#select spectra from 400m/z to 600m/z
#trim(spectra3, c(400,600))