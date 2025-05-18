library(zoo)
library(forecast)
library(patchwork)
library(ggplot2)
library(Metrics)
library(dplyr)
library(tseries)
library(patchwork)
library(mice)
library(bootImpute)
library(rugarch)
library(corrplot)
data <- read.csv("spiff_data.csv")  #getting time series data

bootstrapmeans <- function(samp, n) {     #Creating n bootstrap samples and computing the mean for each sample
  means <- c()
  for (boot in 1:n) {
    bootsamp <- sample(samp, size = length(samp), replace = TRUE)
    means <- c(means, mean(bootsamp))
  }
  return(means)
}

data <- ts(data)
plot.ts(data)

data[c(36,1194,2836,3430,4118),-c(1,2)] <- NA
for (i in c(36,1194,2836,3430,4118)) {
  data[c(i-1,i,i+1),] <- na.approx(data[c(i-1,i,i+1),])   #replacing outliers by mean of two closest points
}

plot.ts(data)

for (ts in 3:9) {   #Running the imputation for each time series

tmpdata <- data[,ts]

miss <- which(is.na(tmpdata[1:(length(tmpdata) - 200)]))         #indices for missing value gap
 
nainterpolated <- approx(c(miss[1]-1, miss[length(miss)] + 1), c(tmpdata[miss[1]-1], tmpdata[miss[length(miss)] + 1]), xout = (miss[1]-1):(miss[length(miss)] + 1))    #Using linear interpolation to impute missing values
nostartendinterptmpdata <- nainterpolated$y[-c(1,length(nainterpolated))]           #Only values we have imputed, ends are same as orig data

interpresids <- data.frame(NA)
k <- 0
set.seed(1234)
par(mfrow=c(1,1))
plot.ts(tmpdata)
interpnames <- c()
for (i in c(seq(from = 52, to = miss[length(miss)], by = 51), seq(from = (miss[length(miss)]+1), to = (length(tmpdata) - 200), by = 51) ) ) {    #Fitting linear interp. and calc residuals to get samples for imputed values residuals
  if (i %in% (miss[1]):(miss[length(miss)] + 51)  ) {
    next
  }
  k <- k + 1
  interp <- approx(c(i-51,i),c(tmpdata[i-51],tmpdata[i]), xout = (i-51):i)
  lines(interp, col = "red")
  interpresids <- cbind(interpresids, (interp$y[-c(1,length(interp$y))] - tmpdata[(i-51 + 1):(i - 1)]))
  interpnames <- c(interpnames, paste0("interpolation ", k))
}

interpresids <- interpresids[,-1]
names(interpresids) <- interpnames

bootmeans <- list()
meanofboots <- c()
upperci <- c()
lowerci <- c()
n <- 1000
for (i in 1:50) {                      #Getting upper & lower confidence intervals and the mean of the distribution for the mean for each residual
  bootmeans[[i]] <- sort(bootstrapmeans(as.numeric(interpresids[i,]), n))
  meanofboots <- c(meanofboots, mean(bootmeans[[i]]))
  lowerci <- c(lowerci, bootmeans[[i]][0.025*n])
  upperci <- c(upperci, bootmeans[[i]][0.975*n])
  print(paste0("iter", i))
}

#Getting upper and lower CI for expected value of the real unobserved value
exptrueupper <- nostartendinterptmpdata - lowerci
exptruelower <- nostartendinterptmpdata - upperci

newimputation <- nostartendinterptmpdata- meanofboots     #The new values used for imputation
data[miss,ts] <- newimputation            #Adding imputed values to data

plot.ts(tmpdata, xlim = c(miss[1] - 50, miss[length(miss)] + 50), ylim = c(min(tmpdata[(miss[1] - 50):(miss[length(miss)] + 50)], na.rm = TRUE),max(tmpdata[(miss[1] - 50):(miss[length(miss)] + 50)], na.rm = TRUE)))   #Plotting the imputation with CI
lines(miss,newimputation, col = "red")
lines(miss,exptrueupper, col = "blue")
lines(miss,exptruelower, col = "blue")

}


plot.ts(data[,3], xlim = c(150,300), ylim = c(1.8,1.95))    


write.csv(data, "bootimputation.txt")   #Writing the new data to a CSV/txt file
