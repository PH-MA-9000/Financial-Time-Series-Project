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

bootstrapmeans <- function(samp, n) {
  means <- c()
  for (boot in 1:n) {
    bootsamp <- sample(samp, size = length(samp), replace = TRUE)
    means <- c(means, mean(bootsamp))
  }
  return(means)
}
#Taking logs of data
data <- ts(data)
data[,-c(1,2)] <- log(data[,-c(1,2)])

plot.ts(data)

data[c(36,1194,2836,3430,4118),-c(1,2)] <- NA
for (i in c(36,1194,2836,3430,4118)) {
  data[c(i-1,i,i+1),] <- na.approx(data[c(i-1,i,i+1),])   #replacing outliers by mean
}

plot.ts(data)


gurk <- data[,3]

miss <- which(is.na(gurk[1:(length(gurk) - 200)]))


interpresids <- data.frame(NA)
k <- 0
set.seed(1234)
par(mfrow=c(1,1))
plot.ts(gurk)
interpnames <- c()
for (i in c(seq(from = 50, to = miss[length(miss)], by = 49), seq(from = (miss[length(miss)]+1), to = (length(gurk) - 200), by = 49) ) ) {
  if (i %in% (miss[1]):(miss[length(miss)] + 49)  ) {
    next
  }
  k <- k + 1
  interp <- approx(c(i-49,i),c(gurk[i-49],gurk[i]))
  lines(interp, col = "red")
  interpresids <- cbind(interpresids, (interp$y[-c(1,length(interp$y))] - gurk[(i-49 + 1):(i - 1)]))
  interpnames <- c(interpnames, paste0("interpolation ", k))
}

interpresids <- interpresids[,-1]
names(interpresids) <- interpnames

bootmeans <- list()
bootmeanvar <- c()
meanofboots <- c()
for (i in 1:48) {
  bootmeans[[i]] <- bootstrapmeans(interpresids[,i], 10000)
  bootmeanvar <- c(bootmeanvar, var(bootmeans[[i]]))
  meanofboots <- c(meanofboots, mean(bootmeans[[i]]))
}


upperci <- meanofboots + 1.96*sqrt(bootmeanvar)
lowerci <- meanofboots - 1.96*sqrt(bootmeanvar)



set.seed(1234)
testboot <- bootstrapmeans(interpresids[,25], 10000)
hist(testboot, breaks = 50)
var(testboot)
win.graph()
par(mfrow = c(2,2))
for (mn in bootmeans) {
  hist(mn, breaks = 50)
}



set.seed(100967)
sample(interpresids[[1]], size = length(interpresids[[1]]) ,replace = TRUE)

