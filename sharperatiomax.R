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




#Taking logs of data
data <- ts(data)

data[c(36,1194,2836,3430,4118),-c(1,2)] <- NA
for (i in c(36,1194,2836,3430,4118)) {
  data[c(i-1,i,i+1),] <- na.approx(data[c(i-1,i,i+1),])   #replacing outliers by mean
}

approxing <- na.approx(data[-(5257:5456),])
which(is.na(approxing[,3]))
plot.ts(approxing)
diffdata <- diff(approxing, lag = 252)

covmat <- cov(diffdata[,-c(1,2)])
covmat


n_portfolios <- 500000

weights <- matrix(runif(7 * n_portfolios), ncol = 7)
weights <- weights / rowSums(weights)



plot.ts(diffdata)

mu <- diffdata/approxing[1:5004,]
mu <- colMeans(mu)

returns <- weights %*% mu[-c(1,2)]
risks <- apply(weights, 1, function(w) sqrt(t(w) %*% covmat %*% w))
risk_free_rate <- 0.03
sharpe_ratios <- (returns - risk_free_rate) / risks


max_sharpe_idx <- which.max(sharpe_ratios)
tangent_return <- returns[max_sharpe_idx]
tangent_risk <- risks[max_sharpe_idx]

# Create data frame for plotting
frontier <- data.frame(Return = returns, Risk = risks)

cml_slope <- (tangent_return - risk_free_rate) / tangent_risk
cml_x <- seq(0, max(risks), length.out = 100)
cml_y <- risk_free_rate + cml_slope * cml_x
cml <- data.frame(Risk = cml_x, Return = cml_y)

# Plot efficient frontier and CML
ggplot(frontier, aes(x = Risk, y = Return)) + xlim(0,tangent_risk + 0.25) + ylim(0,0.1) +
  geom_point(alpha = 0.4, color = "darkblue") +
  geom_line(data = cml, aes(x = Risk, y = Return), color = "red", size = 1.2) +
  geom_point(aes(x = tangent_risk, y = tangent_return), color = "green", size = 3) +
  annotate("text", x = tangent_risk, y = tangent_return + 0.005,
           label = "Tangency Portfolio", color = "green", hjust = 0.5) +
  ggtitle("Efficient Frontier with Capital Market Line (Rf = 3%)") +
  xlab("Portfolio Risk (Standard Deviation)") +
  ylab("Expected Return") +
  theme_minimal()
