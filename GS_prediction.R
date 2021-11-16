rm(list=ls())


# Required libraries

library(ggplot2)
library(plotly)
library(tseries)
library(FinTS)
library(astsa)
library(rugarch)
library(spgs)

# Loading the data set
path <- 'https://raw.githubusercontent.com/s-karmakar16/Time-Series-Project/main/GS_yahoo.csv'
raw.data <- read.csv(url(path))
data <- raw.data[raw.data$Date>= "2018-01-01"  & raw.data$Date <="2021-08-31",]
data$Date <- as.Date.character(data$Date)


# Plotting train data
p <-ggplot(data,aes(x=Date, y=Adj.Close)) +
  geom_line(colour = "royalblue") + labs(x = 'Date', y = 'Adj Close', title = "Adj Close Plot with Time") + 
  theme_bw()
p <- ggplotly(p)
p


# Required Data
x <-  data$Adj.Close
n <- length(x)

#Turning Point Test to check if there is deterministic component
turningpoint.test(x)


#Relative Ordering Test Function
ro.test <- function (y = timeseries){
  n<-length(y)
  q<-0
  for(i in 1:(n-1))
  {
    for(j in (i+1):n)
    {
      if(y[i]>y[j])
      {
        q<-q+1
      }
    }
  }
  eq<-n*(n-1)/4
  tau<-1-(4*q/(n*(n-1)))
  var_tau<-(2*(2*n+5))/(9*n*(n-1))
  z<-tau/sqrt(var_tau)
  if(z>0)
  {
    p_value<-1-pnorm(z)
  }
  if(z<0)
  {
    p_value<-pnorm(z)
  }
  cat("            Relative Ordering Test for Presence of Trend \n\n")
  cat("Null Hypothesis: Absence of Trend, and \n")
  cat("Alternative Hypothesis: Presence of Trend. \n\n")
  cat("Test Statistic:",paste(round(z,4)),"\n")
  cat("p_value:", paste(round(p_value,4)),"\n")
  cat("No. of Discordants:",paste(q),"\n")
  cat("Expected No. of Discordants:",paste(eq),"\n")
}



ro.test(x)


#lth order backward difference function
detrend <- function(x,l){
  k <- 1
  y <- array(0)
  for(i in (l+1):length(x)){
    y[k] <-  x[i] - x[i-l]
    k <- k+1
  }
  return(y)
}



detrended = detrend(x,1)
ro.test(detrended)
plot.ts(detrended , xlab = "Date", ylab = "First Order Differences", main = "Detrended Data")



# Checking for Stationarity - Augmented Dickey Fuller test
adf.test(detrended)


# acf and pacf plot
acf(detrended, lag.max =30, main  = "ACF of Detrended Data")
pacf(detrended, lag.max =30, main = "PACF of Detrended Data")



# Order Estimation of ARIMA
best.order = c(0,0,0)
best.aic = Inf
for (q in 0:7) for (p in 0:7){
  fit.model = arima(detrended, order = c(p,0,q) ,optim.control = list(maxit = 1000))
  fit.aic = fit.model$aic
  if(fit.aic < best.aic){
    best.order = c(p,1,q)
    best.aic = fit.aic
  }
}
best.order   # Best order ARIMA is at (5,1,3)
best.aic


# Fitting ARIMA 
fit.model = arima(x, order = c(5,1,3) ,optim.control = list(maxit = 1000) )
res = fit.model$residuals
#acf(res, lag.max = 30, main = "ACF plot of Residuals of ARIMA Model")
#pacf(res, lag.max =  30, main = "PACF plot of Residuals of ARIMA Model")
plot(density(res), main = "Density plot of Residuals of ARIMA Model")
plot.ts(res, main = "Plot of Residuals of ARIMA MODEL")


# Checking for autocorelation of residuals - Ljung-Box Test
Box.test(res,lag=20,type='Ljung-Box')


# Estimating conditional volatility
acf(res^2, lag.max = 30, main = "ACF plot of Residual^2 of ARIMA Model")
pacf(res^2, lag.max =  30, main = "PACF plot of Residual^2 of ARIMA Model")

# Checking for ARCH effect in model
ArchTest(res,lag=7)


# Forecast using ARIMA
forecast1=sarima.for(x,n.ahead=20,5,1,3)


# Fitting ARCH(7)
model<-ugarchspec(variance.model = list(model ="sGARCH",garchOrder = c(7, 0)), mean.model =list(armaOrder = c(0,0), include.mean = FALSE),distribution.model = "norm")
fit1<-ugarchfit(model,res,out.sample = 100)
forecast2<-ugarchforecast(fit1, n.ahead = 20)

b<-forecast2@forecast$sigmaFor
a<-forecast1$pred
set.seed(12)
w<-rnorm(20)
predicted <- a+b*w
observed <- raw.data[raw.data$Date > "2021-08-31",]$Adj.Close[1:20]


#Plot of Observed vs Predicted data
cols = c("Predicted" = "red", "Observed" = "royalblue")
df.new <- data.frame(predicted, observed)
ggplot(df.new, aes(x = 1:20)) + geom_line(aes(y = predicted, col = "Predicted")) +
  geom_line(aes(y = observed, col = "Observed")) + theme_bw() + 
  labs(title = "Predicted vs Oberved Adj Close", x = "Time", y = "Adj Close", color = "Legend") +
  scale_color_manual(values = cols)



#Measures of Prediction Error Rate
mape = mean(abs((predicted - observed)/observed))*100
mape

#MAPE is pretty low. Hence we conclude that our Fitted model is quite good.














