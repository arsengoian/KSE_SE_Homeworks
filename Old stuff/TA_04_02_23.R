library(tidyverse)
library(tsibble)
library(lubridate) #manipulation with dates
library(tseries) #time series analysis
library(feasts) #tools for analyzing TS
library(fable) #models for TS
library(forecast) #forecasting by models
library(rugarch) #for GARCH models
library(tseries) # for `adf.test()`
library(zoo)
library(readxl)
remotes::install_github("ccolonescu/PoEdata")
library(PoEdata) #for PoE4 datasets
library(urca)


# load data
data("byd", package = "PoEdata")
colnames(byd) <- 'return'
return_ts <- ts(byd$return, start = c(1961, 1), frequency = 12) # the returns to shares in BrightenYourDay Lighting

# estimate arima model
fit <- auto.arima(byd$return)
summary(fit)

# get residuals
byd$resid <- fit$residuals
byd$resid2 <- byd$resid^2

# check for garch effect visually
ggAcf(byd$resid2)
ggPacf(byd$resid2)

byd$resid %>% autoplot()

# Engle’s ARCH Test
FinTS::ArchTest(byd$resid, lag = 1)

# Estimate models
# model on residuals
garchSpec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0))
  )

garchFit <- ugarchfit(spec = garchSpec, data = byd$resid)
print(garchFit)
coef(garchFit)

# model on residuals
garchSpec_2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
  mean.model = list(armaOrder = c(0,0))
)

garchFit_2 <- ugarchfit(spec = garchSpec_2, data = byd$resid)
print(garchFit_2)
coef(garchFit_2)


# model on data
garchSpec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,2))
)

garchFit_alt <- ugarchfit(spec = garchSpec_alt, data = byd$return)
print(garchFit_alt)
coef(garchFit_alt)

# Important tests:
# Ljung Box Tests are used to test serial autocorrelation among the residuals. (Null: No autocorrelation)
# ARCH LM test is used to check presence of ARCH effect. (Null: Adequately fitted ARCH process)


# Forecasting
forecast_arch <- ugarchboot(garchFit_alt, method = c("Partial","Full")[1],
                            n.ahead = 12, n.bootpred = 1000, n.bootfit = 1000)

# method: Either the full or partial bootstrap (see help).
# n.ahead: The forecast horizon.
# n.bootfit: The number of simulation based re-fits used to generate
# the parameter distribution (i.e the parameter uncertainty). Not relevant for the Partial method.
#n.bootpred: The number of bootstrap replications per parameter distribution per
# n.ahead forecasts used to generate the predictive density.

# extract forecasted data
f_arch <- forecast_arch@forc@forecast$seriesFor #this is for series forecasts
f_arch

forecast_arch@forc@forecast$sigmaFor

# get forecast from ARIMA(0,0,2) model
f_arima <- forecast(fit, h = 12)
f_arima

# accuracy(f_arima$mean, 'test sample')
# accuracy(f_arch, 'test sample')
forecast_arch

plot(forecast_arch)



