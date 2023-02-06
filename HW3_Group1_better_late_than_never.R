# HW3 done by Group1

# 0.0 PREPARATION

library(tidyverse)
library(tibble) #creates tidy data frames
library(lubridate) #manipulation with dates
library(tsibble) #allowing temporal structures in tidy data frames
library(tseries) #time series analysis
library(feasts) #tools for analyzing TS
library(fable) #models for TS
library(forecast) #forecasting by models
library(readxl)
library(zoo)
library(urca)

#install.packages("FinTS")
#but do not load!

#install.packages("dynlm")
library(dynlm)

#install.packages("erer")
library(erer)

#install.packages("rugarch")
library(rugarch)

# 0.1 UPLOADING DATA

USMacro_rates <- read_xls("quarterly.xls", sheet = 1, col_types = c("text", rep("numeric", 18))) 


USMacro_rates <- tryCatch(
  {
    return(read_xls("quarterly.xls", sheet = 1, col_types = c("text", rep("numeric", 18))))
  }, 
  error = function(e){
    # If this is failing, the only explanation is I'm running on Arsen's device
    # Let's patch this up real quick
    return(read_xls("C:\\Users\\arsen\\OneDrive\\UNIV\\2023 term III-IV\\Stastics & Econometrics\\SE\\Term 3\\Homeworks\\quarterly.xls", sheet = 1, col_types = c("text", rep("numeric", 18))))
  }
)


USMacro_rates$DATE <- as.yearqtr(USMacro_rates$DATE, format = "%Y Q%q")
#view(USMacro_rates)

#transforming data into ts
USMacro_rates <- USMacro_rates %>% mutate(Quarter = yearquarter(DATE),  .before = ends_with("E"))
USMacro_rates <- USMacro_rates %>% select(-DATE)


#setting the tsibble
USMacro_tsb <- USMacro_rates %>% as_tsibble(index = Quarter)

# Step 2. calculating the spread
USMacro_tsb$ir_spread <- (USMacro_tsb$r5 - USMacro_tsb$Tbill)

# Step 3. Visualizing and trying to guess stationarity 
autoplot(USMacro_tsb, ir_spread) + labs(title = "Spread between 3-m Tbills and 5-Y bonds")

####################################

# 1. ADF test

####################################

# Step A) 

#GTS method: testing 12 lags
gst_test <- ur.df2(USMacro_tsb$ir_spread, type = "drift", lags = 12)
gst_test$testreg

USMacro_tsb %>% ACF(ir_spread, lag_max = 12) %>% autoplot() # autocor goes out of bound — non-stationary, suspected seasonality
USMacro_tsb %>% PACF(ir_spread, lag_max = 12) %>% autoplot() # partial autocor goes out of bound — non-stationary


# AIC and BIC method: testing for 12 lags 

#doing an augmented version of the test for every lag
summic_names <- vector("character",12)
summic_aic <- vector("numeric",12)
summic_bic <- vector("numeric",12)

for (i in 1:12){
  test_results<- ur.df2(USMacro_tsb$ir_spread, type = c("none", "drift", "trend"), lags = i,
                        selectlags = c("Fixed", "AIC", "BIC"), digit = 2)
  summic_names[i] <- paste0("Lag:", i)
  summic_aic[i] <- test_results$aic
  summic_bic[i] <- test_results$bic
  print(paste0("Lag AIC ", i, ": ", test_results$aic))
  print(paste0("Lag BIC ", i, ": ", test_results$bic))
}

stats_table <- data.frame(summic_names, summic_aic, summic_bic)
print(stats_table)
#the results were influenced by  

# automatic approach (does not show the best results)
lags <- ur.df(USMacro_tsb$ir_spread, type = "none", selectlags = "AIC") # number of lags based upon the AIC
summary(lags) 
#the smallest score is given for 1pct, therefore k = 1 for ADF


# Step b)

# Augmented Dickey-Fuller test for spread, 8th lag
adf.test(USMacro_tsb$ir_spread, alternative = "stationary", k = 8) 
# as we can se, the SE of the test is -4.5275 which is close to the value provided in the equation
# p-val is 0.01 — the data is stationary

# Step c)


# Augmented Dickey-Fuller test for r5, 7 lags
adf.test(USMacro_tsb$r5, alternative = "stationary", k = 7) 
# p-value = 0.837 — the data is non-stationary 

# Step d)


# Augmented Dickey-Fuller test for tbill, 7 lags
adf.test(USMacro_tsb$Tbill, alternative = "stationary", k = 11) 
# p-value = 0.5568 — the data is non-stationary 


# Step e)
# Explanation provided in the word. doc.


#################################

# 2.1 Forecast Evaluation. OLD!

#################################


#Continuing with forecasting (part we did before)

# selecting the necessary amount of obs.
macro_spread_models <- USMacro_tsb %>% filter_index("1961 Q4" ~ "2012 Q4")


macro_spread_models %>% gg_tsdisplay(ir_spread, "partial")
#PACF cuts of after 6 lag -> AR(6).
#PACF cuts of after 7 lag -> AR(7),
#PACF cuts of after 2 lag -> AR(2)
#PACF cuts of after 2 lag -> AR(2). 

#Estimate AR(2)
ar2 = ar(macro_spread_models$ir_spread, order=2, method= "yule-walker")
ar2

#Estimate AR(6)
ar6 = ar(macro_spread_models$ir_spread, order=6, method= "yule-walker")
ar6

#Estimate AR(7)
ar7 = ar(macro_spread_models$ir_spread, order=7, method= "yule-walker")
ar7


#Estimating best ARIMA model

fit <- macro_spread_models %>%
  model(arima101 = ARIMA(ir_spread ~ pdq(1,0,1)),
        arima201 = ARIMA(ir_spread ~ pdq(2,0,1)),
        auto = ARIMA(ir_spread, stepwise = FALSE, greedy = FALSE))

fit

glance(fit)

#Reviewing the coefficients of each model
#fit %>% select(arima101) %>% report()
#fit %>% select(arima201) %>% report()


# Simulating and testing various models

# ARMA(1,1)
fit %>% select(arima101) %>% gg_tsresiduals() +
  labs(title = "Residual plots for the ARIMA(1, 0, 1) model")

#Portmanteau + ljung_box test (PT)
augment(fit) %>% filter(.model == 'arima101') %>% features(.innov, ljung_box, lag = 12, dof = 1)
#p-val 0.0127 < 0.05 -> residuals are not white noise 


# ARMA(2,1)
fit %>% select(arima201) %>% gg_tsresiduals() +
  labs(title = "Residual plots for the ARIMA(2, 0, 1) model")

#Portmaneu + ljung_box test (PT)
augment(fit) %>% filter(.model == 'arima201') %>% features(.innov, ljung_box, lag = 12, dof = 1)
#p-val 0.0646 > 0.05 -> residuals are white noise 

gg_arma(fit %>% select(arima101, arima201))


#finding best model according to our consideration
best_model_ours <- auto.arima(macro_spread_models$ir_spread)
summary(best_model_ours)


###########################################################

# Forecast - new part! 

############################################################

# defining the model of ARMA[2,(1,7)]

best_model <- arima(macro_spread_models$ir_spread, order = c(2,0,7), fixed = c(NA, NA, NA, 0, 0, 0, 0, 0, NA, NA))
#summary(best_model)
stargazer::stargazer(best_model, type = "text")


#Checking ARMA[2,(1,7)]
glance(best_model)

#The Ljung–Box Q-statistic
Box.test(resid(best_model),type="Ljung",lag=4,fitdf=0)
Box.test(resid(best_model),type="Ljung",lag=8,fitdf=0)
Box.test(resid(best_model),type="Ljung",lag=12,fitdf=0)

#gg_arma(best_model)


# FORECAST BASED ON OTHER ARIMAS

FORECAST_LENGTH <- 50

macro_spread_forecast <- USMacro_tsb %>% 
  filter_index("1961 Q4" ~ "2012 Q4") %>% 
  head(-1*FORECAST_LENGTH) # cut the period
autoplot(macro_spread_forecast)


fore_fit <- macro_spread_forecast %>%
  model(arima101 = ARIMA(ir_spread ~ pdq(1,0,1)),
        arima201 = ARIMA(ir_spread ~ pdq(2,0,1)),
        auto = ARIMA(ir_spread, stepwise = FALSE, greedy = FALSE))
fore_fit

glance(fore_fit)




# Plot both forecast and actual values
fore_fit %>% fabletools::forecast(h = FORECAST_LENGTH) %>% autoplot(macro_spread_models)

# ATTEMPT: Visulising fitted vs actual

fit.arima<-fitted.values(best_model)
str(arima2)
str(best_model)
low<-fit.arima-1.96*sqrt(ht) #generate heteroscedastic 95% CIs
high<-fit.arima+1.96*sqrt(ht)
plot.ts(Y, col='blue')
lines(fit.arima, col='red')
  lines(low, col='red')
  lines(high, col='red')


# ATTEMPT - visualising forecast
mydata.arima214 <- arima(macro_spread_forecast$ir_spread, order = c(2,1,4))
mydata.pred1 <- predict(mydata.arima111, n.ahead=100)
plot.ts(macro_spread_forecast$ir_spread)
lines(mydata.pred1$pred, col="blue")
lines(mydata.pred1$pred+2*mydata.pred1$se, col="red")
lines(mydata.pred1$pred-2*mydata.pred1$se, col="red")



# ESTIMATION FORECAST
estimation <- fore_fit %>% dplyr::select(auto, arima101, arima201) %>% fabletools::forecast(h = FORECAST_LENGTH)
fabletools::accuracy(estimation, macro_spread_models)

# FORECAST BASED ON OTHER ARMA[2,(1,7)]

# Actual data
#          RMSE   MAE   MPE    MAPE
# arima101 0.859 0.750 -20.3  367.
# arima201 0.867 0.758 -22.2  366.

# Now let's try to replicate these results

actual_values <- USMacro_tsb %>% filter_index("2000 Q3" ~ "2012 Q4") %>% select(ir_spread)

# Errors of forecast (according to Kevin): e_f(t+h), R <= t < = T
e_f <- function(estimated_values) {
  return(actual_values$ir_spread - estimated_values$.mean)
}

f_average <- function(vector, P) {
  return (sum(vector) / P)
}


f_bias <- function(estimated_values, P) {
  return( estimated_values %>% e_f %>% f_average(P))  
}

f_RMSE <- function(estimated_values, P) {
  return( estimated_values %>% e_f %>% `^`(2) %>% f_average(P) %>% sqrt )
}

f_MAE <- function(estimated_values, P) {
  return( estimated_values %>% e_f %>% abs %>% f_average(P))
}

f_MAPE <- function(estimated_values, P) {
  return( estimated_values %>% e_f %>% `/`(actual_values$ir_spread) %>% abs %>% f_average(P))
}


f_arima101 <- estimation %>% filter(.model == 'arima101')
f_arima201 <- estimation %>% filter(.model == 'arima201')


<<<<<<< HEAD
model_report <- function(name, m, n) {
  return( c(name, f_bias(m,n), f_RMSE(m,n), f_MAE(m,n), f_MAPE(m,n)))
}

different_models_report <- data.frame(
  model_101 = model_report("Arima 101", f_1t, FORECAST_LENGTH),
  model_201 = model_report("Arima 201", f_2t, FORECAST_LENGTH)
)

print(different_models_report)


=======
f_bias(f_arima101, FORECAST_LENGTH)
f_RMSE(f_arima101, FORECAST_LENGTH) # 0.859, exactly as expected
f_MAE(f_arima101, FORECAST_LENGTH) # 0.75, as expected
f_MAPE(f_arima101, FORECAST_LENGTH) # 3.67 = 367%


f_bias(f_arima201, FORECAST_LENGTH) # 0.1149302
f_RMSE(f_arima201, FORECAST_LENGTH) # 0.8670345
f_MAE(f_arima201, FORECAST_LENGTH) # 0.7577366
f_MAPE(f_arima201, FORECAST_LENGTH) #3.656373
>>>>>>> 057a477afe021964212693b563f06d995f5fcbd6

#############################################################

# 3. ARCH/GARCH
# (based on variance of squared residuals of the best selected model)

#############################################################

# begin working with residuals
macro_spread_models$resid <- best_model$residuals
macro_spread_models$resid2 <- best_model$resid^2

var(best_model$resid^2)

# check for garch effect visually
#ggAcf(macro_spread_models$resid2)
#ggPacf(macro_spread_models$resid2)
macro_spread_models %>% gg_tsdisplay(resid2, "partial")


macro_spread_models$resid %>% autoplot()

# Engle’s ARCH Test
FinTS::ArchTest(macro_spread_models$resid, lag = 1)
#lag 1 taken as a rule of thumb


# estimating the ARCH(1) model (just for laughs) 
ppi.arch <- garch(macro_spread_models$resid2,c(0,1)) # for residuals from the ARMA(2,1) above
sum_ppi.arch <- summary(ppi.arch)
sum_ppi.arch  


#get the conditional variance h from the fitted ARCH(1) above
hhat <- ts(2*ppi.arch$fitted.values[-1,1]^2)
plot.ts(hhat)
ppi.arch$fitted.values[,1]^2


# Estimating Garch(1,0)

garchSpec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0))
)

garchFit <- ugarchfit(spec = garchSpec, data = macro_spread_models$resid)
print(garchFit)
coef(garchFit)

# Empty model on residuals
garchSpec_2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,0)),
  mean.model = list(armaOrder = c(0,0))
)

garchFit_2 <- ugarchfit(spec = garchSpec_2, data = macro_spread_models$resid)
print(garchFit_2)
coef(garchFit_2)


# model on teh data itself
garchSpec_alt <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,2))
)

garchFit_alt <- ugarchfit(spec = garchSpec_alt, data = macro_spread_models$ir_spread)
print(garchFit_alt)
coef(garchFit_alt)

# Important tests:

# Ljung Box Tests are used to test serial autocorrelation among the residuals. (Null: No autocorrelation)
# ARCH LM test is used to check presence of ARCH effect. (Null: Adequately fitted ARCH process)


# Forecasting
forecast_arch <- ugarchboot(garchFit_alt, method = c("Partial","Full")[1],
                            n.ahead = 12, n.bootpred = 1000, n.bootfit = 1000)


# method: Either the full or partial bootstrap (see help).
# n.ahead: The forecast horizon. (what do we need according to the TB?)
# n.bootfit: The number of simulation based re-fits used to generate
# the parameter distribution (i.e the parameter uncertainty). Not relevant for the Partial method.
#n.bootpred: The number of bootstrap replications per parameter distribution per
# n.ahead forecasts used to generate the predictive density.

# extract forecasted data
f_arch <- forecast_arch@forc@forecast$seriesFor #this is for series forecasts
f_arch

forecast_arch@forc@forecast$sigmaFor

# get forecast from ARMA(1,7) model
f_arima <- forecast(best_model, h = 12)
f_arima

# accuracy(f_arima$mean, 'test sample')
# accuracy(f_arch, 'test sample')
forecast_arch

plot(forecast_arch)

