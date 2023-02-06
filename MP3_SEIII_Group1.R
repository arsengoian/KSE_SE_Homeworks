## Content: MINI-PROJECT_3
## Provided by: Arsen Goian, Petro Nochovkin, Daria Sivka
## Date: 04/02/2023
# Disclaimer: we have divided the code into two main parts: 
# 1) "Previously done"
# 2) "MP3"
# So, please, do not waste time and scroll to the 2) part.

# RECAP ON MP1,MP2 >>
# We have proved that the data is non-stationary with AR(1) process. 
# We justified to take a 1st difference. 
# There is also no seasonal pattern, so no seasonal diff taken. 
# We worked with the non-seasonal ARIMA model.
# ADF and KPSS test suggested that we have a stationary data, although under the visual 
# exception it was clear, that we have some conditional heteroscedasticity.
# So, we will proceed from the point of choosing ARIMA and checking on ARCH existence.

library(tidyverse)
library(tibble) 
library(lubridate) 
library(tsibble) 
library(tseries) 
library(feasts) 
library(fable) 
library(forecast) 
library(readxl)
library(zoo)
library(urca)
library(rugarch)
library(Metrics)


# UPLOAD
setwd("/Users/dariasivka/Desktop/KSE_EA24/Econometrics_III/HW/HW3")
raw_data <- read_excel("CMO-Historical-Data-Monthly.xlsx", 
                       sheet = "Monthly Prices", skip = 6) 
raw_data <- raw_data %>% dplyr::slice(143:nrow(raw_data))

# CLEAN DATE
raw_data$...1 <- as.yearmon(raw_data$...1, format = "%YM%m") 

# TSIBBLE DATA
raw_data <- raw_data %>% dplyr::mutate(Month = yearmonth(...1), .before = ends_with("1")) 
raw_data <- raw_data %>% select(Month, GOLD)
tsb_gold <- raw_data %>% as_tsibble(index = Month)

# TRANSFORM THE DATA INTO STATIONARY STOCHASTIC PROCESS, 1ST DIFFERENCE
tsb_gold <- tsb_gold %>% mutate(gold_diff = difference(GOLD))
tsb_gold %>% gg_tsdisplay(gold_diff, "partial")

# ARIMA. REMOVE 10 OBSERVATIONS
arima_gold <- tsb_gold %>% filter_index("1971 Dec" ~ "2022 Feb")
adf.test(arima_gold$gold_diff)

# ARIMA (p,d,q), d is 1 >> (0,1,2), (1,1,1), (1,1,2), (2,1,2)

fit_1 <- arima_gold %>%
  model(arima012 = ARIMA(GOLD ~ pdq(0,1,2) + PDQ(0,0,0)),
        arima111 = ARIMA(GOLD ~ pdq(1,1,1) + PDQ(0,0,0)),
        arima112 = ARIMA(GOLD ~ pdq(1,1,2) + PDQ(0,0,0)),
        auto = ARIMA(GOLD, stepwise = FALSE, greedy = FALSE))

glance(fit_1) %>% arrange(AICc) %>% select(.model:BIC)
# The best results (based on AICc, BIC) have arima111 and auto ARIMA models.

# ARIMA(1,1,1)
fit_1 %>% select(arima111) %>% gg_tsresiduals() +
  labs(title = "ARIMA(1, 1, 1)_residuals")
# Residuals check (RESULT: p-value = 0.0967 (> 0.05) >> white noise)
augment(fit_1) %>% 
  filter(.model == 'arima111') %>% features(.innov, ljung_box, lag = 36, dof = 0)

# ARIMA(auto)
fit_1 %>% select(auto) %>% gg_tsresiduals() +
  labs(title = "ARIMA(auto)_residuals")
# Residuals check (RESULT: p-value = 0.268 (> 0.05) >> white noise)
augment(fit_1) %>% 
  filter(.model == 'auto') %>% features(.innov, ljung_box, lag = 36, dof = 4)


# MP3

# TASK 1

# FORECAST
forecast_arima <- arima_gold %>% filter_index("2020 Dec" ~ "2022 Feb") 
autoplot(forecast_arima)

fit_1 %>% fabletools::forecast(h = 10) %>% autoplot(forecast_arima)

fit_1 %>% select(arima111) %>% fabletools::forecast(h = 10) %>% autoplot(forecast_arima)
fit_1 %>% select(auto) %>% fabletools::forecast(h = 10) %>% autoplot(forecast_arima)

# ESTIMATION FORECAST (LOSS FUNCTIONS)
estimation <- fit_1 %>% select(auto, arima111) %>% fabletools::forecast(h = 10)
fabletools::accuracy(estimation, tsb_gold)

# PRICE FORECAST ARIMA FOR Y2023
fit_2 <- tsb_gold %>% model(arima111 = ARIMA(GOLD ~ pdq(1,1,1) + PDQ(0,0,0)))
glance(fit_2)

fit_2 %>% select(arima111) %>% gg_tsresiduals() +
  labs(title = "ARIMA(1, 1, 1)_residuals")
augment(fit_2) %>% 
  filter(.model == 'arima111') %>% features(.innov, ljung_box, lag = 36, dof = 0)

# Building the forecast, plot >>
fit_2 %>% fabletools::forecast(h = 12) %>% filter_index("2023 Jan" ~ "2023 Dec") %>% autoplot() + 
  labs(title = "Gold prices forecast for Y2023", y= "USD nominal")

# Look at the forecasted values >>
fit_2 %>% fabletools::forecast(h = 12) %>% filter_index("2023 Jan" ~ "2023 Dec")


# TASK 2

# ARIMA(1,1,1)

price_train <- tsb_gold$GOLD[-c(605, 606, 607, 608, 609, 610, 611, 612, 613, 614)]
price_test <- tsb_gold %>% filter_index("2022 Mar" ~ "2022 Dec") 
price_train_df <- na.aggregate(difference(price_train))
price_test_df <- na.aggregate(difference(price_test$GOLD))


arima(price_train, order = c(1,1,1))
arima_t <- arima(price_train, order = c(1,1,1))
resid_arima <- residuals(arima_t)

Box.test(resid_t, lag=5) # >> p-value = 0.291


# RESIDUALS
ggtsdisplay(resid_arima)
resid2_sq<-resid_arima^2
ggtsdisplay(resid2_sq)

# LM TEST >> p-value = 1.807e-12 >> artch effect present
resid2ArchLM <- FinTS::ArchTest(resid_t, lags=2, demean=TRUE)
resid2ArchLM

#LET US TRY SOME GARCH
arch_t = garchFit(~garch(1, 0), data = price_train_df, cond.dist = c("std"), trace = F)
summary(md_arch_t) #do not looks good

#ESTIMATE garch_t = garchFit(~garch(1, 1), data = price_train_df, trace = F)
summary(garch_t)
pred_garch_G = predict(garch_t, 10, plot = TRUE)
pred_garch_G[1]
garch_t = garchFit(~garch(1, 1), data = price_train_df, trace = F)
summary(garch_t)
pred_garch_G = predict(garch_t, 10, plot = TRUE)
pred_garch_G[1]
rmse(predicted = as.numeric(unlist(pred_garch_G[1])), actual = as.numeric(unlist(price_test_df)))
mape(predicted = as.numeric(unlist(pred_garch_G[1])), actual = as.numeric(unlist(price_test_df)))
mae(predicted = as.numeric(unlist(pred_garch_G[1])), actual = as.numeric(unlist(price_test_df)))

#GARCH+ARIMA
arima_garch <- garchFit(price_train_df~arma(1,1)+garch(1,1), data=price_train_df) 
predict_garch_arch = predict(arima_garch,n.ahead=10, plot = TRUE)
predict_garch_arch[1]


#PREDICTION FOR Y2023
price_2023 <- tsb_gold$GOLD
price_2023_df <- na.aggregate(difference(price_2023))
arima_garch_2023 <- garchFit(price_2023_df~arma(1,1)+garch(1,1), data=price_2023_df) 
predict_2023 = predict(arima_garch_2023,n.ahead = 12, plot = TRUE)
predict_2023[1]

dev.off()




