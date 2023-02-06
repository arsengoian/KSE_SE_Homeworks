## Content: MINI-PROJECT_2
## Provided by: Arsen Goian, Petro Nochovkin, Daria Sivka
## Date: 26/01/2023


# 0. UPPLOADING PACKAGES

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


## 1. FORMATTING

# Recap from the MP1 >> We remove 6 header lines & first 143 first observations 
# as the price was fixed for the period until Y1972.

# UPLOAD
raw_data <- read_excel(path = "CMO-Historical-Data-Monthly.xlsx", 
                       sheet = "Monthly Prices", skip = 6) 
raw_data <- raw_data %>% dplyr::slice(143:nrow(raw_data))

# CLEAN DATE
raw_data$...1 <- as.yearmon(raw_data$...1, format = "%YM%m") 

# TSIBBLE DATA
raw_data <- raw_data %>% dplyr::mutate(Month = yearmonth(...1), .before = ends_with("1")) 
raw_data <- raw_data %>% dplyr::select(Month, GOLD)
tsb_gold <- raw_data %>% as_tsibble(index = Month)

## 2.TEST STATIONARITY

# PLOTS
autoplot(tsb_gold) + 
  labs(title = "Historical gold prices from 1972, USD") # non stationary

tsb_gold %>% 
  gg_season(GOLD, labels = "both") +
  labs(y = "$, nominal",
       title = "Seasonal plot: Gold prices") # no obvious seasonal pattern

tsb_gold %>%
  gg_subseries(GOLD) +
  labs(y = "$, nominal",
       title = "Gold prices") # no obvious seasonal pattern

# UNIT ROOT TEST

#H0 = data is stationary, small p-value (<0.05) => differencing required
tsb_gold %>%
  features(GOLD, unitroot_kpss) # > p-value is 0.01 => hyp. rejected

#Determine the appropriate number of first differences
tsb_gold %>%
  features(GOLD, unitroot_ndiffs) # RESULT: ndiffs = 1 => take 1st difference

#Determine if seasonal differencing is required
tsb_gold %>%
  mutate(log_gold = log(GOLD)) %>%
  features(log_gold, unitroot_nsdiffs) # RESULT: nsdiffs = 0 => no seasonal pattern

# RUN ACF, PACF

tsb_gold %>% gg_tsdisplay(GOLD, "partial") # >> AR(1) process

# AUGMENTED DICKEY-FULLER TEST

adf.test(tsb_gold$GOLD, alternative = "stationary", k = 1) # >> p-value is 0.8344
# k is a lag parameter, reflects the level of AR process in our case = 1.
# data in levels is nonstationary

# Intermediate conclusion: 
# We have proved that the data is non-stationary with AR(1) process. 
# According to the tests above, we should take a 1st difference. 
# There is also no seasonal pattern, so no seasonal diff required. 
# We will deal with the non-seasonal ARIMA models.

## 3. TRANSFORM THE DATA INTO STATIONARY STOCHASTIC PROCESS

# 1ST DIFFERENCE
tsb_gold %>% mutate(gold_diff = difference(GOLD)) %>%
  gg_tsdisplay(gold_diff, "partial") # data looks not perfect > make ADF

# AUGMENTED DICKEY-FULLER TEST with differenced data
tsb_gold <- tsb_gold %>% mutate(gold_diff = difference(GOLD))
tsb_gold <- tsb_gold[-1,] 
adf.test(tsb_gold$gold_diff, alternative = "stationary", k = 1) 
# p-value = 0.01 >> data is stationary >> proceed with the analysis

## 4. ARIMA

# REMOVE 10 OBSERVATIONS
arima_gold <- tsb_gold %>% filter_index("1971 Dec" ~ "2022 Feb")

# 1ST DIFF ACF, PACF w/o those observations
arima_gold %>% gg_tsdisplay(gold_diff, "partial")

# ?ARIMA
# ACF cuts of after 1st lag, significant on 3rd lag as well
# PACF significant on first two lags and has some sample errors on lag 5 and 11
# ARIMA (p,d,q), d is 1 >> (0,1,2), (1,1,1), (1,1,2), (2,1,2)

fit_1 <- arima_gold %>%
  model(arima012 = ARIMA(GOLD ~ pdq(0,1,2) + PDQ(0,0,0)),
        arima111 = ARIMA(GOLD ~ pdq(1,1,1) + PDQ(0,0,0)),
        arima112 = ARIMA(GOLD ~ pdq(1,1,2) + PDQ(0,0,0)),
        auto = ARIMA(GOLD, stepwise = FALSE, greedy = FALSE))

glance(fit_1) %>% arrange(AICc) %>% dplyr::select(.model:BIC)

# The best resuls has arima111 and auto ARIMA models.
# auto ARIMA is ARIMA(0,1,5)(0,0,1)[12] w/ drift >> 
# AR=0, Diff=1, MA=5, w/o drift, with one seasonal pattern under MA component.
# AICc of both of them = 5818, BIC for arima111 = 5836, auto = 5853.
# Log likelihood of auto a little higher >> better goodness-of-fit of a model
# AIC, BIC (goodness of fit + penalty for complexity)
# Model with the best combination AICc&BIC is arima111.

fit_1 %>% dplyr::select(arima111) %>% report()
fit_1 %>% dplyr::select(auto) %>% report()    


# ARIMA(1,1,1)
fit_1 %>% dplyr::select(arima111) %>% gg_tsresiduals() +
  labs(title = "ARIMA(1, 1, 1)_residuals")

# Residuals check (RESULT: p-value = 0.0967 (> 0.05) >> white noise)
augment(fit_1) %>% 
  dplyr::filter(.model == 'arima111') %>% features(.innov, ljung_box, lag = 36, dof = 0)

# ARIMA(auto)
fit_1 %>% dplyr::select(auto) %>% gg_tsresiduals() +
  labs(title = "ARIMA(auto)_residuals")

# Residuals check (RESULT: p-value = 0.268 (> 0.05) >> white noise)
augment(fit_1) %>% 
  filter(.model == 'auto') %>% features(.innov, ljung_box, lag = 36, dof = 4)

# 5. FORECAST
forecast_arima <- tsb_gold %>% filter_index("2020 Dec" ~ "2022 Feb") # cut the period
autoplot(forecast_arima)

fit_1 %>% fabletools::forecast(h = 10) %>% autoplot(forecast_arima)
fit_1 %>% dplyr::select(arima111) %>% fabletools::forecast(h = 10) %>% autoplot(forecast_arima)

# ESTIMATION FORECAST
estimation <- fit_1 %>% dplyr::select(auto, arima111) %>% fabletools::forecast(h = 10)
fabletools::accuracy(estimation, tsb_gold)
# RMSE, RMSSE is better for arima111 >> choose arima111





