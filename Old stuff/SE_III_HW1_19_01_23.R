## Content: HW1, Tasks 2 (R simulation), Mini-Project
## Provided by: Arsen Goian, Petro Nochovkin, Daria Sivka
## Date: 19/01/2023

# Loading libraries
library(tseries)
pacman::p_load(tidyverse, car, stargazer, readxl, corrplot, rms, jtools, margins
, ggeffects, multcomp, readxl, zoo, lubridate, ggplot2, forecast, dplyr, tseries)



## R SIMULATION

# In this task we consider the AR(2) 
# y_t = delta + th_1 * y_t-1 + th_2 * y_t-2 + epsilon_t
# Let us choose the coefficients and generate the sample of 1000 observations:

theta1 <- 0.6
theta2 <- - 0.2
sigma <- 1
delta <- 2
N <-1002

epsilon <- rnorm(n = N, mean = 0, sd = 1)
y <- numeric(N)
y[1] <- 0
y[2] <- 0

for (x in (3:N)) {
  y[x] <- delta + theta1*y[x-1] + theta2*y[x-2] + sigma*epsilon[x]
}

y <- y[3:1002]

plot(y, type = "l") # plotting the data >> the data is stationary
title(main = "Plot 1. AR(2) simulation")
mean(y) # calculating the mean >> 3.322545

# Autocovariances

var(y) # gamma 0 >> 1.405953
cov(y[2:1000], y[1:999]) # gamma 1 >> 0.7352901
cov(y[101:1000], y[1:900]) # gamma s for s = 100 >> 0.02887981

# Autocorrelations

acf(y, pl=FALSE)
pacf(y, pl=FALSE)


## MINI-PROJECT


# 1-st step. Clearing the data.

# We remove 6 header lines.
# We also remove 143 first observations as the price was fixed for the period 
# until Y1972.
raw_data <- read_excel(path = "CMO-Historical-Data-Monthly.xlsx", 
                       sheet = "Monthly Prices", skip = 6) 
raw_data <- raw_data %>% dplyr::slice(143:nrow(raw_data))

# Filtering data for GOLD.

gold <- raw_data %>% dplyr::select(GOLD) 
gold$n <- 1:nrow(gold)

gold$month <- raw_data[1] %>% map(function(row) {
  make_date(
    parse_number(substring(row,1,5)),
    parse_number(substring(row,6,7)),
    1
  )
}) %>% unlist

# Formatting data
gold$month <- as.Date(gold$month)


#gold$date <- as.Date(data$date, "%Y-%m-%d")
#data <- xts(data[,-1], order.by = data[,1])

ggplot(gold, aes(x=month,y=GOLD)) +
  geom_line() +
  ggtitle("Gold prices from 1972") +
  xlab("Date") +
  ylab("Value")

ggplot(gold, aes(x=month,y=GOLD)) +
  geom_line() +
  geom_smooth(method = "loess", size = 1.5) +
  geom_forecast(h=12) + 
  ggtitle("Time Series Plot with Moving Average and Forecast") +
  xlab("Date") +
  ylab("Value")

# Testing raw data for stationary using 

ggplot(gold, aes(x=month,y=GOLD)) +
  geom_line() +
  ggtitle("Gold prices, nominal USD") +
  xlab("Date") +
  ylab("Value")

adf.test(gold$GOLD, k=0)
acf(gold$GOLD, p=FALSE)
pacf(gold$GOLD, p=FALSE)


# 2-nd step. Adjusting the data.

# Removing linear trend

summary <-lm(GOLD ~ n, data=gold) %>% summary
beta_n <- summary$coefficients[2]
gold$no_linear_trend = gold$GOLD - beta_n * gold$n

ggplot(gold, aes(x=month,y=no_linear_trend)) +
  geom_line() +
  ggtitle("Time Series Plot") +
  xlab("Date") +
  ylab("Value")

# Removing moving average

period <- 48 # months
gold$moving_average <- rollmean(gold$GOLD, k=period, fill=NA)

ggplot(gold, aes(x=month)) +
  geom_line(aes(y=GOLD), color = "navy") +
  geom_line(aes(y=moving_average), color = "darkred") +
  ggtitle("Time Series Plot") +
  xlab("Date") +
  ylab("Value")

# Removing moving average (continuation)

pure_gold <- gold %>% na.omit
pure_gold$no_moving_average <- pure_gold$GOLD - pure_gold$moving_average


ggplot(pure_gold, aes(x=month)) +
  geom_line(aes(y=no_moving_average), color = "navy") +
  geom_line(aes(y=no_linear_trend), color = "darkred") +
  ggtitle("Time Series Plot") +
  xlab("Date") +
  ylab("Value")

# Step 3. Descriptive statistics

mean(pure_gold$no_moving_average)
sd(pure_gold$no_moving_average)
adf.test(pure_gold$no_moving_average, k=0)

# ACF, PACF tests

acf(pure_gold$no_moving_average)
pacf(pure_gold$no_moving_average)

pure_gold$logs = log(abs(pure_gold$no_moving_average))*(pure_gold$no_moving_average/abs(pure_gold$no_moving_average))
pure_gold$first_differences = diff(pure_gold$no_moving_average) %>% append(NA)

pure_gold$gold_lagged = pure_gold$no_moving_average[2:nrow(pure_gold)] %>% append(NA)
pure_gold$logs_lagged = pure_gold$logs[2:nrow(pure_gold)] %>% append(NA)

ggplot(pure_gold, aes(x=month,y=logs)) +
  geom_line() +
  ggtitle("Time Series Plot") +
  xlab("Date") +
  ylab("Value")

ggplot(pure_gold, aes(x=month,y=first_differences)) +
  geom_line() +
  ggtitle("Time Series Plot") +
  xlab("Date") +
  ylab("Value")

# Histograms and kernel densities of levels and logs

# Histograms
ggplot(pure_gold, aes(x=no_moving_average)) +
  geom_histogram() +
  ggtitle("Time Series Plot") +
  xlab("Date") +
  ylab("Value")

ggplot(pure_gold, aes(x=logs)) +
  geom_histogram() +
  ggtitle("Time Series Plot") +
  xlab("Date") +
  ylab("Value")


plot(density(pure_gold$no_moving_average))
plot(density(pure_gold$logs))


# Scatter plots of levels against their lagged counterparts

ggplot(pure_gold, aes(x=no_moving_average,y=gold_lagged)) +
  geom_point(size=2, shape=23) +
  ggtitle("Scatterplot") +
  xlab("Actual value") +
  ylab("Lagged value")

ggplot(pure_gold, aes(x=logs,y=logs_lagged)) +
  geom_point(size=2, shape=23) +
  ggtitle("Scatterplot (logs)") +
  xlab("Actual value") +
  ylab("Lagged value")

sd(pure_gold$GOLD)


fit <- ets(pure_gold$logs)

checkresiduals(fit)


