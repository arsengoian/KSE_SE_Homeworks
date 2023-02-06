rm(list = ls())

library(tidyverse)
library(tsibble)
library(tsibbledata)
library(tseries)
library(feasts)
library(fable)
library(urca)

### Code with pipe operators and tsibble

# importing data
prison_raw <- read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

# creating quarterly data
prison_tsbl <- prison_raw %>% mutate(Quarter = yearquarter(Date))

# deleting unnecessary columns
prison_tsbl <- prison_tsbl %>% select(-Date)

# setting the tsibble
prison_tsbl <- prison_tsbl %>% as_tsibble(key = c(State, Gender, Legal, Indigenous), index = Quarter)

# data exploration
prison_tsbl %>% distinct(State)

# preparing final data set
df1 <- prison_tsbl %>% filter(State == "QLD", Gender == "Female", Indigenous == "ATSI", Legal == "Sentenced")
df1 <- df1[, 5:6]

# plotting data
autoplot(df1, Count) + labs(title = "Prisoners sentenced in QLD, 2005 Q1—2016 Q4")

# conducting formal testing
adf.test(df1$Count) # p-val is 0.2598 — non-stationary

# plotting ACF and PACF
df1 %>% ACF(Count, lag_max = 25) %>% autoplot() # autocor goes out of bound — non-stationary
df1 %>% PACF(Count, lag_max = 25) %>% autoplot() # partial autocor goes out of bound — non-stationary
# ACF decays slowly, PACF cuts of after 1 lag — AR1 

# differencing ts and plotting PACF
df1 %>% mutate(diff_count = difference(Count)) %>% PACF(diff_count, lag_max = 25) %>% autoplot()

df1 %>% mutate(diff_count = difference(Count)) %>% autoplot(diff_count)

### Shitcode (I swear I'm unbiased)

# preparing data set
df2 <- prison_raw %>% filter(State == "QLD", Gender == "Female", Indigenous == "ATSI", Legal == "Sentenced")
df2 <- df2[,c(-2,-3,-4,-5)]

# Defining variables
Count <- ts(df2$Count, frequency = 4, start = c(2005,1))
d_Count <- diff(Count) # differenced data 

t <- df2$Date

# Descriptive statistics and plotting the data
summary(Count)
summary(d_Count)

plot.ts(Count)
plot.ts(d_Count)

# Augmented Dickey-Fuller test, 1 lag
adf.test(Count, alternative = "stationary", k = 1) # p-val is 0.5694 — non-stationary

# Alternative implication of ADF-test
x <- ur.df(Count, type="none", selectlags ="AIC") # number of lags based upon the AIC
summary(x) # Value of test-statistic is greater than 5% tau — non-stationary

# Number of lags affects conclusions!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# DF and ADF tests for differenced variable
adf.test(d_Count, k = 0) # stationary

d_x <- ur.df(d_Count, type="none", selectlags ="AIC") # number of lags based upon the AIC 
summary(d_x) # Value of test-statistic is lower than 5% tau — stationary

# ACF and PACF
acf(Count) # autocor goes out of bound — non-stationary 
# Here and below x-axis is fucked and shows each 4th obs. :(
pacf(Count) # partial autocor goes out of bound — non-stationary

acf(d_Count) # stationary (NB! The first bar isn't a correlation! It's a variation, so we disregard its value)
pacf(d_Count) # stationary