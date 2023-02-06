phi1 <- 0.5
phi2 <- -0.3
n <-1000

epsilon <- rnorm(n = n, mean = 0, sd = 1)
X <- numeric(1000)

for (i in (3:n)) {
  X[i] <- phi1*X[i-1] + phi2*X[i-2] + epsilon[i]
}


plot(X, type = "l")

ar2_new <- arima(X, order = c(2, 0, 0), include.mean = F)
ar2_new


pacf(X, plot = F)[1:2]

mod1 <- lm(X ~ 0 + lag(X,1) + lag(X,2))
summary(mod1)
pacf(X, plot = F)[1:2]
mod1$coefficients

pacf(X, plot = F)[1:2]
lm(X ~ 0 + lag(X,1))$coefficients[1]
lm(X ~ 0 + lag(X,1) + lag(X,2))$coefficients[2]


theta1 <- 0.3
theta2 <- -0.6

ro0 <- 1
ro1 <- theta1/(1-theta2)
ro2 <- theta1*ro1 + theta2*ro0
ro3 <- theta1*ro2 + theta2*ro1
ro4 <- theta1*ro3 + theta2*ro2
ro5 <- theta1*ro4 + theta2*ro3

ro1
ro2
ro3
ro4
ro5

vec <- c(ro1, ro2, ro3, ro4, ro5)

(ro2 - ro1^2)/(1-ro1^2)


plot(vec)



