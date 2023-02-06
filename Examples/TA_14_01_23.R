phi1 <- 0.5
phi2 <- -0.3
sigma <- 1
X1 <- 0
X2 <- 0
n <-102

epsilon <- rnorm(n = n, mean = 0, sd = 1)
X <- numeric(n)
X[1] <- X1
X[2] <- X2

for (i in (3:n)) {
  X[i] <- phi1*X[i-1] + phi2*X[i-2] + sigma*epsilon[i]
}


plot(X, type = "l")

ar2_new <- arima(X, order = c(2, 0, 0), include.mean = F)
ar2_new














