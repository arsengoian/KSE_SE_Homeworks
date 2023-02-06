# Example with the German barley prices in the 90's

#getwd()
#setwd('XXXX')

#install.packages("tseries")
library(tseries)
#library(foreign)

# import the data
ba <- read.csv("barley.csv")

# variables description
# obs - time variable
# ccFBpp - barley producer price
# ccFBwp - barley wholesales price
# ccD - structural break dummy (introduction of the MacSharry reform in the framework of the EU Common Agricultural Policy )
# dm... - seasonal dummies


str(ba)
View(ba)
row.names(ba)
class(ba)

attach(ba)

#plot the prices
plot.ts(ccFBwp) # barley whole sales prices

# OLS model to discover deterministic process

lm.ba<-lm(ccFBwp~time+dm2+dm3+dm4+dm5+dm6+dm7+dm8+dm9+dm10+dm11+dm12)
summary(lm.ba)
ccFBwp.fit<-fitted(lm.ba) # extract fitted values of prices based on the model estimates 
lines(ccFBwp.fit, lty=2, lwd=2, col='red') # add fitted prices to the actual ones

#add a structural break and repeat the exercise
lm.ba<-lm(ccFBwp~time+dm2+dm3+dm4+dm5+dm6+dm7+dm8+dm9+dm10+dm11+dm12+ccD)
summary(lm.ba)
ccFBwp.fit2<-fitted(lm.ba)

#plot the prices
lines(ccFBwp.fit2, lty=2, lwd=2, col='blue')

#plot residuals to have a look if stationary
uhat<-residuals(lm.ba)

plot.ts(uhat)
hist(uhat)



