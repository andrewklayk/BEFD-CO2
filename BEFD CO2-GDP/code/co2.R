library(chron)
require(RColorBrewer)
library(purrr)
library(car)
library(MASS)
library(leaps)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(data.table)
library(corrplot)
library(xgboost)
library(cowplot)
library(zoo)
library(tidyverse)
library(tidyr)
library(Hmisc)
library(psych)
library(repr)
library(caret)
library(glmnet)
library(randomForest)
library(caretEnsemble)
library(ROSE)
library(TTR)
library(forecast)
library(quantmod)
library(tseries)
library(rugarch)
library(prophet)
library(tsfknn)
library(astsa)


data = read.csv("../datasets/BEFD_CO2_DATA_prp.csv")
DEU = read.csv("../datasets/DEU.csv")
ITA = read.csv("../datasets/ITA.csv")
FRA = read.csv("../datasets/FRA.csv")
NLD = read.csv("../datasets/NLD.csv")
ESP = read.csv("../datasets/ESP.csv")
POL = read.csv("../datasets/POL.csv")
USA = read.csv("../datasets/USA.csv")
CHN = read.csv("../datasets/CHN.csv")
ZAF = read.csv("../datasets/ZAF.csv")

DEU_gdp = read.csv("../datasets/DEU_gdp.csv")
ITA_gdp = read.csv("../datasets/ITA_gdp.csv")
FRA_gdp = read.csv("../datasets/FRA_gdp.csv")
NLD_gdp = read.csv("../datasets/NLD_gdp.csv")
ESP_gdp = read.csv("../datasets/ESP_gdp.csv")
POL_gdp = read.csv("../datasets/POL_gdp.csv")
USA_gdp = read.csv("../datasets/USA_gdp.csv")
CHN_gdp = read.csv("../datasets/CHN_gdp.csv")
ZAF_gdp = read.csv("../datasets/ZAF_gdp.csv")

#-------------------------------------------------------------------------------------------------------------------#
#                                                                                                                   #
#-------------------------------------------------------------------------------------------------------------------#
# plot co2 emissions by year, for each country
ggplot(data, aes(x=Year, y = CO2_emissions)) +
  geom_line(color="red",size=1) +
  geom_point(color="blue") +
  facet_wrap(~Country) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  labs(title = "Emissions per country",
       x = "Year",
       y = "CO2 Emissions (metric tons per capita)") 


# plot gdp by year, for each country
ggplot(data, aes(x=Year, y = gdp)) +
  geom_line(color="red",size=1) +
  geom_point(color="blue") +
  facet_wrap(~Country) + 
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1)) +
  labs(title = "GDP per country",
       x = "Year",
       y = "GDP (current US$)") 

#Check correlation of gdp vs co2 emissions

cor(data$gdp, data$CO2_emissions) #comment : moderately correlated
#cor(ZAF$gdp, ZAF$CO2_emissions)
#-------------------------------------------------------------------------------------------------------------------#
#                                                                                                                   #
#-------------------------------------------------------------------------------------------------------------------#

dat_name = DEU_gdp


#Series Plots

#GDP
ggplot(dat_name, aes(x=Year, y = gdp)) +
  geom_line(color="red",size=1) +
  geom_point(color="blue")  +
  labs(title = "DEU GDP",
       x = "Year",
       y = "GDP (current US$)") 

dat_name = DEU

#CO2 Emissions
ggplot(dat_name, aes(x=Year, y = CO2_emissions)) +
  geom_line(color="red",size=1) +
  geom_point(color="blue")  +
  labs(title = "DEU Emissions",
       x = "Year",
       y = "CO2 Emissions (metric tons per capita)")

#-------------------------------------------------------------------------------------------------------------------#
#                                                                                                                   #
#-------------------------------------------------------------------------------------------------------------------#

#convert to time series
dat_ts <- ts(dat_name[,2:length(dat_name)], start = 1990, end = 2021)


# Convert time series to tibble object
dat_tbl <- tibble(CO2_emissions=as.numeric(dat_ts),
                  year=as.numeric(time(dat_ts)))


# Plot the data
ggplot(dat_tbl,aes(x=year,y=CO2_emissions)) +
  # ggtitle("DEU CO2 Emissions (1990-2021)")+
  ggtitle("DEU GDP (current US$) (1990-2021)")+
  geom_line(color="red") + geom_point(color="blue") +
  # ylab("CO2 Emissions (metric tons per capita)") + xlab("Year")
  ylab("GDP (current US$)") + xlab("Year")



#Trends

dat_linear <- tslm(dat_ts ~ trend) ## Fit linear trend 
dat_quad <- tslm(dat_ts ~ trend + I(trend^2)) ## Fit quadratic trend 
dat_ma7 <- ma(dat_ts, order=7) # Fit a q=3 moving average (ma=2q+1)
dat_ma3 <- ma(dat_ts, order=3) # Fit a q=3 moving average (ma=2q+1)

deu_with_fits <- cbind(dat_ts, 
                             Linear_trend=fitted(dat_linear), 
                             Quadratic_trend=fitted(dat_quad),
                             Moving_avg7 = dat_ma7,
                             Moving_avg3 = dat_ma3) 

# Construct the plot 
autoplot(deu_with_fits) + 
  # ylab("CO2 Emissions (metric tons per capita)") + 
  ylab("DEU GDP (current US$)") +
  xlab("Year") +
  # ggtitle("DEU CO2 Emissions (1990-2021)") +  
  ggtitle("DEU GDP (current US$) (1990-2021)") + 
  guides(colour= guide_legend(title = "Data series")) + 
  scale_colour_manual(values=c("black","red","blue","orange","green")) 


# Extract residuals for each estimated trend
dat_resids<-cbind(Res_orig=dat_ts-mean(dat_ts),
                        Res_linear=dat_ts-fitted(dat_linear),
                        Res_quad=dat_ts-fitted(dat_quad), 
                        Res_MA7 = dat_ts - dat_ma7,
                        Res_MA3 = dat_ts - dat_ma3)

# produce the autoplot 
autoplot(dat_resids, facet=TRUE) + xlab("Year") + ylab("Residuals") +  
  # ggtitle("DEU CO2 Emissions (1990-2021) Residuals") + 
  ggtitle("DEU GDP (current US$) (1990-2021) Residuals") +
  geom_hline(yintercept = 0) + 
  guides(colour=guide_legend(title="Data Series"))

ggAcf(dat_ts) + ggtitle("ACF for original series") + ylim(c(-1,1))
ggAcf(dat_resids[,"Res_linear"]) + ggtitle("ACF removing linear trend") + ylim(c(-1,1)) 
ggAcf(dat_resids[,"Res_quad"]) + ggtitle("ACF removing quadratic trend") + ylim(c(-1,1)) 
ggAcf(dat_resids[,"Res_MA7"]) + ggtitle("ACF removing MA7 trend")+ ylim(c(-1,1))
ggAcf(dat_resids[,"Res_MA3"]) + ggtitle("ACF removing MA3 trend")+ ylim(c(-1,1))



acf2(diff(dat_ts), max.lag = 30)
testy <- auto.arima(dat_ts)
testy

testy.f <- forecast(testy, length(dat_ts))

plot(testy.f, type="o", pch=16, xlim=c(1990, 2030),  main="ARMA(0,1) Model Multistep Forecasts")
lines(testy.f$mean, type="p", pch=16, lty="dashed", col="blue")
lines(dat_ts, type="o", pch=16, lty="dotted")

#___________________________________________________________________________________________________________________#
#___________________________________________________________________________________________________________________#
dlrGDP <- diff(log(dat_ts))
plot(dlrGDP,  xlab="Year", ylab="log difference in Real GDP",  main="Log Differences in Real GDP")
Acf(dlrGDP, type="correlation", lag=48, main="ACF for  Real GDP")
Acf(dlrGDP, type="partial", lag=48, main="PACF for  Real GDP")

a1 <- auto.arima(dlrGDP, seasonal = FALSE, stationary = TRUE, stepwise = FALSE, ic ="aicc")
a1

a2 <- auto.arima(dlrGDP, seasonal = TRUE, stationary = TRUE, stepwise = FALSE, ic ="aicc")
a2

arma22 <- Arima(dlrGDP, order=c(0, 0, 1))
plot.Arima(arma22)
tsdiag(arma22, gof.lag=36)

arma22.f <- forecast(arma22, length(dlrGDP))

plot(arma22.f, type="o", pch=16, xlim=c(1990, 2040),  main="ARMA(0,1) Model Multistep Forecasts")
lines(arma22.f$mean, type="p", pch=16, lty="dashed", col="blue")
lines(dlrGDP, type="o", pch=16, lty="dotted")
#___________________________________________________________________________________________________________________#
#___________________________________________________________________________________________________________________#
#___________________________________________________________________________________________________________________#
#ARIMA Models
sarima(dat_ts, 0, 0, 1)
# ARIMA(1, 1, 1)
sarima(dat_ts, 1, 1, 1)
# ARIMA(2, 1, 1)
sarima(dat_ts, 2, 1, 1)
# ARIMA(1, 1, 2)
sarima(dat_ts, 1, 1, 2)

sarima.for(dat_ts, n.ahead = 10, 1, 1, 1)


#Linear models
co2 <- ts(CHN$CO2_emissions)
gdp <- ts(CHN_gdp$gdp)
co2_train <- ts(co2[1:25])
gdp_train <- ts(gdp[1:25])

l1 <- tslm(co2_train~trend)
plot(forecast(l1,h=7))
summary(l1)
AIC(l1)

l2 <- tslm(co2~trend+gdp)
plot(forecast(l1,h=7))
summary(l2)
AIC(l2)

## GAM ##
library(gam)

tt <- (1:length(co2_train))
g1 <- gam(co2_train~lo(tt))
summary(g1)
plot(g1,se=T)
tsdisplay(residuals(g1))

test_g1 <- list(tt=1:32)
plot(co2)
lines(predict(g1, newdata=test_g1),col=2)

g2 <- gam(co2_train~lo(tt)+lo(gdp_train), control=gam.control(maxit=200,bf.maxit=200))
summary(g2)
plot(g2,se=T)
tsdisplay(residuals(g2))

test_g2 <- list(tt=1:32, gdp_train=gdp)
plot(predict(g2, newdata=test_g2),col=3,type='l')
lines(co2)
lines(predict(g1, newdata=test_g1),col=2)

lines(g1$fitted.values, type='l',col=2)
lines(g2$fitted.values, type='l',col=3)


################################
###### Gradient Boosting #######
################################
# Set train and test
set.seed(1)
train = sample (1:nrow(DEU), 0.7*nrow(DEU))
data.train=DEU[train ,]
data.test=DEU[-train ,]

# make some variables factor
##data.train[,c(3,7, 10:24)]= lapply(data.train[,c(3,7, 10:24)],factor)
##data.test[,c(3,7, 10:24)]= lapply(data.test[,c(3,7, 10:24)],factor)

str(data.train)

library (gbm)

?gbm

# 1 Boosting- 
boost.CO2 = gbm(CO2_emissions ~ Year, data=data.train, 
                distribution="gaussian", n.trees=500, interaction.depth=1, bag.fraction = 2)
boost.CO2
#
#for the plot
par(mfrow=c(1,1))
#
#plot of training error
plot(boost.CO2$train.error, type="l", ylab="training error")

#always decreasing with increasing number of trees
#
#
#relative influence plot
summary(boost.CO2) 
#let us modify the graphical parameters to obtain a better plot
#
#more space on the left
#
# default vector of parameters
mai.old<-par()$mai
mai.old
#new vector
mai.new<-mai.old
#new space on the left
mai.new[2] <- 2.5 
mai.new
#modify graphical parameters
par(mai=mai.new)
summary(boost.CO2, las=1) 
#las=1 horizontal names on y
summary(boost.CO2, las=1, cBar=10) 
#cBar defines how many variables
#back to orginal window
par(mai=mai.old)



# test set prediction for every tree (1:5000)


yhat.boost=predict(boost.CO2, newdata=data.test, n.trees=1:100)

# calculate the error for each iteration
#use 'apply' to perform a 'cycle for' 
# the first element is the matrix we want to use, 2 means 'by column', 
#and the third element indicates the function we want to calculate

err = apply(yhat.boost, 2, function(pred) mean((data.test$CO2_emissions - pred)^2))
#
plot(err, type="l")

# error comparison (train and test)
plot(boost.CO2$train.error, type="l")
lines(err, type="l", col=2)
#minimum error in test set
best=which.min(err)
abline(v=best, lty=2, col=4)
#
min(err) #minimum error


# 2 Boosting - Deeper trees
boost.CO2 = gbm(CO2_emissions ~ Year, data=data.train, 
                distribution="gaussian", n.trees=500, interaction.depth=4, bag.fraction = 2)

plot(boost.CO2$train.error, type="l")

#par(mai=mai.new)

summary(boost.movies, las=1, cBar=10)  

#par(mai=mai.old)

yhat.boost=predict(boost.movies ,newdata=data.test,n.trees=1:500)
err = apply(yhat.boost,2,function(pred) mean((data.test$CO2_emissions-pred)^2))
plot(err, type="l")


plot(boost.CO2$train.error, type="l")
lines(err, type="l", col=2)
best=which.min(err)
abline(v=best, lty=2, col=4)
min(err) #0.1174786


# 3 Boosting - Smaller learning rate 

boost.CO2 = gbm(CO2_emissions ~ Year, data=data.train, 
                distribution="gaussian", n.trees=500, interaction.depth=1, shrinkage=0.01, bag.fraction = 2)
plot(boost.CO2$train.error, type="l")

par(mai=mai.new)

summary(boost.CO2, las=1, cBar=10) 
par(mai=mai.old)

yhat.boost=predict(boost.CO2 ,newdata=data.test,n.trees=1:500)
err = apply(yhat.boost,2,function(pred) mean((data.test$CO2_emissions-pred)^2))
plot(err, type="l")


plot(boost.CO2$train.error, type="l")
lines(err, type="l", col=2)
best=which.min(err)
abline(v=best, lty=2, col=4)
min(err) #0.1174786


# 4 Boosting - combination of previous models
boost.CO2 = gbm(CO2_emissions ~ Year ,data=data.train, 
                distribution="gaussian",n.trees=500, interaction.depth=4, shrinkage=0.01, bag.fraction = 2)

plot(boost.CO2$train.error, type="l")
#

par(mai=mai.new)

summary(boost.CO2, las=1, cBar=10) 

par(mai=mai.old)


err = apply(yhat.boost, 2, function(pred) mean((data.test$CO2_emissions-pred)^2))
plot(err, type="l")


plot(boost.CO2$train.error, type="l")
lines(err, type="l", col=2)
best=which.min(err)
abline(v=best, lty=2, col=4)
err.boost= min(err)


##Comparison of models in terms of residual deviance
dev.gbm<- (sum((yhat.boost-data.test$CO2_emissions)^2))
dev.gbm  ##653.5258
dev.gam
dev.lm



boost.CO2
# partial dependence plots
plot(boost.CO2, i.var=1, n.trees = best)
plot(boost.CO2, i.var=2, n.trees = best)
plot(boost.CO2, i.var=5, n.trees = best)
plot(boost.CO2, i.var=c(1,5), n.trees = best) #bivariate (library(viridis) may be necessary)
#
plot(boost.CO2, i.var=3, n.trees = best) # categorical
plot(boost.CO2, i.var=6, n.trees = best)

plot(boost.CO2, i=23, n.trees = best)# categorical
plot(boost.CO2, i=17, n.trees = best) #no effect

