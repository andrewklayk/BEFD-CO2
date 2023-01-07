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
