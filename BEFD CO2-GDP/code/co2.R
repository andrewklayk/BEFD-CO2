library(chron)
require(RColorBrewer)
library(purrr)
library(gbm)
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
library(lmtest)
library(DIMORA)


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
plot(arma22)
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


################################
###### Linear Regression #######
################################

get_pop <- function(ctry){
  dataset_pop <- c()
  dataset_pop[1:32] <- total.pop[
    total.pop$Year >= 1990 & total.pop$Year <= 2021 & total.pop$Entity == ctry,
  ]$Population...Sex..all...Age..all...Variant..estimates
  dataset_pop[33:41] <- total.pop[
    total.pop$Year >= 2022 & total.pop$Year <= 2030 & total.pop$Entity == ctry,
  ]$Population...Sex..all...Age..all...Variant..medium
  
  return(dataset_pop)
}

# GET CO2

spa_co2 <- total.co2[total.co2$Entity == 'Spain' & total.co2$Year >= 1990,]$Annual.CO..emissions
ger_co2 <- total.co2[total.co2$Entity == 'Germany' & total.co2$Year >= 1990,]$Annual.CO..emissions
pol_co2 <- total.co2[total.co2$Entity == 'Poland' & total.co2$Year >= 1990,]$Annual.CO..emissions
fra_co2 <- total.co2[total.co2$Entity == 'France' & total.co2$Year >= 1990,]$Annual.CO..emissions
ita_co2 <- total.co2[total.co2$Entity == 'Italy' & total.co2$Year >= 1990,]$Annual.CO..emissions

eu_co2 <- spa_co2 + ger_co2 + fra_co2 + ita_co2
chn_co2 <- total.co2[
  total.co2$Entity == 'China' & total.co2$Year >= 1990,]$Annual.CO..emissions

# GET GDP

spa_gdp <- total.gdp[total.gdp$LOCATION == 'ESP' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value
ger_gdp <- total.gdp[total.gdp$LOCATION == 'DEU' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value
pol_gdp <- total.gdp[total.gdp$LOCATION == 'POL' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value
fra_gdp <- total.gdp[total.gdp$LOCATION == 'FRA' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value
ita_gdp <- total.gdp[total.gdp$LOCATION == 'ITA' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value

eu_gdp <- ger_gdp + ita_gdp + fra_gdp + spa_gdp
chn_gdp <- total.gdp[
  total.gdp$LOCATION == 'CHN' & total.gdp$TIME <= 2030,]$Value

# GET POPULATION

spa_pop <- get_pop('Spain')
ger_pop <- get_pop('Germany')
pol_pop <- get_pop('Poland')
fra_pop <- get_pop('France')
ita_pop <- get_pop('Italy')

eu_pop <- spa_pop + ger_pop + fra_pop + ita_pop
chn_pop <- get_pop('China')

normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

co2 <- ts(normalize(chn_co2))
gdp <- ts(normalize(chn_gdp))
pop <- ts(normalize(chn_pop))
#co2_train <- ts(co2[1:25])
#co2_test <- ts(co2[25:32])
gdp_train <- ts(gdp[1:32])
gdp_test <- ts(gdp[32:40])
pop_train <- ts(pop[1:32])
pop_test <- ts(pop[32:40])

l1 <- tslm(co2~trend)
# plot the fitted model
plot(co2, ylim=c(min(co2, l1$fitted.values), max(co2, l1$fitted.values)))
lines(l1$fitted.values,col=2)
# plot forecast
plot(forecast(l1,h=7))
# summary
summary(l1)
AIC(l1)
dwtest(l1)
# residuals
resl1 <- residuals(l1)
tsdisplay(resl1,lag.max=25)
# rmse

l2 <- tslm(co2~trend+gdp_train)
# plot the fitted model
plot(l2$fitted.values,col=2)
lines(co2)
# plot the forecast
test_l2 <- list(gdp_train=gdp[32:40])
plot(forecast(l2,newdata=as.data.frame(test_l2)))
# summary
summary(l2)
AIC(l2)
dwtest(l2)
# residuals
resl2 <- residuals(l2)
tsdisplay(resl2, lag.max=25)
# rmse

l3 <- tslm(co2~trend+gdp_train+pop_train)
# plot the fitted model
plot(l3$fitted.values,col=2)
lines(co2)
# plot the forecast
test_l3 <- list(gdp_train=gdp_test, pop_train=pop_test)
plot(forecast(l3,newdata=as.data.frame(test_l3)))
# summary
summary(l3)
AIC(l3)
dwtest(l3) # BEST
# residuals
resl3 <- residuals(l3)
tsdisplay(resl3, lag.max=25)
# rmse


l4 <- tslm(co2~gdp_train+pop_train)
# plot the fitted model
plot(l4$fitted.values,col=2)
lines(co2)
# plot the forecast
test_l4 <- list(gdp_train=gdp_test, pop_train=pop_test)
plot(forecast(l4,newdata=as.data.frame(test_l4)))
# summary
summary(l4)
AIC(l4)
dwtest(l4) # BEST
# residuals
resl4 <- residuals(l4)
tsdisplay(resl4, lag.max=25)

## BASS MODELS ##

###we estimate a simple Bass Model 
tsdisplay(co2, lag.max=10)
bm_cass<-BM(co2,display = T)
summary(bm_cass)

###prediction (out-of-sample)
pred_bmcas<- predict(bm_cass, newx=c(1:41))
pred.instcas<- make.instantaneous(pred_bmcas)

###plot of fitted model 
plot(co2, type= "b",xlab="Year", ylab="Emissions, tons",  pch=16, lty=3, 
     xaxt="n", cex=0.6, xlim=c(0, 41))
lines(pred.instcas, lwd=2, col=2)

###we estimate the model with 50% of the data
bm_cass50<-BM(chn_co2[1:18],display = T)
summary(bm_cass50)

pred_bmcas50<- predict(bm_cass50, newx=c(1:50))
pred.instcas50<- make.instantaneous(pred_bmcas50)

plot(chn_co2, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred.instcas50, lwd=2, col=2)

###we estimate the model with 25% of the data
bm_cass75<-BM(chn_co2[1:9],display = T)
summary(bm_cass75)

pred_bmcas75<- predict(bm_cass75, newx=c(1:50))
pred.instcas75<- make.instantaneous(pred_bmcas75)

###Comparison between models (instantaneous)
###instantaneous
plot(chn_co2, type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred.instcas75, lwd=2, col=2)
lines(pred.instcas50, lwd=2, col=3)
lines(pred.instcas, lwd=2, col=4)

###Comparison between models (cumulative)
plot(cumsum(chn_co2), type= "b",xlab="Year", ylab="Annual sales",  pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred_bmcas75, lwd=2, col=2)
lines(pred_bmcas50, lwd=2, col=3)
lines(pred_bmcas, lwd=2, col=4)


###GBMr1
GBMr1tw<- GBM(chn_co2,shock = "rett",nshock = 2,prelimestimates = c(3.625255e+02, 4.028274e-03, 8.484603e-02 , 24,38,-0.1))

###GBMe1
GBMe1tw<- GBM(chn_co2,shock = "exp",nshock = 2,prelimestimates = c(3.625255e+02, 4.028274e-03, 8.484603e-02, 12,-0.1,0.1))
summary(GBMe1tw)

pred_GBMe1tw<- predict(GBMe1tw, newx=c(1:60))
pred_GBMe1tw.inst<- make.instantaneous(pred_GBMe1tw)

plot(chn_co2, type= "b",xlab="Year", ylab="CO2 emissions",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GBMe1tw.inst, lwd=2, col=2)

######GGM 
GGM_tw<- GGM(chn_co2, prelimestimates=c(3.625255e+02, 0.001, 0.01, 4.028274e-03, 8.484603e-02))
summary(GGM_tw)

pred_GGM_tw<- predict(GGM_tw, newx=c(1:60))
pred_GGM_tw.inst<- make.instantaneous(pred_GGM_tw)

plot(chn_co2, type= "b",xlab="Quarter", ylab="Quarterly revenues",  pch=16, lty=3, cex=0.6, xlim=c(1,60))
lines(pred_GGM_tw.inst, lwd=2, col=2)
#lines(pred.insttw, lwd=2, col=3)

###Analysis of residuals
res_GGMtw<- residuals(GGM_tw)
acf<- acf(residuals(GGM_tw))

fit_GGMtw<- fitted(GGM_tw)
fit_GGMtw_inst<- make.instantaneous(fit_GGMtw)



#################################
## Generalized Additive Models ##
#################################
library(gam)

tt <- (1:length(co2))
g1 <- gam(co2~lo(tt))
# plot forecast
test_g1 <- list(tt=1:41)
plot(predict(g1, newdata=test_g1),col='blue',type='l')
lines(co2)
# residuals
plot(g1,se=T)
tsdisplay(residuals(g1),lag.max=25)
# summary
summary(g1)
# rmse

g2 <- gam(co2~lo(tt)+lo(gdp_train),
          control=gam.control(maxit=200,bf.maxit=200))
# plot forecast
test_g2 <- list(tt=1:41, gdp_train=gdp)
plot(predict(g2, newdata=test_g2),col='blue',type='l')
lines(co2)
# residuals
par(mfrow=c(2,1))
plot(g2,se=T)
tsdisplay(residuals(g2),lag.max=25)
# summary
summary(g2)

g3 <- gam(co2~lo(tt)+lo(gdp_train)+lo(pop_train),
          control=gam.control(maxit=500,bf.maxit=500))
# plot forecast
test_g3 <- list(tt=1:41, gdp_train=gdp, pop_train=pop)
plot(predict(g3, newdata=test_g3),col='blue',type='l')
lines(co2)
# residuals
par(mfrow=c(3,1))
plot(g3,se=T)
tsdisplay(residuals(g3),lag.max=25)
# summary
summary(g3)


g4 <- gam(co2~lo(gdp_train)+lo(pop_train),
          control=gam.control(maxit=500,bf.maxit=500))
# plot forecast
test_g4 <- list(gdp_train=gdp, pop_train=pop)
plot(predict(g4, newdata=test_g4),col='blue',type='l')
lines(co2)
# residuals
par(mfrow=c(3,1))
plot(g4,se=T)
tsdisplay(residuals(g4),lag.max=25)
# summary
summary(g4)




################################
###### Gradient Boosting #######
################################
# Set train and test
set.seed(1)

dataset_co2 <- total.co2[
  total.co2$Year >= 1990 & total.co2$Entity=='China',
]$Annual.CO..emissions
train = sample (1:length(dataset_co2), 0.7*length(dataset_co2))
deu_co2_train=dataset_co2[train]
deu_co2_test=dataset_co2[-train]

dataset_gdp <- CHN_gdp
gdp_train=dataset_gdp[train ,]
gdp_test=dataset_gdp[-train ,]

dataset_pop <- total.pop[
  total.pop$Year >= 1990 & total.pop$Entity=='China',
  ]$Population..historical.estimates.
pop_train=dataset_pop[train]
pop_test=dataset_pop[-train]

years <- 1990:2021
years_train=years[train]
years_test = years[-train]

data_train <- list(
  Year=years_train, CO2=deu_co2_train, Pop=pop_train, GDP=gdp_train$gdp
)
data_test <- list(
  Year=years_test, CO2=deu_co2_test, Pop=pop_test, GDP=gdp_test$gdp
)

# 1 Boosting
tt <- 1:length(deu_co2_train)
boost.CO2 = gbm(CO2 ~ Year,
                data=data_train,
                distribution="gaussian", n.trees=500,
                interaction.depth=1, bag.fraction = 2)

# plot training error
plot(boost.CO2$train.error, type="l", ylab="training error")
# predict and get test error
yhat.boost=predict(boost.CO2, newdata=data_test, n.trees=1:500)
err = apply(yhat.boost, 2, 
            function(pred) mean((data_test$CO2 - pred)^2))

# error comparison (train and test)
plot(boost.CO2$train.error, type="l",
     ylim=c(min(c(boost.CO2$train.error, err)),max(c(boost.CO2$train.error, err))))
lines(err, type='l', col=2)
best=which.min(err)
abline(h=min(err),lty=2, col=4)
abline(v=best, lty=2, col=4)
min(err) #0.09 / 0.21

# 2 Boosting - Deeper trees
boost.CO2 = gbm(CO2 ~ Year + Pop + GDP,
                data=data_train,
                distribution="gaussian", n.trees=500,
                interaction.depth=4, bag.fraction = 2)

# predict and get test error
yhat.boost=predict(boost.CO2 ,newdata=data_test,n.trees=1:500)
err = apply(yhat.boost,2,function(pred) mean((deu_co2_test$CO2_emissions-pred)^2))
# error comparison
plot(boost.CO2$train.error, type="l",
     ylim=c(min(c(boost.CO2$train.error, err)),max(c(boost.CO2$train.error, err))))
lines(err, type="l", col=2)
best=which.min(err)
abline(v=best, lty=2, col=4)
abline(h=min(err), lty=2, col=4)
min(err) #0.08 / 0.2


# 3 Boosting - Smaller learning rate 
boost.CO2 = gbm(CO2_emissions ~ Year, data=deu_co2_train, 
                distribution="gaussian", n.trees=500,
                interaction.depth=1, shrinkage=0.01, bag.fraction = 2)
# predict and get test error
yhat.boost=predict(boost.CO2 ,newdata=deu_co2_test,n.trees=1:500)
err = apply(yhat.boost,2, function(pred) mean((deu_co2_test$CO2_emissions-pred)^2))

# error comparison
plot(boost.CO2$train.error, type="l",ylim=c(0:1))
lines(err, type="l", col=2)
best=which.min(err)
abline(v=best, lty=2, col=4)
abline(h=min(err), lty=2, col=4)
min(err) # 0.09/ 0.22

# 4 Boosting - combination of previous models
boost.CO2 = gbm(CO2_emissions ~ Year ,data=deu_co2_train, 
                distribution="gaussian",n.trees=500, interaction.depth=4, shrinkage=0.01, bag.fraction = 2)

# predict and get test error
yhat.boost=predict(boost.CO2 ,newdata=deu_co2_test,n.trees=1:500)
err = apply(yhat.boost,2, function(pred) mean((deu_co2_test$CO2_emissions-pred)^2))

# error comparison
plot(boost.CO2$train.error, type="l",ylim=c(0:1))
lines(err, type="l", col=2)
best=which.min(err)
abline(v=best, lty=2, col=4)
abline(h=min(err), lty=2, col=4)
min(err) # 0.08 / 0.2

# plot boost forecast

future.preds <- predict(boost.CO2, newdata=list(Year=2021:2030), n.trees=1:500)

end.year <- 2030
plot(x=years_train, y=deu_co2_train,
     xlim=c(1990, end.year))
for(i in 1:length(years)){
  yr <- years[i]
  if(yr %in% years_test) {c='red'}
  else {c='black'}
  points(x=yr, y=dataset_co2[i],col=c)
}
points(x=years_test, y=rowMeans(yhat.boost),col='blue')
points(x=(Year=2021:2030), y=rowMeans(future.preds))
##Comparison of models in terms of residual deviance
dev.gbm<- (sum((yhat.boost-deu_co2_test$CO2_emissions)^2))
dev.gbm  ##910.3061