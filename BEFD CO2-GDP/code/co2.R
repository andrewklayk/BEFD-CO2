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

total.co2 <- read.csv('../datasets/co2_by_country.csv')
total.pop <- read.csv('../datasets/un_population.csv')
total.gdp <- read.csv('../datasets/gdp.csv')

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

mae <- function(y, yhat){
  return(mean(abs(y - yhat)))
}
normalize <- function(x, min, max){
  return((x - min)/(max-min))
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
usa_co2 <- total.co2[
  total.co2$Entity == 'United States' & total.co2$Year >= 1990,]$Annual.CO..emissions

# GET GDP

spa_gdp <- total.gdp[total.gdp$LOCATION == 'ESP' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value
ger_gdp <- total.gdp[total.gdp$LOCATION == 'DEU' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value
pol_gdp <- total.gdp[total.gdp$LOCATION == 'POL' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value
fra_gdp <- total.gdp[total.gdp$LOCATION == 'FRA' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value
ita_gdp <- total.gdp[total.gdp$LOCATION == 'ITA' & total.gdp$TIME >= 1990 & total.gdp$TIME<=2030,]$Value

eu_gdp <- ger_gdp + ita_gdp + fra_gdp + spa_gdp
chn_gdp <- total.gdp[
  total.gdp$LOCATION == 'CHN' & total.gdp$TIME <= 2030,]$Value
usa_gdp <- total.gdp[
  total.gdp$LOCATION == 'USA' & total.gdp$TIME <= 2030,]$Value

# GET POPULATION

spa_pop <- get_pop('Spain')
ger_pop <- get_pop('Germany')
pol_pop <- get_pop('Poland')
fra_pop <- get_pop('France')
ita_pop <- get_pop('Italy')

eu_pop <- spa_pop + ger_pop + fra_pop + ita_pop
chn_pop <- get_pop('China')
usa_pop <- get_pop("United States")

label <- 'USA CO2 EMISSIONS, Tons'

co2 <- ts(usa_co2)
gdp <- ts(usa_gdp)
pop <- ts(usa_pop)
r.train <- 1:25
r.test <- 26:32
r.forecast <- 33:41

co2_train <- ts(co2[r.train])
co2_test <- ts(co2[r.test])

gdp_train <- ts(gdp[r.train])
gdp_test <- ts(gdp[r.test])
gdp_fut <- ts(gdp[r.forecast])
gdp_real <- c(gdp_train, gdp_test)


pop_train <- ts(pop[r.train])
pop_test <- ts(pop[r.test])
pop_fut <- ts(pop[r.forecast])
pop_real <- c(pop_train, pop_test)

train <- data.frame(y=co2_train, g=gdp_train, p=pop_train, tt=r.train)
test <- data.frame(y=co2_test, g=gdp_test, p=pop_test, tt=r.test)

l <- tslm(y~trend,data=train)
# plot the fitted model on train + test
f<-forecast(l,h=length(co2_test))
plot(f, type='p',
     ylim=c(min(f$lower, co2_train, co2_test), max(f$upper, co2_train, co2_test)))
points(x=r.test, y=co2_test, type='p')
lines(x=r.train, y=l$fitted.values,col='blue')
abline(v=head(r.test,1),lty=3,col='red')
# summary
summary(l)
AIC(l)
dwtest(l)
# residuals
tsdisplay(residuals(l),lag.max=25)
# mae
mae(c(f$mean),c(co2_test))

l <- tslm(y~trend+g,data=train)
# plot the fitted model on train + test
f<-forecast(l,newdata=test)
plot(f, type='p',
     ylim=c(min(f$lower, co2_train, co2_test), max(f$upper, co2_train, co2_test)))
points(x=r.test, y=co2_test, type='p')
lines(x=r.train, y=l$fitted.values,col='blue')
abline(v=head(r.test,1),lty=3,col='red')
# summary
summary(l)
AIC(l)
dwtest(l)
# residuals\
tsdisplay(residuals(l),lag.max=25)
# mae
mae(c(f$mean),c(co2_test))

l <- tslm(y~trend+g+p,data=train)
# plot the fitted model on train + test
f<-forecast(l,newdata=test)
plot(f, type='p',
     ylim=c(min(f$lower, co2_train, co2_test), max(f$upper, co2_train, co2_test)))
points(x=r.test, y=co2_test, type='p')
lines(x=r.train, y=l$fitted.values,col='blue')
abline(v=head(r.test,1),lty=3,col='red')
# summary
summary(l)
AIC(l)
dwtest(l)
# residuals
tsdisplay(residuals(l),lag.max=25)
# mae
mae(c(f$mean),c(co2_test))

l <- tslm(y~g+p,data=train)
# plot the fitted model on train + test
f<-forecast(l,newdata=test)
plot(f, type='p',
     ylim=c(min(f$lower, co2_train, co2_test), max(f$upper, co2_train, co2_test)))
points(x=r.test, y=co2_test, type='p')
lines(x=r.train, y=l$fitted.values,col='blue')
abline(v=head(r.test,1),lty=3,col='red')
# summary
summary(l)
AIC(l)
dwtest(l)
# residuals\
tsdisplay(residuals(l),lag.max=25)
# mae
mae(c(f$mean),c(co2_test))

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

g <- gam(y~lo(tt), data=train,
         control=gam.control(maxit=200,bf.maxit=200))
# plot model on train + test
preds <- predict(g, newdata=test)
plot(g$fitted.values,col='blue',type='l', xlim=c(1,32),
     ylim=c(min(co2, g$fitted.values, preds), max(co2, g$fitted.values, preds)))
lines(x=r.test,preds,col='blue')
lines(co2_train,type='b')
lines(x=r.test,co2_test,type='b')
abline(v=head(r.test,1),col='red',lty=3)
# residuals
par(mfrow=c(1,1))
plot(g,se=T,residuals=T)
tsdisplay(residuals(g),lag.max=25)
# summary
summary(g)
# rmse
mae(co2_test,preds)

g <- gam(y~lo(tt)+lo(g), data=train,
         control=gam.control(maxit=200,bf.maxit=200))
# plot model on train + test
preds <- predict(g, newdata=test)
plot(g$fitted.values,col='blue',type='l', xlim=c(1,32),
     ylim=c(min(co2, g$fitted.values, preds), max(co2, g$fitted.values, preds)))
lines(x=r.test,preds,col='blue')
lines(co2_train,type='b')
lines(x=r.test,co2_test,type='b')
abline(v=head(r.test,1),col='red',lty=3)
# residuals
par(mfrow=c(2,1))
plot(g,se=T,residuals=T)
tsdisplay(residuals(g),lag.max=25)
# summary
summary(g)
# rmse
mae(co2_test,preds)

g <- gam(y~lo(tt)+lo(g)+lo(p), data=train,
         control=gam.control(maxit=200,bf.maxit=200))
# plot model on train + test
preds <- predict(g, newdata=test)
plot(g$fitted.values,col='blue',type='l', xlim=c(1,32),
     ylim=c(min(co2, g$fitted.values, preds), max(co2, g$fitted.values, preds)))
lines(x=r.test,preds,col='blue')
lines(co2_train,type='b')
lines(x=r.test,co2_test,type='b')
abline(v=head(r.test,1),col='red',lty=3)
# residuals
par(mfrow=c(3,1))
plot(g,se=T,residuals=T)
tsdisplay(residuals(g),lag.max=25)
# summary
summary(g)
# rmse
mae(co2_test,preds)


g <- gam(y~lo(g)+lo(p), data=train,
         control=gam.control(maxit=500,bf.maxit=500))
# plot model on train + test
preds <- predict(g, newdata=test)
plot(g$fitted.values,col='blue',type='l', xlim=c(1,32),
     ylim=c(min(co2, g$fitted.values, preds), max(co2, g$fitted.values, preds)))
lines(x=r.test,preds,col='blue')
lines(co2_train,type='b')
lines(x=r.test,co2_test,type='b')
abline(v=head(r.test,1),col='red',lty=3)
# residuals
par(mfrow=c(2,1))
plot(g,se=T,residuals=T)
tsdisplay(residuals(g),lag.max=25)
# summary
summary(g)
# rmse
mae(co2_test,preds)




################################
###### Gradient Boosting #######
################################
# Set train and test

train.mask = sample(1:length(co2), 0.7*length(co2))
co2_train = co2[train.mask]
co2_test = co2[-train.mask]
gdp_train=gdp_real[train.mask]
gdp_test=gdp_real[-train.mask]
pop_train=pop_real[train.mask]
pop_test=pop_real[-train.mask]
years <- 1990:2021
years_train = years[train.mask]
years_test = years[-train.mask]

data_train <- data.frame(
  YR=years_train, CO2=co2_train, POP=pop_train, GDP=gdp_train
)
data_test <- data.frame(
  YR=years_test, CO2=co2_test, POP=pop_test, GDP=gdp_test
)

# 2 Boosting - Deeper trees
boost.CO2 = gbm(CO2 ~ YR + GDP+POP,
                data=rbind(data_train, data_test),
                distribution="gaussian", n.trees=500,
                interaction.depth=1, shrinkage=0.1225, bag.fraction=2,
                train.fraction=1,cv.folds=32)
# error comparison
plot(boost.CO2$train.error, type="l",
     ylim=c(min(c(boost.CO2$train.error, boost.CO2$cv.error)),
            max(c(boost.CO2$train.error, boost.CO2$cv.error))))
lines(boost.CO2$cv.error, type='l',col='red')
min(boost.CO2$cv.error)
#lines(err, type="l", col=2)
best=which.min(boost.CO2$cv.error)
abline(v=best, lty=2, col=4)
abline(h=min(boost.CO2$cv.error), lty=2, col=4)

p <- plot(boost.CO2, n.trees=best,return.grid = TRUE)
plot(x=data_train$YR, y=data_train$CO2,xlim=c(1990,2030))
points(x=data_test$YR, y=data_test$CO2)
lines(p,type='l',col='blue')
print(boost.CO2)
barplot(relative.influence(boost.CO2, n.trees = best))

# plot boost forecast

data_future <- data.frame(
  YR=2022:2030, POP=pop_fut, GDP=gdp_fut
)

future.preds <- predict(boost.CO2, newdata=data_future, n.trees=best)
lines(x=data_future$YR, future.preds)

end.year <- 2030
plot(x=data_train, y=deu_co2_train,
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