##################
#install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(forecast)
library(fpp)
library(aTSA)
library(seasonal)
library(timsac)
library(gridExtra)
library(readxl)
library(itsmr)
library(MuMIn)
library(dplyr)
library(MASS)
library(tseries)
library(nortest)
#install.packages('seastests')
library(seastests)
library(urca)
#install.packages("uroot")
library(uroot)
library(itsmr)
library(vars)
#####################################

setwd("C:/Min/graduate/timeseriesstatistics")
getwd()
data<-read.csv('C:/Min/graduate/timeseriesstatistics/UNRATE.csv')
y<-data$UNRATE

#플롯 그리기
dev.off()
par(mfrow=c(1,1))
plot.ts(y)
acf(y, lag.max=120)
pacf(y, lag.max=120)

#정상성 검정 (raw data 정상성 벗어남)
kpss.test(y)
adf.test(y)


#분산 안정화를 위한 로그변환(y1)
y1<-log(y)
dev.off()
par(mfrow=c(1,1))
plot.ts(y1)
acf(y1, lag.max=120)
pacf(y1, lag.max=120)

#단위근 검정 (유의확률 1%)
y1 %>% ur.kpss() %>% summary() #유의확률보다 커 차분 필요
adf.test(y1)


#보류
#ts(y,frequency=12,start(1,1))%>% log() %>% nsdiffs()
#ts(y,frequency=12,start(1,1))%>% log() %>% diff(lag=12) %>% ndiffs()


#aicc로 모델 선택
#auto_arima_beer <- auto.arima(y1, trace=TRUE, test="adf", ic="aicc")

#차분 1번(y3)
y2<-diff(y1)
dev.off()
par(mfrow=c(1,1))
plot.ts(y2)
acf(y2, lag.max=120)
pacf(y2, lag.max=120)


#정상성 검정 (kpss는 p-value가 크고, adf는 반대라 정상성 만족)
kpss.test(y2)
adf.test(y2)

#계절성 확인
#qs(y,frequency=12)


#계절성
#pacf : 천천히 decay - MA
#acf : 1에서 튐 - MA(1)
#주기 내
#pacf - ar(1) / arma(1,2) / arma(1,1)

auto.arima(y2) #ar(1) 나옴
lm1=arima(y1, order=c(1,1,0), seasonal=list(order=c(0,0,0), period=12))
lm2=arima(y1, order=c(1,1,1), seasonal=list(order=c(0,0,0), period=12))
lm3=arima(y1, order=c(1,1,2), seasonal=list(order=c(0,0,0), period=12))
#aic가 가장 낮은 lm1이 best이다.

bm<-lm1

dev.off()

#검정
test(bm$residuals)
checkresiduals(bm)

#예측
plot(forecast::forecast(bm, h=6))

###train/test 나눠서 예측 확인해보기?
#정규성 가정 만족 못함

#sarimax
# some random data
obs <- y1
ex <- data$cpi
length(obs)
length(ex)

#외생변수 넣고 모델링
model <- auto.arima(obs, xreg = ex)
summary(model)
#summary(lm11)
#lm11=arima(y1, order=c(1,1,0),xreg=ex)
test(model$residuals)


#######
#코로나 영향력 -> 더미변수를 외생변수로?
model <- auto.arima(obs, xreg = data$dummy)
summary(model)
lm11=arima(y1, order=c(1,1,0),xreg=data$dummy)
summary(lm11)
test(lm11$residuals)
######

# forecasting
plot(forecast::forecast(lm11, h=6))


############var모형######################
######
#이차원 데이터
#ts(b_data, start=c(1,1),frequency=12)

#플롯 여러개 그리기
dev.off()
par(mfrow=c(1,1))
plot(data$fuels, type = 'l', col = 'blue')
lines(data$cpi, type = 'l', col = 'red')
#상관관계 있음을 확인

#var모형시작
df<-data.frame(data$UNRATE, data$cpi)


#차분 실시
y1<-(diff(data$UNRATE))
y2<-(diff(data$cpi))
par(mfrow=c(1,1))
plot(y1, type = 'l', col = 'blue')
lines(y2, type = 'l', col = 'red') 
#해석 : 비슷하게 움직이는 패턴 같아 보임

#adf 정상성 검정 (각각 1차, 2차 차분 실시)
adf.test(y1)
adf.test(y2)


#crosscorrelation 살펴보기
par(mfrow=c(2,2))
acf(y1)
ccf(y1,y2)
ccf(y2,y1)
acf(y1)
#튀어나온 값 있음 -> crosscorrelation 존재하여 multivariate 모델링 필요함

df_d<-data.frame(y1,y2)

#시차 선택 (importmation criterior)
lag_selection <- VARselect(df_d, lag.max = 20, type = "const")
print(lag_selection$selection) #10으로 잡을까?

#bic인 7로 잡겠다
var_model <- VAR(df, p = 6, type = "const")
summary(var_model)


# 예측
var_forecast <- predict(var_model, n.ahead = 5)

# 예측 결과 출력
print(var_forecast)

# 예측 결과 시각화
plot(var_forecast)

#의문 : 다변량 모델이 뭐가 좋나?
#mspe 줄어듦 
summary(var_model)
summary(lm1)
 


#######################
setwd("C:/Min/graduate/timeseriesstatistics")
getwd()
data<-read.csv('C:/Min/graduate/timeseriesstatistics/UNRATE.csv')
y<-data$cpi

dev.off()
par(mfrow=c(1,1))
plot.ts(y)
acf(y, lag.max=120)
pacf(y, lag.max=120)

#정상성 검정 (raw data 정상성 벗어남)
kpss.test(y)


#분산 안정화를 위한 로그변환(y1)

#계절차분(y2) - 주기 12인 월별 데이터니까
#y2<-diff(y1,lag=12)
#dev.off()
#par(mfrow=c(1,1))
#plot.ts(y2)
#acf(y2, lag.max=120)
#pacf(y2, lag.max=120)

#단위근 검정 (유의확률 1%)
y1 %>% ur.kpss() %>% summary() #유의확률보다 커 차분 필요

#보류
#ts(y,frequency=12,start(1,1))%>% log() %>% nsdiffs()
#ts(y,frequency=12,start(1,1))%>% log() %>% diff(lag=12) %>% ndiffs()


#aicc로 모델 선택
#auto_arima_beer <- auto.arima(y1, trace=TRUE, test="adf", ic="aicc")

#차분 1번(y3)
y2<-diff(y)
dev.off()
par(mfrow=c(1,1))
plot.ts(y2)
acf(y2, lag.max=40)
pacf(y2, lag.max=40)


#정상성 검정 (계절차분+1차차분한 데이터는 정상성 만족)
kpss.test(y2)

y2<-diff(diff(y))
dev.off()
par(mfrow=c(1,1))
plot.ts(y2)
acf(y2, lag.max=40)
pacf(y2, lag.max=40)


#계절성 확인
#qs(y,frequency=12)


#계절성
#pacf : 천천히 decay - MA
#acf : 1에서 튐 - MA(1)
#주기 내
#pacf - ar(1) / arma(1,2) / arma(1,1)

auto.arima(y2) #arma(1,1) 나옴
lm1=arima(y1, order=c(1,1,0), seasonal=list(order=c(0,0,0), period=12))
lm2=arima(y1, order=c(1,1,1), seasonal=list(order=c(0,0,0), period=12))
lm3=arima(y1, order=c(1,1,2), seasonal=list(order=c(0,0,0), period=12))

#aic가 가장 낮은 lm1이 best이다.

bm<-lm1



#auto.arima
test(bm$residuals)
checkresiduals(am)


#검정 실시
checkresiduals(bm)


#잔차검정
test(lm1$residuals)

#예측
plot(forecast::forecast(lm1, h=6))

#정규성 가정 만족 못함

#sarimax
# some random data
obs <- y1
ex <- data$fuels

require(forecast)
# build the model (check ?auto.arima)
model <- auto.arima(obs, xreg = ex)
lm11=arima(y1, order=c(1,1,0),xreg=cbind(data$dummy,data$fuels))
test(lm11$residuals)


# forecasting
plot(forecast::forecast(lm11, h=6))

# quick way to visualize things
plot(forec)

# model diagnosis
tsdiag(model)

# model info
summary(forec)
