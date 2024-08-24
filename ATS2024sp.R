setwd("C:/ts")
install.packages("vars")

library(UsingR)
library(ggplot2)
library(itsmr)
library(glmnet)
library(MASS)
library(TSlibrary)
library(forecast)
library(tseries)
library(readr)
library(zoo)
library(astsa)
library(vars)

train = read.csv("trainset.csv")
test = read.csv("testset.csv")

tr=train[9:745,]
par(mfrow=c(1,1))
data = train$unrate


plot.ts(data) # non-stationary & heteroschadascity
acf2(data) ## acf는 점차 감소, pacf 시차 1이후 매우 작음

# 로그 변환
data_log = log(data)
plot.ts(data_log)
acf2(data_log)
test(data_log)

# 1차 차분해주자. 
diff1 = diff(data_log,1)
plot.ts(diff1) # 이분산성 아직 있는듯함.
acf2(diff1)

test(diff1) # 정상성 테스트

diff365 = diff(diff1,365)
test(diff365)
acf2(diff365)
diff365 = diff(diff12,365)
test(diff365)

# AR(1) 모델링
ar1 = arima(diff1, order = c(1, 0, 0))
summary(ar1)
forecasts1 = forecast::forecast(ar1, h = length(test$unrate))
plot(forecasts1)
lines(test$unrate, col = "red")
test(ar1$residuals)

# AR(2) 모델링
ar2 = arima(diff1, order = c(2, 0, 0))
summary(ar2)
forecasts2 = forecast::forecast(ar2, h = length(test$unrate))
plot(forecasts2)
lines(test$unrate, col = "red")
test(ar2$residuals)

# ARMA 모델
arma11 = arima(diff1, order = c(1, 0, 1))
arma_forecast = forecast::forecast(arma11, h = length(test$unrate))
summary(arma11)
plot(arma_forecast)
test(arma11$residuals)

# SARIMA 모델(로그 변환 취한거로 적합)
sarima_model = auto.arima(data_log, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
sarima_forecast = forecast::forecast(sarima_model, h = length(test$unrate))
summary(sarima_model)


plot(sarima_forecast)
test(sarima_model$residuals)

# auto.arima 함수 활용
autoarima = auto.arima(diff1)
autoarima_forecast = forecast :: forecast(autoarima, h = length(test$unrate))
summary(autoarima)
plot(autoarima_forecast)
test(autoarima$residuals)


# arimax model 
# 데이터 준비
ue = train$ue
ue_test = test$ue
plot.ts(ue)

# scale차이가 크므로 로그 변환
ue_log = log(ue)
plot.ts(ue_log)
ue_diff = diff(ue_log, 1)
ue_test_log = log(ue_test)
ue_test_diff = diff(ue_test_log, 1)

# 모델링 및 예측
orders = list(c(1,0,0), c(2,0,0), c(1,0,1), c(1,0,2), c(1,1,4))

par(mfrow=c(1,1))
arimax100 = Arima(diff1, order = c(1,0,0), xreg = ue_diff)
arimax100_forecast = forecast(arimax100, xreg = ue_test_diff, h = length(ue_test))
summary(arimax100)
test(arimax100$residuals)

arimax200 = Arima(diff1, order = c(2,0,0), xreg = ue_diff)
arimax200_forecast = forecast(arimax200, xreg = ue_test_diff, h = length(ue_test))
summary(arimax200)
plot(arimax200_forecast)
test(arimax200$residuals)


arimax101 = Arima(diff1, order = c(1,0,1), xreg = ue_diff)
arimax101_forecast = forecast(arimax101, xreg = ue_test_diff, h = length(ue_test))
summary(arimax101)
plot(arimax101_forecast)
plot.ts(diff1)
test(arimax101$residuals)

arimax102 = Arima(diff1, order = c(1,0,2), xreg = ue_diff)
arimax102_forecast = forecast(arimax102, xreg = ue_test_diff, h = length(ue_test))
summary(arimax102)
plot(arimax102_forecast)
test(arimax102$residuals)


arimax114 = Arima(diff1, order = c(1,1,4), xreg = ue_diff)
arimax114_forecast = forecast(arimax114,xreg = ue_test_diff, h = length(ue_test))
summary(arimax114)
plot(arimax114_forecast)
test(arimax114$residuals)

sarima111101=arima(x = data_log, order = c(1,1,1), seasonal=list(order=c(0,0,1)))

sarima111001 = arima(data_log, order = c(1,1,1), seasonal = list(order = c(0,0,1), period=12))
sarima111001_forecast = forecast::forecast(sarima111001, h = length(test$unrate))
summary(sarima111001)
plot(sarima111001_forecast)
test(sarima111001$residuals)
sarima111001

sarimax111001 = Arima(data_log, order = c(1,1,1), seasonal = list(order = c(0,0,1), period=12), xreg = ue_log)
sarimax111001_forecast = forecast :: forecast(sarimax111001, xreg=ue_test_log , h = length(test$unrate))
summary(sarimax111001)
lot(sarimax111001_forecast)
test(sarimax111001$residuals)
# sarimax111001

# 성능 지표 계산
# Mean Squared Prediction Error (MSPE)
mspe = function(forecast, actual) {
  mean((forecast$mean - actual)^2)
}

ar1_mspe = mspe(forecasts1, log(test$unrate))
ar2_mspe = mspe(forecasts2, log(test$unrate))
arma11_mspe = mspe(arma_forecast, log(test$unrate))
sarima_mspe = mspe(sarima_forecast, log(test$unrate))
autoarima_mspe = mspe(autoarima_forecast, log(test$unrate))
arimax100_mspe = mspe(arimax100_forecast, log(test$unrate[-1]))
arimax200_mspe = mspe(arimax200_forecast, log(test$unrate[-1]))
arimax101_mspe = mspe(arimax101_forecast, log(test$unrate[-1]))
arimax102_mspe = mspe(arimax102_forecast, log(test$unrate[-1]))
arimax114_mspe = mspe(arimax114_forecast, log(test$unrate[-1]))
sarima111001_mspe = mspe(sarima111001_forecast, log(test$unrate))
sarimax111001_mspe = mspe(sarimax111001_forecast, log(test$unrate))

# 성능 지표 테이블
model_comparison <- data.frame(
  Model = c("AR1", "AR2", "ARMA11", "SARIMA", "AUTO", "ARIMAX100", "ARIMAX200", "ARIMAX101", "ARIMAX102", "ARIMAX114", "SARIMA111001", "SARIMAX111001"),
  AIC = c(ar1$aic,ar2$aic,arma11$aic, sarima_model$aic, autoarima$aic, arimax100$aic, arimax200$aic, arimax101$aic, arimax102$aic, arimax114$aic, sarima111001$aic, sarimax111001$aic),
  MSPE = c(ar1_mspe, ar2_mspe, arma11_mspe, sarima_mspe, autoarima_mspe, arimax100_mspe, arimax200_mspe, arimax101_mspe, arimax102_mspe, arimax114_mspe, sarima111001_mspe, sarimax111001_mspe)
)

# 예측 결과 시각화
plot(train$unrate, type = "l", col = "red", ylim = range(test$unrate, exp(forecasts1$mean),exp(forecasts2$mean), exp(arma_forecast$mean), exp(sarima_forecast$mean), exp(autoarima_forecast$mean), exp(arimax100_forecast$mean), exp(arimax200_forecast$mean), exp(arimax101_forecast$mean), exp(arimax102_forecast$mean), exp(arimax114_forecast$mean)), ylab = "Unemployment Rate", xlab = "Time", main = "Model Predictions vs Actual")
lines(exp(forecasts1$fitted), col = "yellow")
lines(exp(forecasts2$fitted), col = "green")     
lines(exp(arma_forecast$fitted), col = "blue")
lines(exp(sarima_forecast$fitted), col = "purple")
lines(exp(autoarima_forecast$fitted), col = "skyblue")
lines(exp(arimax100_forecast$fitted), col = "pink")
lines(exp(arimax200_forecast$fitted), col = "brown")
lines(exp(arimax101_forecast$fitted), col = "black")
lines(exp(arimax102_forecast$fitted), col = "gray")
lines(exp(arimax114_forecast$fitted), col = "gold")
legend("topright", legend = c("Actual", "AR1", "AR2" , "ARMA11", "SARIMA", "AUTO", "ARIMAX100", "ARIMAX200", "ARIMAX101", "ARIMAX102", "ARIMAX114"), col = c("red", "yellow", "green" , "blue", "purple", "skyblue", "pink", "brown", "black", "gray", "gold"), lty = 1)

# 결과 출력
print(model_comparison)

## VAR
library(vars)
library(tseries)
library(forecast)

# 데이터 불러오기
trainset <- read.csv("trainset.csv")
testset = read.csv("testset.csv")
trainset$DATE <- as.Date(trainset$DATE, format="%Y-%m-%d")
testset$DATE<- as.Date(testset$DATE, format="%Y-%m-%d")
trainset <- trainset[order(trainset$DATE), ]
testset <- testset[order(testset$DATE),]

# 필요한 변수 선택
ts_data <- trainset[, c("DATE", "unrate", "ue")]
test_d = testset[,c("DATE", "unrate", "ue")]
ts_unrate <- ts(ts_data$unrate, start = c(1955, 5), frequency = 12)
ts_ue <- ts(ts_data$ue, start = c(1955, 5), frequency = 12)
test_unrate = ts(test_d$unrate, start = c(2017, 6), frequency = 12)
test_ue = ts(test_d$ue, start = c(2017, 6), frequency = 12)
# test set 변수 로그 -> 차분
a = log(test_unrate)
b = log(test_ue)
c = diff(a,1)
d = diff(b,1)

# 시계열 데이터 시각화
par(mfrow=c(2,1))
plot(ts_unrate, main="Unemployment Rate", ylab="Unrate", xlab="Time",col = "blue")
plot(ts_ue, main="UE", ylab="UE", xlab="Time",col ="red")


# ACF 확인
acf(ts_unrate, main="ACF of Unrate")
acf(ts_ue, main="ACF of UE")

# 모델링하여 잔차 구하기
ln_unrate = log(ts_unrate)
ln_ue = log(ts_ue)
diff1 = diff(ln_unrate,1)
diff2 = diff(ln_ue,1)
fit_unrate = auto.arima(diff1,d=0,seasonal=FALSE)
fit_ue <- auto.arima(diff2)

# 잔차 계산
residuals_ue <- residuals(fit_ue)
residuals_unrate = residuals(fit_unrate)

par(mfrow=c(1,1))

# 잔차의 ACF 및 CCF 확인
par(mfrow=c(2,1))
acf(residuals_unrate, main="ACF of Residuals (Unrate)",lag.max = 60)
pacf(residuals_unrate, main="PACF of Residuals (Unrate)",lag.max = 60)
acf(residuals_ue, main="ACF of Residuals (UE)",lag.max = 60)
pacf(residuals_ue, main="PACF of Residuals (UE)",lag.max = 60)
par(mfrow=c(1,1))
ccf(residuals_unrate, residuals_ue, main="CCF of Residuals")

## var(2) 나 var(3) 의 근거

# 적절한 시차 선택
VARselect(cbind(diff1, diff2), lag.max = 20, type = "const")

# 선택한 시차로 VAR 모델 구축
var_model2 <- VAR(cbind(diff1, diff2), p = 2)
var_model3 = VAR(cbind(diff1, diff2), p = 3)
var_model6 = VAR(cbind(diff1, diff2), p = 6)
var_model5 = VAR(cbind(diff1, diff2), p = 5)
var_model4 = VAR(cbind(diff1, diff2), p = 4)

# 모델 요약
Bcoef(var_model5)
summary(var_model2)
summary(var_model3)
summary(var_model6)
summary(var_model5)
summary(var_model4)


# 잔차의 ACF 및 CCF 확인
acf(residuals(var_model2), main="ACF of VAR Model Residuals")
ccf(residuals(var_model2)[,1], residuals(var_model2)[,2], main="CCF of VAR Model Residuals")
library(vars)

# 예측
forecast_var2=predict(var_model2, n.ahead = length(test$ue))
forecast_var3=predict(var_model3, n.ahead = length(test$ue))
forecast_var6=predict(var_model6, n.ahead = length(test$ue))
forecast_var5=predict(var_model5, n.ahead = length(test$ue))
forecast_var4=predict(var_model4, n.ahead = length(test$ue))
forecast_var4$fcst$diff1

# AIC 및 MSPE 계산 함수
calculate_mspe <- function(model, actual_data) {
  forecasted_values <- predict(model, n.ahead = length(actual_data))$fcst
  forecasted_diff1 <- forecasted_values$diff1[, 1]  # 예측된 diff1 값
  actual_diff1 <- tail(actual_data, length(forecasted_diff1))  # 실제 diff1 값
  mspe <- mean((forecasted_diff1 - actual_diff1)^2)  # MSPE 계산
  return(mspe)
}
calculate_mspe2 <- function(model, actual_data) {
  forecasted_values <- predict(model, n.ahead = length(actual_data))$fcst
  forecasted_diff2 <- forecasted_values$diff2[, 1]  # 예측된 diff1 값
  actual_diff2 <- tail(actual_data, length(forecasted_diff2))  # 실제 diff1 값
  mspe <- mean((forecasted_diff2 - actual_diff2)^2)  # MSPE 계산
  return(mspe)
}

# AIC 및 MSPE 계산
aic_values <- c(AIC(var_model2), AIC(var_model3), AIC(var_model6), AIC(var_model5), AIC(var_model4))
unrate_mspe_values <- c(calculate_mspe(var_model2, c), calculate_mspe(var_model3, c), 
                        calculate_mspe(var_model6, c), calculate_mspe(var_model5, c), 
                        calculate_mspe(var_model4, c))
aic_values <- c(AIC(var_model2), AIC(var_model3), AIC(var_model6), AIC(var_model5), AIC(var_model4))
ue_mspe_values <- c(calculate_mspe(var_model2, d), calculate_mspe(var_model3, d), 
                    calculate_mspe(var_model6, d), calculate_mspe(var_model5, d), 
                    calculate_mspe(var_model4, d))

# 결과 정리
results <- data.frame(
  Model = c("VAR(2)", "VAR(3)", "VAR(6)", "VAR(5)", "VAR(4)"),
  AIC = aic_values,
  unrateMSPE = unrate_mspe_values,
  ueMSPE = ue_mspe_values
)

print(results)


resi=residuals(var_model5)
par(mfrow=c(1,1))
plot.ts(resi[, 1],main="Residuals of Unrate", ylab="Residuals", xlab="Time",col="blue")
plot.ts(resi[, 2],main="Residuals of UE", ylab="Residuals", xlab="Time",col="red")

# 잔차의 ACF 및 PACF 확인
par(mfrow=c(2,2))
acf(resi[, 1], main="ACF of Residuals (Unrate)")
pacf(resi[, 1], main="PACF of Residuals (Unrate)")
acf(resi[, 2], main="ACF of Residuals (UE)")
pacf(resi[, 2], main="PACF of Residuals (UE)")

# Ljung-Box 테스트
Box.test(resi[, 1], lag = 12, type = "Ljung-Box")
Box.test(resi[, 2], lag = 12, type = "Ljung-Box")
## ue는 독립처럼보이고, unrate로그취하고 차분한건 살짝 경계 0.01검정일경우는 독립
test(resi[,1])
test(resi[,2])
# ARCH 테스트
arch_test <- arch.test(var_model5, lags.multi = 5, multivariate.only = FALSE)

# 결과 출력
print(arch_test)
## 이분산성 존재

# 예측값 추출 및 역차분
forecast_diff1 <- forecast_var5$fcst$diff1[, "fcst"]
forecast_diff2 <- forecast_var5$fcst$diff2[, "fcst"]

# 역차분 함수 정의
inverse_diff <- function(diff_data, original_data) {
  last_value <- tail(original_data, 1)
  level_data <- cumsum(c(last_value, diff_data))[-1]
  return(level_data)
}

# 예측값을 원래 수준으로 변환
forecast_unrate <- inverse_diff(forecast_diff1, ln_unrate)
forecast_ue <- inverse_diff(forecast_diff2, ln_ue)

# 예측 결과를 시계열로 변환
forecast_unrate_ts <- ts(forecast_unrate, start = end(ln_unrate) + 1/frequency(ln_unrate), frequency = frequency(ln_unrate))
forecast_ue_ts <- ts(forecast_ue, start = end(ln_ue) + 1/frequency(ln_ue), frequency = frequency(ln_ue))

# 원래 데이터와 예측값을 시각화
par(mfrow=c(2,1))

# Unrate 예측 플롯
plot(exp(a), main="Unemployment Rate Forecast", ylab="Unrate", xlab="Time", col = "red")
lines(exp(forecast_unrate_ts), col = "blue")
legend("topleft", legend=c("Original", "Forecast"), col=c("blue", "red"), lty=1, cex=0.8)

# UE 예측 플롯
plot(b, main="UE Forecast", ylab="UE", xlab="Time", col = "red")
lines(forecast_ue_ts, col = "blue")
legend("topleft", legend=c("Original", "Forecast"), col=c("blue", "red"), lty=1, cex=0.8)

par(mfrow=c(1,1))

summary(var_model5)



if (!require("prophet")) install.packages("prophet")
library(prophet)

# 데이터 준비
train <- read.csv("trainset.csv")
test <- read.csv("testset.csv")

train_data <- data.frame(ds = as.Date(train$DATE), y = log(train$unrate))
test_data <- data.frame(ds = as.Date(test$DATE), y = log(test$unrate))



# Prophet 모델 적합
prophet_model <- prophet(train_data)

# 예측
future <- make_future_dataframe(prophet_model, periods = nrow(test_data))
forecast <- predict(prophet_model, future)

# 예측 결과 시각화
plot(prophet_model, forecast)
dyplot.prophet(prophet_model, forecast)

# 성능 지표 계산 (MSPE)
pred_unrate <- exp(forecast$yhat[(nrow(train_data) + 1):nrow(forecast)])
mspe <- function(forecast, actual) {
  mean((forecast - actual)^2)
}

prophet_mspe <- mspe(pred_unrate, test$unrate)
print(paste("Prophet MSPE:", prophet_mspe))

##############
library(prophet)
library(ggplot2)


# 데이터 불러오기
trainset <- read.csv("trainset.csv")
testset <- read.csv("testset.csv")

trainset$DATE <- as.Date(trainset$DATE, format="%Y-%m-%d")
testset$DATE <- as.Date(testset$DATE, format="%Y-%m-%d")

trainset <- trainset[order(trainset$DATE), ]
testset <- testset[order(testset$DATE),]

# Prophet 모델에서 요구하는 형식으로 데이터 준비
train_data <- data.frame(ds = trainset$DATE, y = log(trainset$unrate))

# Prophet 모델 적합
prophet_model <- prophet(train_data)

# 예측 결과 시각화
plot(prophet_model, forecast)
dyplot.prophet(prophet_model, forecast)

# 예측을 위한 미래 데이터프레임 생성
future <- make_future_dataframe(prophet_model, periods = nrow(testset))

# 예측
forecast <- predict(prophet_model, future)

# 성능 지표 계산 (MSPE)
pred_unrate <- exp(forecast$yhat[(nrow(train_data) + 1):nrow(forecast)])
mspe <- function(forecast, actual) {
  mean((forecast - actual)^2)
}

prophet_mspe <- mspe(pred_unrate, testset$unrate)
print(paste("Prophet MSPE:", prophet_mspe))

# 예측값과 실제값 시간에 따른 시각화
test_dates <- testset$DATE
actual_unrate <- testset$unrate

plot_data <- data.frame(
  Date = test_dates,
  Actual = actual_unrate,
  Predicted = pred_unrate
)

ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1) +
  scale_color_manual(name = "Legend", values = c("Actual" = "black", "Predicted" = "blue")) +
  labs(title = "Actual vs Predicted Unemployment Rate",
       x = "Date",
       y = "Unrate") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10))

# randomForest
if (!require("randomForest")) install.packages("randomForest")
library(randomForest)

# Load the data
train <- read.csv("trainset.csv")
test <- read.csv("testset.csv")

# Prepare the training and testing data
train_data <- train[, c("unrate", "ue")]
test_data <- test[, c("unrate", "ue")]

# Normalize the data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

train_data <- as.data.frame(lapply(train_data, normalize))
test_data <- as.data.frame(lapply(test_data, normalize))

# Train the Random Forest model
rf_model <- randomForest(unrate ~ ue, data = train_data, ntree = 100)
rf_2model = randomForest(unrate ~ ue, data = train_data, ntree = 50)
# Make predictions
predictions <- predict(rf_model, test_data)
predictions2 = predict(rf_2model, test_data)
# De-normalize the predictions
denormalize <- function(x, min_val, max_val) {
  return(x * (max_val - min_val) + min_val)
}

min_unrate <- min(test$unrate)
max_unrate <- max(test$unrate)
pred_unrate <- denormalize(predictions, min_unrate, max_unrate)

min_unrate2 <- min(test$unrate)
max_unrate2 <- max(test$unrate)
pred_unrate2 <- denormalize(predictions2, min_unrate2, max_unrate2)


# Compare predictions with actual values
plot(test$unrate, type = "l", col = "red", ylim = range(c(test$unrate, pred_unrate2)), ylab = "Unemployment Rate", xlab = "Time", main = "Random Forest Predictions vs Actual")
lines(pred_unrate2, col = "blue")
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lty = 1)

plot(test$unrate, type = "l", col = "red", ylim = range(c(test$unrate, pred_unrate)), ylab = "Unemployment Rate", xlab = "Time", main = "Random Forest Predictions2 vs Actual")
lines(pred_unrate2, col = "blue")
legend("topright", legend = c("Actual", "Predicted"), col = c("red", "blue"), lty = 1)

par(mfrow=c(1,2))
# Calculate MSPE
mspe <- function(forecast, actual) {
  
  mean((forecast - actual)^2)
}

rf_mspe <- mspe(pred_unrate, test$unrate)
print(paste("Random Forest MSPE:", rf_mspe))




##### VARMA
# 필요한 라이브러리 로드
library(readr)
library(lubridate)
library(MTS)
library(readr)
library(lubridate)
library(MTS)



# 데이터 로드
train_data <- read_csv("trainset.csv")
test_data <- read_csv("testset.csv")

# 날짜 형식 변환
train_data$DATE <- as.Date(train_data$DATE)
test_data$DATE <- as.Date(test_data$DATE)

# 로그 변환
train_data$log_ue <- log(train_data$ue)
train_data$log_unrate <- log(train_data$unrate)

# 차분
diff_log_ue <- diff(train_data$log_ue)
diff_log_unrate <- diff(train_data$log_unrate)

# 결측값 제거
train_data <- na.omit(train_data)

# unrate에 ARIMA 모델 적합
unrate_arima <- auto.arima(diff_log_unrate)

# unrate 모델의 잔차 계산
unrate_residuals <- residuals(unrate_arima)

# 잔차와 ue의 상관성 확인
ccf(unrate_residuals, diff_log_ue, main = "CCF between Unrate Residuals and Differenced Log UE")
# 잔차 데이터 생성
train_residuals <- data.frame(unrate_residuals, diff_log_ue)
colnames(train_residuals) <- c("unrate_residuals", "diff_log_ue")

# VAR 모델 차수 선택
var_select <- VARselect(train_residuals, lag.max = 10, type = "const")

# 최적의 차수 확인
optimal_lag <- var_select$selection["SC(n)"]

# 데이터 시계열 형식으로 변환
train_residuals_ts <- ts(train_residuals, start = 1, frequency = 1)

# VAR 모델 적용
var_model <- VAR(train_residuals_ts, p = optimal_lag)

# VAR 모델 요약# VAR()VAR 모델 요약
summary(var_model)

# 예측
var_forecast <- predict(var_model, n.ahead = nrow(test_data))

# 예측 결과 추출
predicted_unrate_diff <- var_forecast$fcst$unrate_residuals[, "fcst"]
predicted_ue_diff <- var_forecast$fcst$diff_log_ue[, "fcst"]

# 원래 스케일로 변환
predicted_unrate <- exp(cumsum(predicted_unrate_diff) + log(train_data$unrate[nrow(train_data)]))
predicted_ue <- exp(cumsum(predicted_ue_diff) + log(train_data$ue[nrow(train_data)]))

# 실제 데이터와 예측 데이터 결합
predicted_df <- data.frame(date = test_data$DATE, predicted_ue, predicted_unrate)
combined_df <- cbind(test_data, predicted_df)

# 전체 데이터 (train + test) 결합
full_data <- rbind(
  data.frame(date = train_data$DATE, ue = train_data$ue, unrate = train_data$unrate, source = "Train"),
  data.frame(date = test_data$DATE, ue = predicted_ue, unrate = predicted_unrate, source = "Predicted")
)

# 실제값과 예측값 시각화
par(mfrow = c(2, 1))

# Unrate 시각화
plot(full_data$date, full_data$unrate, type = "l", col = ifelse(full_data$source == "Train", "red", "blue"), lty = ifelse(full_data$source == "Train", 1, 2), main = "Actual vs Predicted Unrate (VAR)", xlab = "Date", ylab = "Unrate")
legend("topright", legend = c("Actual Unrate", "Predicted Unrate"), col = c("red", "blue"), lty = c(1, 2))

# UE 시각화
plot(full_data$date, full_data$ue, type = "l", col = ifelse(full_data$source == "Train", "red", "blue"), lty = ifelse(full_data$source == "Train", 1, 2), main = "Actual vs Predicted UE (VAR)", xlab = "Date", ylab = "UE")
legend("topright", legend = c("Actual UE", "Predicted UE"), col = c("red", "blue"), lty = c(1, 2))

par(mfrow = c(1, 1))

