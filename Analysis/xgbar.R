
library(forecastxgb)
library(xts)
crime_sns_month <- read.csv("D:/Heinz College/2017 Spring/Capstone Project/time series/crime_sns_11to16.csv")
crime_sns_70119_month <- read.csv("D:/Heinz College/2017 Spring/Capstone Project/time series/crime_sns_70119_11to16.csv")
crime_sns_month <- crime_sns_month[, -1]
crime_sns_70119_month <- crime_sns_70119_month[, -1]

crime_sns_xts <- xts(crime_sns_month[, -1], order.by=as.Date(crime_sns_month[,1], "%m/%d/%Y"))
crime_sns_ts <- ts(crime_sns_xts)
crime_sns_70119_xts <- xts(crime_sns_70119_month[, -1], order.by=as.Date(crime_sns_70119_month[,1], "%m/%d/%Y"))
crime_sns_70119_ts <- ts(crime_sns_70119_xts)

crime <- crime_sns_ts[, 1]
sns <- matrix(crime_sns_ts[, 2], dimnames = list(NULL, "num_stop_search"))
crime_70119 <- crime_sns_70119_ts[, 1]
sns_70119 <- matrix(crime_sns_70119_ts[, 2], dimnames = list(NULL, "num_stop_search"))

set.seed(123)
ts_model <- xgbar(y = crime, 
                  xreg = sns, 
                  maxlag = 12, 
                  nrounds_method = "cv",
                  seas_method = "fourier",
                  nfold = 10)

summary(ts_model)

ts_model_70119 <- xgbar(y = crime_70119, 
                        xreg = sns_70119, 
                        maxlag = 12, 
                        nrounds_method = "cv",
                        seas_method = "fourier",
                        nfold = 10)

summary(ts_model_70119)

true_val <- crime_sns_ts[13:nrow(crime_sns_ts), 1]
fit_val <- as.vector(ts_model$fitted)
pred_val <- fit_val[13:length(fit_val)]
rmse <- sum(((true_val - pred_val)^2)) / length(pred_val) 

true_70119_val <- crime_sns_70119_ts[13:nrow(crime_sns_70119_ts), 1]
fit_70119_val <- as.vector(ts_model_70119$fitted)
pred_70119_val <- fit_70119_val[13:length(fit_70119_val)]
rmse_70119 <- sum(((true_70119_val - pred_70119_val)^2)) / length(pred_70119_val) 


crimes_70119_future <- matrix(forecast(xgbar(crime_sns_70119_ts[, 2]), h = 12)$mean, dimnames = list(NULL, "num_stop_search"))
plot(forecast(ts_model_70119, xreg = crimes_70119_future))

crimes_future <- matrix(forecast(xgbar(crime_sns_ts[, 2]), h = 12)$mean, dimnames = list(NULL, "num_stop_search"))
plot(forecast(ts_model, xreg = crimes_future))

future_pred <- forecast(ts_model, xreg = crimes_future)
future_70119_pred <- forecast(ts_model_70119, xreg = crimes_70119_future)

future_pred

whole_pred_data <- as.data.frame(c(fit_val, future_pred$mean))
write.csv(whole_pred_data, "xgb_pred_city_crimes.csv")

pred_70119_data <- as.data.frame(c(fit_70119_val, future_70119_pred$mean))
write.csv(pred_70119_data, "xgb_pred_70119_crimes.csv")