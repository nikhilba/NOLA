
library(forecastxgb)
library(xts)
crime_sns_qol_month <- read.csv("D:/Heinz College/2017 Spring/Capstone Project/time series/crime_sns_qol_11to16.csv")
crime_sns_qol_70119_month <- read.csv("D:/Heinz College/2017 Spring/Capstone Project/time series/crime_sns_qol_70119_11to16.csv")
crime_sns_qol_month <- crime_sns_qol_month[, -1]
crime_sns_qol_70119_month <- crime_sns_qol_70119_month[, -1]

crime_sns_qol_xts <- xts(crime_sns_qol_month[, -1], order.by=as.Date(crime_sns_qol_month[,1], "%m/%d/%Y"))
crime_sns_qol_ts <- ts(crime_sns_qol_xts)
crime_sns_qol_70119_xts <- xts(crime_sns_qol_70119_month[, -1], order.by=as.Date(crime_sns_qol_70119_month[,1], "%m/%d/%Y"))
crime_sns_qol_70119_ts <- ts(crime_sns_qol_70119_xts)

crime <- crime_sns_qol_ts[, 1]
sns_qol <- matrix(crime_sns_qol_ts[, 2:ncol(crime_sns_qol_ts)], nrow = 60, ncol = 11,
                  dimnames = list(NULL, c("sns", "codeEnforcement", "streetLight", "TrashPickup",
                                  "recyclingPrograms", "abandonedVehicle", "LargeGarbagePickup",
                                  "RoadwayRepair", "GeneralService", "StreetFlooding",
                                  "IllegalDumping")))

crime_70119 <- crime_sns_qol_70119_ts[, 1]
sns_qol_70119 <- matrix(crime_sns_qol_70119_ts[, 2:ncol(crime_sns_qol_70119_ts)], nrow = 60, ncol = 11,
                        dimnames = list(NULL, c("sns", "codeEnforcement", "streetLight", "TrashPickup",
                                            "recyclingPrograms", "abandonedVehicle", "LargeGarbagePickup",
                                            "RoadwayRepair", "GeneralService", "StreetFlooding",
                                            "IllegalDumping")))

set.seed(123)
ts_model <- xgbar(y = crime, 
                  xreg = sns_qol,
                  nrounds = 100,
                  maxlag = 12, 
                  nrounds_method = "cv",
                  seas_method = "fourier",
                  nfold = 10)

summary(ts_model)

ts_model_70119 <- xgbar(y = crime_70119, 
                        xreg = sns_qol_70119, 
                        nrounds = 70,
                        maxlag = 12, 
                        nrounds_method = "cv",
                        seas_method = "fourier",
                        nfold = 10)

summary(ts_model_70119)

true_val <- crime_sns_qol_ts[13:nrow(crime_sns_qol_ts), 1]
fit_val <- as.vector(ts_model$fitted)
pred_val <- fit_val[13:length(fit_val)]
rmse <- sum(((true_val - pred_val)^2)) / length(pred_val) 

true_70119_val <- crime_sns_qol_70119_ts[13:nrow(crime_sns_qol_70119_ts), 1]
fit_70119_val <- as.vector(ts_model_70119$fitted)
pred_70119_val <- fit_70119_val[13:length(fit_70119_val)]
rmse_70119 <- sum(((true_70119_val - pred_70119_val)^2)) / length(pred_70119_val) 

crimes_future <- matrix(forecast(xgbar(crime_sns_qol_ts[, 2:ncol(crime_sns_qol_ts)]), h = 1)$mean, 
                        nrow = 60, ncol = 11,
                        dimnames = list(NULL, c("sns", "codeEnforcement", "streetLight", "TrashPickup",
                                                "recyclingPrograms", "abandonedVehicle", "LargeGarbagePickup",
                                                "RoadwayRepair", "GeneralService", "StreetFlooding",
                                                "IllegalDumping")))
plot(forecast(ts_model, xreg = crimes_future))

crimes_70119_future <- matrix(forecast(xgbar(crime_sns_qol_70119_ts[, 2:ncol(crime_sns_qol_70119_ts)]), h = 1)$mean, 
                              nrow = 60, ncol = 11,
                              dimnames = list(NULL, c("sns", "codeEnforcement", "streetLight", "TrashPickup",
                                                      "recyclingPrograms", "abandonedVehicle", "LargeGarbagePickup",
                                                      "RoadwayRepair", "GeneralService", "StreetFlooding",
                                                      "IllegalDumping")))

plot(forecast(ts_model_70119, xreg = crimes_70119_future))

future_pred <- forecast(ts_model, xreg = crimes_future)
future_70119_pred <- forecast(ts_model_70119, xreg = crimes_70119_future)


whole_pred_data <- as.data.frame(c(fit_val, future_pred$mean))
write.csv(whole_pred_data, "xgb_pred_qol_city_crimes.csv")

pred_70119_data <- as.data.frame(c(fit_70119_val, future_70119_pred$mean))
write.csv(pred_70119_data, "xgb_pred_qol_70119_crimes.csv")