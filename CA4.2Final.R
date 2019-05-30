AllDublinRPPRData <- read.csv('AllDublinRPPRData.csv', header = TRUE, 
                              stringsAsFactors = FALSE, 
                              na.strings = c("", "NA"))

# Renaming column vectors
output_colnames <- c("Index",
                     "Month of Sale", 
                     "Total Sales",
                     "Mean Price",
                     "Median Price")

colnames(AllDublinRPPRData) <- output_colnames

head(AllDublinRPPRData)
str(AllDublinRPPRData)

total_sales <- ts(AllDublinRPPRData$`Total Sales`[0:109], start = c(2010, 1), frequency = 12)
total_sales

plot(total_sales)
# Time series is additive
# Time series appears to have a trend and seasonality

start(total_sales)
end(total_sales)
frequency(total_sales)

# Plot the ACF chart - measure of how the observations
# in the time series relate to each other
# If the autocorrelation crosses the dashed blue line, it means
# specific lag is significantly correlated with the current time 
# time series. A stationary time series will have autocorrelation
# fall quickly to 0. With non-stationarity series it drops gradually

#install.packages("tseries")
#library(tseries)
#install.packages("forecast")
#library(forecast)

# Acf() plot
acf_result <- Acf(total_sales, main = "ACF of total_sales")
# Time series is non-stationary

# Pacf() plot
pacf_result <- Pacf(total_sales, main = "PACF of total_sales")

# Removing the seasonality ---------------------------------------------------------------
# Plot the data
plot(total_sales, main = "Raw time series")
# variability is constant, time series is a additive model

seasonal_decomposition <- stl(total_sales, s.window = "period")
plot(seasonal_decomposition)

seasonplot(total_sales, 12, 
           col = rainbow(12), 
           year.labels = TRUE, 
           main = "Seasonal plot of Total Sales")

seasonal_adj_total_sales <- seasadj(seasonal_decomposition)

seasonplot(seasonal_adj_total_sales, 12, 
           col = rainbow(12), 
           year.labels = TRUE, 
           main = "Seasonal element removed from Total Sales")

# Plot the data with removed seasonal element
plot(seasonal_adj_total_sales, main = "Time series with seasonal element removed")

# Removing the trend ---------------------------------------------------------------
ndiffs(seasonal_adj_total_sales)
# d = 1

# There is a trend within the data, so the series is differenced 
# once (lag = 1)
diff_seasonal_adj_total_sales <- diff(seasonal_adj_total_sales, lag = 1)

# Time series is now detrended
ndiffs(diff_seasonal_adj_total_sales)

# Plot the differenced time series
plot(diff_seasonal_adj_total_sales, main = "Differenced time series with removed seasonal element")

# p-values < 0.05 then the time series is stationary
adf.test(diff_seasonal_adj_total_sales)

# ARIMA modelling ---------------------------------------------------------------

# Acf() plot
acf_result <- Acf(diff_seasonal_adj_total_sales, 
                  main = "ACF of diff_seasonal_adj_total_sales")
# q = 1 as it is zero after first lag

# Pacf() plot
pacf_result <- Pacf(diff_seasonal_adj_total_sales, 
                    main = "PACF of seasonal_adj_total_sales")
# p = 2 as it is zero after second lag

# We use the original dataset for the ARIMA model
# and modify the d value to suit our earlier findings
# and d = 1
adj_arima_model <- Arima(seasonal_adj_total_sales, order = c(2, 1, 1))
adj_arima_model
# Accuracy measures using the MAPE
# measures the prediction of accuracy
accuracy(adj_arima_model)

qqnorm(adj_arima_model$residuals)
qqline(adj_arima_model$residuals)

# box test function provides a test that autocorrelates
# are all 0 (Null hyp)
Box.test(adj_arima_model$residuals, type = "Ljung-Box")

forecast(adj_arima_model)
plot(forecast(adj_arima_model, 3), 
     xlab = "Year", 
     ylab = "Total Sales")

auto_arima_model <- auto.arima(seasonal_adj_total_sales)
auto_arima_model
# Accuracy measures using the MAPE
# measures the prediction of accuracy
accuracy(auto_arima_model)

qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)

# box test function provides a test that autocorrelates
# are all 0 (Null hyp)
Box.test(auto_arima_model$residuals, type = "Ljung-Box")

forecast(auto_arima_model)
plot(forecast(auto_arima_model, 3), 
     xlab = "Year", 
     ylab = "Total Sales")

actual_sales <- data.frame(AllDublinRPPRData$`Total Sales`[110:112])
actual_sales

pred_sales_adj <- data.frame(forecast(adj_arima_model, 3))
pred_sales_adj

pred_sales_auto <- data.frame(forecast(auto_arima_model, 3))
pred_sales_auto

actual_vs_pred <- data.frame(cbind(actual_sales, 
                                   pred_sales_adj$Point.Forecast, 
                                   pred_sales_auto$Point.Forecast))

colnames(actual_vs_pred) <- c("Actual Sales", 
                              "Predicted Sales Adj", 
                              "Predicted Sales Auto")

actual_vs_pred

cor(actual_vs_pred$`Actual Sales`, actual_vs_pred$`Predicted Sales Adj`)
cor(actual_vs_pred$`Actual Sales`, actual_vs_pred$`Predicted Sales Auto`)
