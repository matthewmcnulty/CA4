AllDublinRPPRData <- read.csv('AllDublinRPPRData.csv', header = TRUE, 
                              stringsAsFactors = FALSE, 
                              na.strings = c("", "NA"))

# Renaming column vectors.
output_colnames <- c("Index",
                     "Month of Sale", 
                     "Total Sales",
                     "Mean Price",
                     "Median Price")

colnames(AllDublinRPPRData) <- output_colnames

head(AllDublinRPPRData)
str(AllDublinRPPRData)

total_sales <- ts(AllDublinRPPRData$`Total Sales`, start = c(2010, 1), frequency = 12)
total_sales

plot(total_sales)
start(total_sales)
end(total_sales)
frequency(total_sales)

# Plot the data first
# and smooth it to remove significant error components
# through centered moving average

install.packages("forecast")
library(forecast)

default_settings <- par(no.readonly = TRUE)
par(mfrow=c(2, 2))

# ma() function used to smooth the Nile time series
y_range <- c(min(total_sales), max(total_sales))

plot(total_sales, 
     main = "Raw time series", ylim = y_range)
plot(ma(total_sales, 3), 
     main = "Simple Moving Averages (k=3)", ylim = y_range)
plot(ma(total_sales, 6), 
     main = "Simple Moving Averages (k=6)", ylim = y_range)
plot(ma(total_sales, 9), 
     main = "Simple Moving Averages (k=9)", ylim = y_range)

par(default_settings)
# As k increases, plot becomes increasingly smooth

# Plot the ACF chart - measure of how the observations
# in the time series relate to each other
# If the autocorrelation crosses the dashed blue line, it means
# specific lag is significantly correlated with the current time 
# time series. A stationary time series will have autocorrelation
# fall quickly to 0. With non-stationarity series it drops quickly

# Acf() plot
acf_result <- Acf(total_sales, main = "ACF of total_sales")

# Pacf() plot
pacf_result <- Pacf(total_sales, main = "PACF of total_sales")

# Test if the time series is stationary
install.packages("tseries")
library(tseries)

# p-values < 0.05 then the time series TS is stationary
plot(total_sales, main = "Raw time series")
adf.test(total_sales)

# While the p-value is ~ 0.03 (< 0.05), it can be visually determined
# that the stationarity of the time series TS can still be improved
# Therefore, the limit of significance will be set to 0.01. If the
# p-value resulting from the ADF test is less than this, we can
# say the time series is stationary with absolute certainty.

# Assess the presence of a trend in the data
ndiffs(total_sales)

# There is a trend within the data, so the series is differenced 
# once (lag = 1)
diff_total_sales <- diff(total_sales, lag = 1)

# Now the p-value is = 0.01 (< 0.05). We want to aim for a result
# lower than this, so we will continue to transform the time series TS.
adf.test(diff_total_sales)

# Time series TS does is detrended.
ndiffs(diff_total_sales)

# Show both charts side-by-side for comparison
par(mfrow=c(1, 2))

plot(total_sales)
plot(diff_total_sales)

par(default_settings)

Acf(diff_total_sales, 
    main = "Autocorrelation plot for differenced Total Sales series")
# q = 24

Pacf(diff_total_sales, 
     main = "Partial autocorrelation plot for differenced Total Sales series")
# p = 12
# d = 1

# We use the original dataset for the ARIMA model
# and modify the d value to suit our earlier findings
# and d = 1
arima_model <- Arima(total_sales, order = c(12, 1, 24))
arima_model
# Accuracy measures using the MAPE
# measures the prediction of accuracy
accuracy(arima_model)

# Model is 10.93% inaccurate.

qqnorm(arima_model$residuals)
qqline(arima_model$residuals)

# box test function provides a test that autocorrelates
# are all 0 (Null hyp)
Box.test(arima_model$residuals, type = "Ljung-Box")

# Forecast 3 months ahead for the Total Sales time series
forecast(arima_model, 3)
plot(forecast(arima_model, 3), 
     xlab = "Month",
     ylab = "Total Sales")

# Automated ARIMA forecast
auto_arima_model <- auto.arima(total_sales)
auto_arima_model
accuracy(auto_arima_model)

# Auto ARIMA has a seasonal decomposition, however gives a lower
# MAPE of 11.69822. Does this mean my model is overfitting?

qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)

# QQ plot is better for auto arima?

# box test function provides a test that autocorrelates
# are all 0 (Null hyp)
Box.test(auto_arima_model$residuals, type = "Ljung-Box")

