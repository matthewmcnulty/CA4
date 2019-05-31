# Unzipping zip file containing data regarding 
# house sales in Dublin from January 2010 
# until now.
zipfile <- "RPPR.zip"
unzip(zipfile)

# Setting the working directory inside the unzipped folder.
setwd("RPPR/")
getwd()

# Creating a list of directories for 
# all .csv files inside RPPR/ folder.
csv_files <- list.files(full.names = TRUE, recursive = TRUE)
csv_files

# Creating a function to extract date of sale, total sales,
# mean price, and median price from .csv file. 
rppr_stats <- function(x)
{ 
  # Reading in particular .csv file to function.
  input_data <- read.csv(x, 
                         header = TRUE, 
                         stringsAsFactors = FALSE, 
                         na.strings = c("", "NA"))
  
  # Renaming column vectors.
  input_colnames <- c("Date of Sale", 
                      "Address",
                      "Postcode", 
                      "County",
                      "Price", 
                      "Not Full Market Price",
                      "VAT Exclusive", 
                      "Description of Property",
                      "Property Size Description")
  
  colnames(input_data) <- input_colnames
  
  # Removing € and commas from the price to convert to an integer.
  input_data$Price <- gsub("€","", input_data$Price, fixed = TRUE)
  input_data$Price <- gsub(",","", input_data$Price, fixed = TRUE)
  input_data$Price <- as.integer(input_data$Price)
  
  # Converting to date and formatting it as "mmm-yy".
  date <- (input_data$`Date of Sale`[1])
  converted_date <- as.Date(date, "%d/%m/%Y")
  month <- format(converted_date, "%b-%y")
  
  # Creating vectors of month of sale, total sales,
  # mean price, and median price.
  month_col <- c(month)
  total_sales_col <- c(nrow(input_data))
  mean_price_col <- c(as.numeric(mean(input_data$Price)))
  median_price_col <- c(as.numeric(median(input_data$Price)))
  
  # Saving results to temporary dataframe.
  ouput_data <- data.frame(month_col, 
                           total_sales_col, 
                           mean_price_col, 
                           median_price_col)
  
  # Renaming column vectors.
  output_colnames <- c("Month of Sale", 
                       "Total Sales",
                       "Mean Price",
                       "Median Price")
  
  colnames(ouput_data) <- output_colnames
  
  # Returning output dataframe.
  return(ouput_data)
}

# Running the function over all .csv files using lapply.
# Each set of results are row binded to the AllDublinRPPRData dataset.
AllDublinRPPRData <- Reduce(rbind, lapply(csv_files, rppr_stats))

# Exiting the /RPPR folder to the normal working directory.
setwd("../")
getwd()

# Writing an output .csv file for the created dataset.
write.csv(AllDublinRPPRData, file = "AllDublinRPPRData.csv")

# Reading in time series. -------------------------------------------------------------------------------------------------

# Importing dataset and replacing missing entries with "NA".
AllDublinRPPRData <- read.csv('AllDublinRPPRData.csv', header = TRUE, 
                              stringsAsFactors = FALSE, 
                              na.strings = c("", "NA"))

# Adding a suitable title for each attribute of the data.
output_colnames <- c("Index",
                     "Month of Sale", 
                     "Total Sales",
                     "Mean Price",
                     "Median Price")

colnames(AllDublinRPPRData) <- output_colnames

# Showing the total number of rows,
# the first 10 rows of the data frame, 
# and the structure of the data frame.
head(AllDublinRPPRData, 10)
str(AllDublinRPPRData)

# Creating a time series with 'Total Sales'.
# The starting data is specified as January 2010,
# with the frequency set to 12 to indicate it as monthly.
# The last three values were excluded to be used as testing data.
total_sales <- ts(AllDublinRPPRData$`Total Sales`[0:109], 
                  start = c(2010, 1), 
                  frequency = 12)
total_sales

# First is to visually describe the time series.
plot(total_sales, main = "Raw time series")
# Time series appears to have a trend and seasonality.

start(total_sales)
end(total_sales)
frequency(total_sales)

# Installing the necesary time series and forecasting libraries.
#install.packages("tseries")
library(tseries)
#install.packages("forecast")
library(forecast)

# Plotting the autocorrelation function (ACF). This
# measures how the observations in time series relate to each other.
# If the measure of autocorrelation crosses the dashed blue line,
# then that specific lag is significantly correlated with the associated
# time series. For a stationary time series, each measure of autocorrelation
# will quickly drop to zero. A non-stationary time series will drop gradually.

# Plotting the Acf() plot. 
acf_result <- Acf(total_sales, main = "ACF of total_sales")
# Time series is non-stationary.

# Plotting the Pacf() plot. 
pacf_result <- Pacf(total_sales, main = "PACF of total_sales")

# Removing the seasonality. -----------------------------------------------------------------------------------------------

# Plotting the time series.
plot(total_sales, main = "Raw time series")
# As the variation in the number of sales is constant,
# the time series is additive and will not need to
# transformed before decomposing it.

# Decomposing the time series into seaonal, trend, and irregular components.
# In this case, s.window is set to "period", 
# to ensure that extracted seasonality remains constant.
seasonal_decomposition <- stl(total_sales, s.window = "period")

# Plotting the decomposed time series.
plot(seasonal_decomposition)

seasonplot(total_sales, 12, 
           col = rainbow(12), 
           year.labels = TRUE, 
           main = "Seasonal plot of Total Sales")

# Recreating the time series with the seasonal component removed.
seasonal_adj_total_sales <- seasadj(seasonal_decomposition)

seasonplot(seasonal_adj_total_sales, 12, 
           col = rainbow(12), 
           year.labels = TRUE, 
           main = "Seasonal element removed from Total Sales")

# Plotting the time series with removed seasonal element.
plot(seasonal_adj_total_sales, main = "Time series with seasonal element removed")

# Removing the trend. -----------------------------------------------------------------------------------------------------

# Testing whether the time series with removed seasonal element needs differenced.
ndiffs(seasonal_adj_total_sales)
# d = 1

# There is a trend within the data, so the series is differenced 
# once. This is specified by lag = 1.
diff_seasonal_adj_total_sales <- diff(seasonal_adj_total_sales, lag = 1)

# Testing again shows the time series with removed seasonal element does
# not require any more differencing.
ndiffs(diff_seasonal_adj_total_sales)

# Plotting the differenced time series with removed seasonal element.
plot(diff_seasonal_adj_total_sales, main = "Differenced time series with removed seasonal element")

# P-value is < 0.01, which is lower than the significance level of
# 0.05, which indiciates that this time series is now stationary.
adf.test(diff_seasonal_adj_total_sales)

# ARIMA MODEL ESTIMATION. -------------------------------------------------------------------------------------------------

# Plotting the Acf() plot. 
acf_result <- Acf(diff_seasonal_adj_total_sales, 
                  main = "ACF of diff_seasonal_adj_total_sales")
# q = 1 as the autocorrelation is zero after first lag.

# Plotting the Pacf() plot. 
pacf_result <- Pacf(diff_seasonal_adj_total_sales, 
                    main = "PACF of seasonal_adj_total_sales")
# p = 2 as the  partial autocorrelation is zero after second lag.

# We use the undifferenced time series with the removed seasonal element  
# for the ARIMA model. As the number of differences required previously
# was one, then d = 1. p = 2, and q = 1.
adj_arima_model <- Arima(seasonal_adj_total_sales, order = c(2, 1, 1))
adj_arima_model

# The 'Mean Absolute Percentage Error' can be used to
# find the prediction accuracy of the model.
accuracy(adj_arima_model)
# In this case, the MAPE of the model is equal to.

# If model fits well, the residuals should be normally
# and independently distributed.
qqnorm(adj_arima_model$residuals)
qqline(adj_arima_model$residuals)
# Normally distributed data should fall along the line,
# which it does in this case.

# Box test. The null hypothesis H0 states the autocorrelations are
# all zero.
Box.test(adj_arima_model$residuals, type = "Ljung-Box")
# In this case, the p-value is,

# ARIMA MODEL AUTOMATION. -------------------------------------------------------------------------------------------------

# Using auto.arima() to find the best possible model for the
# undifferenced time series with the removed seasonal element.
auto_arima_model <- auto.arima(seasonal_adj_total_sales)
auto_arima_model

accuracy(auto_arima_model)
# In this case, the MAPE of the model is equal to.

# If model fits well, the residuals should be normally
# and independently distributed.
qqnorm(auto_arima_model$residuals)
qqline(auto_arima_model$residuals)
# Normally distributed data should fall along the line,
# which it does in this case.

# Box test. The null hypothesis H0 states the autocorrelations are
# all zero.
Box.test(auto_arima_model$residuals, type = "Ljung-Box")
# In this case, the p-value is,

# Forecasting each ARIMA model. -------------------------------------------------------------------------------------------

# Forecasting values based on the estimated arima model
# and plotting the first three forecasted values after
# along with the time series.
forecast(adj_arima_model)
plot(forecast(adj_arima_model, 3), 
     xlab = "Year", 
     ylab = "Total Sales")

# Forecasting values based on the automated arima model
# and plotting the first three forecasted values after
# along with the time series.
forecast(auto_arima_model)
plot(forecast(auto_arima_model, 3), 
     xlab = "Year", 
     ylab = "Total Sales")

# How do the predicted values compare to the actual values? ---------------------------------------------------------------

# Storing the testing data in a dataframe.
actual_sales <- data.frame(AllDublinRPPRData$`Total Sales`[110:112])
actual_sales

# Storing the forecasted data from the estimated model in a dataframe.
pred_sales_adj <- data.frame(forecast(adj_arima_model, 3))
pred_sales_adj

# Storing the forecasted data from the automated model in a dataframe.
pred_sales_auto <- data.frame(forecast(auto_arima_model, 3))
pred_sales_auto

# Combining each of the previous dataframes in one dataframe.
actual_vs_pred <- data.frame(cbind(actual_sales, 
                                   pred_sales_adj$Point.Forecast, 
                                   pred_sales_auto$Point.Forecast))

# Adding a suitable title for each attribute of the data.
colnames(actual_vs_pred) <- c("Actual Sales", 
                              "Predicted Sales Adj", 
                              "Predicted Sales Auto")
actual_vs_pred

# Finding the correlation between the actual values are predicted values
# for each model.
cor(actual_vs_pred$`Actual Sales`, actual_vs_pred$`Predicted Sales Adj`)
cor(actual_vs_pred$`Actual Sales`, actual_vs_pred$`Predicted Sales Auto`)

