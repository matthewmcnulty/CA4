# Unzipping zip file containing data
# regarding house sales from January 2010 
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
  
  # Converting to data and formatting it as "mmm-yy".
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

