# Read in ------------------------------------------------------------------

zipfile <- "RPPR.zip"
unzip(zipfile)

setwd("RPPR/")
getwd()

csv_files <- list.files(full.names = TRUE, recursive = TRUE)
csv_files

example1 <- read.csv("./RPPR/PPR-2019-04-Dublin.csv", 
                     header = TRUE, 
                     stringsAsFactors = FALSE, 
                     na.strings = c("", "NA"))

#setwd("../")
#getwd()

# Old DF -------------------------------------------------------------------

str(example1)

column_names1 <- c("Date of Sale", 
                  "Address",
                  "Postcode", 
                  "County",
                  "Price", 
                  "Not Full Market Price",
                  "VAT Exclusive", 
                  "Description of Property",
                  "Property Size Description")

colnames(example1) <- column_names1

class(example1$Price)
example1$Price <- gsub("€","", example1$Price, fixed = TRUE)
example1$Price <- gsub(",","", example1$Price, fixed = TRUE)
example1$Price <- as.integer(example1$Price)
class(example1$Price)

str(example1)

# New DF -------------------------------------------------------------------

date <- (example1$`Date of Sale`[1])
restructured_date <- as.Date(date, "%d/%m/%Y")
month <- format(restructured_date, "%b-%y")

month_col <- c(month)
total_sales_col <- c(nrow(example1))
mean_price_col <- c(as.numeric(mean(example1$Price)))
median_price_col <- c(as.numeric(median(example1$Price)))

example2 <- data.frame(month_col, 
                       total_sales_col, 
                       mean_price_col, 
                       median_price_col)

str(example2)

column_names2 <- c("Month of Sale", 
                   "Total Sales",
                   " Mean Price",
                   "Median Price")

colnames(example2) <- column_names2

str(example2)

# End of function ----------------------------------------------------------
  
O = lapply(L, function(x) {
  DF <- read.csv(x, header = T, sep = ",")
  DF$Date <- as.character(CAN$Date)
  DF$Date <- as.Date(CAN$Date, format ="%m/%d/%y")
  DF_Merge <- merge(all.dates.frame, CAN, all = T)
  DF_Merge$Bid.Yield.To.Maturity <- NULL
  return(DF_Merge)})
