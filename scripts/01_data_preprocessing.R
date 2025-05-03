# ------------------------------------------------------------------------------
# Script Name: 01_data_preprocessing.R
# Purpose: Clean Online Retail dataset for Market Basket Analysis
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)

# Read the dataset (adjust file path if needed)
retail_data <- read_excel("data/online_retail_data.xlsx")

# View first few rows
head(retail_data)

# View structure
str(retail_data)

# View summary
summary(retail_data)

# Check missing values per column
colSums(is.na(retail_data))

# Identify special StockCodes with letters or symbols
special_stock_codes <- retail_data %>%
  filter(str_detect(StockCode, "^[^0-9]+")) %>%
  pull(StockCode) %>%
  unique()

# View special stock codes
special_stock_codes

# View StockCode and Description for special codes
df_special_codes <- retail_data %>%
  filter(StockCode %in% special_stock_codes) %>%
  select(StockCode, Description) %>%
  distinct()

# View first 30 rows
head(df_special_codes, 10)

# Save the special stock codes with descriptions into a CSV file
write_csv(df_special_codes, "output/special_stockcodes.csv")

# StockCodes we want to remove based on the analysis
stockcodes_to_remove <- c(
  "POST", "D", "C2", "DOT", "M", "m", 
  "BANK CHARGES", "S", "AMAZONFEE", 
  "gift_0001_40", "gift_0001_50", "gift_0001_30", "gift_0001_20", "gift_0001_10"
)

# Remove unwanted stock codes
retail_data <- retail_data %>%
  filter(!(StockCode %in% stockcodes_to_remove))

# Filter rows where Description is NA or empty
df_missing_description <- retail_data %>%
  filter(is.na(Description) | Description == "")

# Save them into a CSV file
write_csv(df_missing_description, "output/missing_description_records.csv")

# View how many rows have missing descriptions
nrow(df_missing_description)

# Remove rows with negative or zero quantity from main dataset
retail_data <- retail_data %>%
  filter(Quantity > 0)

# Remove invalid rows: Description is NA, UnitPrice is 0, and CustomerID is NA
retail_data <- retail_data %>%
  filter(!(is.na(Description) & UnitPrice == 0 & is.na(CustomerID)))

# Remove rows with missing InvoiceNo
retail_data <- retail_data %>%
  filter(!is.na(InvoiceNo))

# Remove rows with missing StockCode
retail_data <- retail_data %>%
  filter(!is.na(StockCode))

# Save cleaned data set into a CSV file
write_csv(retail_data, "output/cleaned_records.csv")

