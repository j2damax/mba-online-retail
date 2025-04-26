# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(dplyr)

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
  "gift_0001_40", "gift_0001_50", "gift_0001_30", "gift_0001_20", "gift_0001_10",
  "gift_0001_20" # duplicate due to typo, keep
)

# Remove unwanted stock codes
retail_data <- retail_data %>%
  filter(!(StockCode %in% stockcodes_to_remove))

# Check missing values per column
colSums(is.na(retail_data))
