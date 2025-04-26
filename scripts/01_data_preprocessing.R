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