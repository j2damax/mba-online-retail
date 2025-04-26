# Load libraries
library(tidyverse)
library(arules)

# Load cleaned data
retail_data <- read_csv("output/cleaned_records.csv")

# Group by Country and count distinct InvoiceNo
top_countries <- retail_data %>%
  group_by(Country) %>%
  summarise(num_transactions = n_distinct(InvoiceNo)) %>%
  arrange(desc(num_transactions)) %>%
  slice(1:3)

# View Top 3 Countries
top_countries

# Extract the country names from top_countries
country1 <- top_countries$Country[1]
country2 <- top_countries$Country[2]
country3 <- top_countries$Country[3]

# Now filter dataset for each country
retail_country1 <- retail_data %>% filter(Country == country1)
retail_country2 <- retail_data %>% filter(Country == country2)
retail_country3 <- retail_data %>% filter(Country == country3)

# View few rows to confirm
head(retail_country1)
head(retail_country2)
head(retail_country3)

# Country 1: Create transaction object
basket_country1 <- as(split(retail_country1$StockCode, retail_country1$InvoiceNo), "transactions")

# Country 2: Create transaction object
basket_country2 <- as(split(retail_country2$StockCode, retail_country2$InvoiceNo), "transactions")

# Country 3: Create transaction object
basket_country3 <- as(split(retail_country3$StockCode, retail_country3$InvoiceNo), "transactions")

# Quick summary
summary(basket_country1)
summary(basket_country2)
summary(basket_country3)

# Inspect sample transactions
inspect(basket_country1[1:5])

