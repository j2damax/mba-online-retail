# ------------------------------------------------------------------------------
# Script Name: 03_market_basket_analysis.R
# Purpose: Perform Market Basket Analysis using Apriori on top 3 countries
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

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

# Set minimum support and confidence
min_support <- 0.01     # 1% of transactions
min_confidence <- 0.5   # 50% confidence

# United Kingdom (Large)
rules_country1 <- apriori(
  basket_country1,
  parameter = list(supp = 0.015, conf = 0.65, maxlen = 4)
)

# Germany (Medium)
rules_country2 <- apriori(
  basket_country2,
  parameter = list(supp = 0.025, conf = 0.7, maxlen = 4)
)

# France (Small)
rules_country3 <- apriori(
  basket_country3,
  parameter = list(supp = 0.08, conf = 0.8, maxlen = 4)
)

# View summary of rules
summary(rules_country1)
summary(rules_country2)
summary(rules_country3)

# Filter top rules by lift (> 5) and confidence (> 0.7)
#rules_country1_top <- subset(rules_country1, lift > 5 & confidence > 0.7)
#rules_country2_top <- subset(rules_country2, lift > 5 & confidence > 0.7)
#rules_country3_top <- subset(rules_country3, lift > 5 & confidence > 0.7)

# Filter by Lift & Confidence
filtered_country1 <- subset(rules_country1, lift > 5 & confidence > 0.7)
filtered_country2 <- subset(rules_country2, lift > 5 & confidence > 0.7)
filtered_country3 <- subset(rules_country3, lift > 6 & confidence > 0.85)

# Sort top rules by lift (highest first)
#rules_country1_top_sorted <- sort(rules_country1_top, by = "lift", decreasing = TRUE)
#rules_country2_top_sorted <- sort(rules_country2_top, by = "lift", decreasing = TRUE)
#rules_country3_top_sorted <- sort(rules_country3_top, by = "lift", decreasing = TRUE)

# Sort and Save Top N Rules Only
top_rules_country1 <- sort(filtered_country1, by = "lift", decreasing = TRUE)[1:30]
top_rules_country2 <- sort(filtered_country2, by = "lift", decreasing = TRUE)[1:30]
top_rules_country3 <- sort(filtered_country3, by = "lift", decreasing = TRUE)[1:30]

# View top 10 rules for each country
inspect(head(top_rules_country1, 10))
inspect(head(top_rules_country2, 10))
inspect(head(top_rules_country3, 10))

# Convert to data frame and save to CSV
rules_df_country1 <- as(top_rules_country1, "data.frame")
rules_df_country2 <- as(top_rules_country2, "data.frame")
rules_df_country3 <- as(top_rules_country3, "data.frame")

# Export to CSV
write_csv(rules_df_country1, "output/rules_country1.csv")
write_csv(rules_df_country2, "output/rules_country2.csv")
write_csv(rules_df_country3, "output/rules_country3.csv")


