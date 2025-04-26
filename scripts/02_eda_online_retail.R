# Load libraries
library(tidyverse)
library(ggplot2)

# Load cleaned data
retail_data <- read_csv("output/cleaned_records.csv")

# Basic structure and size
dim(retail_data)
colnames(retail_data)
head(retail_data)

# Check missing values per column
colSums(is.na(retail_data))

# Summary Statistics
summary(retail_data %>% select(Quantity, UnitPrice))

# Distribution Plots
ggplot(retail_data, aes(x = Quantity)) +
  geom_histogram(bins = 50, fill = "blue", color = "black") +
  xlim(0, 100) +
  ggtitle("Distribution of Quantity Sold")
ggsave("output/quantity_distribution.png")

ggplot(retail_data, aes(x = UnitPrice)) +
  geom_histogram(bins = 50, fill = "green", color = "black") +
  xlim(0, 50) +
  ggtitle("Distribution of Unit Price")
ggsave("output/unitprize_distribution.png")

# Top 10 Best-Selling Products
retail_data %>%
  group_by(StockCode, Description) %>%
  summarise(total_quantity = sum(Quantity)) %>%
  arrange(desc(total_quantity)) %>%
  head(10)

# Top 10 Countries by Number of Invoices
retail_data %>%
  group_by(Country) %>%
  summarise(num_transactions = n_distinct(InvoiceNo)) %>%
  arrange(desc(num_transactions)) %>%
  head(10)

# Scatterplot: Unit Price vs Quantity
ggplot(retail_data, aes(x = UnitPrice, y = Quantity)) +
  geom_point(alpha = 0.5) +
  xlim(0, 50) + ylim(0, 100) +
  ggtitle("Scatterplot: Unit Price vs Quantity")
ggsave("output/unitprize_quantity_scatterplot.png")

