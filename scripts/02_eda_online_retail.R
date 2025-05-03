# ------------------------------------------------------------------------------
# Script Name: 02_eda_online_retail.R
# Purpose: Perform Exploratory Data Analysis on cleaned Online Retail dataset
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load libraries
library(tidyverse)

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
ggsave("output/unitprice_distribution.png")

# Top 10 Best-Selling Products
retail_data %>%
  group_by(StockCode, Description) %>%
  summarise(total_quantity = sum(Quantity)) %>%
  arrange(desc(total_quantity)) %>%
  head(10)

#save the top 10 best-selling products to a CSV file
write_csv(retail_data %>%
  group_by(StockCode, Description) %>%
  summarise(total_quantity = sum(Quantity)) %>%
  arrange(desc(total_quantity)) %>%
  head(10), "output/top_10_best_selling_products.csv")

# Top 10 Countries by Number of Invoices
retail_data %>%
  group_by(Country) %>%
  summarise(num_transactions = n_distinct(InvoiceNo)) %>%
  arrange(desc(num_transactions)) %>%
  head(10)

#save the top 10 countries by number of invoices to a CSV file
write_csv(retail_data %>%
  group_by(Country) %>%
  summarise(num_transactions = n_distinct(InvoiceNo)) %>%
  arrange(desc(num_transactions)) %>%
  head(10), "output/top_10_countries_by_invoices.csv")

# Scatterplot: Unit Price vs Quantity
ggplot(retail_data, aes(x = UnitPrice, y = Quantity)) +
  geom_point(alpha = 0.5) +
  xlim(0, 50) + ylim(0, 100) +
  ggtitle("Scatterplot: Unit Price vs Quantity")
ggsave("output/unitprice_quantity_scatterplot.png")

# Time Series Analysis: Total Sales Over Time
retail_data %>%
  mutate(InvoiceDate = as_datetime(InvoiceDate)) %>%
  mutate(Date = as.Date(InvoiceDate)) %>%
  group_by(Date) %>%
  summarise(total_sales = sum(Quantity * UnitPrice)) %>%
  ggplot(aes(x = Date, y = total_sales)) +
  geom_line(color = "steelblue") +
  ggtitle("Total Sales Over Time") +
  xlab("Date") + ylab("Sales (GBP)")

ggsave("output/sales_over_time.png")

# Top 10 Products by Revenue
retail_data %>%
  group_by(StockCode, Description) %>%
  summarise(revenue = sum(Quantity * UnitPrice)) %>%
  arrange(desc(revenue)) %>%
  head(10)

#save the top 10 products by revenue to a CSV file
write_csv(retail_data %>%
  group_by(StockCode, Description) %>%
  summarise(revenue = sum(Quantity * UnitPrice)) %>%
  arrange(desc(revenue)) %>%
  head(10), "output/top_10_products_by_revenue.csv")

# Top 10 Countries by Total Sales
retail_data %>%
  group_by(Country) %>%
  summarise(sales = sum(Quantity * UnitPrice)) %>%
  arrange(desc(sales)) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(Country, sales), y = sales)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  ggtitle("Top 10 Countries by Total Sales") +
  xlab("Country") + ylab("Sales (GBP)")

ggsave("output/top10_countries_by_sales.png")

#save the top 10 countries by total sales to a CSV file
write_csv(retail_data %>%
  group_by(Country) %>%
  summarise(sales = sum(Quantity * UnitPrice)) %>%
  arrange(desc(sales)) %>%
  top_n(10), "output/top_10_countries_by_sales.csv")

