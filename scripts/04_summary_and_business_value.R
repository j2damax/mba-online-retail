# ------------------------------------------------------------------------------
# Script Name: 04_summary_and_business_value.R
# Purpose: Generate business summaries for top association rules per country
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(stringr)
library(glue)

# Step 1: Load cleaned retail dataset for StockCode → Description mapping
retail_data <- read_csv("output/cleaned_records.csv")
product_lookup <- retail_data %>%
  select(StockCode, Description) %>%
  distinct()

# Step 2: Read exported Apriori rules CSVs
rules_country1 <- read_csv("output/rules_country1.csv")
rules_country2 <- read_csv("output/rules_country2.csv")
rules_country3 <- read_csv("output/rules_country3.csv")

# Step 3: Function to extract and map StockCodes to product descriptions
convert_codes_to_names <- function(rule_str) {
  codes <- str_extract_all(rule_str, "\\b[0-9A-Za-z]+\\b")[[1]]
  descriptions <- product_lookup$Description[match(codes, product_lookup$StockCode)]
  descriptions <- ifelse(is.na(descriptions), codes, descriptions)
  paste(descriptions, collapse = ", ")
}

# Step 4: Business summary generator
generate_summary <- function(lhs, rhs, confidence, lift, support) {
  glue(
    "Customers who purchase **{lhs}** are also likely to purchase **{rhs}** ",
    "(Confidence: {round(confidence * 100, 1)}%, Lift: {round(lift, 2)}, Support: {round(support * 100, 2)}%). "
  )
}

# Step 5: Summarize rules for a single country
summarise_rules <- function(df, country_label) {
  df %>%
    separate(rules, into = c("lhs_raw", "rhs_raw"), sep = " => ") %>%
    mutate(
      lhs_named = sapply(lhs_raw, convert_codes_to_names),
      rhs_named = sapply(rhs_raw, convert_codes_to_names),
      Country = country_label,
      Summary = mapply(generate_summary, lhs_named, rhs_named, confidence, lift, support)
    ) %>%
    select(Country, lhs_named, rhs_named, support, confidence, lift, Summary)
}

# Step 6: Apply to each country
summary_country1 <- summarise_rules(rules_country1, "United Kingdom")
summary_country2 <- summarise_rules(rules_country2, "Germany")
summary_country3 <- summarise_rules(rules_country3, "France")

# Step 7: Combine and export all summaries
final_summary <- bind_rows(summary_country1, summary_country2, summary_country3)
write_csv(final_summary, "output/business_summary_by_country.csv")

# Preview first few rows
print(head(final_summary, 5))

# Save the summary to a text file for easy reading
write_lines(
  final_summary$Summary,
  "output/business_summary_by_country.txt"
)

