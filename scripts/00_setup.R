# ------------------------------------------------------------------------------
# Script Name: 00_setup.R
# Purpose: Install required packages for the Market Basket Analysis project
# Author: Jayampathy Balasuriya
# ------------------------------------------------------------------------------

required_packages <- c("tidyverse", "lubridate", "arules", "arulesViz")

# Install missing packages only
installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(required_packages[!installed])
}

