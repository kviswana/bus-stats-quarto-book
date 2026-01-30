# Load packages
library(tidyverse)
library(LSTbook)
library(gt)
library(knitr)
library(kableExtra)
library(utils)

# Global options
options(
  digits = 3,
  scipen = 999,
  width = 50
)

# Load or define data
utils::data("Boston_marathon", package = "LSTbook", envir = globalenv())

price_demand <- read_csv("lst-extras-datasets/price_demand.csv", 
                         show_col_types = FALSE)

advertising_sales_channel <- read_csv("lst-extras-datasets/advertising_sales_channel.csv", 
                                      show_col_types = FALSE)


returns_dpo <- read_csv("lst-extras-datasets/returns-dpo.csv", 
                        show_col_types = FALSE)

acct_type_balance <- read_csv("lst-extras-datasets/acct_type_balance.csv", 
                              show_col_types = FALSE)

employees <- read_csv("lst-extras-datasets/employees.csv", 
                                      show_col_types = FALSE)
variance_example <- read_csv("lst-extras-datasets/variance_examples.csv", 
                             show_col_types = FALSE)

homes <- read_csv("lst-extras-datasets/homes.csv", 
                             show_col_types = FALSE)

kiosk <- read_csv("lst-extras-datasets/kiosk.csv", 
                  show_col_types = FALSE)

# Helper functions
