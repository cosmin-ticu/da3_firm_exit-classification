# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(tidyverse)

# load theme and functions
source("code/gabor_textbook/theme_bg.R")
source("code/gabor_textbook/da_helper_functions.R")

data_in <- "data/raw/"

output <- "output/"


###########################################################
# Import data
###########################################################

data <- read_csv(paste(data_in,"cs_bisnode_panel.csv", sep = "/"))

# drop variables with many NAs
data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages, 
            exit_year, begin, end, D, balsheet_flag, balsheet_length, 
            balsheet_notfullyear, ind, nace_main)) %>%
  filter(between(year, 2010, 2015))

describe(data$ind2)

describe(data$liq_assets)

###########################################################
# label engineering
###########################################################

data <- data %>%
  group_by(comp_id) %>% 
  mutate(pct_change = (sales/lag(sales) - 1) * 100)

# generate sales growth between 2013 and 2014
data  <- data %>%
  filter(year == 2014) %>% 
  filter(pct_change > 0) %>% 
  filter(!pct_change == Inf)
# check duplicates
describe(data$comp_id)

describe(data$pct_change)

# defaults in two years if there are sales in this year but no sales two years later
data <- data %>%
  group_by(comp_id) %>%
  mutate(fast_growth = (pct_change > 100) %>%
           as.numeric(.)) %>%
  ungroup()

table(data$fast_growth)
