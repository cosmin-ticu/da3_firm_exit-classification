#### SET UP

# clear memory
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
library(lubridate)
library(RColorBrewer)

# add colours and load functions
source("code/helper/da_helper_functions.R")
color <- c(brewer.pal( 3, "Set2" )[1], brewer.pal( 3, "Set2" )[2], brewer.pal( 3, "Set2" )[3], brewer.pal( 3, "Set2" )[5])

data_in <- "data/raw/"

output <- "output/"

data_out <- "data/clean/"
###########################################################
# Import data
###########################################################

# data <- read_csv(paste0(data_in,"cs_bisnode_panel.csv"))
github_link <- 'https://raw.githubusercontent.com/cosmin-ticu/da3_firm_exit-classification/main/data/raw/cs_bisnode_panel.csv'
data <- read_csv(github_link)

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

data <- data %>% mutate(sales = ifelse(year == 2011 & sales == 0, 1, sales))

data <- data %>%
  group_by(comp_id) %>% 
  mutate(previous_growth = (lag(sales, 2)/lag(sales, 3) - 1) * 100)

# keep 2013 to 2014 growth rate based on sales; keep 2014 values for other variables
# keep only growing firms and exclude those that had 0 sales in 2013
# growth rate is aligned to fiscal year changes
data  <- data %>%
  filter(year == 2014) %>% 
  filter(pct_change > 0) %>% 
  filter(!pct_change == Inf)
# check duplicates
describe(data$comp_id)

describe(data$pct_change)

# fast growth is equivalent to doubling sales year-on-year
data <- data %>%
  group_by(comp_id) %>%
  mutate(fast_growth = (pct_change > 100) %>%
           as.numeric(.)) %>%
  ungroup()

table(data$fast_growth)


# sample design -------------------------------------------------------

describe(data$sales)

# keep companies whose sales are between the 5th and the 95th percentile
data <- data %>% filter(sales > 5582 & sales < 2045845 )

###########################################################
# Feature engineering
###########################################################

# create age variable
data <- data %>%
  mutate(age = (2014 - year(founded_date))) %>% 
  filter(age > 0)

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )

table(data$ind2_cat)

# firm characteristics
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))

# histogram for
h1 <- ggplot( data = data, aes( x = curr_assets ) ) +
  geom_histogram( fill = color[1]) +
  labs( x='\n Current assets', y="",
        title= 'Distribution of current assets by firm (2014)') +
  theme_light() +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) +
  scale_x_continuous(limits = c(-10, 1000000)) +
  scale_y_continuous(limits = c(0, 3000))
h2 <- ggplot( data = data, aes( x = extra_inc ) ) +
  geom_histogram( fill = color[2]) +
  labs( x='\n Extra income', y="",
        title= 'Distribution of extra income by firm (2014)') +
  theme_light() +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) +
  scale_x_continuous(limits = c(-10, 50000)) +
  scale_y_continuous(limits = c(0, 250))
h3 <- ggplot( data = data, aes( x = material_exp ) ) +
  geom_histogram( fill = color[3]) +
  labs( x='\n Material expenditure', y="",
        title= 'Distribution of material expenditure by firm (2014)') +
  theme_light() +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) +
  scale_x_continuous(limits = c(-10, 1000000))
h4 <- ggplot( data = data, aes( x = inventories ) ) +
  geom_histogram( fill = color[1]) +
  labs( x='\n Inventories', y="",
        title= 'Distribution of inventories by firm (2014)') +
  theme_light() +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) ) +
  scale_x_continuous(limits = c(-10, 100000)) +
  scale_y_continuous(limits = c(0, 2000))

sum_histograms <- plot_grid(h1, h2, h3, h4, nrow=2, ncol=2)
sum_histograms

# add the logs of these variables
ln_vars <- c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
             "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
             "profit_loss_year", "share_eq", "subscribed_cap")

# add logs and replace with 1 if it is below or equal to 0
data <- data %>% 
  mutate_at(vars(ln_vars), funs("log" = ifelse( . <= 0, NA, log(.))))

ln_vars2 <- NULL
for (i in ln_vars){
  new <- paste0(i, "_log")
  ln_vars2 <- c(ln_vars2, new)
}

# replace 1s with half of the minimum value of the given column
data <- data %>% 
  mutate_at(vars(ln_vars2), funs(ifelse(is.na(.), min(., na.rm = T)/2, .)))

###########################################################
# look at more financial variables, create ratios
###########################################################

# assets can't be negative but only one observation has that so drop it
data <- data %>% filter(intang_assets >= 0)

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)


pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), funs("pl"=./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), funs("bs"=ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))

########################################################################
# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))


# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))

########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .) %>%
           ifelse(is.na(.), mean(., na.rm = TRUE), .),
         ceo_young = as.numeric(ceo_age < 40))

# number emp, very noisy measure
data <- data %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), mean(labor_avg, na.rm = TRUE), labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(data$labor_avg)
summary(data$labor_avg_mod)

data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))

data <- data %>%
  mutate(fast_growth_f = factor(fast_growth, levels = c(0,1)) %>%
           recode(., `0` = 'no_fast_growth', `1` = "yes_fast_growth"))

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign))

# drop missing
data <- data %>%
  filter( !is.na(material_exp_pl), !is.na(m_region_loc))
Hmisc::describe(data$age)

# drop exit_date and birth_year
data <- data %>% select(-c('birth_year', 'exit_date'))

# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))

# impute & drop values for previous_growth variable
data <- data %>% filter(!comp_id == 24779958272) # faulty data for these companies (missing 2011/2012 sales)
data <- data %>% filter(!comp_id == 460263849984) # faulty data for these companies (missing 2011/2012 sales)

# impute values
data <- data %>%
  mutate(flag_previous_growth = ifelse(is.na(previous_growth) | is.nan(previous_growth), 1, 0 ),
         previous_growth = ifelse(is.na(previous_growth) | is.nan(previous_growth), 0, previous_growth ))

# final checkup for missing values
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# plot fast growth probability distribution across income
fg_inc<-ggplot(data = data, aes(x=inc_bef_tax_pl, y=as.numeric(fast_growth))) +
  geom_point(size=0.1,  shape=20, stroke=2, fill=color[2], color=color[2]) +
  geom_smooth(method="loess", se=F, colour=color[1], size=1.5, span=0.9) +
  labs(x = "Standardized income before tax",y = "Fast growth", title="Fast growth probability distribution across standardized income") +
  theme_bw() +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) )
fg_inc

# standardize original income variable
data <- data %>% mutate( inc_bef_tax_std = inc_bef_tax / sales)

# plot original income variable vs the winsorized one 
wins_inc<-ggplot(data = data, aes(x=inc_bef_tax_std, y=inc_bef_tax_pl)) +
  geom_point(size=0.1,  shape=20, stroke=2, fill=color[2], color=color[2]) +
  labs(x = "Income before tax (original)",y = "Income before tax (winsorized)", title = "Adding a cap to standardized income before tax") +
  theme_bw() +
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10, 5)) +
  theme( panel.grid.minor.x = element_blank(), 
         plot.title = element_text( size = 12, face = "bold", hjust = 0.5 ) )
wins_inc

# export data to csv and rds
write_csv(data,paste0(data_out,"bisnode_firms_clean.csv"))
write_rds(data,paste0(data_out,"bisnode_firms_clean.rds"))