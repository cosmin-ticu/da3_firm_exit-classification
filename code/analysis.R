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
library(stats)

# add colours and load functions
source("code/gabor_textbook/da_helper_functions.R")
color <- c(brewer.pal( 3, "Set2" )[1], brewer.pal( 3, "Set2" )[2], brewer.pal( 3, "Set2" )[3], brewer.pal( 3, "Set2" )[5])

data_in <- "data/clean/"

output <- "output/"

# import data -------------------------------------------------------------

data <- read_rds(paste0(data_in,"bisnode_firms_clean.rds"))


# define predictor sets --------------------------------------------------------

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "share_eq", "subscribed_cap")
rawvars_ln <- c("curr_assets_log", "curr_liab_log", "extra_exp_log", "extra_inc_log",
                "extra_profit_loss_log", "fixed_assets_log", "inc_bef_tax_log",
                "intang_assets_log", "inventories_log", "liq_assets_log", "material_exp_log",
                "personnel_exp_log", "profit_loss_year_log", "share_eq_log",
                "subscribed_cap_log")
engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
            "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")
engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE),
             grep("*flag_high$", names(data), value = TRUE),
             grep("*flag_error$", names(data), value = TRUE),
             grep("*flag_zero$", names(data), value = TRUE))
ceo <- c("gender_m", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
         "flag_miss_ceo_age", "ceo_count", "foreign_management")
firm <- c("age", "age2", "ind2_cat", "m_region_loc", "urban_m","labor_avg_mod",
          "flag_miss_labor_avg" )

# check interactions
###### our way
data %>% group_by(fast_growth, gender_m) %>% summarise(mean = mean(inc_bef_tax_pl)) %>% 
  ggplot(aes(x = as.numeric(gender_m), y = mean)) +
  geom_line(aes(color = factor(fast_growth))) +
  geom_point(aes(color = factor(fast_growth)))

data %>% group_by(fast_growth, gender_m) %>% summarise(mean = mean(inc_bef_tax_pl)) %>% 
  ggplot(aes(x = fast_growth, y = mean)) +
  geom_line(aes(color = factor(gender_m))) +
  geom_point(aes(color = factor(gender_m)))

inter <- function(data, factor_var, num_var){
  data %>% group_by(fast_growth, factor_var) %>% summarise(mean = mean(num_var)) %>% 
    ggplot(aes(x = as.numeric(factor_var), y = mean)) +
    geom_line(aes(color = factor(fast_growth))) +
    geom_point(aes(color = factor(fast_growth)))
}
#########
# plot interactions
# interaction between industry category and income before tax
i1 <- interaction.plot( x.factor = data$ind2_cat, trace.factor = data$fast_growth, response = data$inc_bef_tax_pl, ylab = 'Mean(income before tax)', xlab = "Industry category", trace.label = "Fast growth" )

# interaction between industry category and age of ceo
i2 <- interaction.plot( x.factor = data$ind2_cat, trace.factor = data$fast_growth, response = data$ceo_age, xlab = "Industry category", ylab = 'Mean(CEO age)', trace.label = "Fast growth")

# interaction between industry category and foreign management
i3 <- interaction.plot( x.factor = data$ind2_cat, trace.factor = data$fast_growth, response = data$foreign_management, xlab = "Industry category", ylab = 'Mean(share of foreign mgmt)', trace.label = "Fast growth")

# create lists of interactions
int_vars_temp_ln <- c("curr_assets_log", "curr_liab_log", "extra_exp_log", "extra_inc_log",
                      "extra_profit_loss_log", "fixed_assets_log", 
                      "intang_assets_log", "inventories_log", "liq_assets_log", "material_exp_log",
                      "personnel_exp_log", "profit_loss_year_log", "share_eq_log",
                      "subscribed_cap_log")

int_vars_temp_wins <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
                        "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
                        "extra_inc_pl", "extra_profit_loss_pl",  "inventories_pl",
                        "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")

# lists of interactions for ln models
int_all_ln <- paste0("( gender_m +  m_region_loc + urban_m + ceo_age + age + foreign_management + labor_avg_mod) * (",
       paste(int_vars_temp_ln, collapse=" + "),")")

interactions_fact_cont_ln <- c("ind2_cat*inc_bef_tax_log", "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                               "ind2_cat*urban_m")

# lists of interactions for standardized models
int_all_wins <- paste0("( gender_m +  m_region_loc + urban_m + ceo_age + age + foreign_management + labor_avg_mod) * (",
                     paste(int_vars_temp_wins, collapse=" + "),")")

interactions_fact_cont_wins <- c("ind2_cat*inc_bef_tax_pl", "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                  "ind2_cat*urban_m")
################## TODO: create lists for model with predictors 



