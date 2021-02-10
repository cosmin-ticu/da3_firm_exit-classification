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
library(viridis)

# add colours and load functions
source("code/helper/da_helper_functions.R")
color <- c(brewer.pal( 3, "Set2" )[1], brewer.pal( 3, "Set2" )[2], brewer.pal( 3, "Set2" )[3], brewer.pal( 3, "Set2" )[5])

data_in <- "data/clean/"

output <- "output/"

# import data -------------------------------------------------------------

data <- read_rds(paste0(data_in,"bisnode_firms_clean.rds"))

#########################
# MODEL SETUP
#########################

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
          "flag_miss_labor_avg", "previous_growth")

# check interactions
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

## lists for models with predictors 

X1 <- c("ind2_cat", "inc_bef_tax_pl", "age", "m_region_loc", "previous_growth") # simplest one
X2 <- c(rawvars, ceo, firm) # all raw vars
X3 <- c(rawvars_ln, ceo, firm) # switch to ln for financial data
X4 <- c(rawvars_ln, ceo, firm, interactions_fact_cont_ln) # ln models w/ partial interactions
X5 <- c(rawvars_ln, ceo, firm, interactions_fact_cont_ln, int_all_ln) # ln models w/ all interactions
X6 <- c(engvar, ceo, firm, engvar2)
X7 <- c(engvar, ceo, firm, engvar2, engvar3) # baseline engineered model (no interactions)
X8 <- c(engvar, ceo, firm, engvar2, engvar3, interactions_fact_cont_wins)
X9 <- c(engvar, ceo, firm, engvar2, engvar3, interactions_fact_cont_wins, int_all_wins)

# for LASSO
lasso_vars <- X9

# for RF (no interactions, no modified features)
rf_vars  <-  c(rawvars, ceo, firm)

# Check simplest model X1
glm_modelx1 <- glm(formula(paste0("fast_growth ~", paste0(X1, collapse = " + "))),
                   data = data, family = "binomial")
summary(glm_modelx1)


# Check model X2
glm_modelx2 <- glm(formula(paste0("fast_growth ~", paste0(X2, collapse = " + "))),
                   data = data, family = "binomial")
summary(glm_modelx2)

#calculate average marginal effects (dy/dx) for logit
mx2 <- margins(glm_modelx2)

sum_table <- summary(glm_modelx2) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(mx2)[,c("factor","AME")])

kable(x = sum_table, format = "html", digits = 3,
      col.names = c("Variable", "Coefficient", "dx/dy"),
      caption = "Marginal effects of baseline raw logit model") %>%
  cat(.,file= paste0(output,"logit_X2.html"))

knitr::kable( sum_table, caption = "Marginal effects of baseline raw logit model", digits = 2 ) %>% kable_styling( position = "center", latex_options = 'hold_position' )


# baseline model is X7 based on feature engineering (all vars, but no interactions) -------------------------------------------------------

glm_modelx7 <- glm(formula(paste0("fast_growth ~", paste0(X7, collapse = " + "))),
                 data = data, family = "binomial")
summary(glm_modelx7)

#calculate average marginal effects (dy/dx) for logit
# vce="none" makes it run much faster, here we do not need variances

m <- margins(glm_modelx7, vce = "none")

sum_table2 <- summary(glm_modelx7) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate, `Std. Error`) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(m)[,c("factor","AME")])

kable(x = sum_table2, format = "html", digits = 3,
      col.names = c("Variable", "Coefficient", "SE", "dx/dy"),
      caption = "Marginal effects of baseline engineered logit model") %>%
  cat(.,file= paste0(output,"logit_X7.html"))

knitr::kable( sum_table2, caption = "Marginal effects of baseline engineered logit model", digits = 2 ) %>% kable_styling( position = "center", latex_options = 'hold_position' )

#########################
# PREDICT PROBABILITIES
#########################

# separate datasets -------------------------------------------------------

set.seed(13505)

train_indices <- as.integer(createDataPartition(data$fast_growth, p = 0.8, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

Hmisc::describe(data$fast_growth)
Hmisc::describe(data_train$fast_growth)
Hmisc::describe(data_holdout
                $fast_growth)
# all partitions have comparable distributions

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)


# Train Logit Models ----------------------------------------------

describe(data$fast_growth_f)

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5,
                         "X6" = X6, "X7" = X7, "X8" = X8, "X9" = X9)

CV_RMSE_folds <- list()
logit_models <- list()

for (model_name in names(logit_model_vars)) {
  
  features <- logit_model_vars[[model_name]]
  
  set.seed(13505)
  glm_model <- train(
    formula(paste0("fast_growth_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )
  
  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]
  
}

# Logit lasso -----------------------------------------------------------

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(13505)
system.time({
  logit_lasso_model <- train(
    formula(paste0("fast_growth_f ~", paste0(lasso_vars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))
write.csv(lasso_coeffs, paste0(output, "lasso_logit_coeffs.csv"))

CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]


# Probability forest ------------------------------------------------------

# 5 fold cross-validation
train_control$verboseIter <- TRUE

# set tuning parameters
tune_grid_rf <- expand.grid(
  .mtry = c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = c(10, 15)
)

set.seed(13505)
rf_model_p <- train(
  formula(paste0("fast_growth_f ~", paste0(rf_vars, collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid_rf,
  trControl = train_control
)

# save model
saveRDS(rf_model_p, paste0(output,'rf_model.rds'))

rf_model_p$results

best_mtry <- rf_model_p$bestTune$mtry # 7
best_min_node_size <- rf_model_p$bestTune$min.node.size # 15

# add model to list
logit_models[["Random Forest"]] <- rf_model_p

# calculate RMSE
CV_RMSE_folds[["Random Forest"]] <- rf_model_p$resample[,c("Resample", "RMSE")]

# Draw ROC Curve and calculate AUC for each folds for each model --------------------------------
CV_AUC_folds <- list()

for (model_name in names(logit_models)) {
  
  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$yes_fast_growth)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }
  
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))
}

# For each model: average RMSE and average AUC for models ----------------------------------

CV_RMSE <- list()
CV_AUC <- list()
CV_RMSE_folds[['Random Forest']]$RMSE
for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

# We have 11 models, (9 logit, 1 logit lasso and 1 random forest). For each we have a 5-CV RMSE and AUC.
# We pick our preferred model based on that. -----------------------------------------------

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

model_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE),
                             "CV AUC" = unlist(CV_AUC))

kable(x = model_summary1, format = "html", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors","CV RMSE","CV AUC")) %>%
  cat(.,file= paste0(output, "model_summary1.html"))

knitr::kable( model_summary1, caption = "Performance of all models", digits = 2 ) %>% kable_styling( position = "center", latex_options = 'hold_position' )


# Take best model (RF) and estimate RMSE on holdout  -------------------------------------------

best_model_no_loss <- logit_models[["Random Forest"]]

rf_predicted_probabilities_holdout <- predict(best_model_no_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_model_no_loss_pred"] <- rf_predicted_probabilities_holdout[,"yes_fast_growth"]
RMSE(data_holdout[, "best_model_no_loss_pred", drop=TRUE], data_holdout$fast_growth)

# continuous ROC on holdout with best model (Random Forest) -------------------------------------------

roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout$best_model_no_loss_pred)

createRocPlot(roc_obj_holdout, "ROC curve for best model (RF)")

# Bias and calibration curve -----------------------------------------------------------
# bias = mean(prediction) - mean(actual)
bias_holdout <- mean(data_holdout$best_model_no_loss_pred) - mean( data_holdout$fast_growth )

# plot calibration curve
create_calibration_plot(data_holdout, 
                        prob_var = "best_model_no_loss_pred", 
                        actual_var = "fast_growth",
                        n_bins = 10)

#########################
# CLASSIFICATION
#########################

# define a loss function
# we want to invest in fast growing firms
# FP: bad investment
# FN: foregone business opportunity
FP <- 2
FN <- 5
cost <- FN/FP

# the proportion of TP cases
prevalence <- sum(data_train$fast_growth)/length(data_train$fast_growth)

# Draw ROC Curve and find optimal threshold with loss function --------------------------

best_tresholds <- list()
expected_loss <- list()
models_cv_rocs <- list()
models_cv_threshold <- list()
models_cv_expected_loss <- list()

for (model_name in names(logit_models)) {

  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {

    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$yes_fast_growth)
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevalence))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$yes_fast_growth)
  }
  
  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  expected_loss[[model_name]] <- mean(unlist(expected_loss_cv))
  
  # for fold #5
  models_cv_rocs[[model_name]] <- roc_obj
  models_cv_threshold[[model_name]] <- best_treshold
  models_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]
  
}

model_summary2 <- data.frame("Avg of optimal thresholds" = unlist(best_tresholds),
                             "Threshold for Fold5" = sapply(models_cv_threshold, function(x) {x$threshold}),
                             "Avg expected loss" = unlist(expected_loss),
                             "Expected loss for Fold5" = unlist(models_cv_expected_loss))

kable(x = model_summary2, format = "html", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Avg of optimal thresholds","Threshold for fold #5",
                                  "Avg expected loss","Expected loss for fold #5")) %>%
  cat(.,file= paste0(output, "model_summary2.html"))

knitr::kable( model_summary2, caption = "Best thresholds based on expected loss for all models", digits = 2 ) %>% kable_styling( position = "center", latex_options = 'hold_position' )


# Create plots based on Fold5 in CV ----------------------------------------------

  model_name <- 'Random Forest'
  r <- models_cv_rocs[[model_name]]
  best_coords <- models_cv_threshold[[model_name]]
  createLossPlot(r, best_coords,
                 "Loss plot for RF Fold 5")
  createRocPlotWithOptimal(r, best_coords,
                           "ROC curve for RF Fold 5")

# Pick best model based on average expected loss ----------------------------------

best_model_with_loss <- logit_models[["Random Forest"]]
best_model_optimal_treshold <- best_tresholds[["Random Forest"]]

rf_predicted_probabilities_holdout <- predict(best_model_with_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_model_with_loss_pred"] <- rf_predicted_probabilities_holdout[,"yes_fast_growth"]

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout[, "best_model_with_loss_pred", drop=TRUE])

# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_model_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fast_growth)
expected_loss_holdout

# Confusion table on holdout with optimal threshold
holdout_prediction <-
  ifelse(data_holdout$best_model_with_loss_pred < best_model_optimal_treshold, "no_fast_growth", "yes_fast_growth") %>%
  factor(levels = c("no_fast_growth", "yes_fast_growth"))
cm_object <- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)
cm <- cm_object$table
cm

knitr::kable( cm, caption = "Confusion matrix for best model RF", digits = 2 ) %>% kable_styling( position = "center", latex_options = 'hold_position' )

