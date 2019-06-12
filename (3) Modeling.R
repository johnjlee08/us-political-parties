# Final Project for 301-2
# Modeling script (model-building/validation, testing)
# John Lee

# Load packages
library(haven)
library(modelr)
library(janitor)
library(skimr)
library(leaps) # best subset selection
library(glmnet) # ridge & lasso
library(glmnetUtils) # improves working with glmnet
library(pls) # pcr and pls
library(tidyverse)


## Part 3: Model-building/Validation ---------------------------------------------------------

# Set the random seed to 3
set.seed(3)

# Load the model-building set 
modbuilding_set <- readRDS("data/processed/modbuilding_set.rds")


# Section #1: My models ------------------------------
# Notes: Based on the EDA (and my knowledge of the relevant lit), I've come up with 5 models that I want to test
# Here, I'll split the val set into two parts: I'll fit the models using the data in the first part; 
# Then, I'll compare the model performance using test MSE -- the best performing model (out of these 5) will become one of my candidate models

# Create the baseline fmla
baseline_fmla <- "strong_dem ~ year + age + race_text + female + religion + subj_classid_text + 
ln_famincome + yrs_educ + pol_liberalism + moresp_natwelfare + moresp_natdrug + moresp_natarms + 
  moresp_natenvir + moresp_natrace "

# Mod names
mod_names <- c("+ year:race_text", 
               "+ year:female",
               "+ year:ln_famincome",
               "+ year:yrs_educ",
               "+ year:pol_liberalism")

# Create the fmlas for each model
ols_fmlas <- paste0(baseline_fmla, mod_names)

# Split the validation set into two parts: (1) model_building, (2) model_val 
val_set <- tibble(model_building = modbuilding_set %>% sample_frac(0.5) %>% list(),
                  model_val = modbuilding_set %>% setdiff(model_building) %>% list(),
                  mod_names = mod_names,
                  fmla = ols_fmlas)

# Check the contents
head(val_set)

# Custom function that computes test MSE
compute_test_mse <- function(actual_values, df_with_fitted){
  pred_values <- df_with_fitted %>% pull(pred)
  mean((actual_values - pred_values)^2)
}

# Create a vector of actual outcomes (observed Y values for the val/test set)
actual_outcomes <- val_set[1,] %>% 
  unnest(model_val) %>%
  pull(strong_dem) %>%
  list()

# Generate the test MSE for each model
val_set <- val_set %>%
  # First, fit OLS models using the training set
  mutate(model_fit = map2(fmla, model_building, .f = lm),
         # Next, generate fitted values using the test set
         with_fitted = map2(model_val, model_fit, add_predictions),
         actual_outcomes = actual_outcomes,
         test_mse = map2_dbl(actual_outcomes, with_fitted, .f = compute_test_mse)
  )

# Compare the test MSE across the models
val_set %>%
  dplyr::select(mod_names, test_mse) %>%
  arrange(test_mse) %>%
  knitr::kable(digits = 3)

# Save the best mod 
best_OLS_name <- val_set %>%
  dplyr::select(mod_names, test_mse) %>%
  arrange(test_mse) %>% 
  # Add a var for rank
  mutate(rank = row_number()) %>%
# Just filter in and store the best mod (rank #1)
  filter(rank == 1) %>%
  pull(mod_names)

best_OLS_mod <- paste0(baseline_fmla, best_OLS_name)

# Check the output
best_OLS_mod


# Create a framework for using the best OLS model - and testing it with the test set 
# Do this later
 

         
# Ok, so among my 5 OLS models, the best performing one is: "+ year:pol_liberalism" --> this is the one
# That I'll advance as one of my final candidates models
      

# Section 2: Automating variable selection ------------------------------------------
# In this section, I'll be finding the optimal first-order model for each of the following five methods: 
# (1) Ridge, (2) Lasso, (3) PCR, (4) PLS, (5) stepwise forward selection. 
# I'll do this by using K-fold CV (with k = 10) to find the best tuning parameters for each method.

# Load the validation set 
validation_set <- readRDS("data/processed/validation_set.rds")

validation_set %>% skim


# (1) Find the best Ridge regression model ---------------------------

# Set up the lambda grid (200 values)
lambda_grid <- 10^seq(-2, 10, length = 200)

# Find the optimal lambda by using K-fold CV (with k = 10)
ridge_mod_10fcv <- modbuilding_set %>% 
  cv.glmnet(formula = strong_dem ~ ., data = ., 
            alpha = 0, nfolds = 10, 
            lambda = lambda_grid)

# Check plot of cv error
plot(ridge_mod_10fcv)

# Best lambdas for the Ridge mod (using 10-fold CV)
ridge_lambda_min <- ridge_mod_10fcv$lambda.min
ridge_lambda_1se <- ridge_mod_10fcv$lambda.1se

ridge_lambda_min
ridge_lambda_1se

# (2) Find the best lasso regression model ---------------------------

# lasso: 10-fold cv
lasso_mod_10fcv <- modbuilding_set %>% 
  cv.glmnet(formula = strong_dem ~ ., data = ., 
            alpha = 1, nfolds = 10, 
            lambda = lambda_grid)

# Check plot of cv error
plot(lasso_mod_10fcv)

# lasso's best lambdas
lasso_lambda_1se <- lasso_mod_10fcv$lambda.1se
lasso_lambda_min <- lasso_mod_10fcv$lambda.min

lasso_lambda_min
lasso_lambda_1se

# Save the best model per method in a tibble
partyid_glmnet <- tibble(train = modbuilding_set %>% list(),
                        test  = validation_set %>% list()) %>%
  mutate(ridge_min = map(train, ~ glmnet(strong_dem ~ ., data = .x,
                                         alpha = 0, lambda = ridge_lambda_min)),
         ridge_1se = map(train, ~ glmnet(strong_dem ~ ., data = .x,
                                         alpha = 0, lambda = ridge_lambda_1se)),
         lasso_min = map(train, ~ glmnet(strong_dem ~ ., data = .x,
                                         alpha = 1, lambda = lasso_lambda_min)),
         lasso_1se = map(train, ~ glmnet(strong_dem ~ ., data = .x,
                                         alpha = 1, lambda = lasso_lambda_1se))) %>% 
  gather(key = method, value = fit, -test, -train)

partyid_glmnet

# Inspect/compare model coefficients 
partyid_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(ridge_min = s0.x,
         ridge_1se = s0.y,
         lasso_min = s0.x.x,
         lasso_1se = s0.y.y) %>% 
  knitr::kable(digits = 3)


# (3) Find the best PCR model --------------------------------

# pcr: 10-fold cv
pcr_mod_10fcv <- modbuilding_set %>% 
  pcr(strong_dem ~ ., data = ., scale = TRUE, validation = "CV")

pcr_mod_10fcv %>% summary()

# Using root mean squared error
validationplot(pcr_mod_10fcv, val.type = "MSEP")
# The best M is probably around 40
abline(v = 40)


# (4) Find the best PLS model --------------------------------

# pls: 10-fold cv
pls_mod_10fcv <- modbuilding_set %>% 
  plsr(strong_dem ~ ., data = ., scale = TRUE, validation = "CV")

pls_mod_10fcv %>% summary()

validationplot(pls_mod_10fcv, val.type = "MSEP")
# The best M is probably around 4
abline(v = 4)

# Save the results 
partyid_pcr_pls <- tibble(train = modbuilding_set %>% list(),
                          test  = validation_set %>% list()) %>%
  mutate(pcr_40m = map(train, ~ pcr(strong_dem ~ ., data = .x, ncomp = 40)),
         pls_4m = map(train, ~ plsr(strong_dem ~ ., data = .x, ncomp = 4))) %>% 
  gather(key = method, value = fit, -test, -train)


# (5) Subset selection methods ----------------------------------

## Helper functions

# Get predicted values using regsubset object
predict_regsubset <- function(object, fmla , new_data, model_id)
{
  # Not a dataframe? -- handle resample objects/k-folds
  if(!is.data.frame(new_data)){
    new_data <- as_tibble(new_data)
  }
  
  # Get formula
  obj_formula <- as.formula(fmla)
  
  # Extract coefficients for desired model
  coef_vector <- coef(object, model_id)
  
  # Get appropriate feature matrix for new_data
  x_vars <- names(coef_vector)
  mod_mat_new <- model.matrix(obj_formula, new_data)[ , x_vars]
  
  # Get predicted values
  pred <- as.numeric(mod_mat_new %*% coef_vector)
  
  return(pred)
}

# Calculate test MSE on regsubset objects
test_mse_regsubset <- function(object, fmla , test_data){
  
  # Number of models
  num_models <- object %>% summary() %>% pluck("which") %>% dim() %>% .[1]
  
  # Set up storage
  test_mse <- rep(NA, num_models)
  
  # observed targets
  obs_target <- test_data %>% 
    as_tibble() %>% 
    pull(!!as.formula(fmla)[[2]])
  
  # Calculate test MSE for each model class
  for(i in 1:num_models){
    pred <- predict_regsubset(object, fmla, test_data, model_id = i)
    test_mse[i] <- mean((obs_target - pred)^2)
  }
  
  # Return the test errors for each model class
  tibble(model_index = 1:num_models,
         test_mse    = test_mse)
}


# 5(a) Forward selection --------------------------

# Forward selection: use 10-fold CV to select optimal number of variables
partyid_fwd_cv <- modbuilding_set %>% 
  crossv_kfold(10, id = "folds") %>% 
  mutate(fmla = "strong_dem ~ .",
         model_fits = map2(fmla, train, ~ regsubsets(as.formula(.x), 
                                                     data = .y, nvmax = 50, method = "forward")),
         model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset))

# Look at the outcomes (the best model had 48 predictors)
partyid_fwd_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarize(test_mse = mean(test_mse)) %>% 
  arrange(test_mse) %>%
  head(n = 5) %>%
  knitr::kable(digits = 3)

?knitr::kable

# 5(b) Backward selection -------------------------

# Backward selection: use 10-fold CV to select optimal number of variables
partyid_backw_cv <- modbuilding_set %>% 
  crossv_kfold(10, id = "folds") %>% 
  mutate(fmla = "strong_dem ~ .",
         model_fits = map2(fmla, train, ~ regsubsets(as.formula(.x), 
                                                     data = .y, nvmax = 50, method = "backward")),
         model_fold_mse = pmap(list(model_fits, fmla ,test), test_mse_regsubset))

# Look up the results (48 predictors is the best)
partyid_backw_cv %>% 
  unnest(model_fold_mse) %>% 
  group_by(model_index) %>%  
  summarise(test_mse = mean(test_mse)) %>% 
  arrange(test_mse) %>%
  head(n = 10) %>%
  knitr::kable(digits = 3)


# Ok, now I want to save the results in a combined regsubsets df 
#-- so that it can later be used to compute the test MSE for each "best" model

partyid_regsubsets <- tibble(train = modbuilding_set %>% list(),
                            test  = validation_set %>% list()) %>%
  mutate(fwd_selection = map(train, ~ regsubsets(strong_dem ~ ., 
                                                 data = .x, nvmax = 44, 
                                                 method = "forward")),
         back_selection = map(train, ~ regsubsets(strong_dem ~ ., 
                                                  data = .x, nvmax = 44, 
                                                  method = "backward"))) %>% 
  gather(key = method, value = fit, -test, -train)

# Since both models are using 48 predictors, are the predictors the same? We can check this below

# Inspect/compare model coefficients 
partyid_regsubsets %>% 
  pluck("fit") %>% 
  map( ~ coef(.x, id = 44)) %>% 
  map2(c("fwd", "back"), ~ enframe(.x, value = .y)) %>% 
  reduce(full_join) %>% 
  knitr::kable(digits = 3)

# Answer: it looks like different predictors were used (although most of the same predictors were used in both models)


# Part 4: Final model selection ------------------------------------------------------------

# I'll be using the validation set, which was previously set aside, to compare each of my candidate models. 
# The decision rule is to choose the one that generates the smallest test MSE (i.e., the one with the smallest test error)

# Test error for the best OLS model (I can edit this later)
ols_error <- tibble(train = modbuilding_set %>% list(),
                    test = validation_set %>% list(),
                    mod_name = best_OLS_name,
                    fmla = best_OLS_mod) %>%
  mutate(model_fit = map2(fmla, train, .f = lm),
         with_fitted = map2(test, model_fit, add_predictions)) %>%
         # Next, estimate the test error (MSE) -- note: improve this code later
         unnest(with_fitted) %>% 
           # Compute (actual - fitted)^2 
           mutate(sq_deviations = (strong_dem - pred)^2) %>% 
           # Compute the mean of the squared deviations (which = MSE)
           summarize(test_mse = mean(sq_deviations)) %>%
  pull(test_mse)

ols_error <- tibble(method = "ols", test_mse = ols_error)

# Test error for ridge and lasso fits
glmnet_error <- partyid_glmnet %>% 
  mutate(pred = map2(fit, test, predict),
         test_mse = map2_dbl(test, pred, ~ mean((.x$strong_dem - .y)^2))) %>% 
  unnest(test_mse, .drop = TRUE)

# Test error for pcr and plsr fits
dim_reduce_error <- partyid_pcr_pls %>% 
  mutate(pred = pmap(list(fit, test, c(40,4)), predict),
         test_mse = map2_dbl(test, pred, ~ mean((.x$strong_dem - .y)^2))) %>% 
  unnest(test_mse, .drop = TRUE)

# Test error for regsubset fits
regsubset_error <- partyid_regsubsets %>% 
  mutate(test_mse = map2(fit, test, ~ test_mse_regsubset(.x, strong_dem ~ ., .y))) %>% 
  unnest(test_mse) %>% 
  filter(model_index == 44) %>% 
  dplyr::select(-model_index)

# Test errors combined and organzied
ols_error %>%
  bind_rows(glmnet_error) %>% 
  bind_rows(dim_reduce_error) %>% 
  bind_rows(regsubset_error) %>% 
  arrange(test_mse) %>%
  mutate(rank = row_number()) %>% 
  dplyr::select(rank, method, test_mse) %>%
  knitr::kable(digits = 3)



# Part 5: Test the performance of my final model ----------------------------------------

# Load the final test set 
test_set <- readRDS("data/processed/test_set.rds")

# Test error for the best OLS model 
ols_error <- tibble(train = modbuilding_set %>% list(),
                    test = test_set %>% list(),
                    mod_name = best_OLS_name,
                    fmla = best_OLS_mod) %>%
  mutate(model_fit = map2(fmla, train, .f = lm),
         with_fitted = map2(test, model_fit, add_predictions)) %>%
  # Next, estimate the test error (MSE)
  unnest(with_fitted) %>% 
  # Compute (actual - fitted)^2 
  mutate(sq_deviations = (strong_dem - pred)^2) %>% 
  # Compute the mean of the squared deviations (which = MSE)
  summarize(test_mse = mean(sq_deviations)) %>%
  pull(test_mse)

tibble(model = "best_OLS", test_MSE = ols_error) %>%
  knitr::kable(digits = 3)


# Part 6: Create graphs and tables of the key results -------------------------------------

library(interplot) # to plot interaction effects -- load this here b/c it uses a select() function which
# masks the select() in tidyverse

library(broom)

# Interplot package info
# https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html

# Create a vector of original years for the plot
original_years <- validation_set %>% 
  mutate(original_years = as.numeric(year + 1973)) %>%
  pull(original_years) %>%
  unique() %>%
  sort() 

# Code to center the plot title as the default option
theme_update(plot.title = element_text(hjust = 0.5))

# Get the coefficients of the best OLS mod
best_OLS <- modbuilding_set %>% 
  lm(best_OLS_mod, data = .) 

best_OLS %>%
  tidy() %>%
  knitr::kable(digits = 3)

best_OLS %>%
  summary()

modbuilding_set %>% skim

# Create a plot of the interaction effects between pol_liberalism and year 
# use results from the best OLS model 
interplot(m = best_OLS, var1 = "pol_liberalism", var2 = "year") +
  labs(title = "Estimated Coefficient of Political Liberalism by Year", 
       x = "Number of Years Since 1973", y = "Coefficient of Political Liberalism") +
  geom_hline(yintercept = 0, linetype = "dashed")


# Quick set of diagnostic plots for the best OLS
plot(best_OLS)




