# Running data cleaning and prep
# source("nlp_ml_proj.R")
gc()

##############################################################################
# Running Log Reg
##############################################################################



# Removing scientific notation
options(scipen = 99999)

# Adding spam vs ham to train data
email_train <- cbind( email_train, email_train_label )

#
write.csv(email_train, "test.csv")

# 
email_train <- fread("test.csv") %>% data.frame()

# 0 and 1 for dummy instead of 1 and 2
email_train$email_train_label <- ifelse(email_train$email_train_label == 1, 0, 1)

# Subsetting the data: removing bad explanatory variables
# email_train <- select( email_train, -c( 
                                        # conftest, 
                                        # atol,
                                        # afnumberdecor, 
                                        # apg,
                                        # eneen, 
                                        # cpp, 
                                        # enenkio
                                        # hermio, 
                                        # kio 
                                        # ) )


# Set seed
set.seed(9753)


# converting outcome variable into character vector
email_train <- email_train %>% 
                        mutate(
                          outcome_as_vector = ifelse(email_train_label == 1, "Yes", "No")
                        ) 

# Split for 5-fold cross-validation
folds <- vfold_cv(email_train, v = 5)

# small sample of data 
# sample <- email_train[, ]

# Defining the recipe
data_recipe <- recipe(
                  outcome_as_vector ~ ., 
                  data = email_train 
                    ) %>% 
                  step_rm(email_train_label) %>% 
                  update_role(V1, new_role = 'id variable') %>%
                  # step_num2factor( email_train_label ) %>% 
                  step_dummy(all_nominal(), - all_outcomes()) %>%
                  step_zv(all_predictors()) %>%
                  step_normalize(all_predictors())


# Define the model
model_logistic <- logistic_reg(
                    mode = 'classification') %>%    # Simple LogReg bc no penalty
                    set_engine('glm')

# Workflow
workflow_logistic <- workflow() %>% 
                      add_recipe(data_recipe) %>% 
                      add_model(model_logistic)

# Cross-Validation
cv_logistic <- workflow_logistic %>%
                fit_resamples(
                  resamples = folds,
                  metrics = metric_set(accuracy, roc_auc, sens, spec, precision)
                )

# Visualizing output
cv_logistic %>% collect_metrics() %>% 
      select(.metric, mean)


# # A tibble: 5 × 2
# .metric     mean
# <chr>      <dbl>
# 1 accuracy  0.798 
# 2 precision 0.832 
# 3 roc_auc   0.511 
# 4 sens      0.947 
# 5 spec      0.0710
 












