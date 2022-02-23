# Running data cleaning and prep
source("nlp_ml_proj.R")
gc()


##############################################################################
# Running Lasso
##############################################################################



# Removing scientific notation
options(scipen = 99999)

# Adding spam vs ham to train data
email_train <- cbind( email_train, email_train_label )

#
write.csv(email_train, "test.csv")

# 
email_train <- fread("test.csv") %>% data.frame()

# Splitting for 5-fold cross-validation
folds <- email_train %>% vfold_cv(v = 5)

# Defining Lambdas (from Lecture 005)
lambdas <- data.frame( penalty = c( 0, 10^seq( from = 5, to = -2, length = 100 ) ) )

# Defining the recipe
data_recipe <- recipe(
                email_train_label ~ ., 
                data = email_train 
              ) %>% 
              update_role(V1, new_role = 'id variable')

# Defining Lasso Model
lasso <- linear_reg(
          penalty = tune(), 
          mixture = 1) %>% 
          set_engine("glmnet")

# Setting up workflow
workflow_lasso <- workflow() %>%
                    add_recipe( data_recipe ) %>%
                    add_model( lasso )

# CV
lasso_cv <- workflow_lasso %>% 
              tune_grid(
                resamples = folds,
                grid = lambdas,
                metrics = metric_set(rmse, mae)
              )

# Find best models
lasso_cv %>% collect_metrics() %>% 
            head() %>%
            kbl(caption = "Model Accuracy", format = 'html') %>%
            kable_classic(
              full_width = F,
              html_font = "Cambria",
              font_size = 24
            )






























































