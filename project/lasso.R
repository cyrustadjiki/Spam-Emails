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

# 0 and 1 for dummy instead of 1 and 2
email_train$email_train_label <- ifelse(email_train$email_train_label == 1, 0, 1)

# Subsetting the data: c( conftest, atol, afnumberdecor, apg )
email_train <- select( email_train, -c( conftest, atol, afnumberdecor, apg, 
                                        eneen, cpp, enenkio, hermio, kio ) ) 

# Splitting for 5-fold cross-validation
folds <- email_train %>% vfold_cv(v = 5)

# Defining Lambdas (from Lecture 005)
lambdas <- data.frame( penalty = c( 0, 10^seq( from = 5, to = -2, length = 100 ) ) )

# Defining the recipe
data_recipe <- recipe(
                email_train_label ~ ., 
                data = email_train 
                ) %>% 
              update_role(V1, new_role = 'id variable') %>%
              step_dummy(all_nominal()) %>%
              step_zv(all_predictors()) %>%
              step_normalize(all_predictors())

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



##############################################################################
# Visualizing Results | MAE
##############################################################################


# Filtering `lasso_cv` by mae 
lasso_cv_results_mae <- lasso_cv %>% collect_metrics() %>% filter(.metric == "mae")

# Visualizing results from `lasso_cv` MAE
ggplot( lasso_cv_results_mae, aes( x = penalty, y = mean ) ) + geom_line(  ) + 
  xlab("Lambda") + ylab("MAE") + theme_calc()  #+ geom_point(aes(x = best_lambda, y = min_mae))


##############################################################################
# Visualizing Results | RMSE
##############################################################################


lasso_cv_results_rmse <- lasso_cv %>% collect_metrics() %>% filter(.metric == "rmse")

# Visualizing results from `lasso_cv` RMSE
ggplot( lasso_cv_results_rmse, aes( x = penalty, y = mean ) ) + geom_line(  )  + 
  xlab("Lambda") + ylab("RMSE") + theme_calc() + scale_x_log10() #+ geom_point(aes(x = best_lambda, y = min_rmse))





















































