# Running data cleaning and prep
# source("nlp_ml_proj.R")
gc()


##############################################################################
# Running Lasso
##############################################################################



# Removing scientific notation
options(scipen = 99999)

# Adding spam vs ham to train data
email_train <- cbind( email_train, 
                      email_train_label )

#
write.csv(email_train, "test.csv")

# 
email_train <- fread("test.csv") %>% data.frame()

# 0 and 1 for dummy instead of 1 and 2
email_train$email_train_label <- ifelse(email_train$email_train_label == 1, 0, 1)

# Subsetting the data: c( conftest, atol, afnumberdecor, apg )
# email_train <- select( email_train, -c( conftest, atol, afnumberdecor, apg, 
#                                         eneen, cpp, enenkio, hermio, kio ) ) 

# Splitting for 5-fold cross-validation
folds <- email_train %>% vfold_cv(v = 5)

# Defining Lambdas (from Lecture 005)
lambdas <- data.frame( penalty = c( 0, 10^seq( from = 5, to = -2, length = 100 ) ) )

# Defining the recipe
data_recipe <- recipe(
                email_train_label ~ ., 
                data = email_train 
                ) %>% 
              update_role(V1, new_role = 'id variable')  %>% 
              step_dummy(all_nominal(), - all_outcomes()) %>%
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

# Parallelize 
doParallel::registerDoParallel(cores = 4)

# CV
lasso_cv <- workflow_lasso %>% 
              tune_grid(
                resamples = folds,
                grid = lambdas,
                metrics = metric_set(rmse, mae)
              )

# Find best models            ( source: juliasilge.com/blog/lasso-the-office/ )
lasso_cv %>% collect_metrics() %>%
            ggplot(aes(penalty, mean, color = .metric)) +
            geom_errorbar(aes(
              ymin = mean - std_err,
              ymax = mean + std_err
            ),
            alpha = 0.5
            ) +
            geom_line(size = 1.5) +
            facet_wrap(~.metric, scales = "free", nrow = 2) +
            scale_x_log10() +
            theme(legend.position = "none") + theme_base() + xlim(0, 0.1)


lasso_cv %>% collect_metrics() %>% group_by(.metric) %>% summarize(mean_accuracy = mean(mean, na.rm = T))

# # A tibble: 2 × 2
# .metric mean_accuracy
# <chr>           <dbl>
# 1 mae             0.283
# 2 rmse            0.376






 








































