# Running data cleaning and prep
# source("nlp_ml_proj.R")
gc()


##############################################################################
# Running Lasso Log Reg
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


# Set seed
set.seed(9753)


# converting outcome variable into character vector
email_train <- email_train %>% 
                  mutate(
                    outcome_as_vector = ifelse(email_train_label == 1, "Yes", "No")
                  ) 

# Split for 5-fold cross-validation
folds <- vfold_cv(email_train, v = 5)

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

# Defining Lambdas (from Lecture 005)
lambdas <- data.frame( penalty = c( 0, 10^seq( from = 5, to = -2, length = 100 ) ) )

# Defining Lasso Model
log_lasso <- logistic_reg(
            mode = 'classification',
            penalty = tune(), 
            mixture = 1) %>% 
            set_engine("glmnet")

# Setting up workflow
workflow_lasso <- workflow() %>%
                    add_recipe( data_recipe ) %>%
                    add_model( log_lasso )

# Parallelize 
# doParallel::registerDoParallel(cores = 4)

# CV
log_lasso_cv <- workflow_lasso %>% 
                tune_grid(
                  resamples = folds,
                  metrics = metric_set(accuracy, roc_auc, sens, spec, precision),
                  grid = grid_latin_hypercube(penalty(), size = 5),
                  # grid = grid_latin_hypercube(penalty() , mixture(), size = 20),
                  control = control_grid(parallel_over = 'resamples')
                )

# Find best models            ( source: juliasilge.com/blog/lasso-the-office/ )
log_lasso_cv %>% collect_metrics() %>% arrange(mean) %>%
                ggplot(aes(penalty, mean, color = .metric)) +
                geom_errorbar(aes(
                  ymin = mean - std_err,
                  ymax = mean + std_err
                ),
                alpha = 0.5
                ) +
                geom_line(size = 1.5) +
                facet_wrap(~.metric, scales = "free", nrow = 3) +
                scale_x_log10() +
                theme(legend.position = "none") + theme_base() #+ xlim(0, 0.1)


log_lasso_cv %>% collect_metrics() %>% group_by(.metric) %>% summarize(mean_accuracy = mean(mean, na.rm = T))



# # A tibble: 5 × 2
# .metric   mean_accuracy
# <chr>             <dbl>
# 1 accuracy         0.809 
# 2 precision        0.831 
# 3 roc_auc          0.508 
# 4 sens             0.967 
# 5 spec             0.0398












































