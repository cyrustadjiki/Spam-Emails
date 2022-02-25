# Running data cleaning and prep
# source("nlp_ml_proj.R")
gc()


##############################################################################
# Running Random Forest
##############################################################################


# Trainin Random Forest
random_forest <- train(
                  x = email_train,
                  y = email_train_label,
                  method = "ranger", 
                  num.trees = 200,
                  importance = "impurity",
                  trControl = trainControl(method = "cv", 
                                           number = 3,
                                           verboseIter = TRUE
                  )
                )

# Outputting results
random_forest

# Checking "variable importances 
varImp(random_forest, scale = TRUE)$importance %>% 
                rownames_to_column() %>% 
                arrange(-Overall) %>% 
                top_n(25) %>%
                ggplot() + 
                aes(x = reorder(rowname, Overall), y = Overall) +
                geom_col() + 
                labs(title = "Most Predictive Words") +
                coord_flip()


# Fitting to the test data
predictions <- predict(random_forest, email_test)


# Output
confusionMatrix(predictions, email_test_label)

# Confusion Matrix and Statistics
# 
#           Reference
# Prediction   0   1
#          0 509  91
#          1   1   0
# 
#                Accuracy : 0.8469          
#                  95% CI : (0.8156, 0.8748)
#     No Information Rate : 0.8486          
#     P-Value [Acc > NIR] : 0.5728          
# 
#                   Kappa : -0.0033         
# 
#  Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.9980          
#             Specificity : 0.0000          
#          Pos Pred Value : 0.8483          
#          Neg Pred Value : 0.0000          
#              Prevalence : 0.8486          
#          Detection Rate : 0.8469          
#    Detection Prevalence : 0.9983          
#       Balanced Accuracy : 0.4990          
#                                           
#        'Positive' Class : 0   
       
       

       
        
       
       
       
       
       
       
       
       
       
       