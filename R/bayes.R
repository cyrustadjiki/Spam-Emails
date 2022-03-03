# Running data cleaning and prep
source("nlp_ml_proj.R")
gc()


##############################################################################
# Running Bayes Classifier from e1071 package
##############################################################################



#Create model from the training dataset
bayes_classifier <- e1071::naiveBayes( email_train, 
                                       email_train_label )



##############################################################################
# Evaluating Performance
##############################################################################


# Predicting on test data
email_test_pred <- predict( bayes_classifier, 
                            email_test )



# Creating confusion matrix
confusionMatrix( data = email_test_pred, 
                 reference = email_test_label,
                 positive = "1", 
                 dnn = c("Prediction", "Actual") )



##############################################################################
# Output
##############################################################################


# Confusion Matrix and Statistics
# 
#           Actual
# Prediction   0   1
#          0 452  82
#          1  58   9
# 
#                 Accuracy : 0.7671          
#                   95% CI : (0.7312, 0.8003)
#      No Information Rate : 0.8486          
#      P-Value [Acc > NIR] : 1.00000         
# 
#                    Kappa : -0.0166         
# 
#   Mcnemar's Test P-Value : 0.05191         
#                                           
#              Sensitivity : 0.09890         
#              Specificity : 0.88627         
#           Pos Pred Value : 0.13433         
#           Neg Pred Value : 0.84644         
#               Prevalence : 0.15141         
#           Detection Rate : 0.01498         
#     Detection Prevalence : 0.11148         
#        Balanced Accuracy : 0.49259         
#                                           
#         'Positive' Class : 1    




 