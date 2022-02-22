# Running data cleaning and prep
source("nlp_ml_proj.R")

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