##############################################################################
# Load packages
##############################################################################



library( pacman )
p_load( fastverse, magrittr, here, skimr, dplyr, ggplot2, ggthemes, equatiomatic,
        gridExtra, caret, naivebayes, knitr, kableExtra, shiny, data.table, 
        tidymodels, ggthemes, wordcloud, tm, SnowballC, RColorBrewer, e1071,
        data.table, stringr
        )



##############################################################################
# Load Data
##############################################################################



# Loading email dataset: consists of "text data" and "dummy variable"
email_df <- data.table::fread( "spam_or_not_spam.csv", header = T )

# Randomizing data
set.seed(46234)
email_df <- email_df[ sample( 1:nrow( email_df ) ),  ]

# Factoring dummy variable
email_df$label <- factor( email_df$label )



##############################################################################
# Pre Processing & Cleaning (stemming)
##############################################################################



# To retrieve the root of a word (eg, doing -> do), options are "stemming" &
# "lemmatization".

# STEMMING:      faster but maybe not as effective
# LEMMATIZATION: slower but more effective
# More on this:  towardsdatascience.com/stemming-vs-lemmatization-2daddabcb221


# VectorSource(): creates one document for each email 
# Vcorpus():      creates a volatile corpus from these individual emails
email_corpus <- VCorpus(
                  VectorSource(
                      email_df$email
                      )
                  )

# Using `tm` package to stem email content
email_corpus <- tm::tm_map( email_corpus, 
                            tm::stemDocument )
# Removing puctuations
email_corpus = tm_map( email_corpus, 
                       removePunctuation )

# Removing stopwords
email_corpus <- tm_map( email_corpus, 
                       removeWords, stopwords( "en" ) )

# OPTIONAL -- Removing two most frequent stopwords: "NUMBER", "URL"
# email_corpus <- tm_map( email_corpus, 
#                        removeWords, c("NUMBER", "url", "URL") )

# OPTIONAL -- Removing extra white space
# email_corpus <- tm_map( email_corpus,
#                         stringr::str_squish)

# DocumentTermMatrix(): tokenize the email corpus.
email_dtm <- tm::DocumentTermMatrix( sample( email_corpus, length( email_corpus ) ) )



##############################################################################
# Visualizing cleaned data
##############################################################################



# new data.frame with cleaned data
reverse_email <- data.frame(
                text = sapply( email_corpus, as.character ), 
                stringsAsFactors = FALSE, 
                type = email_df$label
                )

# Visualing text data after cleaning and pre-processing
wordcloud( reverse_email$text, 
           max.words = 150, 
           colors = brewer.pal( 7, "Dark2" ), 
           random.order = FALSE ) 




##############################################################################
# Subsetting SPAM vs. NON-SPAM data; checking most frequent words there
##############################################################################

 

# Subsetting to spam == 1
spam <- reverse_email %>% filter( type == 1 )

# Visualing spam data
wordcloud( spam$text, 
           max.words = 150, 
           colors = brewer.pal( 7, "Dark2" ), 
           random.order = FALSE ) 



# Subsetting to spam == 1
ham <- reverse_email %>% filter( type == 0 )

# Visualing spam data
wordcloud( ham$text, 
           max.words = 150, 
           colors = brewer.pal( 7, "Dark2" ), 
           random.order = FALSE ) 



##############################################################################
# SPLITTING
##############################################################################



# Data is sorted randomly, so it's easy to split Testing vs. Training
email_dtm_train <- email_dtm[   1:2400, ]         # 80% training
email_dtm_test  <- email_dtm[2400:3000, ]         # 20% testing

# Adding labels for convenience
email_train_label <- email_df[   1:2400, ]$label
email_test_label  <- email_df[2400:3000, ]$label

### checking that data is split proportionally
## prop.table( table( email_train_label ) )
#         0         1 
#      0.83      0.17 
#
## prop.table( table( email_test_label ) )
#         0         1 
# 0.8469218 0.1530782 



##############################################################################
# TRIMMING: reducing number of features | currently 25,050 (too many!)
##############################################################################



# Defining threshold (eg. 1 == 1%)
# Goal: Eliminate words that appear in __% of records in the training data
min_freq <- round( 
                email_dtm$nrow * ( ( threshold = 0.8 ) / 100 ),     # using 0.8%
                0 
              ) 

# Create vector of most frequent words
freq_words <- findFreqTerms( x = email_dtm, 
                             lowfreq = min_freq )

# Filter the DTM
email_dtm_freq_train <- email_dtm_train[ , freq_words]
email_dtm_freq_test  <- email_dtm_test[ , freq_words]



##############################################################################
# Final, cleaned, prepared dataset for Naive Bayes
##############################################################################



# Simple dummy transformation fn.
convert_values <- function(x){
                    x = ifelse( x > 0, "Yes", "No" )
                  }

# Declaring final `train` and `test` datasets
email_train <- apply( email_dtm_freq_train, MARGIN = 2,
                      convert_values )

email_test  <- apply( email_dtm_freq_test, MARGIN = 2,
                      convert_values )



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











