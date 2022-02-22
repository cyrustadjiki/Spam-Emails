##############################################################################
# Load packages
##############################################################################

library( pacman )
p_load( fastverse, magrittr, here, skimr, dplyr, ggplot2, ggthemes, equatiomatic,
        gridExtra, caret, naivebayes, knitr, kableExtra, shiny, data.table, 
        tidymodels, ggthemes, wordcloud, tm, SnowballC, RColorBrewer, e1071,
        data.table
        )


##############################################################################
# Load Data
##############################################################################


# Loading email dataset: consists of "text data" and "dummy variable"
email_df <- data.table::fread( "spam_or_not_spam.csv", header = T )

# Factoring dummy variable
email_df$label <- factor( email_df$label )


##############################################################################
# Stem Document Tranformation
##############################################################################


# To retrieve the root of a word (eg, doing -> do), options are "stemming" &
# "lemmatization".

# STEMMING:      faster but maybe not as effective
# LEMMATIZATION: slower but more effective
# More on this:  https://towardsdatascience.com/stemming-vs-lemmatization-2daddabcb221

##############################################################################

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

# DocumentTermMatrix(): tokenize the email corpus.
sms_dtm <- tm::DocumentTermMatrix(email_corpus)






















