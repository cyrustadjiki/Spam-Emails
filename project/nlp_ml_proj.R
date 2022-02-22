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

# Removing stopwords
email_corpus <- tm_map( email_corpus, 
                       removeWords, stopwords( "en" ) )

# OPTIONAL -- Removing two most frequent stopwords: "NUMBER", "URL"
# email_corpus <- tm_map( email_corpus, 
#                        removeWords, c("NUMBER", "url", "urI", "URL") )

# OPTIONAL -- Removing extra white space
# email_corpus <- tm_map( email_corpus,
#                         stringr::str_squish)

# DocumentTermMatrix(): tokenize the email corpus.
email_dtm <- tm::DocumentTermMatrix(email_corpus)



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
# SPLITS
##############################################################################

































































