---
title: "Spam Emails"
author: "Cyrus Tadjiki, Zoe Arnaut, Octavio Lima"
date: "3/9/2022"
output: 
  html_document:
    theme: flatly
    highlight: monochrome 
    toc: yes
    toc_depth: 4
    toc_float: yes
    keep_md: true
# themes include default, cerulean, journal, flatly, darkly, readable, spacelab, united, cosmo, lumen, paper, sandstone, simplex, and yeti. Pass null for no theme
---



# Loading Packages

```r
library(pacman)
p_load(fastverse, magrittr, here, skimr, dplyr, 
       ggplot2, ggthemes, equatiomatic, gridExtra, 
       caret, naivebayes, knitr, kableExtra, shiny, 
       data.table, tidymodels, ggthemes, wordcloud,
       tm, SnowballC, RColorBrewer, e1071, data.table, 
       stringr, disk.frame, knitr, ranger #, randomForest
       )
```

# Loading Data

```r
# Loading email dataset: consists of "text data" and "dummy variable"
email_df <- data.table::fread( "spam_or_not_spam.csv", header = T )

# Randomizing data
set.seed(46234)
email_df <- email_df[ sample( 1:nrow( email_df ) ),  ]

# Factoring dummy variable
email_df$label <- factor( email_df$label )
```


# Pre Processing & Cleaning (stemming)
To retrieve the root of a word (eg, doing -> do), options are "stemming" &
"lemmatization".

**STEMMING:** faster but maybe not as effective
**LEMMATIZATION:** slower but more effective
**More on this:** (towardsdatascience.com/stemming-vs-lemmatization-2daddabcb221)

`VectorSource()`: creates one document for each email 
`Vcorpus()`:      creates a volatile corpus from these individual emails

```r
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
email_corpus <- tm_map( email_corpus,
                       removeWords, c("NUMBER", "number", "url", "URL") )

# OPTIONAL -- Removing extra white space
# email_corpus <- tm_map( email_corpus,
#                         stringr::str_squish)

# DocumentTermMatrix(): tokenize the email corpus.
email_dtm <- tm::DocumentTermMatrix( sample( email_corpus, length( email_corpus ) ) )
```
                            

# Visualizing cleaned data

```r
reverse_email <- data.frame(
                text = sapply( email_corpus, as.character ), 
                stringsAsFactors = FALSE, 
                type = email_df$label
                )
```

# Visualing text data after cleaning and pre-processing

```r
wordcloud( reverse_email$text, 
           max.words = 150, 
           colors = brewer.pal( 7, "Dark2" ), 
           random.order = FALSE ) 
```

```
## Warning in tm_map.SimpleCorpus(corpus, tm::removePunctuation): transformation
## drops documents
```

```
## Warning in tm_map.SimpleCorpus(corpus, function(x) tm::removeWords(x,
## tm::stopwords())): transformation drops documents
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : program could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : check could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : home could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : market could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : secur could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : subject could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : give could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : internet could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : interest could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : come could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : two could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : comput could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : someth could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : person could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : softwar could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : develop could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : site could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : com could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : nation could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : sep could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : realli could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : unsubscrib could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : network could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : unit could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : provid could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : base could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : phone could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : everi could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : thank could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : write could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : build could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : follow could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : countri could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : sinc could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : made could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : last could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : point could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : today could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : found could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : sure could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : case could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : must could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : sponsor could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : technolog could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : current could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(reverse_email$text, max.words = 150, colors =
## brewer.pal(7, : server could not be fit on page. It will not be plotted.
```

![](Spam-Emails_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


# Subsetting SPAM vs. NON-SPAM data; checking most frequent words there

## Visualizing spam data

```r
# Subsetting to spam == 1
spam <- reverse_email %>% filter( type == 1 )
wordcloud( spam$text, 
           max.words = 150, 
           colors = brewer.pal( 7, "Dark2" ), 
           random.order = FALSE,
           main = "Spam")
```

```
## Warning in tm_map.SimpleCorpus(corpus, tm::removePunctuation): transformation
## drops documents
```

```
## Warning in tm_map.SimpleCorpus(corpus, function(x) tm::removeWords(x,
## tm::stopwords())): transformation drops documents
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## guarante could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## respons could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## opportun could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## enenkio could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : repli
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : credit
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## financi could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : requir
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## thousand could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : trade
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## profession could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : onlin
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : befor
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : step
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : must
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : access
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : simpl
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : direct
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## success could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, : phone
## could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(spam$text, max.words = 150, colors = brewer.pal(7, :
## request could not be fit on page. It will not be plotted.
```

![](Spam-Emails_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

## Visualing non-spam data

```r
# Subsetting to spam == 0
ham <- reverse_email %>% filter( type == 0 )
wordcloud( ham$text, 
           max.words = 150, 
           colors = brewer.pal( 7, "Dark2" ), 
           random.order = FALSE,
           main = "Non-Spam") 
```

```
## Warning in tm_map.SimpleCorpus(corpus, tm::removePunctuation): transformation
## drops documents
```

```
## Warning in tm_map.SimpleCorpus(corpus, function(x) tm::removeWords(x,
## tm::stopwords())): transformation drops documents
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : version could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : anoth could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : servic could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : receiv could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : comput could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : code could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : provid could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : folder could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : softwar could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : support could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : without could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : post could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : septemb could not be fit on page. It will not be plotted.
```

```
## Warning in wordcloud(ham$text, max.words = 150, colors = brewer.pal(7,
## "Dark2"), : probabl could not be fit on page. It will not be plotted.
```

![](Spam-Emails_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

# Splitting Data

# Data is sorted randomly, so it's easy to split Testing vs. Training

```r
email_dtm_train <- email_dtm[   1:2400, ]         # 80% training
email_dtm_test  <- email_dtm[2400:3000, ]         # 20% testing
# Adding labels for convenience
email_train_label <- email_df[   1:2400, ]$label
email_test_label  <- email_df[2400:3000, ]$label
```




checking that data is split proportionally
prop.table( table( email_train_label ) )
 0         1 
0.83      0.17 

prop.table( table( email_test_label ) )
0         1 
0.8469218 0.1530782 



TRIMMING: reducing number of features | currently 25,050 (too many!)

Defining threshold (eg. 1 == 1%)
Goal: Eliminate words that appear in __% of records in the training data


```r
min_freq <- round( 
                email_dtm$nrow * ( ( threshold = 10.0 ) / 100 ),     # using 5%
                0 
              ) 
```


# Create vector of most frequent words

```r
freq_words <- findFreqTerms( x = email_dtm, 
                             lowfreq = min_freq )
# Filter the DTM
email_dtm_freq_train <- email_dtm_train[ , freq_words]
email_dtm_freq_test  <- email_dtm_test[ , freq_words]
```


# Final, cleaned, prepared dataset for Naive Bayes

```r
# Simple dummy transformation fn.
convert_values <- function(x){
                    x = ifelse( x > 0, "Yes", "No" )
                  }

# Declaring final `train` and `test` datasets
email_train <- apply( email_dtm_freq_train, MARGIN = 2,
                      convert_values )

email_test  <- apply( email_dtm_freq_test, MARGIN = 2,
                      convert_values )
```


























