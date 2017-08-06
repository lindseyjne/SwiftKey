---
title: "NLP SwiftKey"
author: "Lindsey Erickson"
date: "August 2, 2017"
output: html_document
---


## Introduction

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r, echo = TRUE, warning = FALSE, message = FALSE ,collapse = TRUE}
# load necessary packages
library(stringi)
library(NLP)
library(tm)
library(qdap)
library(wordcloud)
library(RWeka)
library(Rgraphviz)
library(stringr)
```

```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE, cache = TRUE}
# set the working directory
setwd('C:/Users/linds/OneDrive/Documents/Coursera-SwiftKey/final/en_US')

# load the tweets
blogs <- readLines("en_US.blogs.txt")
blogs <- as.data.frame(blogs)
str(blogs)
```

```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}
set.seed(199)
blogsSample <- blogs[sample(nrow(blogs), 2000), 1]
dim(blogsSample)


fix.contractions <- function(doc) {
	# "won't" is a special case as it does not expand to "wo not"
  #doc <- gsub("â", "'", doc)
  doc <- gsub("â", "'", doc)
  doc <- gsub("â s", "", doc)
	doc <- gsub("won't", "will not", doc)
	doc <- gsub("n't", " not", doc)
	doc <- gsub("'ll", " will", doc)
	doc <- gsub("'re", " are", doc)
	doc <- gsub("'ve", " have", doc)
	doc <- gsub("'m", " am", doc)
	# 's could be 'is' or could be possessive: it has no expansion
	doc <- gsub("'s", "", doc)
	return(doc)
}

blogsSample <- fix.contractions(blogsSample)
#blogsSample <- gsub("[[:punct:]]", " ",blogsSample)
blogsSample <- gsub("[^[:alnum:]///' ]", "", blogsSample)
```

clean the dataset
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}
# turn the sample tweets to a vector source then to a corpus
blogsSource <- VectorSource(blogsSample)
blogsCorpus <- VCorpus(blogsSource)

# create a function that will clean a corpus
cleanBlogs <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)   # remove all punctuation
  corpus <- tm_map(corpus, content_transformer(tolower))  # turn all upper case letters to lower case letters
  corpus <- tm_map(corpus, removeNumbers)   # remove all numbers in the corpus
  corpus <- tm_map(corpus, removeWords, stopwords("en"))  # remove words having little informational content
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))    # remove all the extra white space
  return(corpus)
 }

# clean the sample tweets by running it through the cleanTweets function
clean_Blogs <- cleanBlogs(blogsCorpus)


# view some sample tweets to see if the cleanTweets function worked properly 
clean_Blogs[[523]][1]
clean_Blogs[[40]][1]

# create a Term Document Matrix from the cleaned tweets
blogsTDM <- TermDocumentMatrix(clean_Blogs)

# create a matrix from the Term Document Matrix
blogsMatrix <- as.matrix(blogsTDM)

# calculate the row sum of the tweets matrix
blogsFreq <- rowSums(blogsMatrix)

# sort the row sums in decreasing order
blogsFreq <- sort(blogsFreq, decreasing = TRUE)

# plot the top 20 cleaned words
barplot(blogsFreq[1:20], col = "hot pink", las = 2)
```

```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}
blogsTokenizer <- function(x)
  NGramTokenizer(x, Weka_control(min = 2, max = 2))


blogsDTM <- DocumentTermMatrix(
  clean_Blogs,
  control = list(tokenize = blogsTokenizer)
)

blogsDTM_m <- as.matrix(blogsDTM)

blogsFrequency <- colSums(blogsDTM_m)

blogsBi <- names(blogsFrequency)
wordcloud(blogsBi, blogsFrequency, max.words = 30, colors = brewer.pal(4, "PiYG"))

```


```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE, cache = TRUE}
# set the working directory
setwd('C:/Users/linds/OneDrive/Documents/Coursera-SwiftKey/final/en_US')

# load the tweets
tweets <- read.csv("en_US.twitter.txt", stringsAsFactors = FALSE)
str(tweets)
```

Looking at the english tweets, it seems that there are 1,276,052 rows.  So I know that there are at least 1,276,052 tweets.  I'm going to take a random sample of the text data and gather some basic properties on the sample dataset.
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}
set.seed(199)
tweetsSample <- tweets[sample(nrow(tweets), 2000), 1]
dim(tweetsSample)
```


clean the dataset
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}
# turn the sample tweets to a vector source then to a corpus
tweetsSource <- VectorSource(tweetsSample)
tweetsCorpus <- VCorpus(tweetsSource)

# create a function that will clean a corpus
cleanTweets <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)   # remove all punctuation
  corpus <- tm_map(corpus, content_transformer(tolower))  # turn all upper case letters to lower case letters
  corpus <- tm_map(corpus, removeNumbers)   # remove all numbers in the corpus
  corpus <- tm_map(corpus, removeWords, stopwords("en"))  # remove words having little informational content
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))    # remove all the extra white space
  return(corpus)
 }

# clean the sample tweets by running it through the cleanTweets function
clean_Tweets <- cleanTweets(tweetsCorpus)

# view some sample tweets to see if the cleanTweets function worked properly 
clean_Tweets[[523]][1]
clean_Tweets[[40]][1]

# create a Term Document Matrix from the cleaned tweets
tweetsTDM <- TermDocumentMatrix(clean_Tweets)

# create a matrix from the Term Document Matrix
tweetsMatrix <- as.matrix(tweetsTDM)

# calculate the row sum of the tweets matrix
tweetsFreq <- rowSums(tweetsMatrix)

# sort the row sums in decreasing order
tweetsFreq <- sort(tweetsFreq, decreasing = TRUE)

# plot the top 20 cleaned words
barplot(tweetsFreq[1:20], col = "hot pink", las = 2)

```
