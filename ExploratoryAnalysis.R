---
title: "NLP SwiftKey Exploratory Analysis"
author: "Lindsey Erickson"
date: "August 2, 2017"
output: html_document
---


## Introduction
This SwiftKey NLP project looks into three documents that contains blogs, tweets, and news bits.  This project conducts exploratory analysis on these documents and illustrates important summaries on the data set.  At the end of this project, I have noted my plans for creating a prediction algorithm and Shiny app.
<br>
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
<br>
<br>

## Load Data
Load the data from all sources: blogs, tweets, and news.
<br>
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE, cache = TRUE}
setwd('C:/Users/linds/OneDrive/Documents/Coursera-SwiftKey/final/en_US')

# load the blogs
blogs <- readLines("en_US.blogs.txt") # read in the lines of the file
blogs <- as.data.frame(blogs) # convert the data to a data frame
dim(blogs)  # show the dimensions of the dataset

# load the tweets
tweets <- readLines("en_US.twitter.txt") # read in the lines of the file
tweets <- as.data.frame(tweets) # convert the data to a data frame
dim(tweets) # show the dimensions of the dataset

# load the news
news <- readLines("en_US.news.txt") # read in the lines of the file
news <- as.data.frame(news) # convert the data to a data frame
dim(news) # show the dimenstions of the dataset
```
<br>
Because the dimensions of the data are large for the blogs, tweets, and news (899,288, 2,360,148, and 77,259, respectively), I'm going to take a subset of the data. This subset will be a random sample of 2,000 rows.
<br>
<br>
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}
# take a random sample of 2,000 rows from each data source
set.seed(199)
blogsSample <- blogs[sample(nrow(blogs), 2000), 1]
tweetsSample <- tweets[sample(nrow(tweets), 2000), 1]
newsSample <- news[sample(nrow(news), 2000), 1]

# convert the data to a matrix
blogsSample <- as.matrix(blogsSample)
tweetsSample <- as.matrix(tweetsSample)
newsSample <- as.matrix(newsSample)

# view the dimensions of the data
dim(blogsSample)
dim(tweetsSample)
dim(newsSample)
```
<br>
Now, I have a sample of 2,000 rows per document, which will be easier to deal with in regards to processing time.
<br>
<br>

## Clean Data
I want to find the most common words in the documents; however, I have to clean the datat prior to doing this to ensure accuracy.  This first step in completing this is to convert contraction words to two-words(i.e. we've to we have, we'll to we will, etc.)
<br>
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}

clean.contract <- function(contract) {
  #contract <- gsub("â", "'", contract)
  contract <- gsub("â", "'", contract)
  doc <- gsub("â s", "", contract)
	contract <- gsub("won't", "will not", contract)
	contract <- gsub("n't", " not", contract)
	contract <- gsub("'ll", " will", contract)
	contract <- gsub("'re", " are", contract)
	contract <- gsub("'ve", " have", contract)
	contract <- gsub("'m", " am", contract)
	contract <- gsub("'s", "", contract)
	return(contract)
}

blogsSample <- clean.contract(blogsSample)
blogsSample <- gsub("[^[:alnum:]///' ]", "", blogsSample)

tweetsSample <- clean.contract(tweetsSample)
tweetsSample <- gsub("[^[:alnum:]///' ]", "", tweetsSample)

newsSample <- clean.contract(newsSample)
newsSample <- gsub("[^[:alnum:]///' ]", "", newsSample)
```
<br>
Now that I have replaced all the contraction words, I'm going to take the following steps to clean the data:

    1. Remove all puncutation from the the documents
    2. Turn all letters to lowercase
    3. Remove all numbers
    4. Remove all stop words
    5. Remove all white space
<br>
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}

# turn the sample tweets to a vector source then to a corpus
blogsSource <- VectorSource(blogsSample)
blogsCorpus <- VCorpus(blogsSource)

tweetsSource <- VectorSource(tweetsSample)
tweetsCorpus <- VCorpus(tweetsSource)

newsSource <- VectorSource(newsSample)
newsCorpus <- VCorpus(newsSource)

# create a function that will clean a corpus
cleanText <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)   # remove all punctuation
  corpus <- tm_map(corpus, content_transformer(tolower))  # turn all upper case letters to lower case letters
  corpus <- tm_map(corpus, removeNumbers)   # remove all numbers in the corpus
  corpus <- tm_map(corpus, removeWords, stopwords("en"))  # remove words having little informational content
  corpus <- tm_map(corpus, content_transformer(stripWhitespace))    # remove all the extra white space
  return(corpus)
 }

# clean the sample data by running it through the cleanTweets function
clean_Blogs <- cleanText(blogsCorpus)
clean_Tweets <- cleanText(tweetsCorpus)
clean_News <- cleanText(newsCorpus)

# view the first line of each cleaned data
clean_Blogs[[1]][1]
clean_Tweets[[1]][1]
clean_News[[1]][1]

```
<br>
The first line of the data shows that the punctuation, numbers, stopwords, and white space was removed.  Also, there are no contractions or uppercase letters.  Now, I can view the top words.
<br>
<br>

## View Top Words
I want to view the most frequent words in each data sources.  This will plot the top 20 used words.
<br>
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE, out.width='750px', dpi=200}
# create a Term Document Matrix from the cleaned tweets
blogsTDM <- TermDocumentMatrix(clean_Blogs)
tweetsTDM <- TermDocumentMatrix(clean_Tweets)
newsTDM <- TermDocumentMatrix(clean_News)

# create a matrix from the Term Document Matrix
blogsMatrix <- as.matrix(blogsTDM)
tweetsMatrix <- as.matrix(tweetsTDM)
newsMatrix <- as.matrix(newsTDM)


# sort the row sums in decreasing order
blogsFreq <- sort(rowSums(blogsMatrix), decreasing = TRUE)
tweetsFreq <- sort(rowSums(tweetsMatrix), decreasing = TRUE)
newsFreq <- sort(rowSums(newsMatrix), decreasing = TRUE)


# plot the top 20 cleaned words
par(mfrow=c(2, 2))
barplot(blogsFreq[1:20], col = "hot pink", las = 2, main = "Top 20 Blog Words")
barplot(tweetsFreq[1:20], col = "hot pink", las = 2, main = "Top 20 Tweet Words")
barplot(newsFreq[1:20], col = "hot pink", las = 2, main = "Top 20 News Words")
```
<br>
The top words in each source shows some interesting characteristics.  First, each source has a few of the same word such as, *will* and *one*.  Each source has some uniqueness that seems obvious such as, *love*, *lol*, and *thanks* are top words news are *state*, *percent*, and *year*.
<br>
<br>

## View Top Two-Word Combinations
This will look at the top two-word combinations from each data source.  The results will be in wordclouds to view which two-word combinations are more frequent than others, based on their size and color.
<br>
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}
# create a 2 word tokenizer
textTokenizer <- function(x)
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

# apply the tokenizer to each data source
blogsDTM <- DocumentTermMatrix(
  clean_Blogs,
  control = list(tokenize = textTokenizer)
)
tweetsDTM <- DocumentTermMatrix(
  clean_Tweets,
  control = list(tokenize = textTokenizer)
)
newsDTM <- DocumentTermMatrix(
  clean_News,
  control = list(tokenize = textTokenizer)
)

# convert to matrix
blogsDTM_m <- as.matrix(blogsDTM)
tweetsDTM_m <- as.matrix(tweetsDTM)
newsDTM_m <- as.matrix(newsDTM)

# calculate the column sums of the matrices 
blogsFrequency <- colSums(blogsDTM_m)
tweetsFrequency <- colSums(tweetsDTM_m)
newsFrequency <- colSums(newsDTM_m)

# get the names of the top words
blogsBi <- names(blogsFrequency)
tweetsBi <- names(tweetsFrequency)
newsBi <- names(newsFrequency)

# create 2-word wordclouds
par(mfrow=c(1, 3))
wordcloud(blogsBi, blogsFrequency, max.words = 20, colors = brewer.pal(4, "PiYG"))
wordcloud(tweetsBi, tweetsFrequency, max.words = 20, colors = brewer.pal(4, "PiYG"))
wordcloud(newsBi, newsFrequency, max.words = 20, colors = brewer.pal(4, "PiYG"))

```
<br>
The wordclouds show that for blogs, the most frequent two-word combination is *can see*.  The most frequent two-word combination in the tweets is *right now*.  Lastly, the most frequent two-word combinations in the news is *new jersey* and *st louis*.
<br>
<br>

## View Top Two-Word Combinations from All Data Sources Combined
Now, I'm going to view the top two-word combinations when all data sources are combined.  The results will be put in wordclouds, and the highest frequency will be determined by size and color.
<br>
```{r, echo = TRUE, warning = FALSE, message = FALSE, collapse = TRUE}
# create a 2 word tokenizer
textTokenizer <- function(x)
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

# apply the tokenizer to each data source
blogsDTM <- DocumentTermMatrix(
  clean_Blogs,
  control = list(tokenize = textTokenizer)
)
tweetsDTM <- DocumentTermMatrix(
  clean_Tweets,
  control = list(tokenize = textTokenizer)
)
newsDTM <- DocumentTermMatrix(
  clean_News,
  control = list(tokenize = textTokenizer)
)

# convert to matrix
blogsDTM_m <- as.matrix(blogsDTM)
tweetsDTM_m <- as.matrix(tweetsDTM)
newsDTM_m <- as.matrix(newsDTM)

# combine the above matrices to create one big matrix
allData_m <- cbind(blogsDTM_m, tweetsDTM_m)
allData_m <- cbind(allData_m, newsDTM_m)

# calculate the column sums of the matrices 
allDataFrequency <- colSums(allData_m)


# get the names of the top words
allDataBi <- names(allDataFrequency)

# create 2-word wordclouds

wordcloud(allDataBi, allDataFrequency, max.words = 20, colors = brewer.pal(4, "PiYG"))
```
<br>
The wordcloud shows that the highest frequency for all data sources combined is *last year* followed by *new jersey* and *st louis*.
<br>
<br>

## View Top Three-Word Combinations of all data sources
Now, I'm going to view the top three-word combinations when all data sources are combined.  The results will be put in wordclouds, and the highest frequency will be determined by size and color.  I'm going to suppress that actual code; however, this follows the exact code from above but changing the min/max numbers in the tokenizer to 3.
<br>
```{r, echo = FALSE, warning = FALSE, message = FALSE, collapse = TRUE}
# create a 2 word tokenizer
textTokenizer <- function(x)
  NGramTokenizer(x, Weka_control(min = 3, max = 3))

# apply the tokenizer to each data source
blogsDTM <- DocumentTermMatrix(
  clean_Blogs,
  control = list(tokenize = textTokenizer)
)
tweetsDTM <- DocumentTermMatrix(
  clean_Tweets,
  control = list(tokenize = textTokenizer)
)
newsDTM <- DocumentTermMatrix(
  clean_News,
  control = list(tokenize = textTokenizer)
)

# convert to matrix
blogsDTM_m <- as.matrix(blogsDTM)
tweetsDTM_m <- as.matrix(tweetsDTM)
newsDTM_m <- as.matrix(newsDTM)

# combine the above matrices to make one big matrix
allData_m <- cbind(blogsDTM_m, tweetsDTM_m)
allData_m <- cbind(allData_m, newsDTM_m)

# calculate the column sums of the matrices 
allDataFrequency <- colSums(allData_m)


# get the names of the top words
allDataBi <- names(allDataFrequency)

# create 2-word wordclouds

wordcloud(allDataBi, allDataFrequency, max.words = 20, colors = brewer.pal(4, "PiYG"))
```
<br>
The wordcloud shows that the highest frequency of three-word combinations for all data sources combined is *cricket world cup* and *st louis county*.  I will have to deal with the a hat result, as it needs to be fixed.
<br>
<br>

## Exploratory Analysis Conclusion
Though the one-word frequencies on each data source was interesting, it did not provide much information on what the blogs, tweets, and news documents were about.  In fact, doing this two-word combinations on each source was not that useful. However, when combining the documents together and finding the top three-word combinations, it was easy to see that there is a lot of words regarding world cup and st louis.

## Prediction Algorithm Plans
To create a prediction algorithm, I'm going to use an N-gram model to guess the next word based on a word one types.  An N-gram model is an N-1 order Markov model, where each word depends on the last N-1 words.  The blogs, tweets, and news will train a the Markov model on which word is likely to appear after a given word.

I am going to attempt a Shiny app that allows a user to type a word.  Once the word is typed, the app will predict what the next word will be and give the user three choices.  The three choices will be the words that typically appear after their typed word in the documents.
