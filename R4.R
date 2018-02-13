
#  The package textcat is used for text categorization based on n-grams.
if (!require("textcat")) {
  install.packages("textcat")
  library(tm)
}

library(ggplot2)
library(readr)
library(tm)


setwd("Users/pabloleal...") #add your location

#  We create functions that are specific to tweets.  These functions are very helpful to
#  use in many different problems.
removeURL = function(x) gsub("http[[:alnum:]]*", "", x)
removeNonAlpha = function(x) gsub("[^a-zA-Z]"," ",x)
removeUserMention = function(x) gsub("@\\w+","",x)

isis_tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
names(isis_tweets)
head(isis_tweets)

#  TextCat determines the language of a search string by comparing it to a model for
#  each language. These models are basically a ranked list of n-grams that are common in
#  a particular language, and that are built from a set of training data. In our case, as a
#  first cust, we want only tweets in english.  It looks like there are 10K+ tweets in english.
#  textcat() can take a while since we haev a lot of tweets.
#
#  textcat() modifies the language feature of isis_tweets to be from a specific language based
#  on its models instead of relying on the value put there by Twitter.
isis_tweets$language = textcat(isis_tweets$tweets)
head(isis_tweets)
isis_tweets = subset(isis_tweets,language=="english")
head(isis_tweets)
#  Remove the feature language since all are now in english
isis_tweets = isis_tweets[,!names(isis_tweets)=="language"]
head(isis_tweets)

#  Use tm_map to remove all the unimportant characters in the tweets.  The construct c("str")
#  is a vector that has one column whose value is "str".  This is required by tm_map().
isis_tweets$tweets = removeNonAlpha(isis_tweets$tweets)
corpus = Corpus(VectorSource(isis_tweets$tweets))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,PlainTextDocument)
corpus = tm_map(corpus,removeWords,c(stopwords("english")))
corpus = tm_map(corpus,removeWords,c("isi"))
corpus = tm_map(corpus, content_transformer(removeURL)) 
corpus = tm_map(corpus,content_transformer(removeUserMention))
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,stemDocument)

#  Create a Document Term Matrix and Remove terms that occur in less than 0.01% of the documents
DTM = DocumentTermMatrix(corpus)
sparse = removeSparseTerms(DTM,0.99)

#  Perform the kmeans analysis with k = 20 and a maximum of 100 iterations to assign data points
#  to a particular cluster.
kmeans_20 = kmeans(x=sparse,centers=20,iter.max=100)

#  List the words that appear most frequently in each cluster.  To do this we look at the
#  most frequent words in the cluster centers, which were determined from the data points
#  assgined to that cluster.  Remember that the cluster centers are NOT real data points.  They
#  are the data points that are the CENTROID of the real data points that are most similar to
#  it.
#
#  This FOR loop creates a string to display that the word "Cluster ", concatenates the number
#  of the cluster, inserts a ": ", and does not introduce a separation character (sep="").  It
#  then sorts the features (words) of center given by the value of i, from highest frequency
#  to lowest frequency and assigns that sorted list to s.  It then concatenates the first 5
#  values of the sorted list (the five most frequent words) and puts a line break in ("\n").
#  Each cat() prints out something.
for (i in 1:20) {
  cat(paste("Cluster ", i, ": ", sep = ""))
  s = sort(kmeans_20$centers[i, ], decreasing = TRUE)
  cat(names(s)[1:5], "\n")
}
