#EDA

str(tt_data)
head(tt_data)

tt_sample <- tt_data

#Analyse wordcounts

wordcount_text <- stri_count_words(tt_data$full_text)
word_range <- range(wordcount_text)
word_mean  <- mean(wordcount_text)
word_max   <- max(wordcount_text)

hist(wordcount_text)


#Sampling and Cleaning
text_corpus <- Corpus(VectorSource(tt_sample$full_text2))

toSpace <- content_transformer(function(x, pattern) gsub(pattern, "", x))
text_corpus <- tm_map(text_corpus, toSpace, "[^[:print:]]")
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, profanity)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus_no_stopwords <- tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus_no_stopwords <- tm_map(text_corpus_no_stopwords, stripWhitespace)


TwoGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
ThreeGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

ngram_freqdf <- function(tdm, sparsity){
  freq <- sort(rowSums(as.matrix(removeSparseTerms(tdm, sparsity))), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}

onegram_tdm <- TermDocumentMatrix(text_corpus)
onegram_freqdf <- ngram_freqdf(onegram_tdm, 0.99)

twogram_tdm <- TermDocumentMatrix(text_corpus, control = list(tokenize = TwoGramTokenizer))
twogram_freqdf <- ngram_freqdf(twogram_tdm, 0.99)

threegram_tdm <- TermDocumentMatrix(text_corpus, control = list(tokenize = ThreeGramTokenizer))
threegram_freqdf <- ngram_freqdf(threegram_tdm, 0.999)

onegramNS_tdm <- TermDocumentMatrix(text_corpus_no_stopwords)
onegramNS_freqdf <- ngram_freqdf(onegramNS_tdm, 0.99)

twogramNS_tdm <- TermDocumentMatrix(text_corpus_no_stopwords, control = list(tokenize = TwoGramTokenizer))
twogramNS_freqdf <- ngram_freqdf(twogramNS_tdm, 0.999)

threegramNS_tdm <- TermDocumentMatrix(text_corpus_no_stopwords, control = list(tokenize = ThreeGramTokenizer))
threegramNS_freqdf <- ngram_freqdf(threegramNS_tdm, 0.9999)

ngram_barplot <- function(df, title){
  dfsub <- subset(df[1:20,])
  ggplot(dfsub, aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity") + 
    labs(x = "Words", y = "Count", title = title) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
}

onegram_barplot <- ngram_barplot(onegram_freqdf,"Top 20 Words (All)")
onegramNS_barplot <- ngram_barplot(onegramNS_freqdf,"Top 20 Words (No stopwords)")
twogram_barplot <- ngram_barplot(twogram_freqdf,"Top 20 2-grams (All)")
twogramNS_barplot <- ngram_barplot(twogramNS_freqdf,"Top 20 2-grams (No stopwords)")
threegram_barplot <- ngram_barplot(threegram_freqdf,"Top 20 3-grams (All)")
threegramNS_barplot <- ngram_barplot(threegramNS_freqdf,"Top 20 3-grams (No stopwords)")

grid.arrange(onegram_barplot, onegramNS_barplot, ncol = 1)

grid.arrange(twogram_barplot, twogramNS_barplot, ncol = 1)

grid.arrange(threegram_barplot, threegramNS_barplot, ncol = 1)





tt_data$full_text2 <- as.character(tt_data$full_text)
count(is.na(head(tt_data$full_text2)))



#use a small sample of data set due to memory limitations
set.seed(123)
blogs.sample <- blogs[sample(length(blogs), length(blogs)*.1/100)] #.1%
twitter.sample <- twitter[sample(length(twitter), length(twitter)*.1/100)] #.1%
news.sample <- news[sample(length(news), length(news)*.1/100)] #.1%
sample <- c(blogs.sample, twitter.sample, news.sample)

#convert to ASCII encoding to get rid of non-standard characters
sample <- iconv(sample, to = "ASCII", sub = "")

#convert to corpus
library(tm)


is.na


head(tt_data$full_text2)

as.Date(tt_data$created_at)


library(tidytext)
get_sentiments("afinn")
get_sentiments("bing")\
get_sentiments("nrc")

# EDA on NLP and Sentiment

library(dplyr)
library(stringr)





