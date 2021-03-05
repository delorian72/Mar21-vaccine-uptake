# EDA - Hashtag processing
# From https://rstudio-pubs-static.s3.amazonaws.com/595002_2d1617098c8c44b494bc2ec97018a82b.html
library(rtweet)
library(stringr)

hashtag_pat <- "#[a-zA-Z0-9_-ー\\.]+"
hashtag <- str_extract_all(tt_data$full_text, hashtag_pat)

#Number of Hashtags in each tweet

tt_data$no_hashtags <- lengths(hashtag)
mean(tt_data$no_hashtags)
range(tt_data$no_hashtags)

#Process hashtags

hashtag_word <- unlist(hashtag)
hashtag_word <- tolower(hashtag_word)
hashtag_word <- gsub("[[:punct:]ー]", "", hashtag_word)
hashtag_word <- hashtag_word[!str_detect(hashtag_word, "covid")]
hashtag_word <- hashtag_word[!str_detect(hashtag_word, "corona")]

hashtag_count <- table(hashtag_word)
top_25_freqs <- sort(hashtag_count, decreasing = TRUE)[1:25]
top_25_freqs

as.data.frame(hashtag_word) %>%
  count(hashtag_word, sort = TRUE) %>%
  mutate(hashtag_word = reorder(hashtag_word, n)) %>%
  top_n(25) %>%
  ggplot(aes(x = hashtag_word, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = "Count",
       y = "Hashtag",
       title = "Top 25 Popular Hashtags along with Covid19")

library(wordcloud)
top_25_hashtags <- as.character(as.data.frame(top_25_freqs)[,1])
wordcloud(top_25_hashtags, top_25freqs, 
          scale=c(3.5,1.5), random.order=FALSE, rot.per=.25)


