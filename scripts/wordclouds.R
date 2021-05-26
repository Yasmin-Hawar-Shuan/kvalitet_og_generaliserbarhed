library(wordcloud)
library(tm)
library(tidyverse)
library(SnowballC)
library(syuzhet)
library(stopwords)

df <- read.csv("trustpilot-reviews.csv")[-1]

løbehjul <- df[df$transport == 'tier' | df$transport == 'voi',]
fynbus <- df[df$transport == 'fynbus',]


prep <- function(text) {
  stemmed <- wordStem(text, language = "danish")
  corpus <- Corpus(VectorSource(stemmed)) %>% 
    tm_map(tolower) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removeWords, stopwords::stopwords('da')) %>% 
    tm_map(stripWhitespace) %>% 
    TermDocumentMatrix() %>% 
    as.matrix()
  return(corpus)
}

word_counter <- function(tdm, n=5) {
  w <- sort(rowSums(tdm), decreasing = T)
  words <- subset(w, w>=n)
  return(list(w = w, words = words))
}

bar_plot <- function(words) {
  barplot(words, 
          las = 2,
          col = rainbow(10))
}

corpus <- prep(løbehjul$.)

word_counts <- word_counter(corpus)

bar_plot(word_counts$words)

wordcloud(words = names(word_counts$w),
          freq = word_counts$w,
          max.words = 300,
          random.order = F,
          min.freq = 2,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.5)


corpus <- prep(fynbus$.)

word_counts <- word_counter(corpus)

bar_plot(word_counts$words)

wordcloud(words = names(word_counts$w),
          freq = word_counts$w,
          max.words = 300,
          random.order = F,
          min.freq = 2,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.5)
