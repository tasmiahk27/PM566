install.packages("tidytext")
library(tidytext)
library(tidyverse)
alice <-readRDS("~/Downloads/alice.rds")
alice
library(dplyr)
library(ggplot2)
alice |> unnest_tokens(token, text) |> group_by(chapter) |> count(token) |> top_n(10,n) |> ggplot(aes(n, fct_reorder(token, n))) + geom_col()

stop_words
stop_words |> filter(lexicon == "snowball") |> pull(word)
sort(table(stop_words$word), decreasing=TRUE)

alice |> unnest_tokens(token, text) |> anti_join(stop_words, by=c("token"="word")) |> count(token, sort=TRUE)
alice |>
  unnest_ngrams(ngram, text, n = 2)
alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  count(ngram, sort = TRUE)

alice |>
  unnest_ngrams(ngram, text, n = 2) |>
  separate(ngram, into = c("word1", "word2"), sep = " ") |>
  select(word1, word2)
install.packages("textdata")
library(textdata)
nrc_fun <- get_sentiments("nrc")
nrc_fun <- nrc_fun[!nrc_fun$sentiment %in% c("positive","negative"), ]
alice |>
  unnest_tokens(word, text) |>
  inner_join(nrc_fun) |> 
  group_by(chapter) |> 
  summarise(sentiment = names(which.max(table(sentiment))))

