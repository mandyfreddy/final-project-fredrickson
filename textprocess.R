# Amanda Fredrickson
# Github ID: mandyfreddy
# DAP2 Final Project
# Preliminary Analysis of Veteran Disability

## textprocess.R

library(rvest)
library(tidytext)
library(tidyverse)
library(stringr)
library(ggplot2)

# A list of content from each article
names(content_list) <- paste("Article", seq_along(content_list))
print(content_list)

# Convert the list to a data frame suitable for text analysis
content_data <- data.frame(text = unlist(content_list), stringsAsFactors = FALSE)

# Add an identifier for each article
content_data$article_id <- rep(names(content_list), times = sapply(
  content_list,
  length
))

# Tokenize the text into words
tidy_text <- content_data %>%
  unnest_tokens(word, text) %>%
  # Remove common stopwords
  anti_join(get_stopwords())

# Load the Bing sentiment lexicon
bing_lexicon <- get_sentiments("bing")

# Perform sentiment analysis
sentiment_analysis <- tidy_text %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(article_id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

# View the sentiment analysis results
print(sentiment_analysis)

# Plotting the sentiment scores for each article
plot_4 <- ggplot(sentiment_analysis, aes(
  x = article_id, y = sentiment_score,
  fill = article_id
)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  labs(
    title = "Sentiment Analysis of Articles",
    x = "Article",
    y = "Sentiment Score",
    fill = "Article ID"
  ) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename =
    "/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/images/plot_4.png",
  plot = plot_4, width = 10, height = 8, units = "in"
)

# Reshape for plotting counts of positive and negative words
sentiment_counts <- sentiment_analysis %>%
  gather(key = "sentiment", value = "count", -article_id, -sentiment_score)

# Plotting the counts of positive and negative words
plot_5 <- ggplot(sentiment_counts, aes(x = article_id, y = count, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Counts of Positive and Negative Words in Articles",
    x = "Article",
    y = "Count",
    fill = "Sentiment"
  ) +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename =
    "/Users/amandaharrison/Desktop/DAP2/final-project-fredrickson/images/plot_5.png",
  plot = plot_5, width = 10, height = 8, units = "in"
)
