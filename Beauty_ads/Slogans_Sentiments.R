

# We want to see if there is an emotional encoding associated with the slogans. We can do that by importing a sentiment lexicon and associating each word with a sentiment score.

# Sentiment analysis ---------------

# we can use the "nrc" lexicon, which is a lexicon that contains words associated with emotions


# install.packages("tidytext")
# install.packages("textdata")
# install.packages("syuzhet")
# install.packages("wesanderson")
# install.packages("pals")


library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(syuzhet)
library(wesanderson)
library(pals)
library(RColorBrewer)

# load the dataset
read_csv("data/structured_slogans.csv") -> slogans_df

# we can easily extract sentiments with the package syuzhet
# we can use the get_sentiment function to extract the sentiment of each slogan

slogans_df_sentiments <- slogans_df %>%
  mutate(sentiment_syuzhet = get_sentiment(Slogan)) %>%
  mutate(sentiment_nrc = get_sentiment(Slogan, method = 'nrc')) %>%
  mutate(sentiment_bing = get_sentiment(Slogan, method = 'bing')) %>%
  mutate(sentiment_afinn = get_sentiment(Slogan, method = 'afinn'))

# we can make a first plot to see the variability of the sentiment according to different lexicons

slogans_df_sentiments %>% 
  gather(key = "lexicon", value = "sentiment", -Company, -Year, -Country, -Slogan) %>% # 'gather' is used to transform the data from wide to long format, this way we can plot it.
  ggplot(aes(x = lexicon, y = sentiment,
             fill = lexicon)) +
  geom_boxplot() +
  facet_wrap(~lexicon, scales = "free") +
  scale_fill_manual(values = wes_palette("AsteroidCity1")) +
  theme_minimal()


# we can also see the distribution of the sentiment of the slogans according to the company, focusing on one lexicon at a time.
load(file = "data/companies_with_enough_words.RData")

slogans_df_sentiments %>%
  gather(key = "lexicon", value = "sentiment", -Year, -Country, -Slogan, -Company) %>% # we transform the data from wide to long format
  # again let's pick only the companies with enough slogans
  filter(Company %in% companies_with_enough_words) %>%
  filter(lexicon == "sentiment_nrc") %>% # we focus on the nrc lexicon
  ggplot(aes(y = sentiment, x=Company, fill=Company)) +
  geom_boxplot() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
    ) +
  scale_fill_manual(values = unname(pals::kelly()))


# we can also see the distribution of the sentiment of the slogans according to the company, focusing on one lexicon at a time.
slogans_df_sentiments %>%
  gather(key = "lexicon", value = "sentiment", -Year, -Country, -Slogan, -Company) %>%
  filter(Company %in% companies_with_enough_words) %>%
  filter(lexicon == "sentiment_afinn") %>% # we focus on the afinn lexicon
  ggplot(aes(y = sentiment, x=Company, fill=Company)) +
  geom_boxplot() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_manual(values = unname(pals::kelly()))


# we can look at the sentiments more closely

text_words <- slogans_df_sentiments %>%
  select(Slogan) %>%
  unnest_tokens(word, Slogan) %>%
  anti_join(stop_words)

text_words <- text_words$word

head(text_words)

sentiment_scores_nrc <- get_nrc_sentiment(text_words)

summary(sentiment_scores_nrc)


barplot(
  colSums(prop.table(sentiment_scores_nrc[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = pals::kelly()[3:12],
  xlab="emotions", ylab = NULL)


sad_words <- text_words[sentiment_scores_nrc$sadness> 0]

sad_word_order <- sort(table(unlist(sad_words)), decreasing = TRUE)
head(sad_word_order, n = 12)



sentiment_scores_sz <- get_sentiment(text_words)

summary(sentiment_scores_sz)

# syuzhet sentiment scores are on a continuous scale, so we can plot the distribution of the sentiment scores

ggplot(data = as_tibble(sentiment_scores_sz), aes(value)) +
  geom_density()

sad_words <- text_words[sentiment_scores_sz<0]

sad_word_order <- sort(table(unlist(sad_words)), decreasing = TRUE)
head(sad_word_order, n = 12)

