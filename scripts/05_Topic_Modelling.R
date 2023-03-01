library(quanteda)
library(tidytext)
library(topicmodels)
library(ggplot2)

# Load data (tweets)
text_data <- read.csv("scripts/tweets.csv")

# # or your books (bigger corpus, much more processing intensive)
text_data <- readtext::readtext("corpus/*.txt")
text_data <- text_data %>%
  sample_n(20)
  

# Convert text data into a corpus object
mycorpus <- corpus(text_data$text)

# Remove stopwords and punctuation from the corpus
mycorpus_clean <- mycorpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_remove(stopwords("english"))

# Remove sparse terms
mycorpus_clean <- dfm(mycorpus_clean)
mycorpus_clean <- dfm_trim(mycorpus_clean, min_docfreq = 5)

# Convert to matrix
dtm <- as.matrix(mycorpus_clean)

# Fit LDA model
set.seed(123)
lda <- LDA(dtm, k = 5, control = list(seed = 1234))

# Visualize the topics
topics <- tidy(lda, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = "Beta") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Extract topic distribution for each document
doc_topics <- tidy(lda, matrix = "gamma", document_names = rownames(dtm))

# Plot the distribution of topics across documents
doc_topics %>%
  ggplot(aes(x = topic, y = gamma, fill = factor(topic))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(width = 0)
               
