# Word Embeddings

# install.packages(c("text2vec", "quanteda", "tidyverse", "data.table", "readr", "R.utils"))
library(text2vec)
library(quanteda)
library(tidyverse)
library(data.table)
library(readr)
library(R.utils)
library(readr)
library(tidytext)


# Download FastText word vectors (if not already downloaded)
fasttext_url <- "https://dl.fbaipublicfiles.com/fasttext/vectors-crawl/cc.en.300.vec.gz"
fasttext_file <- "cc.en.300.vec.gz"

if (!file.exists(fasttext_file)) {
  download.file(fasttext_url, fasttext_file)
}

# Load FastText embeddings

word_vectors <- fread("/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester(2)/Manchester/UoM/Teaching/2024-2025/MA Semester 2 - DIGI65522 Data in Culture and Society/cc.en.300.vec.gz", skip = 1, header = FALSE, quote = "")
colnames(word_vectors) <- c("word", paste0("V", 1:300))  # 300-dimensional vectors

# Convert to matrix format for similarity calculations
embedding_matrix <- as.matrix(word_vectors[ , -1, with = FALSE])
rownames(embedding_matrix) <- word_vectors$word

# Load beauty ad dataset
ads <- read_csv("data/structured_slogans.csv")

# Add a column 'target_audience' with random values "M" or "F"
set.seed(123)  # For reproducibility
ads$target_audience <- sample(c("Men", "Women"), size = nrow(ads), replace = TRUE)
ads <- ads %>% rename(text=Slogan) %>%
  select(text, target_audience)

# View the updated dataset
head(ads)

  
# Tokenize & clean text
ads <- ads %>%
  unnest_tokens(word, text)


# Separate by audience
ads_men <- ads %>% filter(target_audience == "Men")
ads_women <- ads %>% filter(target_audience == "Women")

# Define target words
target_words <- c("strong", "beautiful", "fresh", "power", "glow", "smooth")

# Function to get top 5 similar words
get_similar_words <- function(word, embeddings) {
  if (word %in% rownames(embeddings)) {
    similarities <- text2vec::sim2(embeddings, embeddings[word, , drop = FALSE], method = "cosine")
    return(sort(similarities[,1], decreasing = TRUE)[2:6])  # Top 5 excluding the word itself
  } else {
    return(NA)
  }
}

# Find nearest words in men's beauty ads
nearest_words_men <- lapply(target_words, function(word) get_similar_words(word, embedding_matrix))
names(nearest_words_men) <- target_words

# Find nearest words in women's beauty ads
nearest_words_women <- lapply(target_words, function(word) get_similar_words(word, embedding_matrix))
names(nearest_words_women) <- target_words

# Display results
nearest_words_men
nearest_words_women
