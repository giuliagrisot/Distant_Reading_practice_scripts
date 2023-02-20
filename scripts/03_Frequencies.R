
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(readxl)

# first we need to create a corpus

corpus_files <- list.files(path = "corpus", pattern = "*.txt", full.names = T)

corpus_source <- readtext::readtext(corpus_files) %>%
  left_join(read.csv("metadata.csv", stringsAsFactors = F) %>%
              rename(doc_id = filename) %>%
              mutate(doc_id = stringr::str_replace_all(
                string = doc_id,
                pattern = "corpus/", 
                replacement = "")
                )) %>%
  select(text, doc_id, author.name, book.title, year, gender.cat) 

names <- c("doc_id", "author.name", "book.title", "gender.cat")
corpus_source[,names] <- lapply(corpus_source[,names] , factor)

remove(corpus_files)


# and to transform this into a corpus that we can use with the package "quanteda", a friendly package that allows us to analyse various aspects of a corpus

quanteda_texts <- quanteda::corpus(corpus_source)

remove(corpus_source)


# Tokens corpus ---------------

# quanteda mainly works with so called DFM (Document-feature matrix). These
# - Represents frequencies of features in documents in a matrix
# - Have an efficient structure, but do not have information on positions of words
# - Allow for a bag-of-words approach


## Let's create a dfm corpus --------------

## first we need to create a "token" corpus. This file is very big,
## so we recommend that you do NOT execute this code. (that's why it's green)

  # quanteda_texts_tok <- tokens(quanteda_texts,
  #                              # we don't want pucntuation
  #                              remove_punct = T, 
  #                              # we want to keep hyphens
  #                              split_hyphens = F,
  #                              # but no symbols
  #                              remove_symbols = T) 


# instead, load the one we prepared for you

load("resources/quanteda_texts_tokens_gg.Rdata")

## then we can create a dfm

quanteda_texts_dfm <- dfm(quanteda_texts_tok) 

# and we can have a forst look at the most frequent words, for instance with a wordcloud

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

# in a "table" form

frequency_table <- textstat_frequency(quanteda_texts_dfm)
print(head(frequency_table))

# or for in a plot, such as a this one

quanteda_texts_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

## it is quite evident that we have at the top of the list words that are not 
## extremely interesting (unless you are focusing on conjunctions and pronouns).
## We can thus decide to look only at words that fall into a specific range 
## of our frequency ranking.

# If you look at the table we produced before, you'll see that 
# somewhere around the 5000 occurrences words seem to be getting more juicy.
# Let's see how that goes if we set that up.

quanteda_texts_dfm <- dfm(quanteda_texts_tok)  %>%
  dfm_trim(max_termfreq = 5000) # here we say "we want a frequency max of 5000"

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

frequency_table <- textstat_frequency(quanteda_texts_dfm)
print(head(frequency_table))

quanteda_texts_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# colnames(quanteda_texts_dfm@docvars)


# Alternatively, we can decide beforehand that we want to remove stopwords
# again, this is a big file, so load it directly.

  # quanteda_texts_tok <- quanteda::tokens(quanteda_texts, 
  #                                        remove_punct = T,
  #                                        split_hyphens = F,
  #                                        remove_symbols = T) %>%
  #   tokens_remove(c(stopwords("german")))

load(file="resources/quanteda_texts_tok_nostop_gg.RData")

quanteda_texts_dfm <- dfm(quanteda_texts_tok)

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

# better, right?

# another thing we can do is to visualize group differences in frequency,
# for instance, we might want to see which words are the most frequently used by women vs men authors.

quanteda_texts_dfm %>% 
  textstat_frequency(n = 15, groups = gender.cat) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color=group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


## or look at single words comparisons:

sorted_features <- topfeatures(quanteda_texts_dfm, n = nfeat(quanteda_texts_dfm))
sorted_features[c("mädchen", "junge", "frau", "mann")]

# Stats ---------------

## frequencies
## as we saw before, we can easily craete a table with all the information 
## about toke frequencies in our corpus

frequency_table <- textstat_frequency(quanteda_texts_dfm)

# and observe it either in the console

print(head(frequency_table, 20))

# or as a full table

view(frequency_table)

### plots

quanteda_texts_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


# we can also zoom in a particluar temporal frame

quanteda_texts_dfm %>% 
  textstat_frequency(n = 5, groups = year) %>% 
  filter(group > 1900 & group < 1910) %>%
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color=group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


# or look at how a specific term is used over time

quanteda_texts_dfm %>% 
  textstat_frequency(groups = as.numeric(year)) %>% 
  filter(feature == "berg") %>%
  ggplot(aes(x = group, y = frequency, label=feature)) +
  geom_col(width = .1) +
  geom_point(size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) +
  # geom_text() +
  xlab("frequency of the word 'berg' in the corpus over the years")


quanteda_texts_dfm %>% 
  textstat_frequency(groups = author.name) %>% 
  filter(feature == "berg") %>%
  ggplot(aes(x = group, y = frequency, label=feature)) +
  geom_col(width = .1) +
  geom_point(size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) +
  # geom_text() +
  xlab("frequency of the word 'berg' in the corpus by author")
