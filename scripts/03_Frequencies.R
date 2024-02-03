
# install.packages("quanteda")
# install.packages("quanteda.textmodels")
# install.packages("quanteda.textplots")
# install.packages("quanteda.textstats")


library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(readtext)

# first we need to create a corpus.
# this time we'll be using "quanteda", a package that allows to do corpus analysis very easily

files_list <- list.files("corpus", full.names = T, pattern = "txt") %>% sample(5)


corpus_source <- readtext(files_list, encoding = "UTF-8") %>%
  mutate(text = gsub("\\s+"," ", text))  %>%
  as_tibble() %>%
  mutate(doc_id = stringr::str_remove(doc_id, ".txt")) %>%
  left_join(readtext("corpus/ELTeC-eng_metadata.tsv",
                     docid_field = "filename") %>%
              rename(author = `author.name`, # i want to rename wome variables
                     author_gender = `author.gender`,
                     year = `first.edition`) %>%
              select(doc_id, # let's just preserve a few metadata infos
                     author,
                     title,
                     author_gender,
                     year,
                     -text) %>%
              as_tibble()
  ) %>%
  group_by(author_gender) %>%
  # sample_n(5) %>% # for this session, let's limit the corpus to 10 texts (5 for each gender)
  ungroup()


head(corpus_source)

# and to transform this into a corpus that we can use with the package "quanteda", a friendly package that allows us to analyse various aspects of a corpus

quanteda_texts <- quanteda::corpus(corpus_source,
                                   docid_field = "doc_id",
                                   text_field = "text",
                                   meta = list("author",
                                               "title",
                                               "author_gender",
                                               "year"))

remove(corpus_source)


# Tokens corpus ---------------

# quanteda mainly works with so called DFM (Document-feature matrix). These
# - Represents frequencies of features in documents in a matrix
# - Have an efficient structure, but do not have information on positions of words
# - Allow for a bag-of-words approach


## Let's create a dfm corpus --------------

## first we need to create a "token" corpus. This file is very big,
## so we recommend that you do NOT execute this code. (that's why it's green)
# 
# quanteda_texts_tok <- tokens(quanteda_texts,
#                              # we don't want pucntuation
#                              remove_punct = T,
#                              # we want to keep hyphens
#                              split_hyphens = F,
#                              # but no symbols
#                              remove_symbols = T)
# save(quanteda_texts_tok, file = "quanteda_texts_tok.RData")


# instead, load the one we prepared for you

load("quanteda_texts_tok.RData")

## then we can create a dfm

quanteda_texts_dfm <- dfm(quanteda_texts_tok)

# and we can have a first look at the most frequent words, for instance with a wordcloud

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

# in a "table" form

textstat_frequency(quanteda_texts_dfm) %>%
  head(30)

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

textstat_frequency(quanteda_texts_dfm) %>%
  head(20)

quanteda_texts_dfm %>% 
  textstat_frequency(n = 15) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()



# Alternatively, we can decide beforehand that we want to remove stopwords
# again, this is a big file, so load it directly.

  # quanteda_texts_tok_nostop <- quanteda::tokens(quanteda_texts,
  #                                        remove_punct = T,
  #                                        split_hyphens = F,
  #                                        remove_symbols = T) %>%
  #   tokens_remove(c(stopwords("english")))
  # 
  # save(quanteda_texts_tok_nostop, file = "quanteda_texts_tok_nostop.RData")
  
load(file="quanteda_texts_tok_nostop.RData")



quanteda_texts_dfm <- dfm(quanteda_texts_tok_nostop)

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

# better, right?

# another thing we can do is to visualize group differences in frequency,
# for instance, we might want to see which words are the most frequently used by women vs men authors.

# colnames(quanteda_texts_dfm@docvars) #this will show you which metadta we have


quanteda_texts_dfm %>% 
  textstat_frequency(n = 30, groups = author_gender) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color=group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)


## or look at single words comparisons:

sorted_features <- topfeatures(quanteda_texts_dfm, n = nfeat(quanteda_texts_dfm))
sorted_features[c("cat", "dog", "rat", "rabbit")]

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


# we can also look at how a specific term is used over time

quanteda_texts_dfm %>% 
  textstat_frequency(groups = as.numeric(year)) %>% 
  filter(feature == "war") %>%
  ggplot(aes(x = group, y = frequency, label=feature)) +
  geom_col(width = .1) +
  geom_point(size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )


# or compare the presence of a term by author

quanteda_texts_dfm %>% 
  textstat_frequency(groups = year) %>% 
  filter(feature == "power") %>%
  ggplot(aes(x = group, y = frequency, label=feature)) +
  geom_col(width = .1) +
  geom_point(size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) 


## Concordance

# with quateda you can also visualise concordances



