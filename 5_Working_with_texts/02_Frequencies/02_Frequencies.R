
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

files_list <- list.files("data/corpus", full.names = T, pattern = "txt") %>% sample(5)


corpus_source <- readtext(files_list, encoding = "UTF-8") %>%
  mutate(text = gsub("\\s+"," ", text))  %>%
  as_tibble() %>%
  mutate(doc_id = stringr::str_remove(doc_id, ".txt")) %>%
  left_join(readtext("data/corpus/ELTeC-eng_metadata.tsv",
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
# - Have an efficient structure, but do not have information on the position of words
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
# 
# save(quanteda_texts_tok, file = "data/quanteda/quanteda_texts_tok.RData")


# instead, load the one we prepared for you

load("data/quanteda/quanteda_texts_tok.RData")

## then we can create a dfm

quanteda_texts_dfm <- dfm(quanteda_texts_tok)

# Frequencies -------------

# and now can have a first look at the most frequent words, for instance with a wordcloud

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

# in a "table" form

textstat_frequency(quanteda_texts_dfm) %>%
  head(30)

# or in a plot, such as a this one

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
# save(quanteda_texts_tok_nostop, file = "data/quanteda/quanteda_texts_tok_nostop.RData")

load(file="data/quanteda/quanteda_texts_tok_nostop.RData")



quanteda_texts_dfm <- dfm(quanteda_texts_tok_nostop)

textplot_wordcloud(quanteda_texts_dfm, max_words = 100)

# better, right?


# another thing we can do is to visualize group differences in frequency,
# for instance, we might want to see which words are the most frequently used by women vs men authors.

colnames(quanteda_texts_dfm@docvars) #this will show you which metadta we have


quanteda_texts_dfm %>% 
  textstat_frequency(n = 20, groups = author_gender) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency, color=group)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()


# we can also compare word frequencies by document
textplot_wordcloud(quanteda_texts_dfm, 
                   max_words = 100, 
                   comparison = TRUE,
                   labelsize = 1, 
                   color = rev(RColorBrewer::brewer.pal(10, "Dark2")))

# check which other color palettes you can use with
RColorBrewer::display.brewer.all()


## or look at single words comparisons:

sorted_features <- topfeatures(quanteda_texts_dfm, n = nfeat(quanteda_texts_dfm))
sorted_features[c("cat", "dog", "bird", "rabbit")]



## as we saw before, we can easily craete a table with all the information 
## about toke frequencies in our corpus

frequency_table <- textstat_frequency(quanteda_texts_tok_nostop)

# and observe it either in the console

print(head(frequency_table, 20))

# or as a full table

# view(frequency_table)

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
  # geom_point(size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) +
  ggtitle("Occurrences of the word 'war' over time")


# or compare the presence of a term by author

quanteda_texts_dfm %>% 
  textstat_frequency(groups = author) %>% 
  filter(feature == "power") %>%
  ggplot(aes(x = group, y = frequency, label=feature)) +
  geom_col(width = .1) +
  # geom_point(size = 1) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  ) +
  ggtitle("Occurrences of the word 'power' by author")



# Concordance -----------

# with quanteda you can also visualise concordances (KWIC = keywords in context). Assume we are interested in the theme of 'war' and we want to look at what occurs near the word 'enemy':

kwic_test <- quanteda_texts_tok %>%
  kwic(pattern =  "enem*", window = 10)

head(kwic_test, 10)


# Or we might be interested in the theme of 'gender' and we want to look at what occurs near the word 'woman' AND the word 'female':

kwic_test <- quanteda_texts_tok %>%
  kwic(pattern =  c("woman*","femal*"), 
       window = 5)

head(kwic_test, 10)

# if you want to find multi-word expressions, separate words by white space and wrap the character vector by phrase().

kwic_test <- quanteda_texts_tok %>%
  kwic(pattern = phrase("old woman"))

head(kwic_test)

remove(kwic_test)


# Lexical Diversity -------------

# one more thing that you might want to do, is to chech how different various documents are
# you can do that with the function "textstat_lexdiv()", which calculates various lexical diversity measures based on the number of unique types of tokens and the length of a document. 

# It is useful, for instance, for analysing speakers’ or writers’ linguistic skills, or the complexity of ideas expressed in documents. Often it is used as a measure of style. It is a numerical value that is higher for more diverse texts.

tstat_lexdiv <- quanteda_texts_dfm %>%
  textstat_lexdiv()

head(tstat_lexdiv)


# textstat_dist() calculates similarities of documents or features for various measures. The output is compatible with R’s dist(), so hierarchical clustering can be performed without any transformation.

tstat_dist <- quanteda_texts_dfm %>% 
  textstat_dist() %>%
  as.dist()

tstat_dist %>%
  hclust() %>%
  plot(xlab = "Distance", ylab = NULL)

# you can also use the function textstat_simil() to calculate similarities between documents or features. The output is a matrix of similarities.

tstat_simil <- quanteda_texts_dfm %>% 
  textstat_simil()

head(tstat_simil)

# to plot the similarity you can use a dednogram (like we did before), or a heatmap. We can use ggplot and heatmap() to do that.

tstat_simil %>%
  as.matrix() %>%
  heatmap()

# or with ggplot

tstat_simil %>%
  as.matrix() %>%
  as_tibble(rownames = "doc_id") %>%
  pivot_longer(-doc_id) %>%
  ggplot(aes(x = doc_id, y = name, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = NULL, y = NULL)

# Keyness ----------------

# you can also use the function textstat_keyness() to calculate keyness statistics for features in a corpus. Keyness is a measure of the significance of the difference in frequency of a feature between two groups of texts. It is often used to identify words that are characteristic of a particular genre or author.

tstat_keyness <- quanteda_texts_dfm %>%
  textstat_keyness

colnames(tstat_keyness)

# you can also plot the results. tstat_keyness has these columns: "feature"     "chi2"        "p"           "n_target"    "n_reference". We can use the chi2 value to plot the most significant words.
# The Chi-squared test is a statistical test that is used to determine whether there is a significant association between two categorical variables. In this case, we are using it to determine whether there is a significant association between the frequency of a word in two groups of texts.
# Chi2 value is a measure of the significance of the difference in frequency of a feature between two groups of texts. The higher the value, the more significant the difference.

tstat_keyness %>%
  arrange(desc(chi2)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(feature, chi2), y = chi2)) +
  geom_col() +
  coord_flip() +
  theme_minimal()



# Plot “keyness” in a target and reference group

# If you want to compare the differential associations of keywords in a target and reference group, you can calculate “keyness” which is based on textstat_keyness. In this example, we compare the texts by women with those written by men.

# Create a dfm grouped by gender

gender_dfm <- tokens(quanteda_texts_tok_nostop, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  # tokens_remove(readtext("scripts/first_names.txt")$text) %>%
  tokens_group(groups = author_gender) %>%
  dfm()

# Calculate keyness and determine gender as target group
result_keyness <- textstat_keyness(gender_dfm)

# Plot estimated word keyness
textplot_keyness(result_keyness) 


# obviously doing this focusing on gender is not really meaningful: typically you would look at one author vs the rest, but you would need at least two texts by a single author.

# for instance, we can try with Hamilton vs the rest (if you created another dfm in the previous scrpts, use ine of the authors you have there)

tstat_key <- quanteda_texts_dfm %>% 
  textstat_keyness(target = grepl("Hamilton", quanteda_texts_dfm@docvars$docname_))

textplot_keyness(tstat_key)


# Collocations ----------------


# you can also use the function textstat_collocations() to identify collocations in a corpus. A collocation is a sequence of words that occur together more often than would be expected by chance. Collocations are often used to identify phrases that are characteristic of a particular genre or author. This time you need to use it on the token object, not the dfm.

tstat_collocations <- quanteda_texts_tok_nostop %>%
  textstat_collocations()

head(tstat_collocations)

# you can also plot the results. tstat_collocations has these columns:  collocation, count, count_nested, length, lambda, z. Respectively, count indicates the number of times the collocation occurs in the corpus, count_nested indicates the number of times the collocation occurs in the corpus as a nested collocation, length indicates the number of words in the collocation, lambda indicates the ratio of the observed frequency of the collocation to the expected frequency of the collocation, and z indicates the z-score of the collocation.

tstat_collocations %>%
  arrange(desc(lambda))


# you can also use the function textstat_readability() to calculate readability statistics for documents in a corpus. Readability statistics are measures of the complexity of a text. They are often used to assess the difficulty of a text for readers. The function calculates a number of different readability statistics, including the Flesch-Kincaid Grade Level, the Gunning Fog Index, and the Coleman-Liau Index.


# Readability ------------

tstat_readability <- quanteda_texts %>%
  textstat_readability()

head(tstat_readability)

# you can also plot the results
tstat_readability %>%
  ggplot(aes(x = document, y=Flesch, fill=document)) +
  geom_col() +
  theme_minimal() +
  scale_colour_discrete()



# Your turn!! -----------

# run the same analysis on a different author or on a different corpus
# and then re-run the script from the beginning




# once you are finished experimenting, you can clear the environment with the following command

rm(list = ls())

# and clear the plot window with

dev.off()


