# Keyness analysis


# call the packages
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyverse)
library(readtext)



# load the tokenized texts
load("quanteda_texts_tok.Rdata")

# let#s see which authors we have in here

list(names(quanteda_texts_tok))

# Plot “keyness” in a target and reference group
# If you want to compare the differential associations of keywords in a target and reference group, you can calculate “keyness” which is based on textstat_keyness. In this example, we compare the texts by women with those written by men.

# Create a dfm grouped by gender

gender_dfm <- tokens(quanteda_texts_tok, remove_punct = TRUE) %>%
  tokens_remove(stopwords("english")) %>%
  tokens_group(groups = author_gender) %>%
  dfm()

# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(gender_dfm)

# Plot estimated word keyness
textplot_keyness(result_keyness) 



## -----------


# define the name of the author on which we want to work
my_author <- "Dickens, Charles"


# separate target and reference corpus
my_selection <- rep("Other", length(quanteda_texts_tok))
my_selection[which(metadata$author == my_author)] <- "My author"
quanteda_texts_tok <- tokens_group(quanteda_texts_tok, groups = my_selection)



# separate target and reference corpus (for instance, Eliot)

quanteda_texts_tok <- tokens_group(quanteda_texts_tok, groups = my_selection)

# transform the corpus into a document-feature matrix
document_feature_matrix <- dfm(quanteda_texts_tok)
# note that the "grouping" is based on the names of the corpus, i.e. "My author" and "Others"

# calculate the keyness for each word
# choosing as a target the documents with the "My author" name
# and using as a measure the "log-likelihood ratio" method ("lr")
keyness_results <- textstat_keyness(document_feature_matrix, target = "My author", measure = "lr")

# plot the results!
textplot_keyness(keyness_results, n = 20)

################
### Your turn!!
#############

# run the same analysis on a different author
# tip: you will have just to change the name of the author in line 29
# and then re-run the script from the beginning
