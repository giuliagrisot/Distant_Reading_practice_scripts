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
  # tokens_remove(readtext("scripts/first_names.txt")$text) %>%
  tokens_group(groups = author_gender) %>%
  dfm()

# Calculate keyness and determine Trump as target group
result_keyness <- textstat_keyness(gender_dfm)

# Plot estimated word keyness
textplot_keyness(result_keyness) 


# obviously doing this focusing on gender is not really meaningful: typically you would look at one author vs the rest, but you would need at least two texts by a single author.

# for instance, we can try with Ward, Humphry, Mrs. (two texts by her are in our small corpus)


## -----------


# reload the initial tokenized texts
load("quanteda_texts_tok.Rdata")

metadata <- readtext("corpus/ELTeC-eng_metadata.tsv",
                     docid_field = "filename" 
                     # the doc_id element is called 
                     #"filename" in the tsv file,
                     # we want to specify that those correspond
)


# separate target and reference corpus
my_selection <- rep("Other", length(quanteda_texts_tok))
my_selection[which(grepl("_Ward", names(quanteda_texts_tok)), names(quanteda_texts_tok))] <- "My author"

# separate target and reference corpus

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

# run the same analysis on a different author or on a different corpus
# tip: you will have just to change the name of the author in line 29
# and then re-run the script from the beginning
