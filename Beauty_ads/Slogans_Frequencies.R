
# in this script we will explore the dataset 'slogans_df', containing slogans of beauty products, their date of publication and the company that published them. We will use the 'slogans_df' dataset to answer the following questions:

# 1. What are the top 5 most common words in the slogans?

# 2. What are the top 5 most common words in the slogans of each company?

# 3. Is there a gender bias in the slogans? (i.e. do the slogans contain more words that are associated with a specific gender?)

# 4. is there an emotional bias in the slogans, and does it differentiate between the gender the product is aimed at?

# The dataset contains the following columns:

# Company, Country, Year, Slogan

# Let's start by loading the dataset and taking a look at the first few rows.

# Load the dataset

# Read the file into a variable slognas_df

library(readr)
library(tidyverse)
library(tidytext)
library(textdata)
library(syuzhet)
library(wesanderson)
library(pals)

# Load the dataset
slogans_df = read_csv(file = 'data/structured_slogans.csv')

# Display the first few rows of the dataset
head(slogans_df)

# let's check how many companies we have
unique(slogans_df$Company) %>% length()


# Count the number of slogans per company
company_slogan_count <- slogans_df %>%
  count(Company, sort = TRUE)

# we have here more than 2000 companies, so we might want to limit the number of companies we are going to use in the wordcloud. Let's use the 10 companies with most slogans.


# Select the top 15 companies with the most slogans

top_companies <- company_slogan_count %>%
  slice(1:15) %>%
  pull(Company)

# Filter the slogans for the top 10 companies

slogans_df <- slogans_df %>%
  filter(Company %in% top_companies)

# let's save this new dataset

save(slogans_df, file = 'data/slogans_df_top.RData')

# Let's start with question 1: What are the top 5 most common words in the slogans?

# To answer this question, we will first need to preprocess the slogans by converting them to lowercase, removing punctuation, and tokenizing the text into words.

# Preprocess the slogans

# Convert the slogans to lowercase

slogans_df_tok <- slogans_df %>% # we create a new dataset with the same columns as the original dataset
  mutate(Slogan = tolower(Slogan)) %>% # Convert the slogans to lowercase
  mutate(Slogan = str_replace_all(Slogan, "[[:punct:]]", "")) %>% # Remove punctuation from the slogans
  unnest_tokens(word, Slogan) %>%  # Tokenize the slogans into words
  ungroup()

# Now let's count the frequency of each word in the slogans

# Count the frequency of each word

word_freq <- slogans_df_tok %>%
  count(word, sort = TRUE)

# we can try and remove the stopwords from the word_freq table, using the stop_words function from the 'tidytext' package. If unspecified, they will be in English.

# Load the stopwords

stopwords <- tidytext::stop_words

# Remove the stopwords from the word frequency table
word_freq <- word_freq %>%
  anti_join(stopwords, by = "word")

# Display the top 5 most common words
head(word_freq, 5)



# ---------- Now let's move on to question 2: What are the top 5 most common words in the slogans of each company?


# To answer this question, we will group the slogans by company and then count the frequency of each word within each group.

# Count the frequency of each word in the slogans of each company

word_freq_company <- slogans_df_tok %>%
  anti_join(stopwords, by = "word") %>% # Remove stopwords
  count(Company, word, sort = TRUE) %>% # Count the frequency of each word in the slogans of each company
  group_by(Company) %>%
  slice(1:5) # Select the top 5 most common words for each company


head(word_freq_company_top, 20)

# we can also plot the top 5 words for each company

word_freq_company %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = Company)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 8)
  )



# we can do similar and more complex analysis easily with the package quanteda. Let's see how we can use it to analyse the slogans.
# let's empty our environment and start from scratch.

rm(list = ls())

# we can also restart the R session to make sure our memory is not full

# install.packages("quanteda")
# install.packages("quanteda.textplots")
# install.packages("quanteda.textstats")

library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(wesanderson)
library(dplyr)
library(ggplot2)
library(pals)
library(sjPlot)
library(readr)

color_palette_kelly <- kelly(n = 11)
color_palette_kelly <- color_palette_kelly[2:11]

# load the dataset


read_csv(file = 'data/structured_slogans.csv') -> slogans_df


# and to transform this into a corpus that we can use with the package "quanteda", a friendly package that allows us to analyse various aspects of a corpus

# Corpus ---------------

# quanteda wants each slogan to be ideantifiable and unique, so let's add a 'doc_id' number to each slogan

slogans_df <- slogans_df %>%
  mutate(doc_id = row_number())

quanteda_texts <- quanteda::corpus(slogans_df,
                                   docid_field = "doc_id",
                                   text_field = "Slogan",
                                   meta = list("Company",
                                               "Year",
                                               "Country"))

# remove(slogans_df) # we can remove the original dataset if we want to save memory


# Tokens corpus ---------------

# quanteda mainly works with so called DFM (Document-feature matrix). These
# - Represents frequencies of features in documents in a matrix
# - Have an efficient structure, but do not have information on the position of words
# - Allow for a bag-of-words approach


## Let's create a dfm corpus --------------

# first we need to create a "token" corpus. This file is very big,
# so we recommend that you do NOT execute this code. (that's why it's green)

# we can already remove stopwords and punctuation
# 
# quanteda_texts_tok <- tokens(quanteda_texts,
#                              # we don't want pucntuation
#                              remove_punct = T,
#                              # we want to keep hyphens
#                              split_hyphens = F,
#                              # but no symbols
#                              remove_symbols = T,
#                              # we want to remove numbers
#                              remove_numbers = T,
#                              # we want to remove stopwords
#                              ) %>%
#   tokens_remove(stopwords("en"))
# 
# save(quanteda_texts_tok, file = "data/quanteda_texts_tok.RData")


# instead, load the one we prepared for you

load("data/quanteda_texts_tok.RData")

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


# we might be interested in the difference between companies (for now let's just select random 10 companies for demonstration)


# Ensure 'Company' exists in the metadata of the dfm
company_vector <- quanteda_texts$Company  # Extract company info separately

# Compute word frequencies per company
freq_data <- quanteda_texts_dfm %>%
  textstat_frequency(groups = company_vector) %>%
  filter(!feature %in% stopwords("en")) %>%  # Remove stopwords
  group_by(group)

# Filter companies that have at least 4 words with frequency > 2
companies_with_enough_words <- freq_data %>%
  filter(frequency > 2) %>%
  count(group) %>%
  filter(n >= 4) %>%
  pull(group)

save(companies_with_enough_words, file = "data/companies_with_enough_words.RData")

# Sample up to 20 companies from the filtered set
selected_companies <- sample(companies_with_enough_words, min(30, length(companies_with_enough_words)))

# Filter original data for selected companies
freq_data %>%
  filter(group %in% selected_companies & frequency > 2) %>%
  mutate(feature = reorder(feature, frequency)) %>%
  ggplot(aes(x = feature, y = frequency, fill = group)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  facet_wrap(~group, scales = "free") +
  theme_minimal()


# we can obtain a similar plot also with a slightly different code. This code will allow us to see the top 15 words per company ordered by frequency for each company.

fs=12

freq_data %>%
  filter(group %in% selected_companies & frequency > 2) %>% # group by company
  arrange(desc(frequency)) %>% # highest freq on top
  group_by(group) %>% # group by company
  mutate(top = seq_along(feature)) %>% # identify rank within group
  filter(top <= 15) %>% # retain top 15 frequent words
  # create barplot
  ggplot(aes(x = -top, fill = group)) +  # plot bars
  geom_bar(aes(y = frequency), stat = 'identity', col = 'black') + # make bars
  # make sure words are printed either in or next to bar
  geom_text(aes(y = ifelse(frequency > max(frequency) / 2, # we add a layer of text and we use the ifelse function to determine if the frequency is higher than half of the maximum frequency, so that we can print the word inside the bar or next to it depending on the frequency.
                           max(frequency) / 50, frequency +  
                             max(frequency) / 50),
                label = feature), size = fs/3, hjust = "left") + # add text
  theme(legend.position = 'none', # get rid of legend
        text = element_text(size = fs), # determine fs
        axis.text.x = element_text(angle = 45, hjust = 1, size = fs/1.5), # rotate x text
        axis.ticks.y = element_blank(), # remove y ticks
        axis.text.y = element_blank()) + # remove y text
  labs(y = "token count", x = "", # add labels
       title = "Most frequent words throughout the novels") +
  facet_grid(. ~ group) + # separate plot for each title
  coord_flip() + # flip axes
  scale_fill_sjplot() # use sjPlot color palette

