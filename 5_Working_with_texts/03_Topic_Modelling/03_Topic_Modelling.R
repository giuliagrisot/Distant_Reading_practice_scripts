# TOPIC MODELLING

# for a very good tutorial, see https://quanteda.io/articles/pkgdown/replication/digital-humanities.html


# basic topic modelling can be done with quanteda. You can use the function textmodel_lda() to fit a Latent Dirichlet Allocation (LDA) model to a document-feature matrix (dfm). The function requires a dfm and the number of topics (k) to be specified. The function returns a textmodel_lda object, which can be used to extract the top terms for each topic.

# The function terms() can be used to extract the top terms for each topic. The function requires the textmodel_lda object and the number of terms to be extracted for each topic. The function returns a matrix with the top terms for each topic.

# Let's try.

# First let's load the data and create a dfm

library(quanteda)

# Load data
load("data/quanteda/quanteda_texts_tok_nostop.RData")

# Create a dfm
quanteda_texts_dfm <- quanteda_texts_tok %>%
  dfm()

library(topicmodels)




# Fit LDA model
# now we will fit a LDA model to the dfm. We will specify 10 topics.
quanteda_texts_lda <- textmodel_lda(quanteda_texts_dfm, k = 10)

# Extract top terms for each topic
quanteda_texts_terms <- terms(quanteda_texts_lda, 10)

# Print top terms for each topic
quanteda_texts_terms

# This corpus is probably too small, but you might be still able to see some interesting patterns. Remember this requires your critical interpretation. 


# You can also use the function convert() to convert a dfm to a DocumentTermMatrix object, which can be used to fit a LDA model with the topicmodels package. The function requires the dfm and the target format to be specified. The function returns a DocumentTermMatrix object, which can be used to fit a LDA model with the LDA() function.

LDA_fit <- convert(quanteda_texts_dfm, to = "topicmodels") %>%
  LDA(k = 10)

# A get top five terms per topic
get_terms(LDA_fit, 10)

# You can see that the results are fairly different with these two procedures. Always read what is happening in the background, and choose the best method for your data.



# once you are finished experimenting, you can clear the environment with the following command

rm(list = ls())

# and clear the plot window with

dev.off()
