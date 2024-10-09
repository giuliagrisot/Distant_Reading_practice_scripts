# Where do I start? -------

# Creating your corpus and setting up your data with R Studio -----



# This is an R script file, created by Giulia (reads like English "Julia")

# Everything written after an hashtag is a comment (normally appears in green). If you don't want to type the hash manually every time, you can type your comments normally, and after you finish, with the cursor on the sentence, press ctrl+shift+c. it will turn text into a comment and vice versa.

# Everything else is R code. To execute the code, place the cursor on the corresponding line and press Ctrl+Enter (windows)

# If you are a beginner, don't worry: for today's practice you will not need much knowledge of R. The scripts are provided for you. You will be guided through a simple case exploratory Sentiment Analysis, and then use those same scripts to experiment with data in your possess or of your choice.
# If you are unfamiliar with R language and basic operations and want to learn more about it, there is plenty of tutorials online. Have a look at the resources at the end of this script for a few recommendations.

# before you start, check the working directory!
# you can click on the Files panel, go to the folder you are going to work in, and once you are inside click on the little arrow near the "More" button, and select "Set as working directory"


# now we're ready to start!




# PS: Have you noticed that there is a little symbol on the top right of this panel, made of little horizontal lines? It shows and hide the document outline. if you write a comment (any text preceded by a "#" and a space) and put a series of --------- (any number, more than 4) after it, the comment will become the header of a section, which you can then see in the outline for an easier navigation of the script.



# Creating your dataset ----------

# Often one of the factors that prevents us humanists from doing computational analysis is that tutorials sometimes assume that a certain amount of knowledge is somehow pre-existing. Unfortunately, it is often not the case.
# So it happens that right when you want to finally try to adapt someone else's existing scripts to your lovely literary texts (yes, that's how we often do, and it's ok!), you are not really sure how to put those books into a shape that you can use and analayse.

# Here we will try and show how different text formats can be imported in R and made ready for some analysis.

# Packages -----


# Before you begin you will need to load some packages. These allow you to execute specific operations.
# If you have not done so already, you have to install them first: it might take a few minutes and you only have to do it once. If R asks you whether you want to install dependencies for the packages, say yes


# UNCOMMENT THE FOLLOWING LINES AND EXECUTE; THEN COMMENT AGAIN

# install.packages("tidyverse")
# install.packages("tidytext")
# install.packages("readtext")
# install.packages("readxl")
# install.packages("syuzhet")

# Once you have installed the packages you can comment the installation code like this (as mentioned, with "# " at the beginning of a line):

# install.packages("blablabla")

# so this operation will not be execute again in the future. (it does not need to)


library(tidyverse)
library(tidytext)
library(readtext)
library(readxl)
library(syuzhet)



# Importing data ----

## .txt files ----

# One easy way to import texts into R is to start from txt files.

# You might have more than one, so it is important that you store them all together in one folder, and ideally with a consistent filename (often also named doc_id). Information in the filename can be used later on to add metadata to your dataset. The format "surname_title_year.txt" could be a good option, for example, where the surname and the title have to be one word.

# In order to import a txt file, you can use the "read.delim" function from base R (which means you do not need to install extra packages). 

# let's try it out. As you can see in the files panels, there is a folder called "samples", where some texts in different formats are stored.

# before you execute the code, make sure the working directory is set to your main repository folder (the one "above" the /samples folder)

getwd()


ENG18400_Trollope <- readtext("data/corpus/ENG18400_Trollope.txt", 
                              encoding = "utf-8",  # we want to read it as unicode text
                              ) # we can name the column text

head(ENG18400_Trollope)

# your file has been imported! in this case, it looks just fine.

# It could be that your texts has lost the sentence structure and it's just one very long string of text. If so, you can split it into sentences, for instance with packages tidytext (the result will be a dataframe), with the formula below:

names(ENG18400_Trollope)

ENG18400_Trollope_sentences <- unnest_sentences(ENG18400_Trollope, 
                                                          input = "text", 
                                                          output = "sentence",
                                                          to_lower = F) 

# let's have a look

head(ENG18400_Trollope_sentences)

# if necessary (like in this case -- have a look at sentences 3 and 6) we we can also eliminate extra white spaces

ENG18400_Trollope_sentences <- ENG18400_Trollope_sentences %>%
  mutate(sentence = gsub("\\s+"," ", sentence))


head(ENG18400_Trollope_sentences)


# YOUR TURN 1 ---------

## can you create a corpus with another file in the corpus folder?















# # this command will empty our environment
# rm(list = ls())



# Multiple .txt files ----------

# if you have more than one text, you probably won't want to repeat this operations manually several times.
# you can then proceed as follows:
# (this is just one way but there are many out there)

# run the "readtext" function from the "readtext" package, simply indicating the folder in which your texts are stored, and the format preceded by "*." (this means "all files that have this extension").

# the corpus we are using here is the ELTEC UK collection, available online.
# because 100 texts require quite a lot of processing effort, for this practice
# we have scaled it down to 5

# this code creates a random list of 5 of the files inside the folder 'corpus"

files_list <- list.files(path = "data/corpus", full.names = T, pattern = "txt") %>% sample(5)

corpus <- 
  readtext(files_list, encoding = "UTF-8") %>%
          # readtext("data/corpus/*.txt", encoding = "UTF-8") %>% # this would read the whole corpus
  mutate(text = gsub("\\s+"," ", text)) # let's not forget about those extra white spaces

head(corpus)

# if we waned to reduce the corpus after having created it we could do this
# corpus <- corpus %>%
#   sample_n(size = 10)
# head(corpus)


# Split sentences -------

# for the moment, each row contains a whole book in text form, under the variable "text"
# we might what to split that into sentences

corpus_sentence <- corpus %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, # for the moment we do not want to convert to lower case
                   drop = T) %>% # we can spare memory and drop the 'full text', 
                                  # keeping only the new column" sentence"
  as_tibble()
  
#  you might have noticed that this package (tidytext) splits a sentence at full stop, even when we night not want to (for instance with Mr. or Mrs.)
# You can try a different package and see if it works better, or correct it manually

corpus <- corpus %>%
  mutate(text = str_replace_all(text, "Mrs. ", "Mrs "))  %>%
  mutate(text = str_replace_all(text, "Mr. ", "Mr ")) %>%
  mutate(text = str_replace_all(text, "Dr. ", "Dr "))


# re-run the corpus sentence creation code

corpus_sentence <- corpus %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, # for the moment we do not want to convert to lower case
                   drop = T) %>% # we can spare memory and drop the 'full text', 
  # keeping only the new column" sentence"
  as_tibble()

# let's have another look

corpus_sentence %>%
  filter(grepl("Mr", sentence))


# the alternative is to use a different package or method.
# one way to go about it is to use a more complex language model via the package Udpipe.
# you should make sure you have a decent working memory and processing power, because it is a rather intensive job. you can reduce the sample to 3 texts just to see.

corpus3 <- corpus %>%  
  sample_n(size = 3) %>%
  mutate(text = str_sub(text, 1, 1000)) # we'll look at a subset to make it less intensive

library(udpipe)

corpus_udp <- udpipe(x = corpus3$text, object = "english")

view(corpus_udp)

# now, as we mentioned you might want to use the information in the doc_id to create more variables (that's how "columns" are called in R) in our corpus
# alternatively, and maybe more efficiently, you can have a separate file where you store metadata.
# Just remember to make yure that the variable "doc_id" in the corpus and in the metadata correspond,
# otherwise you won' be able to match the data to the corpus.


# Add metadata ------

# so let's first load the metadata

metadata <- readtext("data/corpus/ELTeC-eng_metadata.tsv",
                     docid_field = "filename" 
                          # the doc_id element is called 
                          #"filename" in the tsv file,
                          # we want to specify that those correspond
                     )

metadata


# we can see a variable "text" in the metadata, which really is the "collection". let's rename it

metadata <- metadata %>%
  rename(collection = text)

# if you have a look and the doc_id, you'll see that here the doc_id does not features the extension .txt

metadata$doc_id

# while the same is not true for the doc_id in the corpus

unique(corpus_sentence$doc_id)


# we must then either remove the ".txt" string from one, or add it to the other.

# let's remove it from the corpus with the appropriate function from the package "stringr":

corpus_sentence <- corpus_sentence %>%
  mutate(doc_id = str_remove(doc_id, ".txt"))

unique(corpus_sentence$doc_id) # now it should look fine


# it might not be necessary to do so, but you might want to combine the two
# you can do so easily weith the left_join function of the dplyr package, as long as there is one common variable (that's why we cared about the doc_id matching)


corpus_sentence <- corpus_sentence %>%
  left_join(metadata, by = "doc_id")

corpus_sentence$first.edition <- as.numeric(corpus_sentence$first.edition)

# let's see how it looks

head(corpus_sentence)

# Neat, right?

# you might also want to add an identification number for the sentences, which can be useful for later analysis

corpus_sentence <- corpus_sentence %>%
  group_by(doc_id) %>% # your doc_id must be always unique
  mutate(sentence_id = seq_along(sentence)) %>% # this means "sequence along the column sentence"
  ungroup()


view(head(corpus_sentence))


# Tokenization -------

# we might want then to split the text into tokens.
# we can easily use the unnest_tokens function from tidytext:

corpus_token <- unnest_tokens(corpus_sentence,
                              input = "sentence",
                              output = "token", 
                              to_lower = F, 
                              drop = F)

# 

corpus_ngrams <- unnest_ngrams(corpus_sentence,
                              input = "sentence",
                              output = "ngram",
                              n = 5,
                              to_lower = F, 
                              drop = F)

# as we did for sentences, we might want to preserve the position of the tokens inside sentences, 
# and add a token_id index

corpus_token <- corpus_token %>%
  group_by(title, sentence) %>%
  mutate(token_id = seq_along(token)) %>% # this means "sequence along the column "token"
  ungroup()


# so let's see how does it look now

head(corpus_token, 10)

# splitting into tokes can be useful if we want to match our corpus to lists of 
# labelled data (for instance, locations or sentiment lexicons).
# We'll talk about this during the SA session.





# .csv and .xslx ----

# another common format for texts is csv or xlsx. Importing a this is very easy, because R understands the csv and xslx formats well. You can either use code, or click directly on the file you want to import in the files panel.
# R studio will ask if you want to import it, and you will be able to determine options with a friendly interface.

# navigate into the samples folder and click on the file small_corpus.xlsx. or 
# execute the following code

pride_excel <- read_excel("data/samples/pride.xlsx")

# have a look at it

head(pride_excel)


## multiple .xlsx ----

# the procedure similar to the one we saw for the txt files, except it has read_excel as function, and it does not need to add a header or other variables

sample_texts <- readtext("data/samples/*.xlsx")

head(sample_texts)

remove(sample_texts, pride_excel)



## epubs ----------

## another format that is quite popular today, especially for books, is the epub
## for these files you will need a specific package, calle "epubr"

# install.packages("epubr")

library(epubr)

kafka_all <- epubr::epub("data/samples/kafka.epub")

# have a look a the dataset "kafka_all"

structure(kafka_all)

# epubs often have a more complex internal structure, and you might need to modify your dataset according to your needs

kafka_werke <- kafka_all[[9]][[1]]

head(kafka_werke)


remove(kafka_all, kafka_werke)


# and that's it!
# you can remove all objects from your environents with this code

rm(list = ls()) 

