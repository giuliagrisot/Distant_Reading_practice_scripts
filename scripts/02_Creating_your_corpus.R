# Where do I start? -------

## Creating your corpus and setting up your data with R Studio -----



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

## packages -----


# Before you begin you will need to load some packages. These allow you to execute specific operations.
# If you have not done so already, you have to install them first: it might take a few minutes and you only have to do it once. If R asks you whether you want to install dependencies for the packages, say yes.

install.packages("tidyverse")
install.packages("tidytext")
install.packages("readtext")
install.packages("readxl")
install.packages("syuzhet")

# Once you have installed the packages you can comment the installation code like this (as mentioned, with "# " at the beginning of a line):

# install.packages("blablabla")

# so this operation will not be execute again in the future.


library(tidyverse)
library(tidytext)
library(readtext)
library(readxl)
library(syuzhet)



# Importing data ----

## txt ----

# One easy way to import texts into R is to start from txt files.

# You might have more than one, so it is important that you store them all together in one folder, and ideally with a consistent filename. Information in the filename can be used later on to add metadata to your dataset. The format "surname_title_year.txt" could be a good option, for example, where the surname and the title have to be one word.

# In order to import a txt file, you can use the "read.delim" function from base R (which means you do not need to install extra packages). 

# let's try it out. As you can see in the files panels, there is a folder called "samples", where some texts in different formats are stored.

# before you execute the code, make sure the working directory is set to your main repository folder (the one "above" the /samples folder)


federer_pilatus <- read.delim("samples/federer_pilatus_1912.txt", # this is the url to your file
                              fileEncoding = "utf-8",  # we want to read it as unicode text
                              header = F) %>% # we do not want the first line as a header 
  rename(text = V1) # we can name the column text

# your file has been imported! in this case, it looks just fine.
# It could be that your texts has lost the sentence structure and it's just one very long string of text. If so, you can split it into sentences, for instance with packages tidytext (the result will be a dataframe), with the formula below:

# federer_sentences <- tidytext::unnest_sentences(federer, input = "text", output = "sentence", to_lower = F)

head(federer_pilatus)


# YOUR TURN 1 ---------

# can you create a corpus with another file in the samples folder?







## multiple txt files ----------

# if you have more than one text, you probably won't want to repeat this operations manually several times.
# you can then proceed as follows:
# (this is just one way but there are many out there)

# run the "readtext" function from the "readtext" package, simply indicating the folder in which your texts are stored, and the format preceded by "*." (this means "all files that have this extension").

corpus <- readtext("samples/*.txt", encoding = "UTF-8")  %>%
  unnest_sentences(input = "text",
                   output = "sentence",
                   to_lower = F, drop = T) %>%
  as_tibble()

head(corpus)


# let's see which files are in our corpus:

corpus %>% 
  select(doc_id) %>%
  distinct()


# now, as we mentioned you might want to use the information in the filename to create more variables (that's how "columns" are called in R) in our corpus

corpus <- corpus %>%
  mutate(sentence = str_squish(sentence)) %>% # eliminate unwanted extra spaces
  separate(doc_id, into = c("author", "title", "year"), sep = "_", remove = T) %>% # and separate the metadata
  mutate(year = str_remove(str_trim(year, side = "both"), ".txt")) # and make sure there are no extra spaces before/after the words

corpus$year <- as.numeric(corpus$year)

# let's see how it looks

head(corpus)

# Neat, right?

# you might also want to add an identification number for the sentences, which can be useful for later analysis

corpus <- corpus %>%
  group_by(title) %>%
  mutate(sentence_id = seq_along(sentence)) %>% # this means "sequence along the column sentence"
  ungroup()

# and we might want then to split the text into tokens. we can easily use the unnest_tokens function of tidytext:

corpus_token <- unnest_tokens(corpus, input = "sentence", output = "token", to_lower = F, drop = F)

# as we did for sentences, we might want to preserve the position of the tokens insider sentences, and add a token_id index

corpus_token <- corpus_token %>%
  group_by(title, sentence) %>%
  mutate(token_id = seq_along(token)) %>% # this means "sequence along the column "token"
  ungroup()


# so let's see how does it look now

head(corpus_token, 10)

# splitting into tokes can be useful if we want to match our corpus to lists of 
# labelled data (for instance, locations or sentiment lexicons).
# We'll talk about this during the SA session.





## csv and xslx ----

# another common format for texts is csv or xlsx. Importing a this is very easy, because R understands the csv and xslx formats well. You can either use code, or click directly on the file you want to import in the files panel.
# R studio will ask if you want to import it, and you will be able to determine options with a friendly interface.

# navigate into the samples folder and click on the file small_corpus.xlsx. or 
# execute the following code

pride_excel <- read_excel("samples/pride.xlsx")

# have a look at it

head(pride_excel)


## multiple files ----

# the procedure similar to the one we saw for the txt files, except it has read_excel as function, and it does not need to add a header or other variables

corpus_source <- readtext("samples/*.xlsx")

head(corpus_source)

## epubs ----------

## another format that is quite popular today, especially for books, is the epub
## for these files you will need a specific package, calle "epubr"

# install.packages("epubr")

library(epubr)

kafka_all <- epubr::epub("samples/kafka.epub")

# have a look a the dataset "kafka_all"

structure(kafka_all)

# epubs often have a more complex internal structure, and you might need to modify your dataset according to your needs

kafka_werke <- kafka_all[[9]][[1]]

head(kafka_werke)
