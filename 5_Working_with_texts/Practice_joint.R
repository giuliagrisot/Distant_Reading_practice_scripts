
# space and sentiment

library(readtext)
library(tidyverse)
library(tidytext)

# making a corpus

corpus_source <- readtext("corpus/*.txt") %>%
  sample_n(20)

# geolocations import: https://download.geonames.org/export/dump/

geoloc_uk <- read_delim("GB/GB.txt", col_names = F)

geoloc_uk <- geoloc_uk %>%
  rename(
    space_type = X7,
    token = X2,
    latitude = X5,
    longitude = X6
  ) %>%
  select(token, space_type, latitude, longitude)

# tokenize corpus

colnames(corpus_source)

corpus_tokens <- corpus_source %>%
  unnest_tokens(input = "text", output = "token", to_lower = F, drop = T)



# match space onto the corpus

## choice 1: single-word geolocations only

corpus_space <- corpus_tokens %>%
  left_join(geoloc_uk)

corpus_space %>%
  filter(!is.na(space_type)) %>%
  ggplot(aes(space_type)) +
  geom_histogram(stat="count")


## sentiment?


