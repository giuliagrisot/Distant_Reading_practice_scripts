library(tidyverse)
library(readxl)

# Read the raw text file from Excel
hair_commercials <- read_excel("data/Data in Culture and Society - Mini-project Dataset.xlsx", 
                                                               sheet = "Hair removal scripts")

# Let's specify that this is real data with a new column 'data_type'
hair_commercials <- hair_commercials %>% 
  mutate(data_type = "real")


hair_commercials_synth <- read_excel("data/Data in Culture and Society - Mini-project Dataset.xlsx", 
                               sheet = "Additional_synthetic_data")
# we can also specify that this is synthetic data

hair_commercials_synth <- hair_commercials_synth %>% 
  mutate(data_type = "synthetic")

# Combine the two datasets
hair_commercials <- bind_rows(hair_commercials, hair_commercials_synth)

# Remove the synthetic data
remove(hair_commercials_synth)

# Add a unique id to each row

hair_commercials <- hair_commercials %>% 
  mutate(id = row_number())

save(hair_commercials, file = "data/hair_commercials.RData")
