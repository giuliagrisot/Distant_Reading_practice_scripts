library(tidyverse)
library(readxl)

# Read the raw text file from Excel
file_path <- "data/Data in Culture and Society - Mini-project Dataset.xlsx"
slogans <- read_excel(file_path, sheet = "Slogans", col_names = FALSE) %>%
  rename(Slogan_Text = 1)

# Initialize an empty dataframe
slogans_df <- tibble(Company = character(), Country = character(), Year = integer(), Slogan = character())

# Variables to track company and country
company <- NA
country <- NA

# Process each line
for (line in slogans$Slogan_Text) {
  line <- str_trim(line)  # Remove leading/trailing spaces
  
  if (str_detect(line, "^Slogans of")) {
    # Extract company and country
    match <- str_match(line, "^Slogans of ([^(]+) \\(([^)]+)\\)$") # if string starts with "Slogans of" followed by company name and country
    if (!is.na(match[1])) { # if match is not NA
      company <- str_trim(match[2]) # trim the company name
      country <- str_trim(match[3]) # trim the country name
    }
  } else if (str_detect(line, "\\((\\d{4})\\)$")) { # if string ends with a year in brackets
    # Extract year and slogan 
    match <- str_match(line, "^(.+) \\((\\d{4})\\)$") # if string starts with any character followed by year in brackets
    slogan_text <- str_trim(match[2]) # trim the slogan text
    year <- as.integer(match[3]) # convert year to integer
    
    # Add to dataframe (ensuring column names match)
    slogans_df <- add_row(slogans_df, 
                          Company = company, 
                          Country = country, 
                          Year = year, 
                          Slogan = slogan_text)
  }
}

# Print the structured dataframe
print(slogans_df)

# Save as CSV
write_csv(slogans_df, "data/structured_slogans.csv")

remove(file_path, slogans, company, country, line, match, slogan_text, year)

# End of script

