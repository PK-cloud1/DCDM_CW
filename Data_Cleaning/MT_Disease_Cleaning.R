#MT attempt at improving Disease_cleaning script
install.packages("dplyr")
install.packages("stringr")

# Load required libraries
library(dplyr)
library(stringr)

# Read the raw file (skip header row if present)
raw_lines <- readLines("/scratch/grp/msc_appbio/DCDM/Group11/data/Disease_information.csv")[-1]

# Wrap into a tibble
raw_df <- tibble(full_row = raw_lines)

# Define the cleaning function
clean_disease_table <- function(raw_df) {
  raw_df %>%
    mutate(
      DOID = str_extract(full_row, "DOID:\\d+"),
      OMIM = str_extract(full_row, "OMIM:\\d+"),
      MGI  = str_extract(full_row, "MGI:\\d+"),
      Disease_Name = case_when(
        !is.na(DOID) ~ {
          temp <- str_remove(full_row, paste0("^.*", DOID, "\\s*"))
          temp <- str_remove(temp, "\\s*(OMIM|MGI):\\d+.*$")
          str_squish(temp)
        },
        !is.na(OMIM) ~ {
          temp <- str_remove(full_row, paste0("\\s*", OMIM, ".*$"))
          str_squish(temp)
        },
        TRUE ~ NA_character_
      )
    ) %>%
    filter((!is.na(DOID) | !is.na(OMIM) | !is.na(MGI)) & !is.na(Disease_Name)) %>%
    mutate(Disease_Name = str_replace_all(Disease_Name, ",", " ")) %>%
    # Explicitly select and order the four columns
    select(DOID, Disease_Name, OMIM, MGI)
}

# Run the cleaning function
cleaned_df <- clean_disease_table(raw_df)

# View the cleaned table in RStudio (optional)
# View(cleaned_df)

# Write the cleaned table to CSV with headers DOID, Disease_Name, OMIM, MGI
write.csv(
  cleaned_df,
  file = "/scratch/grp/msc_appbio/DCDM/Group11/data/mt_cleaned_disease_table.csv",
  row.names = FALSE
)

