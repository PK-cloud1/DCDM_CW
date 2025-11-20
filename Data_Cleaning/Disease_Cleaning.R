library(dplyr)
library(stringr)

# Use readLines to read entire lines as raw text, avoiding automatic splitting by commas
raw_lines <- readLines("/scratch/grp/msc_appbio/DCDM/Group11/data/Disease_information.csv")[-1]  # 跳過第一行

# Split each line into columns using commas or spaces, depending on the actual data format
raw_df <- tibble(
  full_row = raw_lines
)


# Assume the data has been read using read.csv(header = FALSE) and stored in raw_df
clean_disease_table <- function(raw_df) {
  cleaned_df <- raw_df %>%
    rowwise() %>%
    mutate(
      # Convert the entire row into a single string
      full_row = paste(c_across(everything()), collapse = " "),
      
      # Extract IDs using regular expressions
      DOID = str_extract(full_row, "DOID:\\d+"),
      OMIM = str_extract(full_row, "OMIM:\\d+"),
      MGI = str_extract(full_row, "MGI:\\d+"),
      
      # Extract disease name from the text between DOID and OMIM/MGI
      Disease_Name = {
        if (!is.na(DOID)) {
          # Remove DOID and all identifier tags (e.g., OMIM, MGI), keeping only the disease name segment
          temp <- str_remove(full_row, paste0("^.*", DOID, "\\s*"))
          temp <- str_remove(temp, "\\s*(OMIM|MGI|MGI):\\d+.*$")
          str_squish(temp)
        } else if (!is.na(OMIM)) {
          # If only OMIM is present, extract the text from the beginning of the line up to the OMIM tag
          temp <- str_remove(full_row, paste0("\\s*", OMIM, ".*$"))
          str_squish(temp)
        } else {
          NA_character_
        }
      }
    ) %>%
    ungroup() %>%
    select(DOID, Disease_Name, OMIM, MGI) %>%
    filter((!is.na(DOID) | !is.na(OMIM) | !is.na(MGI)) & !is.na(Disease_Name))%>%
    mutate(Disease_Name = str_replace_all(Disease_Name, ",", " "))
  
}

cleaned_df <- clean_disease_table(raw_df)
View(cleaned_df)

write.csv(cleaned_df, 
          file = "/scratch/grp/msc_appbio/DCDM/Group11/data/cleaned_disease_table.csv",
          row.names = FALSE)

