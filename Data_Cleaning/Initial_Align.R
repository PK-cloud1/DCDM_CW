#Initial aligning of raw data

# Install and load required package
install.packages("dplyr")
library(dplyr)

# Function to clean and parse procedure-style CSV files
clean_data <- function(input_path) {
  raw_lines <- readLines(input_path)[-1]
  
  parsed_entries <- lapply(raw_lines, function(entry) {
    fields <- strsplit(entry, ",")[[1]]
    
    if (length(fields) < 4) {
      warning(paste("Skipping malformed entry:", entry))
      return(NULL)
    }
    
    if (length(fields) > 4) {
      label <- trimws(fields[1])
      mandatory_flag <- trimws(fields[length(fields) - 1])
      parameter_id <- trimws(fields[length(fields)])
      details <- trimws(paste(fields[2:(length(fields) - 2)], collapse = ","))
    } else {
      label <- trimws(fields[1])
      details <- trimws(fields[2])
      mandatory_flag <- trimws(fields[3])
      parameter_id <- trimws(fields[4])
    }
    
    label <- trimws(gsub("^\\d+\\s+", "", label))
    
    return(data.frame(
      label = label,
      details = details,
      mandatory_flag = mandatory_flag,
      parameter_id = parameter_id,
      stringsAsFactors = FALSE
    ))
  })
  
  parsed_entries <- parsed_entries[!sapply(parsed_entries, is.null)]
  result_df <- do.call(rbind, parsed_entries)
  rownames(result_df) <- NULL
  
  return(result_df)
}

# Directory containing raw CSV files
source_folder <- "/scratch/grp/msc_appbio/DCDM/Group11/data"

# Identify all CSV files in the directory
file_list <- list.files(path = source_folder, pattern = "\\.csv$", full.names = TRUE)

# Apply the cleaning function to each file
merged_data <- lapply(file_list, function(path) {
  message("Processing file: ", path)
  clean_data(path)
})

# Combine all cleaned data frames
final_dataset <- bind_rows(merged_data)

# Preview results
View(final_dataset)
head(final_dataset)
warnings()
length(file_list)
print(file_list[1])
readLines(file_list[1], n = 5)
sample_df <- clean_data(file_list[1])
head(sample_df)

# Alternative function for key-value style CSVs
parse_key_value_data <- function(input_path) {
  raw_content <- readLines(input_path)
  
  kv_table <- lapply(raw_content, function(entry) {
    pair <- strsplit(entry, ",")[[1]]
    if (length(pair) == 2) {
      key_field <- trimws(pair[1])
      value_field <- trimws(pair[2])
      return(data.frame(key_field = key_field, value_field = value_field, stringsAsFactors = FALSE))
    } else {
      warning(paste("Skipping malformed entry:", entry))
      return(NULL)
    }
  })
  
  kv_table <- kv_table[!sapply(kv_table, is.null)]
  result_df <- do.call(rbind, kv_table)
  return(result_df)
}

# Apply key-value parser to all files
merged_data <- lapply(file_list, function(path) {
  message("Processing file: ", path)
  parse_key_value_data(path)
})

# Combine results
combined_rawdata <- bind_rows(merged_data)

# Rename the columns
colnames(combined_rawdata) <- c("key", "value")

# Define the path where the output (combined_rawdata should go in HPC)
output_combinedraw <- "/scratch/grp/msc_appbio/DCDM/Group11/combined_rawdata.csv"

# Save the data frame as a CSV file
write.csv(combined_rawdata, file = output_combinedraw, row.names = FALSE)

# There are 8 variables in each case
rows_per_case <- 8

# Define a variable for the total number of cases
num_cases <- nrow(combined_rawdata) / rows_per_case

# Split the data into a list of data frames, each with 8 rows
case_list <- split(combined_rawdata, rep(1:num_cases, each = rows_per_case))

# Merge all the data frames inside case_list into a single dataframe, stacking the rows
do.call(rbind.data.frame, case_list)

# Assign it to a variable
combined_columns <- do.call(rbind.data.frame, case_list)

print(combined_columns[[1]])
length(combined_columns[[1]])