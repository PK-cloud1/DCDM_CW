
# Defines the main function to clean the IMPC CSV file.
clean_impc_csv <- function(input_file, output_file = NULL) {
  # Sets default output filename by appending _cleaned.csv.
  if (is.null(output_file)) {
    output_file <- sub(".csv", "_cleaned.csv", input_file, fixed = TRUE)
  }
  
  # Reads the raw lines from the input file.
  raw_lines <- readLines(input_file, encoding = "UTF-8", warn = FALSE)
  total_lines <- length(raw_lines) - 1  # Exclude header
  
  # Initializes vectors to store parsed data.
  impc_ids <- character(total_lines)
  names_vec <- character(total_lines)
  descriptions <- character(total_lines)
  parameter_ids <- character(total_lines)
  
  valid_count <- 0
  
  # Starts parsing from line 2 (skipping header).
  for (line_num in 2:length(raw_lines)) {
    line <- raw_lines[line_num]
    
    # Splits the line into fields using comma delimiter.
    fields <- strsplit(line, ",", fixed = TRUE)[[1]]
    
    # Skips lines with fewer than 3 fields.
    if (length(fields) < 3) {
      next
    }
    
    valid_count <- valid_count + 1
    
    # Extracts impcParameterOrigId from the first field.
    impc_ids[valid_count] <- if (fields[1] != "") fields[1] else NA_character_
    
    # Finds the last non-empty field to use as parameterId.
    parameter_id <- NA_character_
    last_valid_idx <- -1
    
    for (i in length(fields):1) {
      if (fields[i] != "") {
        parameter_id <- fields[i]
        last_valid_idx <- i
        break
      }
    }
    
    parameter_ids[valid_count] <- parameter_id
    
    if (last_valid_idx <= 2) {
      names_vec[valid_count] <- NA_character_
      descriptions[valid_count] <- NA_character_
      next
    }
    
    # Extracts middle fields between impcParameterOrigId and parameterId.
    middle_fields <- fields[2:(last_valid_idx - 1)]
    
    # Identifies indices of empty fields.
    empty_indices <- which(middle_fields == "")
    
    if (length(empty_indices) == 0) {
      # If no empty fields, treat all middle fields as name.
      names_vec[valid_count] <- paste(middle_fields, collapse = " ")
      descriptions[valid_count] <- NA_character_
      next
    }
    
    # Detects runs of consecutive empty fields.
    empty_runs <- list()
    if (length(empty_indices) > 0) {
      run_start <- empty_indices[1]
      run_end <- empty_indices[1]
      
      for (i in 2:length(empty_indices)) {
        if (empty_indices[i] == run_end + 1) {
          run_end <- empty_indices[i]
        } else {
          empty_runs[[length(empty_runs) + 1]] <- c(run_start, run_end)
          run_start <- empty_indices[i]
          run_end <- empty_indices[i]
        }
      }
      empty_runs[[length(empty_runs) + 1]] <- c(run_start, run_end)
    }
    
    # Parses name and description based on empty field runs.
    if (length(empty_runs) >= 2) {
      # Extracts name between first and second empty runs.
      name_start <- empty_runs[[1]][2] + 1
      name_end <- empty_runs[[2]][1] - 1
      
      if (name_start <= name_end && name_start <= length(middle_fields)) {
        name_parts <- middle_fields[name_start:name_end]
        name_parts <- name_parts[name_parts != ""]
        if (length(name_parts) > 0) {
          names_vec[valid_count] <- paste(name_parts, collapse = " ")
        } else {
          names_vec[valid_count] <- NA_character_
        }
      } else {
        names_vec[valid_count] <- NA_character_
      }
      
      # Extracts description after second empty run.
      desc_start <- empty_runs[[2]][2] + 1
      if (desc_start <= length(middle_fields)) {
        desc_parts <- middle_fields[desc_start:length(middle_fields)]
        desc_parts <- desc_parts[desc_parts != ""]
        if (length(desc_parts) > 0) {
          descriptions[valid_count] <- paste(desc_parts, collapse = " ")
        } else {
          descriptions[valid_count] <- NA_character_
        }
      } else {
        descriptions[valid_count] <- NA_character_
      }
      
    } else if (length(empty_runs) == 1) {
      # If only one empty run, treat remaining fields as name.
      split_idx <- empty_runs[[1]][2] + 1
      if (split_idx <= length(middle_fields)) {
        name_parts <- middle_fields[split_idx:length(middle_fields)]
        name_parts <- name_parts[name_parts != ""]
        if (length(name_parts) > 0) {
          names_vec[valid_count] <- paste(name_parts, collapse = " ")
        } else {
          names_vec[valid_count] <- NA_character_
        }
      } else {
        names_vec[valid_count] <- NA_character_
      }
      descriptions[valid_count] <- NA_character_
    }
  }
  
  # Trims vectors to actual valid count.
  impc_ids <- impc_ids[1:valid_count]
  names_vec <- names_vec[1:valid_count]
  descriptions <- descriptions[1:valid_count]
  parameter_ids <- parameter_ids[1:valid_count]
  
  # Constructs the cleaned data frame.
  df <- data.frame(
    impcParameterOrigId = impc_ids,
    name = names_vec,
    description = descriptions,
    parameterId = parameter_ids,
    stringsAsFactors = FALSE
  )
  
  initial_rows <- nrow(df)
  
  # Removes duplicate rows.
  df <- df[!duplicated(df), , drop = FALSE]
  
  # Converts impcParameterOrigId to numeric.
  df$impcParameterOrigId <- as.numeric(df$impcParameterOrigId)
  
  # Converts all forms of "NA" to empty string for consistency
  for (col in c("name", "description")) {
    df[[col]] <- ifelse(
      is.na(df[[col]]) | 
        toupper(df[[col]]) %in% c("NA", "N/A"),
      NA_character_,
      df[[col]]
    )
  }
  
  # Sorts by impcParameterOrigId.
  order_idx <- order(df$impcParameterOrigId, na.last = TRUE)
  df <- df[order_idx, , drop = FALSE]
  rownames(df) <- NULL
  
  # Saves cleaned data to output file.
  # Use write.table with na = "" to save NA as empty string
  write.table(
    df,
    file = output_file,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    na = "",
    fileEncoding = "UTF-8",
    quote = TRUE
  )
  
  cat("Cleaned file saved to:", output_file, "\n")
  invisible(df)
}

# Main execution
input_file <- "/scratch/grp/msc_appbio/DCDM/Group11/data/IMPC_parameter_description.csv"
output_file <- "/scratch/grp/msc_appbio/DCDM/Group11/data/IMPC_parameter_description_cleaned.csv"

tryCatch(
  {
    df_cleaned <- clean_impc_csv(input_file, output_file)
  },
  error = function(e) {
    cat("Error:", e$message, "\n")
  }
)

