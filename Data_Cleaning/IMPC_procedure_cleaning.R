# Defines the main function to clean the IMPC procedure CSV file (unquoted commas in description).
clean_impc_csv <- function(input_file, output_file = NULL) {
  # Sets default output filename by appending _cleaned.csv.
  if (is.null(output_file)) {
    output_file <- sub(".csv", "_cleaned.csv", input_file, fixed = TRUE)
  }
  
  # Reads the raw lines from the input file.
  raw_lines <- readLines(input_file, encoding = "UTF-8", warn = FALSE)
  if (length(raw_lines) == 0) {
    stop("Input file is empty.")
  }
  
  # Expect header: name,description,isMandatory,impcParameterOrigId
  header <- raw_lines[1]
  # Proceed with manual parsing due to broken CSV quoting in description.
  
  total_lines <- length(raw_lines) - 1  # Exclude header
  if (total_lines <= 0) {
    stop("No data rows detected after header.")
  }
  
  # Pre-allocate vectors
  names_vec      <- character(total_lines)
  descriptions   <- character(total_lines)
  is_mandatory_v <- character(total_lines)
  impc_ids       <- character(total_lines)
  
  valid_count <- 0
  
  # Helper to trim and drop empty tokens
  .pack_tokens <- function(x) {
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (length(x) == 0) "" else paste(x, collapse = " ")
  }
  
  # Parse each data line
  for (line_num in 2:length(raw_lines)) {
    line <- raw_lines[line_num]
    if (!nzchar(line)) next
    
    # Split by comma (description contains many commas)
    fields <- strsplit(line, ",", fixed = TRUE)[[1]]
    # Remove trailing newlines/spaces in tokens
    fields <- trimws(fields)
    
    if (length(fields) < 3) next
    
    # Strategy:
    # - Last token: impcParameterOrigId (expect numeric-like)
    # - 2nd last: isMandatory (TRUE/FALSE or empty)
    # - 1st: name
    # - Middle [2 .. (n-2)]: description pieces to join with space (dropping empty)
    last_idx <- length(fields)
    id_token <- fields[last_idx]
    
    # Soft check: if last token not numeric-like, try to walk backwards to find last non-empty numeric
    is_num_like <- function(x) grepl("^[0-9]+$", x)
    if (!is_num_like(id_token)) {
      # find from end first numeric-like token
      idx_numeric <- max(which(is_num_like(fields)))
      if (length(idx_numeric) == 0 || is.infinite(idx_numeric)) {
        # cannot parse this line
        next
      }
      last_idx <- idx_numeric
      id_token <- fields[last_idx]
    }
    
    # 2nd last (isMandatory) should be just before ID
    second_last_idx <- last_idx - 1
    if (second_last_idx < 2) next
    mandatory_token <- fields[second_last_idx]
    
    # First field is name
    name_token <- if (length(fields) >= 1) fields[1] else NA_character_
    if (!nzchar(name_token)) name_token <- NA_character_
    
    # Description is the middle
    if (second_last_idx - 1 >= 2) {
      mid_tokens <- fields[2:(second_last_idx - 1)]
    } else {
      mid_tokens <- character(0)
    }
    description_str <- .pack_tokens(mid_tokens)
    if (!nzchar(description_str)) description_str <- NA_character_
    
    valid_count <- valid_count + 1
    names_vec[valid_count]      <- name_token
    descriptions[valid_count]   <- description_str
    is_mandatory_v[valid_count] <- ifelse(nzchar(mandatory_token), mandatory_token, NA_character_)
    impc_ids[valid_count]       <- id_token
  }
  
  if (valid_count == 0) {
    stop("No valid rows parsed. Input file format may be incompatible.")
  }
  
  # Trim to actual count
  names_vec      <- names_vec[1:valid_count]
  descriptions   <- descriptions[1:valid_count]
  is_mandatory_v <- is_mandatory_v[1:valid_count]
  impc_ids       <- impc_ids[1:valid_count]
  
  # Build data frame
  df <- data.frame(
    name = names_vec,
    description = descriptions,
    isMandatory = is_mandatory_v,
    impcParameterOrigId = impc_ids,
    stringsAsFactors = FALSE
  )
  
  # Normalize isMandatory to boolean
  canon_bool <- function(x) {
    y <- toupper(trimws(x))
    out <- ifelse(y %in% c("TRUE", "T", "1"), TRUE,
           ifelse(y %in% c("FALSE", "F", "0"), FALSE, NA))
    out
  }
  df$isMandatory <- canon_bool(df$isMandatory)
  
  # Convert ID to numeric
  suppressWarnings({
    df$impcParameterOrigId <- as.numeric(df$impcParameterOrigId)
  })
  
  # Remove exact duplicate rows
  df <- df[!duplicated(df), , drop = FALSE]
  
  # Normalize textual NA
  for (col in c("name", "description")) {
    v <- df[[col]]
    v_trim <- trimws(v)
    v_trim[v_trim %in% c("NA", "N/A", "")] <- NA_character_
    df[[col]] <- v_trim
  }
  
  # Sort by ID
  df <- df[order(df$impcParameterOrigId, na.last = TRUE), , drop = FALSE]
  rownames(df) <- NULL
  
  # Save to CSV (quote fields, write NA as empty)
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
  
  cat("Done! Cleaned file saved to:", output_file, "\n")
  invisible(df)
}

# Main execution with your specified paths
input_file  <- "/scratch/grp/msc_appbio/DCDM/Group11/data/IMPC_procedure.csv"
output_file <- "/scratch/grp/msc_appbio/DCDM/Group11/data/IMPC_procedure_cleaned.csv"

tryCatch(
  {
    df_cleaned <- clean_impc_csv(input_file, output_file)
  },
  error = function(e) {
    cat("Error:", e$message, "\n")
  }
)
