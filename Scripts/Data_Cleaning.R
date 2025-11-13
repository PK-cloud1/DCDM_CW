#Install these R packages:
install.packages(c("ggplot2", "dplyr", "readr", "tibble"))
install.packages("dplyr")
install.packages("Amelia")
install.packages("purrr")
install.packages('data.table')
install.packages("naniar", repos = "https://cran.r-project.org")

library('ggplot2')
library('readr')
library('tibble')
library("dplyr")
library("Amelia")
library('purrr')
library('data.table')
library('naniar')



#set the directory where all your files are located.
setwd("/scratch/grp/msc_appbio/DCDM/Group11/data")
qc_dir <- "/scratch/grp/msc_appbio/DCDM/Group11/data/rawcsv_files/QC"
dir.create(qc_dir, showWarnings = FALSE, recursive = TRUE)

# Load all data files
file_list <- list.files("rawcsv_files", pattern = "\\.csv$", full.names = TRUE)
batch_size <- 1000  # Adjustable based on memory capacity

# Store all QC results
all_qc <- list()

for (i in seq(1, length(file_list), by = batch_size)) {
  batch_files <- file_list[i:min(i + batch_size - 1, length(file_list))]
  
  # Read and transpose each file
  batch_data <- map_df(batch_files, function(file) {
    raw <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
    transposed <- as.data.frame(t(raw), stringsAsFactors = FALSE)
    colnames(transposed) <- raw[[1]]
    transposed[-1, ]
  })
  
  #Load the SOP from your working directory.
  sop <- read.csv("../IMPC_SOP.csv", header = TRUE)
  
  #Set up the QC workflow function, which takes two inputs: data and SOP table
  apply_metadata_qc <- function(data, sop) {  
    required_cols <- c("dataField", "dataType", "minValue", "maxValue")
    missing <- setdiff(required_cols, colnames(sop))
    if (length(missing) > 0) stop(paste("Missing columns in SOP:", paste(missing, collapse = ", ")))
    
    # Create a tracker for missing columns
    missing_fields <- c()
    
    #Loop through each row in the SOP table and apply the corresponding QC rule
    for (i in seq_len(nrow(sop))) {    
      # col is the column to be checked against the rule, for example analysis_id, pvalue
      # type indicates the column‘s data type, e.g., “String” or “Float“
      # min_val and max_val define the minimum and maximum limits for this column.
      col <- sop$dataField[i]
      type <- sop$dataType[i]    
      min_val <- as.numeric(sop$minValue[i])    
      max_val <- as.numeric(sop$maxValue[i])    
      
      # Generate a new column name based on col to store its QC result.
      status_col <- paste0(col, "_status")         
      
      # If the column is missing, skip QC and log the name for later inspection
      if (!(col %in% colnames(data))) {
        warning(paste("Skipping QC for missing column:", col))
        missing_fields <- c(missing_fields, col)
        next
      }
      
      
      if (type == "String") { 
        # Create a new QC result column based on statOAus_col using conditional logic.
        data <- data %>%        
          mutate(!!status_col := case_when(  
            #If the value in col is not NA and falls within the range between min_val and max_val, mark it as "pass".
            !is.na(.data[[col]]) &           
              nchar(.data[[col]]) >= min_val &           
              nchar(.data[[col]]) <= max_val ~ "pass",          
            
            #If none of the previous conditions are met, label the result as "fail".
            TRUE ~ "fail"        
          ))  
        
      } else if (type == "Float") {      
        data <- data %>%        
          mutate(!!status_col := case_when(          
            !is.na(.data[[col]]) &           
              .data[[col]] >= min_val &           
              .data[[col]] <= max_val ~ "pass",          
            TRUE ~ "fail"        
          ))       
        
        #If the data type is not recognized, create a QC column and label all entries as "unknown_type".  
      } else {
        data <- data %>%
          mutate(!!status_col := "unknown_type")
      }
    }
    
    # 可以選擇把 missing_fields 存成一欄或印出來
    if (length(missing_fields) > 0) {
      message("Missing columns skipped: ", paste(missing_fields, collapse = ", "))
    }
    
    
    return(data)
#Install these R packages:
install.packages(c("ggplot2", "dplyr", "readr", "tibble"))
install.packages("dplyr")
install.packages("Amelia")
install.packages("purrr")
install.packages('data.table')
install.packages("naniar", repos = "https://cran.r-project.org")

library('ggplot2')
library('readr')
library('tibble')
library("dplyr")
library("Amelia")
library('purrr')
library('data.table')
library(naniar)



#set the directory where all your files are located.
setwd("/scratch/grp/msc_appbio/DCDM/Group11/data")
qc_dir <- "/scratch/grp/msc_appbio/DCDM/Group11/data/rawcsv_files/QC"
dir.create(qc_dir, showWarnings = FALSE, recursive = TRUE)

# Load all data files
file_list <- list.files("rawcsv_files", pattern = "\\.csv$", full.names = TRUE)
batch_size <- 1000  # Adjustable based on memory capacity

# Store all QC results
all_qc <- list()

for (i in seq(1, length(file_list), by = batch_size)) {
  batch_files <- file_list[i:min(i + batch_size - 1, length(file_list))]
  
  # Read and transpose each file
  batch_data <- map_df(batch_files, function(file) {
    raw <- read.csv(file, header = FALSE, stringsAsFactors = FALSE)
    transposed <- as.data.frame(t(raw), stringsAsFactors = FALSE)
    colnames(transposed) <- raw[[1]]
    transposed[-1, ]
  })
  
  #Load the SOP from your working directory.
  sop <- read.csv("../IMPC_SOP.csv", header = TRUE)
  
  #Set up the QC workflow function, which takes two inputs: data and SOP table
  apply_metadata_qc <- function(data, sop) {  
    required_cols <- c("dataField", "dataType", "minValue", "maxValue")
    missing <- setdiff(required_cols, colnames(sop))
    if (length(missing) > 0) stop(paste("Missing columns in SOP:", paste(missing, collapse = ", ")))
    
    # Create a tracker for missing columns
    missing_fields <- c()
    
    #Loop through each row in the SOP table and apply the corresponding QC rule
    for (i in seq_len(nrow(sop))) {    
      # col is the column to be checked against the rule, for example analysis_id, pvalue
      # type indicates the column‘s data type, e.g., “String” or “Float“
      # min_val and max_val define the minimum and maximum limits for this column.
      col <- sop$dataField[i]
      type <- sop$dataType[i]    
      min_val <- as.numeric(sop$minValue[i])    
      max_val <- as.numeric(sop$maxValue[i])    
      
      # Generate a new column name based on col to store its QC result.
      status_col <- paste0(col, "_status")         
      
      # If the column is missing, skip QC and log the name for later inspection
      if (!(col %in% colnames(data))) {
        warning(paste("Skipping QC for missing column:", col))
        missing_fields <- c(missing_fields, col)
        next
      }
      
      
      if (type == "String") { 
        # Create a new QC result column based on statOAus_col using conditional logic.
        data <- data %>%        
          mutate(!!status_col := case_when(  
            #If the value in col is not NA and falls within the range between min_val and max_val, mark it as "pass".
            !is.na(.data[[col]]) &           
              nchar(.data[[col]]) >= min_val &           
              nchar(.data[[col]]) <= max_val ~ "pass",          
            
            #If none of the previous conditions are met, label the result as "fail".
            TRUE ~ "fail"        
          ))  
        
      } else if (type == "Float") {      
        data <- data %>%        
          mutate(!!status_col := case_when(          
            !is.na(.data[[col]]) &           
              .data[[col]] >= min_val &           
              .data[[col]] <= max_val ~ "pass",          
            TRUE ~ "fail"        
          ))       
        
        #If the data type is not recognized, create a QC column and label all entries as "unknown_type".  
      } else {
        data <- data %>%
          mutate(!!status_col := "unknown_type")
      }
    }
    
    # 可以選擇把 missing_fields 存成一欄或印出來
    if (length(missing_fields) > 0) {
      message("Missing columns skipped: ", paste(missing_fields, collapse = ", "))
    }
    
    
    return(data)
  }
  
  qc_result <- apply_metadata_qc(batch_data, sop)  # Apply QC to this batch of data
  all_qc[[paste0("batch_", i)]] <- qc_result



#Save QC results
write.csv(qc_result, file.path(qc_dir, paste0("qc_result_batch_", i, ".csv")), row.names = FALSE)
}

#Combine all QC results
final_qc <- bind_rows(all_qc)
write.csv(final_qc, file.path(qc_dir, "qc_result_all.csv"), row.names = FALSE)

qc <- read.csv("/scratch/grp/msc_appbio/DCDM/Group11/data/rawcsv_files/QC/cleaned_qc_result.csv")
head(qc)

# to visualise the missing value
setwd("/scratch/grp/msc_appbio/DCDM/Group11/data/rawcsv_files/QC")
ex2File=read.table("rawExperimentData2.csv",sep =",",header=T)
ex2File_trimmed <- ex2File[, 1:8]
gg_miss_var(ex2File_trimmed)
missmap(ex2File_trimmed, main = "Missing Values", col = c("pink", "snow2"))

  }
  
  qc_result <- apply_metadata_qc(batch_data, sop)  # Apply QC to this batch of data
  all_qc[[paste0("batch_", i)]] <- qc_result



#Save QC results
write.csv(qc_result, file.path(qc_dir, paste0("qc_result_batch_", i, ".csv")), row.names = FALSE)
}

#Combine all QC results
final_qc <- bind_rows(all_qc)
write.csv(final_qc, file.path(qc_dir, "qc_result_all.csv"), row.names = FALSE)

qc <- read.csv("/scratch/grp/msc_appbio/DCDM/Group11/data/rawcsv_files/QC/cleaned_qc_result.csv")
head(qc)

# to visualise the missing value
setwd("/scratch/grp/msc_appbio/DCDM/Group11/data/rawcsv_files/QC")
ex2File=read.table("rawExperimentData2.csv",sep =",",header=T)
ex2File_trimmed <- ex2File[, 1:8]
gg_miss_var(ex2File_trimmed)
missmap(ex2File_trimmed, main = "Missing Values", col = c("pink", "snow2"))





