# This interface has a multitab layout - each required visualisation has been displayed in a tab
# Package Installation and Loading
# Define required packages for the analysis
required_packages <- c("shiny", "bslib", "dplyr", "tidyr", "readr", "ggplot2", 
                       "DT", "pheatmap", "ggrepel", "uwot", "cluster")

# Identify packages that are not yet installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# Install missing packages if any
if(length(new_packages)) install.packages(new_packages)

# Load all required packages silently (suppress startup messages)
suppressPackageStartupMessages({
  library(shiny)      # Web application framework
  library(bslib)      # Bootstrap themes for Shiny
  library(dplyr)      # Data manipulation
  library(tidyr)      # Data tidying (pivot operations)
  library(readr)      # Fast CSV reading
  library(ggplot2)    # Data visualization
  library(DT)         # Interactive tables
  library(pheatmap)   # Heatmap visualization
  library(ggrepel)    # Label repelling for ggplot2
  library(uwot)       # UMAP dimensionality reduction
  library(cluster)    # Clustering algorithms (silhouette analysis)
})

# ============================================================================
# Configuration
# ============================================================================

DATA_PATH <- "/scratch/grp/msc_appbio/DCDM/Group11/data/qc_result_all_group_v2.csv"
# Reads the CSV file and performs basic data cleaning and validation
read_qc_data <- function(path) {
  # Read CSV file
  df <- read_csv(path, show_col_types = FALSE)
  # Define required column names
  required_cols <- c("gene_symbol", "parameter_name", "pvalue", "final_group")
  
  
  
  # Clean and filter the data
  df %>%
    mutate(
      gene_symbol = toupper(as.character(gene_symbol)),      # Convert gene symbols to uppercase
      parameter_name = as.character(parameter_name),         # Ensure parameter names are character
      pvalue = suppressWarnings(as.numeric(pvalue)),         # Convert p-values to numeric
      final_group = as.character(final_group)                # Ensure final_group is character
    ) %>%
    filter(
      !is.na(gene_symbol),      # Remove rows with missing gene symbols
      !is.na(parameter_name),   # Remove rows with missing parameter names
      !is.na(pvalue),           # Remove rows with missing p-values
      
      pvalue > 0,               # Remove p-values <= 0 (invalid)
      pvalue <= 1               # Remove p-values > 1 (invalid)
    )
}

# make_score_matrix: Create gene-by-phenotype score matrix
# matrix with genes as rows, phenotypes as columns, -log10(p) as values
make_score_matrix <- function(df, selected_groups = NULL, p_threshold = 1.0) {
  
  # Filter by selected phenotype groups if specified 
  data_filtered <- df
  if (!is.null(selected_groups) && length(selected_groups) > 0) {
    data_filtered <- data_filtered %>%
      filter(final_group %in% selected_groups)
  }
  
  # Filter by p-value threshold only (no other filtering)
  data_filtered <- data_filtered %>%
    # Filter out invalid p-values: keep only finite values between 0 and p_threshold
    filter(pvalue > 0, pvalue <= p_threshold, is.finite(pvalue)) %>%
    mutate(
      # Ensure p-values are not too small to avoid -Inf after log10
      pvalue_safe = pmax(pvalue, 1e-300),
      # Convert p-value to score using -log10 transformation
      score = -log10(pvalue_safe),
      # Cap score at 300 to avoid extreme values
      score = pmin(score, 300),
      # Ensure score is non-negative
      score = pmax(score, 0)
    ) %>%
    # Final check: keep only rows with finite score
    filter(is.finite(score))
  
  # Check if any data remains after filtering
  if (nrow(data_filtered) == 0) {
    stop("No data matching criteria.")
  }
  
  # Keep ALL unique genes
  all_genes <- unique(data_filtered$gene_symbol)
  all_params <- unique(data_filtered$parameter_name)
  
  # Print information about retained data
  cat(sprintf("\nRetained all %d genes and %d phenotypes\n", length(all_genes), length(all_params)))
  
  # Create the score matrix
  mat <- data_filtered %>%
    select(gene_symbol, parameter_name, score) %>%
    # Average scores if there are duplicate gene-phenotype combinations
    group_by(gene_symbol, parameter_name) %>%
    summarise(score = mean(score, na.rm = TRUE), .groups = "drop") %>%
    # Pivot to wide format (genes as rows, phenotypes as columns)
    pivot_wider(
      names_from = parameter_name, 
      values_from = score, 
      values_fill = 0  # KEY: Fill missing values with 0 instead of NA
    )
  
  
  
  # Extract gene names and convert to matrix
  gene_names <- mat$gene_symbol
  mat <- as.matrix(mat[, -1, drop = FALSE])  # Remove gene_symbol column
  rownames(mat) <- gene_names                 # Set row names to gene symbols
  
  # Validate matrix dimensions
  if (ncol(mat) == 0 || nrow(mat) == 0) {
    stop("Matrix is empty.")
  }
  
  
  # Final validation: ensure sufficient data for clustering
  if (nrow(mat) < 2 || ncol(mat) < 2) {
    stop("Insufficient data for clustering.")
  }
  
  
  # Print final matrix dimensions
  cat(sprintf("Final matrix: %d genes × %d parameters\n", nrow(mat), ncol(mat)))
  
  
  return(mat)
}

# Perform row-wise z-score normalization to make each gene/row comparable,
# ensuring differences are measured relative to its own mean and variance.
# This prevents rows with larger absolute values from dominating the analysis
# and provides a fair basis for clustering or visualization.
row_zscore_safe <- function(mat) {
  # If matrix has no rows or no columns, just return it directly
  if (nrow(mat) == 0 || ncol(mat) == 0) return(mat)
  
  # Initialize a new matrix filled with zeros, same size as input
  scaled <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  # Preserve row and column names from the original matrix
  rownames(scaled) <- rownames(mat)
  colnames(scaled) <- colnames(mat)
  
  # Loop through each row of the matrix
  for (i in seq_len(nrow(mat))) {
    # Calculate standard deviation of the row (ignore NA values)
    row_sd <- sd(mat[i, ], na.rm = TRUE)
    # If sd is NA or extremely small (almost constant row)
    if (is.na(row_sd) || row_sd < 1e-10) {
      # Keep original values (no scaling)
      scaled[i, ] <- mat[i, ]
    } else {
      # Otherwise, calculate row mean (ignore NA values)
      row_mean <- mean(mat[i, ], na.rm = TRUE)
      # Apply z-score: (value - mean) / sd
      scaled[i, ] <- (mat[i, ] - row_mean) / row_sd
    }
  }
  
  # Replace any non-finite values (NA, Inf, -Inf) with 0
  scaled[!is.finite(scaled)] <- 0
  # Return the scaled matrix
  return(scaled)
}

# Automatically determine the optimal number of clusters (k) 
# by evaluating average silhouette scores across different k values.
# This ensures that the chosen clustering solution balances cohesion 
# (points within the same cluster being close) and separation 
# (clusters being well apart), providing a data-driven choice 
# instead of manually guessing k.
find_optimal_clusters <- function(dist_mat, max_k = 10) {
  # Get the number of items from the distance object
  n <- attr(dist_mat, "Size")
  # If fewer than 3 items, default to 2 clusters (minimal meaningful split)
  if (n < 3) return(2)
  
  # Limit max_k to be feasible: at most n-1, and cap at 15 for stability
  max_k <- min(max_k, n - 1, 15)
  # If the feasible max_k is below 2, default to 2 clusters
  if (max_k < 2) return(2)
  
  # Compute average silhouette width for k = 2 ... max_k
  sil_scores <- sapply(2:max_k, function(k) {
    tryCatch({
      # Hierarchical clustering with average (UPGMA) linkage
      hc <- hclust(dist_mat, method = "average")
      # Cut the dendrogram into k clusters
      clusters <- cutree(hc, k = k)
      # Compute silhouette for this clustering against the same distance
      sil <- silhouette(clusters, dist_mat)
      # Use the mean silhouette width as the score
      mean(sil[, "sil_width"])
    }, error = function(e) -1)  # On error, return a sentinel score
  })
  
  # Pick k with the highest silhouette score (index + 1 because k starts at 2)
  best_k <- which.max(sil_scores) + 1
  # Return the selected number of clusters
  return(best_k)
}
# cleaned data for first two visualisations has been defined as the variable mouse_data
mouse_data <- read.csv("/scratch/grp/msc_appbio/DCDM/Group11/QC/qc_result_all.csv", 
                      stringsAsFactors = FALSE, # Prevent automatic factor conversion
                       check.names = FALSE)  %>% # Preserve original column names
  #the below command converts all values within the respective columns (gene_symbol,parameter_name) into upper case values
  #this ensures format consistency across values for effective plot visualisations and processing
  mutate(gene_symbol = toupper(gene_symbol), parameter_name = toupper(parameter_name))

#the variable "cards" has been assigned as a list
#each card is assigned a title (card_header) and outputs (plotOutput,tableOutput) to be displayed within each card
#therefore each card can be assigned a specific tab within the interface. 
#as such, each card can be linked to specific visualisationss (e.g. phenotypes for specific KO mouse)
#full_screen means that each card should fill the page
cards <- list(
  card(full_screen=TRUE, 
       card_header("KO Mouse Selection"), 
       plotOutput("distPlotKO"), 
       tableOutput("summaryTableKO")),
  card(full_screen=TRUE, 
       card_header("Phenotype Selection"), 
       plotOutput("distPlotPhenotype"),
       tableOutput("summaryTablePhenotype")),
  card(full_screen=TRUE,
       card_header("Clustering"),
       plotOutput("heatmap_plot"),
       plotOutput("pca_plot"),
       plotOutput("umap_plot"),
       DTOutput("cluster_table"),
       verbatimTextOutput("summary_stats"))
)

#this command defines the user interface features of the app.
#the page fillable command ensures that the outputs of each tab fills the page
ui <- page_fillable(
  "IMPC Explorer", #title of the app
  full_screen=TRUE, #ensures each tab fills the page
  navset_card_tab( #creates a set of tabs within a card (this is required for the third visualisation)
  nav_panel("KO Mouse Selection", cards[[1]], #assigns one tab to the KO mouse selection card
          sidebarLayout(
            sidebarPanel( #within a sidebar layout, there should be a panel to select inputs
              selectInput("selected_gene",               # Input ID
                          "Select KO Gene:",             # Label shown to user
                          choices = sort(unique(mouse_data$gene_symbol)),  # List of KO genes
                          selected = unique(mouse_data$gene_symbol)[1])), #default selected gene is the first gene in the gene_symbol column
            # Main panel to show plot and summary table
            mainPanel(
              plotOutput("distPlotKO"),      # Output plot of p-values
              tableOutput("summaryTableKO")  # Output table of phenotype stats
            ))),
  nav_panel("Phenotype Selection", cards[[2]], #assigns a tab to the Phenotype Selection card
            sidebarLayout(
              sidebarPanel( # Sidebar panel for selecting phenotype  
                selectInput("selected_phenotype",               # Input ID
                            "Select Phenotype:",             # Label shown to user
                            choices = sort(unique(mouse_data$parameter_name)),  # List of KO genes
                            selected = unique(mouse_data$parameter_name)[1])  # Default selection is the first value in the parameter_name column
              ),
            # Main panel to show plot and summary table
            mainPanel(
              plotOutput("distPlotPhenotype"),      # Output plot of p-values
              tableOutput("summaryTablePhenotype")  # Output table of phenotype stats
            ))),
  nav_panel("Clustering Analysis", cards[[3]], #assigns the final visualisation card to the third tab
    fluidPage(
      # Use Bootstrap 5 with the "flatly" bootswatch theme for styling
      theme = bs_theme(version = 5, bootswatch = "flatly"),
      
      titlePanel(
        div(
          # App title
          h2("Gene Clustering by Phenotype Scores"),
          # Subtitle: explain zero filtering policy, styled as muted text
          p("Zero filtering - keeps ALL unique genes", 
            style = "color: #7f8c8d; font-size: 14px;")
        )
      ),
      
      # Two-column layout: controls on the left, outputs on the right
      sidebarLayout(
        sidebarPanel(
          width = 3,  # Sidebar occupies 3/12 of the width
          
          # Data source banner with background and code-styled path
          div(style = "background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin-bottom: 15px;",
              strong("Data Source:"),       # Label for data source
              br(),                         # Line break
              code(style = "font-size: 11px;", basename(DATA_PATH))  # Show base name of the input path
          ),
          
          # Dynamically generated group selector (server-side)
          uiOutput("group_selector"),
          
          # Filter section header
          h4("Filter"),
          # Slider for p-value threshold: from 0.001 to 1.0, default 0.05
          sliderInput("p_threshold", "P-value threshold (≤):", 
                      min = 0.001, max = 1.0, value = 0.05, step = 0.001),
          
          # Options section header
          h4("Options"),
          # Toggle row-wise z-score normalization
          checkboxInput("row_scale", "Row z-score normalization", value = TRUE),
          # Toggle gene label rendering on plots
          checkboxInput("show_labels", "Show gene labels", value = FALSE),
          
          # Choice of distance metric for clustering/embeddings
          selectInput("dist_metric", "Distance:",
                      choices = c("Correlation" = "correlation",
                                  "Euclidean" = "euclidean"),
                      selected = "correlation"),
          
          # Button to trigger the analysis pipeline
          actionButton("run_analysis", "Run Analysis", 
                       icon = icon("play"), class = "btn-primary btn-lg",
                       style = "width: 100%; margin-top: 20px;"),
          
          # Separator line
          hr(),
          # Button to download computed results (tables/metrics/figures)
          downloadButton("download_results", "Download Results",
                         class = "btn-success", style = "width: 100%;")
        ),
        
        # Main content area for visualizations and outputs
        mainPanel(
          width = 9,  # Main panel occupies 9/12 of the width
          tabsetPanel(
            # Heatmap tab: large plotting area
            tabPanel("Heatmap", br(), plotOutput("heatmap_plot", height = "700px")),
            # PCA tab: principal component analysis plot
            tabPanel("PCA", br(), plotOutput("pca_plot", height = "600px")),
            # UMAP tab: low-dimensional embedding visualization
            tabPanel("UMAP", br(), plotOutput("umap_plot", height = "600px")),
            # Table tab: interactive data table of clusters/genes/metrics
            tabPanel("Table", br(), DTOutput("cluster_table")),
            # Summary tab: text stats and a plot of cluster sizes
            tabPanel("Summary", br(), verbatimTextOutput("summary_stats"),
                     br(), plotOutput("cluster_size_plot", height = "400px"))
          )
        )
      )
    )
)))
#the below command defines the server features
server <- function(input,output,session) {
  #reactive expression to filter data ased on selected gene
  filtered_data_gene <- reactive({
    mouse_data %>%
      filter(gene_symbol == input$selected_gene) %>%
      mutate(pvalue_status = case_when(
        is.na(pvalue) ~ "missing",
        pvalue >= 0 & pvalue <= 1 ~ "pass",
        TRUE ~ "fail"
      )) %>%
      filter(pvalue_status == "pass")  # keep the data which pass QC
  })
  # Reactive expression to filter data based on selected phenotype
  filtered_data_phenotype <- reactive({
    mouse_data %>%
      filter(parameter_name == input$selected_phenotype) %>%
      mutate(pvalue_status = case_when(
        is.na(pvalue) ~ "missing",
        pvalue >= 0 & pvalue <= 1 ~ "pass",
        TRUE ~ "fail"
      )) %>%
      filter(pvalue_status == "pass")  # keep the data which pass QC
  })
  # Render bar plot of phenotype p-values
  output$distPlotKO <- renderPlot({
    df <- filtered_data_gene() %>% filter(pvalue < 0.05)
    
    #for when there are no significant phenotype-specific values for this gene
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No significant phenotypes found for this KO gene", cex = 1.5)
    } else {
      #otherwise, arrange p values accordingly
      df_top <- df %>%
        arrange(pvalue) 
      
      # Create horizontal bar plot
      ggplot(df_top, aes(x = reorder(parameter_name, pvalue), y = pvalue))+
        geom_bar(stat = "identity", fill = "skyblue") +             # Use actual p-values as bar height
        coord_flip() +                            # Flip axes for horizontal bars
        labs(title = paste("KO Gene:", input$selected_gene),
             subtitle = paste("Please note that only significant results are displayed"),  # Dynamic plot title
             x = "Phenotype",                      # X-axis label
             y = "P-value")+                        # Y-axis label
        theme_minimal(base_size = 14)              # Clean theme with readable text
    }
  })
  
  # Render summary table of filtered data with QC status
  output$summaryTableKO <- renderTable({
    filtered_data_gene() %>%
      arrange(pvalue) %>%  # Sort by p-value
      select(parameter_name, pvalue, parameter_id, analysis_id)# Show selected columns
  })
  # Render bar plot of KO p-values
  output$distPlotPhenotype <- renderPlot({
    df <- filtered_data_phenotype() %>% filter(pvalue < 0.05)
    
    #if there are no significant KO mice values associated with a particular phenotype
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No significant observations across KO mice for this phenotype", cex = 1.5)
    } else {
      #otherwise arrange pvalues accordingly
      df_top <- df %>%
        arrange(pvalue) 
      
      # Create horizontal bar plot
      ggplot(df_top, aes(x = reorder(gene_symbol, pvalue), y = pvalue))+
        geom_bar(stat = "identity", fill = "skyblue") +             # Use actual p-values as bar height
        coord_flip() +                            # Flip axes for horizontal bars
        labs(title = paste("Phenotype:", input$selected_phenotype),
             subtitle = paste("Please note that only significant results are displayed"), # Dynamic plot title
             x = "KO gene",                      # X-axis label
             y = "P-value")+                        # Y-axis label
        theme_minimal(base_size = 14)              # Clean theme with readable text
    }
  })
  
  # Render summary table of filtered data with QC status
  output$summaryTablePhenotype <- renderTable({
    filtered_data_phenotype() %>%
      arrange(pvalue) %>%  # Sort by p-value
      select(gene_symbol, pvalue, parameter_id, analysis_id) # Show selected columns
  })
  raw_data <- reactive({
    # Wrap the data loading in a progress UI for better user feedback
    withProgress(message = 'Loading data...', {
      # Read and quality-check the data from the configured path
      read_qc_data(DATA_PATH)
    })
  })
  
  output$group_selector <- renderUI({
    # Ensure raw_data() is available before proceeding
    req(raw_data())
    
    # Extract all unique values from 'final_group' and sort them
    groups <- sort(unique(raw_data()$final_group))
    # Remove NA and empty string entries
    groups <- groups[!is.na(groups) & groups != ""]
    
    # Count how many rows belong to each group
    group_counts <- raw_data() %>%
      filter(!is.na(final_group), final_group != "") %>%
      group_by(final_group) %>%
      summarise(n = n(), .groups = "drop")
    
    # Create a named vector for choices: group name + sample size
    choices <- setNames(
      groups,
      paste0(groups, " (n=", group_counts$n, ")")
    )
    
    # Render a selectize input widget for choosing phenotype groups
    selectizeInput("selected_groups", "Select Phenotype Group:",
                   choices = choices,
                   selected = groups[1:min(5, length(groups))], # preselect up to 5 groups
                   multiple = TRUE)
  })
  
  analysis_result <- eventReactive(input$run_analysis, {
    # Ensure data is loaded and at least one group is selected before running
    req(raw_data(), input$selected_groups)
    
    # Wrap the analysis with a progress UI and status messages
    withProgress(message = 'Running analysis...', {
      
      # Step 1: build the gene-by-parameter score matrix from filtered data
      incProgress(0.2, detail = "Building matrix")
      mat <- make_score_matrix(
        raw_data(),
        selected_groups = input$selected_groups,
        p_threshold = input$p_threshold
      )
      
      # Guardrail: require at least a 2x2 matrix to proceed with clustering/PCA
      validate(need(nrow(mat) >= 2 && ncol(mat) >= 2, "Insufficient data."))
      
      # Step 2: optionally apply row-wise z-score normalization
      incProgress(0.1, detail = "Normalizing")
      mat_scaled <- if (input$row_scale) row_zscore_safe(mat) else mat
      
      # Step 3: compute a distance matrix based on the chosen metric
      incProgress(0.2, detail = "Computing distance")
      dist_mat <- if (input$dist_metric == "correlation") {
        # Correlation distance = 1 - correlation; use pairwise complete observations
        d <- as.dist(1 - cor(t(mat_scaled), use = "pairwise.complete.obs"))
        # Replace non-finite distances (NA/Inf) with the maximum finite distance
        d[!is.finite(d)] <- max(d[is.finite(d)], na.rm = TRUE)
        d
      } else {
        # Euclidean distance on the scaled (or raw) matrix
        dist(mat_scaled)
      }
      
      incProgress(0.1, detail = "Finding optimal k")
      incProgress(0.1, detail = "Finding optimal k")
      # Update progress bar: step for finding the best number of clusters (k)
      
      optimal_k <- find_optimal_clusters(dist_mat, max_k = min(10, nrow(mat_scaled) - 1))
      # Use silhouette scores to determine the optimal number of clusters,
      # limiting k to at most 10 or (number of rows - 1)
      
      incProgress(0.1, detail = sprintf("Clustering (k=%d)", optimal_k))
      # Update progress bar: step for clustering with the chosen k
      
      hc <- hclust(dist_mat, method = "average")
      # Perform hierarchical clustering using average linkage
      
      clusters <- cutree(hc, k = optimal_k)
      # Cut the dendrogram into k clusters and assign cluster labels
      
      incProgress(0.1, detail = "PCA")
      # Update progress bar: step for principal component analysis (PCA)
      
      pca_result <- prcomp(mat_scaled, center = FALSE, scale. = FALSE)
      # Run PCA on the scaled matrix without centering or scaling again
      
      pca_df <- as.data.frame(pca_result$x[, 1:2, drop = FALSE])
      # Extract the first two principal components into a data frame
      
      pca_df$gene_symbol <- rownames(mat_scaled)
      # Add gene symbols as a column for identification
      
      pca_df$cluster <- factor(clusters[rownames(mat_scaled)])
      # Add cluster assignments to the PCA data frame
      
      pca_var <- summary(pca_result)$importance[2, 1:2] * 100
      # Extract the percentage of variance explained by PC1 and PC2
      
      incProgress(0.2, detail = "UMAP")
      # Update progress bar: step for UMAP dimensionality reduction
      
      set.seed(42)
      # Set random seed for reproducibility
      
      n_neighbors <- min(15, max(2, nrow(mat_scaled) - 1))
      # Choose number of neighbors for UMAP: between 2 and 15, depending on sample size
      
      umap_result <- tryCatch({
        uwot::umap(mat_scaled, n_components = 2, min_dist = 0.1, n_neighbors = n_neighbors)
        # Run UMAP to project data into 2D space
      }, error = function(e) {
        pca_result$x[, 1:2]
        # If UMAP fails, fall back to using the first two PCA components
      })
      
      # Create UMAP data frame
      umap_df <- as.data.frame(umap_result)
      colnames(umap_df) <- c("UMAP1", "UMAP2")
      umap_df$gene_symbol <- rownames(mat_scaled)
      umap_df$cluster <- factor(clusters[rownames(mat_scaled)])
      
      # Return list of all analysis results
      list(
        matrix = mat_scaled,      # Normalized score matrix
        hclust = hc,              # Hierarchical clustering object
        clusters = clusters,      # Cluster assignments
        optimal_k = optimal_k,    # Number of clusters
        pca_df = pca_df,          # PCA coordinates and clusters
        pca_var = pca_var,        # PCA variance explained
        umap_df = umap_df         # UMAP coordinates and clusters
      )
    })
  }, ignoreInit = TRUE) # Don't run on initialization
  
  # Output: Heatmap visualization
  output$heatmap_plot <- renderPlot({
    req(analysis_result()) # Require analysis to be complete
    result <- analysis_result()
    
    # Create row annotation data frame for cluster coloring
    genes_in_matrix <- rownames(result$matrix)
    anno_row <- data.frame(
      Cluster = factor(result$clusters[genes_in_matrix]),
      row.names = genes_in_matrix
    )
    
    
    # Create heatmap with hierarchical clustering
    pheatmap(
      result$matrix,
      # Use same distance metric as clustering
      clustering_distance_rows = if (input$dist_metric == "correlation") {
        d <- as.dist(1 - cor(t(result$matrix), use = "pairwise.complete.obs"))
        d[!is.finite(d)] <- max(d[is.finite(d)], na.rm = TRUE)
        d
      } else {
        dist(result$matrix)
      },
      clustering_method = "average",           # Average linkage
      cutree_rows = result$optimal_k,          # Cut tree to show clusters
      annotation_row = anno_row,               # Cluster annotation
      show_rownames = TRUE,                    # Show gene names
      fontsize_row = 8,                        # Row label font size
      fontsize_col = 8,                        # Column label font size
      scale = "none",                          # No additional scaling
      color = colorRampPalette(c("navy", "white", "firebrick3"))(256),  # Color scheme
      main = sprintf("Gene Clustering (k=%d, p≤%.3f)", result$optimal_k, input$p_threshold),
      border_color = NA                        # No cell borders
    )
  }, res = 96)  # Set resolution for better quality
  
  # Output: PCA scatter plot
  output$pca_plot <- renderPlot({
    req(analysis_result())
    result <- analysis_result()
    
    # Create base PCA plot
    p <- ggplot(result$pca_df, aes(x = PC1, y = PC2, color = cluster)) +
      geom_point(alpha = 0.7, size = 3) +  # Plot points with transparency
      labs(
        title = sprintf("PCA (k=%d, p≤%.3f)", result$optimal_k, input$p_threshold),
        x = sprintf("PC1 (%.1f%%)", result$pca_var[1]),  # X-axis with variance explained
        y = sprintf("PC2 (%.1f%%)", result$pca_var[2]),  # Y-axis with variance explained
        color = "Cluster"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  # Center title
      scale_color_brewer(palette = "Set1")  # Use colorblind-friendly palette
    
    # Add gene labels if requested
    if (input$show_labels) {
      p <- p + geom_text_repel(aes(label = gene_symbol), size = 2.5, max.overlaps = 30)
    }
    
    print(p)
  }, res = 96)
  
  # Output: UMAP scatter plot
  output$umap_plot <- renderPlot({
    req(analysis_result())
    result <- analysis_result()
    
    # Create base UMAP plot
    p <- ggplot(result$umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
      geom_point(alpha = 0.7, size = 3) +  # Plot points with transparency
      labs(
        title = sprintf("UMAP (k=%d, p≤%.3f)", result$optimal_k, input$p_threshold),
        x = "UMAP1", 
        y = "UMAP2", 
        color = "Cluster"
      ) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +  # Center title
      scale_color_brewer(palette = "Set1")  # Use colorblind-friendly palette
    
    # Add gene labels if requested
    if (input$show_labels) {
      p <- p + geom_text_repel(aes(label = gene_symbol), size = 2.5, max.overlaps = 30)
    }
    
    print(p)
  }, res = 96)
  
  # Output: Interactive data table with cluster assignments
  output$cluster_table <- renderDT({
    req(analysis_result())
    result <- analysis_result()
    
    # Create data frame with gene-cluster assignments
    df <- tibble(
      gene_symbol = names(result$clusters),
      cluster = as.integer(result$clusters)
    ) %>% arrange(cluster, gene_symbol)  # Sort by cluster then gene name
    
    # Render as interactive DataTable
    datatable(
      df, 
      options = list(
        pageLength = 50,       # Show 50 rows per page
        scrollY = "500px"      # Enable vertical scrolling
      ),
      rownames = FALSE,        # Don't show row numbers
      class = 'cell-border stripe hover'  # Table styling
    )
  })
  
  # Output: Summary statistics (text output)
  output$summary_stats <- renderPrint({
    req(analysis_result())
    result <- analysis_result()
    
    # Print summary information
    cat("=== Clustering Summary ===\n\n")
    cat(sprintf("Total Genes: %d\n", nrow(result$matrix)))
    cat(sprintf("Total Phenotypes: %d\n", ncol(result$matrix)))
    cat(sprintf("Optimal Clusters: %d (auto)\n", result$optimal_k))
    cat(sprintf("P-value Threshold: %.3f\n\n", input$p_threshold))
    
    # Print gene count per cluster
    cat("Genes per Cluster:\n")
    for (i in sort(unique(result$clusters))) {
      cat(sprintf("  Cluster %d: %d genes\n", i, sum(result$clusters == i)))
    }
  })
  
  # Output: Bar plot of cluster sizes
  output$cluster_size_plot <- renderPlot({
    req(analysis_result())
    result <- analysis_result()
    
    # Count genes per cluster
    df <- data.frame(cluster = factor(result$clusters)) %>%
      group_by(cluster) %>%
      summarise(count = n(), .groups = "drop")
    
    # Create bar plot
    ggplot(df, aes(x = cluster, y = count, fill = cluster)) +
      geom_col(alpha = 0.8, show.legend = FALSE) +  # Bar plot without legend
      geom_text(aes(label = count), vjust = -0.5, size = 5) +  # Add count labels
      labs(title = "Genes per Cluster", x = "Cluster", y = "Count") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_fill_brewer(palette = "Set3")  # Use colorful palette
  }, res = 96)
  
  # Download Handler: Export cluster assignments to CSV
  output$download_results <- downloadHandler(
    # Generate filename with parameters and timestamp
    filename = function() {
      req(analysis_result())
      sprintf("clusters_k%d_p%.3f_%s.csv", 
              analysis_result()$optimal_k, 
              input$p_threshold,
              format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    # Write content to file
    content = function(file) {
      req(analysis_result())
      result <- analysis_result()
      
      # Create data frame with cluster assignments
      df <- tibble(
        gene_symbol = names(result$clusters),
        cluster = as.integer(result$clusters)
      ) %>% arrange(cluster)  # Sort by cluster
      
      # Write to CSV
      write.csv(df, file, row.names = FALSE)
    }
  )
}
# Run the application 
shinyApp(ui = ui, server = server)