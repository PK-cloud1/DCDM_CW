#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#TO DO: PHENOTYPE SCORE, PARTICULAR KO MOUSE, STAT SCORES OF PHENOTYPES TESTED, SIGNIFICANCE OF GENE KO, SELECTED PHENOTYPE STATS SCORES, CLUSTERS OF GENES WITH SIMILAR PHENOTYPE SCORES
library(shiny)
library(ggplot2) # For creating plots
library(dplyr)   # For data manipulation

# Load mouse phenotype data from CSV file
mouse_data <- read.csv("/scratch/grp/msc_appbio/DCDM/Group11/QC/qc_result_all.csv", 
                       stringsAsFactors = FALSE, # Prevent automatic factor conversion
                       check.names = FALSE)  %>% # Preserve original column names
  mutate(gene_symbol = toupper(gene_symbol))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # App title displayed at the top
  titlePanel("Knockout Mice Statistical Scores by Phenotype"),
  
  # Layout with sidebar and main panel 
  sidebarLayout(
    # Sidebar panel for selecting Phenotype  
    sidebarPanel(
      selectInput("selected_phenotype",               # Input ID
                  "Select Phenotype:",             # Label shown to user
                  choices = sort(unique(mouse_data$parameter_name)),  # List of KO genes
                  selected = unique(mouse_data$parameter_name)[1])  # Default selection
    ),
    
    
    
    # Main panel to show plot and summary table
    mainPanel(
      plotOutput("distPlot"),      # Output plot of p-values
      tableOutput("summaryTable")  # Output table of phenotype stats
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactive expression to filter data based on selected KO gene
  filtered_data <- reactive({
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
  output$distPlot <- renderPlot({
    df <- filtered_data() %>% filter(pvalue < 0.05)
    
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No significant gene found for this phenotype", cex = 1.5)
    } else {
      
      df_top <- df %>%
        arrange(pvalue) 
      
      # Create horizontal bar plot
      ggplot(df_top, aes(x = reorder(gene_symbol, pvalue), y = pvalue))+
        geom_bar(stat = "identity", fill = "skyblue") +             # Use actual p-values as bar height
        coord_flip() +                            # Flip axes for horizontal bars
        labs(title = paste("Phenotype", input$selected_phenotype),  # Dynamic plot title
             x = "KO Gene",                      # X-axis label
             y = "P-value")+                        # Y-axis label
        theme_minimal(base_size = 14)              # Clean theme with readable text
    }
  })
  
  # Render summary table of filtered data with QC status
  output$summaryTable <- renderTable({
    filtered_data() %>%
      arrange(pvalue) %>%  # Sort by p-value
      select(gene_symbol, pvalue, gene_accession_id, analysis_id)  # Show selected columns
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

