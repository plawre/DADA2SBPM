# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("shinythemes", quietly = TRUE)) install.packages("shinythemes")
library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shinythemes)

# Define UI for the application
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel("Parker's Stacked Bar Plot for Taxonomic Data Makerer V3.5"),
  sidebarLayout(
    sidebarPanel(
      h3("Upload and Configure Plot"),
      fileInput("file_input", "Upload ASV Abundance Table:", accept = c(".xlsx")),
      selectInput("filter_level", "Select Filter Level:", choices = NULL),
      uiOutput("filter_name_ui"),
      selectInput("plot_level", "Select Plot Level:", choices = NULL),
      actionButton("plot_button", "Create Plot", class = "btn-primary"),
      br(),
      br(),
      actionButton("save_button", "Save Plot", class = "btn-success"),
      br(),
      br(),
      p("Instructions:"),
      p("1. Upload your ASV Abundance Table (Excel file)."),
      p("2. Select the filter level and the corresponding name."),
      p("3. Choose the taxonomic level for plotting."),
      p("4. Click 'Create Plot' to generate the plot."),
      p("5. Click 'Save Plot' to save the generated plot as a PNG file."),
      br(),
      textOutput("note")
    ),
    mainPanel(
      plotOutput("stacked_bar_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store the uploaded data
  uploaded_data <- reactiveVal(NULL)
  
  observeEvent(input$file_input, {
    req(input$file_input)
    file_path <- input$file_input$datapath
    data <- read_excel(file_path)
    uploaded_data(data)
    
    # Update filter level choices based on the uploaded data
    filter_levels <- sort(names(data)[sapply(data, is.character)])
    updateSelectInput(session, "filter_level", choices = filter_levels)
    # Update plot level choices based on the uploaded data
    plot_levels <- sort(names(data)[sapply(data, is.character)])
    updateSelectInput(session, "plot_level", choices = plot_levels)
  })
  
  observeEvent(input$filter_level, {
    req(uploaded_data())
    data <- uploaded_data()
    
    # Determine the higher taxonomic level
    taxonomic_levels <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    level_index <- match(input$filter_level, taxonomic_levels)
    higher_level <- if (!is.na(level_index) && level_index > 1) taxonomic_levels[level_index - 1] else NULL
    
    # Update the options for filter name based on the selected filter level and higher level
    req(input$filter_level)
    if (!is.null(higher_level) && higher_level %in% names(data)) {
      data <- data %>%
        mutate(FilterName = paste(data[[higher_level]], data[[input$filter_level]], sep = " "))
    } else {
      data <- data %>%
        mutate(FilterName = data[[input$filter_level]])
    }
    
    # Filter out 'x__NA' values from the filter choices
    filter_choices <- unique(data$FilterName)
    filter_choices <- filter_choices[!grepl("__NA", filter_choices)]
    filter_choices <- sort(filter_choices)
    updateSelectizeInput(session, "filter_name", choices = filter_choices, server = TRUE)
    
    # Reset the filter name selection
    updateSelectizeInput(session, "filter_name", selected = character(0))
  })
  
  output$filter_name_ui <- renderUI({
    req(uploaded_data())
    selectizeInput("filter_name", "Select Filter Name:", choices = NULL, options = list(maxOptions = 1000))
  })
  
  plot_data <- reactiveVal(NULL)
  
  observeEvent(input$plot_button, {
    req(uploaded_data())
    data <- uploaded_data()
    filter_level <- input$filter_level
    filter_name <- input$filter_name
    plot_level <- input$plot_level
    
    # Filter the data based on the chosen taxonomic level and name
    taxonomic_levels <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
    level_index <- match(filter_level, taxonomic_levels)
    higher_level <- if (!is.na(level_index) && level_index > 1) taxonomic_levels[level_index - 1] else NULL
    
    if (!is.null(higher_level) && higher_level %in% names(data)) {
      filtered_data <- data %>%
        filter(paste(data[[higher_level]], data[[filter_level]], sep = " ") == filter_name)
    } else {
      filtered_data <- data %>%
        filter(data[[filter_level]] == filter_name)
    }
    
    # Handle sequences without taxonomic information
    filtered_data[[plot_level]] <- ifelse(grepl("__NA$", filtered_data[[plot_level]]), "Other", filtered_data[[plot_level]])
    
    # Remove the prefix 'x__' from the taxonomic levels
    filtered_data[[plot_level]] <- gsub("^.__(.*)$", "\\1", filtered_data[[plot_level]])
    
    # Check if the plot level columns are available in the filtered data
    if (!plot_level %in% colnames(filtered_data)) {
      stop(paste("The column", plot_level, "is not found in the filtered data."))
    }
    
    # Identify the numeric columns for abundance data
    numeric_columns <- filtered_data %>%
      select(where(is.numeric)) %>%
      colnames()
    
    # Include the necessary columns for further processing
    abundance_data <- filtered_data %>%
      select(all_of(plot_level), all_of(numeric_columns))
    
    # Sum the abundance data by the selected taxonomic level
    summed_data <- abundance_data %>%
      group_by(!!sym(plot_level)) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      pivot_longer(cols = -!!sym(plot_level), names_to = "Sample", values_to = "Abundance")
    
    # Calculate relative abundances
    summarized_data <- summed_data %>%
      group_by(Sample) %>%
      mutate(Total_Abundance_Sample = sum(Abundance, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(Relative_Abundance = Abundance / Total_Abundance_Sample * 100)
    
    # Create the color palette for the plot
    unique_levels <- unique(summarized_data[[plot_level]])
    colors <- setNames(scales::hue_pal()(length(unique_levels)), unique_levels)
    if ("Other" %in% unique_levels) {
      colors["Other"] <- "gray"
    }
    
    # Create the plot
    plot <- ggplot(summarized_data, aes(x = Sample, y = Relative_Abundance, fill = !!sym(plot_level))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Abundance), position = position_stack(vjust = 0.5), size = 3) +
      labs(title = paste("Stacked Bar Plot of", plot_level, "within", filter_name, "(Relative Abundance)"),
           x = "Sample",
           y = "Relative Abundance (%)",
           fill = plot_level,
           caption = "Relative abundances in percentages\nNumbers in boxes are absolute reads") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = colors)
    
    plot_data(list(plot = plot, filename = paste0("stacked_bar_plot_", filter_name, "_", plot_level, "_with_counts.png")))
    
    # Render the plot
    output$stacked_bar_plot <- renderPlot({
      plot
    })
    
    # Display the note
    output$note <- renderText({
      "Note: Relative abundances are shown as percentages. Numbers in boxes are absolute reads."
    })
  })
  
  observeEvent(input$save_button, {
    # Save the plot
    plot_data_val <- plot_data()
    if (!is.null(plot_data_val)) {
      ggsave(plot_data_val$filename, plot = plot_data_val$plot)
      showNotification("Plot saved successfully!", type = "message")
    } else {
      showNotification("No plot to save. Please create a plot first.", type = "warning")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
