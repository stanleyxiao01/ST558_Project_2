

# ST 558 Project 2: Bank Marketing Analytics Dashboard
# Interactive exploration of customer characteristics and term deposit subscription outcomes

library(ggplot2)
library(shiny)
library(DT)
library(shinycssloaders)

# Load the Bank Marketing dataset
bank_data <- read.csv("Dataset/bank_clean.csv")

# ---------------------------------------------------------
# Validate that the filtered dataset is not empty
# ---------------------------------------------------------
validate_data <- function(data) {
  validate(
    need(
      nrow(data) > 0,
      "No observations match the selected filters. Please broaden your selections and click 'Apply Filters' again"
    )
  )
}

# ---------------------------------------------------------
# Variable Choices for Dropdown Menus
# ---------------------------------------------------------

# Categorical Variables
categorical_choices <- c(
  "Job" = "job",
  "Marital Status" = "marital",
  "Education" = "education",
  "Housing Loan" = "housing",
  "Personal Loan" = "loan",
  "Contact Method" = "contact",
  "Subscription Outcome" = "y"
)

# Numerical Variables
numeric_choices <- c(
  "Age" = "age",
  "Account Balance" = "balance",
  "Campaign Contacts" = "campaign",
  "Call Duration" = "duration",
  "Previous Contacts" = "previous"
)

# Plot Choices
plot_choices <- c(
  "Histogram" = "histogram",
  "Boxplot" = "boxplot",
  "Bar Chart" = "bar",
  "Scatterplot" = "scatter"
)

# Optional choices for summaries without grouping
categorical_choices_optional <- c("None" = "none", categorical_choices)

job_choices <- c("All", sort(unique(bank_data$job)))
education_choices <- c("All", sort(unique(bank_data$education)))

# Define UI
ui <- fluidPage(
  # Dashboard title
  titlePanel("Bank Marketing Analytics Dashboard"),
  
  # Sidebar layout with filters and main content
  sidebarLayout(
    # Sidebar: User-selected data filters
    sidebarPanel(
      # Categorical filter 1: Job
      selectInput("job", "Select Job:", choices = job_choices),
      
      # Categorical filter 2: Education
      
      selectInput("education", "Select Education:", choices = education_choices),
      
      hr(),
      
      # Numeric variable 1
      selectInput(
        "numeric_var1",
        "Select First Numeric Variable:",
        choices = numeric_choices,
        selected = "age"
      ),
      
      # Dynamic Slider for numeric variable 1
      uiOutput("numeric_slider1"),
      
      # Numeric variable 2
      selectInput(
        "numeric_var2",
        "Select Second Numeric Variable:",
        choices = numeric_choices,
        selected = "balance"
      ),
      
      # Dynamic slider for numeric variable 2
      uiOutput("numeric_slider2"),
      
      hr(),
      
      # Apply filters only when clicked
      actionButton("apply_filters", "Apply Filters", class = "btn-primary"),
      
      hr(),
      
      #Display the number of observations after filtering
      strong("Current Dataset Size:"),
      
      textOutput("observation_count")
    ),
    
    # Main panel: Dashboard tabs
    mainPanel(tabsetPanel(
      # ---------------------------------------------------------
      # About Tab
      # ---------------------------------------------------------
      
      tabPanel(
        "About",
        tags$img(src = "banking_photo.jpg?v=2", alt = "Banking and financial analytics", width = "600"),
        h3("Bank Marketing Analytics Dashboard"),
        p(
          "This interactive dashboard explores customer characteristics and marketing campaign outcomes using the Bank Marketing dataset."
        ),
        h4("Project Purpose"),
        p(
          "The goal of this application is to help users investigate relationships between customer demographics, financial characteristics, campaign interactions, and term deposit subscription outcomes"
        ),
        p(
          strong("Response Variable:"),
          "The variable 'y' indicates whether a customer subscribed to a term deposit (yes or no)."
        ),
        h4("About the Dataset"),
        p(
          "The dataset contains 45,211 observations and 17 variables. It includes demographic characteristics, financial information, previous marketing interactions, and term deposit subscription outcomes"
        ),
        h4("How to Use This Dashboard"),
        tags$ol(
          tags$li("Use the sidebar to select customer job and education filters."),
          tags$li("Choose two numerical variables and adjust their ranges."),
          tags$li("Click Apply Filters to update the dataset."),
          tags$li("Open the Data Download tab to view or download the data."),
          tags$li(
            "Open the Data Exploration tab to generate summary tables and customized visualizations."
          ),
          tags$li("Use grouping and faceting to compare different customer groups.")
        ),
        
        h4("Dataset Source"),
        p(
          "The Bank Marketing dataset was obtained from",
          tags$a(
            "Kaggle - Banking Dataset (Marketing Targets)",
            href = "https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets",
            target = "_blank"
          ),
          ". The original Bank Marketing dataset is available through the",
          tags$a(
            "UCI Machine Learning Repository",
            href = "https://archive.ics.uci.edu/dataset/222/bank+marketing",
            target = "_blank"
          ),
          "."
        ),
        
        h4("About This Project"),
        
        p(
          "Developed for ST 558 at North Carolina State University using R, Shiny, ggplot2, and DT."
        )
      ),
      
      # EDA Report
      tabPanel(
        "EDA Report",
        
        h3("Exploratory Data Analysis Report"),
        
        p(
          "This report presents descriptive statistics, categorical and numerical summaries, and six visualizations exploring relationships between customer characteristics and term deposit subscription outcomes."
        ),
        
        tags$p(
          tags$a(
            "Open EDA Report in a New Tab",
            href = "eda.html?v=3",
            target = "_blank",
            rel = "noopener noreferrer"
          )
        ),
        
        tags$iframe(
          src = "eda.html",
          style = paste0(
            "width: 100%; height: 900px; border: 1px solid #ddd; border-radius: 8px;"
          )
        )
      ),
      
      # Data Download tab
      tabPanel(
        "Data Download",
        
        h3("Bank Marketing Dataset"),
        
        p("Explore the dataset and select which data to download."),
        
        shinycssloaders::withSpinner(DT::dataTableOutput("bank_table"), type = 4),
        
        br(),
        
        radioButtons(
          inputId = "download_type",
          label = "Select Dataset to Download:",
          choices = c("Full Dataset" = "full", "Filtered Dataset" = "filtered"),
          selected = "full"
        ),
        
        downloadButton(outputId = "download_data", label = "Download CSV")
      ),
      
      
      # Data Exploration tab: Visual summaries
      tabPanel(
        "Data Exploration",
        
        h3("Explore Bank Marketing Data"),
        
        p(
          "Explore customer characteristics and campaign outcomes using the applied sidebar filters."
        ),
        
        # Categorical Summaries
        h4("Categorical Variable Summary"),
        
        selectInput(
          inputId = "cat_var1",
          label = "Select First Categorical Variable:",
          choices = categorical_choices,
          selected = "job"
        ),
        
        selectInput(
          inputId = "cat_var2",
          label = "Select Second Categorical Variable:",
          choices = categorical_choices_optional,
          selected = "y"
        ),
        
        shinycssloaders::withSpinner(tableOutput("categorical_summary"), type = 4),
        
        hr(),
        
        # Numerical Summaries
        h4("Numerical Variable Summary"),
        
        selectInput(
          inputId = "summary_numeric",
          label = "Select Numerical Variable:",
          choices = numeric_choices,
          selected = "age"
        ),
        
        # Grouping variable
        selectInput(
          inputId = "summary_group",
          label = "Group Numerical Summary By:",
          choices = categorical_choices_optional,
          selected = "y"
        ),
        
        shinycssloaders::withSpinner(tableOutput("numerical_summary"), type = 4),
        
        hr(),
        
        # Interactive Data Visualization
        h4("Interactive Data Visualizations"),
        
        p(
          "Choose a plot type and customize its variables, grouping, and faceting."
        ),
        
        # Users select the type of plot
        selectInput(
          inputId = "plot_type",
          label = "Select Plot Type:",
          choices = plot_choices,
          selected = "histogram"
        ),
        
        # Display only relevant variable selectors
        uiOutput("plot_variable_controls"),
        
        # Select an optional grouping variable
        selectInput(
          inputId = "plot_group",
          label = "Group Plot By:",
          choices = categorical_choices_optional,
          selected = "none"
        ),
        
        # Select an optional faceting variable
        selectInput(
          inputId = "plot_facet",
          label = "Facet Plot By:",
          choices = categorical_choices_optional,
          selected = "none"
        ),
        
        # Display the selected plot with a loading indicator
        shinycssloaders::withSpinner(plotOutput("interactive_plot", height = "450px"), type = 4)
        
      )
    ))
  )
)

# ---------------------------------------------------------
# Server Logic
# ---------------------------------------------------------
server <- function(input, output, session) {
  # Dynamic Numeric Sliders
  # Generate slider for the first numeric variable
  output$numeric_slider1 <- renderUI({
    # Wait until a numeric variable is selected
    req(input$numeric_var1)
    
    # Extract the selected numeric variable
    variable <- bank_data[[input$numeric_var1]]
    
    # Create a range slider based on the variable's values
    sliderInput(
      inputId = "range1",
      label = paste("Select", input$numeric_var1, "Range:"),
      min = min(variable, na.rm = TRUE),
      max = max(variable, na.rm = TRUE),
      value = range(variable, na.rm = TRUE)
    )
  })
  
  # Generate slider for the second numeric variable
  output$numeric_slider2 <- renderUI({
    # Wait until the first numeric variable is selected
    req(input$numeric_var2)
    
    # Extract the selected numeric variable
    variable <- bank_data[[input$numeric_var2]]
    
    # Create a range slider based on the variable's values
    sliderInput(
      inputId = "range2",
      label = paste("Select", input$numeric_var2, "Range:"),
      min = min(variable, na.rm = TRUE),
      max = max(variable, na.rm = TRUE),
      value = range(variable, na.rm = TRUE)
    )
  })
  
  # Reactive Data Filtering
  
  # Initialize the filtered dataset with the complete dataset
  filtered_data <- reactiveVal(bank_data)
  
  # Update the filtered dataset only when Apply Filters is clicked
  observeEvent(input$apply_filters, {
    # Begin with the complete dataset
    data <- bank_data
    
    # Ensure both numeric sliders are available
    req(input$numeric_var1,
        input$numeric_var2,
        input$range1,
        input$range2)
    
    # Filter by Job
    if (input$job != "All") {
      data <- data[data$job == input$job, , drop = FALSE]
    }
    
    # Filter by Education
    if (input$education != "All") {
      data <- data[data$education == input$education, , drop = FALSE]
    }
    
    # Filter by the first numeric range
    data <- data[!is.na(data[[input$numeric_var1]]) &
                   data[[input$numeric_var1]] >= input$range1[1] &
                   data[[input$numeric_var1]] <= input$range1[2], , drop = FALSE]
    
    # Filter by the second numeric range
    data <- data[!is.na(data[[input$numeric_var2]]) &
                   data[[input$numeric_var2]] >= input$range2[1] &
                   data[[input$numeric_var2]] <= input$range2[2], , drop = FALSE]
    
    # Save the newly filtered dataset
    filtered_data(data)
  })
  
  output$observation_count <- renderText({
    # Retrieve the currently applied dataset
    data <- filtered_data()
    
    paste(format(nrow(data), big.mark = ","),
          "of",
          format(nrow(bank_data), big.mark = ","),
          "observations")
    
  })
  
  # Interactive Data Table
  output$bank_table <- DT::renderDataTable({
    data <- filtered_data()
    
    validate_data(data)
    
    data
    
  }, options = list(
    pageLength = 10,
    scrollX = TRUE,
    searching = TRUE,
    ordering = TRUE,
    lengthChange = TRUE
  ), rownames = FALSE)
  
  # Categorical Variable Summary
  
  output$categorical_summary <- renderTable({
    data <- filtered_data()
    
    # Check that observations are available
    validate_data(data)
    
    # Retrieve selected categorical variables
    var1 <- input$cat_var1
    var2 <- input$cat_var2
    
    if (var2 == "none") {
      # One-way frequency table
      result <- table(data[[var1]], useNA = "ifany")
      
      # Convert to a data frame
      summary_table <- as.data.frame(result)
      names(summary_table) <- c(var1, "Frequency")
    } else {
      # Two-way frequency table
      result <- table(data[[var1]], data[[var2]], useNA = "ifany")
      
      # Convert to a data frame
      summary_table <- as.data.frame.matrix(result)
      
      # Add first categorical variable as a column
      summary_table <- data.frame(
        Category = rownames(summary_table),
        summary_table,
        row.names = NULL,
        check.names = FALSE
      )
      
      names(summary_table)[1] <- var1
      
    }
    
    # Display the summary
    summary_table
    
  }, rownames = FALSE)
  
  # Numerical Variable Summary
  
  output$numerical_summary <- renderTable({
    # Filtered dataset
    data <- filtered_data()
    
    # Validate data
    validate_data(data)
    
    # Use selected variables
    numeric_var <- input$summary_numeric
    group_var <- input$summary_group
    
    # Function to calculate descriptive statistics
    calculate_summary <- function(x) {
      # Remove missing values
      x <- x[!is.na(x)]
      
      c(
        Count = length(x),
        Mean = if (length(x) > 0)
          mean(x)
        else
          NA_real_,
        Median = if (length(x) > 0)
          median(x)
        else
          NA_real_,
        SD = if (length(x) > 1)
          sd(x)
        else
          NA_real_,
        Min = if (length(x) > 0)
          min(x)
        else
          NA_real_,
        Max = if (length(x) > 0)
          max(x)
        else
          NA_real_
      )
      
    }
    
    if (group_var == "none") {
      #Calculate overall descriptive statistics
      result <- as.data.frame(t(calculate_summary(data[[numeric_var]])))
      
    } else {
      # Split values by the selected categorical variable
      grouped_values <- split(data[[numeric_var]], data[[group_var]])
      
      # Calculate statistics for each group
      result <- do.call(rbind, lapply(grouped_values, calculate_summary))
      
      # Add the group names as a column
      result <- data.frame(Group = rownames(result),
                           as.data.frame(result),
                           row.names = NULL)
    }
    
    # Round numerical statistics to two decimal places
    numeric_columns <- sapply(result, is.numeric)
    
    result[numeric_columns] <- lapply(result[numeric_columns], round, digits = 2)
    
    result
  }, rownames = FALSE)
  
  # Download either the full or filtered dataset
  output$download_data <- downloadHandler(
    filename = function() {
      if (input$download_type == "full") {
        "bank_marketing_full.csv"
      } else {
        "bank_marketing_filtered.csv"
      }
    },
    
    # Write the selected dataset to a CSV file
    content = function(file) {
      if (input$download_type == "full") {
        data_to_download <- bank_data
      } else {
        data_to_download <- filtered_data()
      }
      
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  # Dynamic Plot Variable Controls
  output$plot_variable_controls <- renderUI({
    req(input$plot_type)
    
    plot_type <- input$plot_type
    
    if (plot_type == "histogram") {
      # Histograms require one numerical variable
      tagList(
        selectInput(
          inputId = "plot_numeric_x",
          label = "Select X-Axis Numerical Variable:",
          choices = numeric_choices,
          selected = "age"
        )
      )
    } else if (plot_type == "boxplot") {
      # Boxplots compare a numerical variable across categories
      tagList(
        selectInput(
          inputId = "plot_category",
          label = "Select X-Axis Categorical Variable:",
          choices = categorical_choices,
          selected = "job"
        ),
        
        selectInput(
          inputId = "plot_numeric_y",
          label = "Select Y-Axis Numerical Variable:",
          choices = numeric_choices,
          selected = "balance"
        )
      )
      
    } else if (plot_type == "bar") {
      # Bar charts require one categorical variable
      tagList(
        selectInput(
          inputId = "plot_category",
          label = "Select Categorical Variable:",
          choices = categorical_choices,
          selected = "job"
        )
      )
      
    } else if (plot_type == "scatter") {
      # ScatterPlots compare two numerical variables
      tagList(
        selectInput(
          inputId = "plot_numeric_x",
          label = "Select X-Axis Numerical Variable:",
          choices = numeric_choices,
          selected = "age"
        ),
        
        selectInput(
          inputId = "plot_numeric_y",
          label = "Select Y-Axis Numerical Variable:",
          choices = numeric_choices,
          selected = "balance"
        )
      )
    }
    
  })
  
  # Generate Various Interactive Plots
  output$interactive_plot <- renderPlot({
    # Retrieve the applied filtered data
    data <- filtered_data()
    
    # Display a message if no observations match
    validate_data(data)
    
    # Retrieve the selected plot settings
    req(input$plot_type)
    
    plot_type <- input$plot_type
    
    # Retreive grouping and faceting selections
    group_var <- input$plot_group
    facet_var <- input$plot_facet
    
    req(group_var, facet_var)
    
    # Histogram
    if (plot_type == "histogram") {
      req(input$plot_numeric_x)
      
      numeric_x <- input$plot_numeric_x
      
      p <- ggplot(data, aes(x = .data[[numeric_x]])) +
        geom_histogram(bins = 30, color = "white") +
        labs(
          title = paste("Distribution of", numeric_x),
          x = numeric_x,
          y = "Frequency"
        )
      
    } else if (plot_type == "boxplot") {
      # Boxplot
      
      req(input$plot_category, input$plot_numeric_y)
      
      category <- input$plot_category
      numeric_y <- input$plot_numeric_y
      
      p <- ggplot(data, aes(x = .data[[category]], y = .data[[numeric_y]])) +
        geom_boxplot() +
        labs(
          title = paste(numeric_y, "by", category),
          x = category,
          y = numeric_y
        )
      
    } else if (plot_type == "bar") {
      # Bar Chart
      
      req(input$plot_category)
      
      category <- input$plot_category
      
      p <- ggplot(data, aes(x = .data[[category]])) +
        geom_bar() +
        labs(
          title = paste("Distribution of", category),
          x = category,
          y = "Frequency"
        )
      
    } else if (plot_type == "scatter") {
      # Scatterplot
      
      req(input$plot_numeric_x, input$plot_numeric_y)
      
      numeric_x <- input$plot_numeric_x
      numeric_y <- input$plot_numeric_y
      
      # Prevent selecting identical variables
      validate(need(
        numeric_x != numeric_y,
        "Select two different numerical variables."
      ))
      
      p <- ggplot(data, aes(x = .data[[numeric_x]], y = .data[[numeric_y]])) +
        geom_point(alpha = 0.4) +
        labs(
          title = paste(numeric_y, "vs.", numeric_x),
          x = numeric_x,
          y = numeric_y
        )
      
    } else {
      validate(need(FALSE, "Please select a valid plot type."))
    }
    
    # Apply optional grouping through fill or color
    if (group_var != "none") {
      if (plot_type == "scatter") {
        # Color scatterplot points by group
        p <- p +
          aes(color = .data[[group_var]]) +
          labs(color = group_var)
        
      } else {
        # Fill bars and boxplots by groups
        p <- p +
          aes(fill = .data[[group_var]]) +
          labs(fill = group_var)
        
      }
    }
    
    # Apply Optional Faceting
    
    if (facet_var != "none") {
      # Create separate panels for each category
      p <- p + facet_wrap(vars(.data[[facet_var]]))
    }
    
    # Apply a consistent theme and display the completed plot
    p + theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  })
  
}

# ---------------------------------------------------------
# Run the application
# ---------------------------------------------------------
shinyApp(ui = ui, server = server)
