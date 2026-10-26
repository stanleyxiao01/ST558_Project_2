
# ST 558 Project 2: Bank Marketing Analytics Dashboard
# Interactive exploration of customer characteristics and term deposit subscription outcomes

library(shiny)
library(DT)

# Load the Bank Marketing dataset
bank_data <- read.csv("Dataset/bank_clean.csv")

# ---------------------------------------------------------
# Validate that the filtered dataset is not empty
# ---------------------------------------------------------
validate_data <- function(data) {
  validate(need(nrow(data) > 0, "No observations match the selected filters."))
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

# Optional choices for summaries without grouping
categorical_choices_optional <- c(
  "None" = "none", categorical_choices
)

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
          selectInput("job",
                        "Select Job:",
                        choices = job_choices
        ),
        
        # Categorical filter 2: Education
        
        selectInput(
          "education",
          "Select Education:",
          choices = education_choices
          ),
        
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
        actionButton(
          "apply_filters",
          "Apply Filters",
          class = "btn-primary"
        )
    ),
    
        # Main panel: Dashboard tabs
        mainPanel(
          tabsetPanel(
            # About tab: Project description
             tabPanel("About",
                      h3("About This Dashboard"),
                      p("Explore bank marketing campaign data.")),
             
             # Data Download tab
             tabPanel("Data Download",
                      
                      h3("Bank Marketing Dataset"),
                      
                      p("Explore the dataset and select which data to download."),
                      
                      DT::dataTableOutput("bank_table"),
                      
                      br(),
                      
                      radioButtons(
                        inputId = "download_type",
                        label = "Select Dataset to Download:",
                        choices = c(
                          "Full Dataset" = "full",
                          "Filtered Dataset" = "filtered"
                        ),
                        selected = "full"
                      ),
                      
                      downloadButton(
                        outputId = "download_data",
                        label = "Download CSV"
                      )
             ),
                    
             
             # Data Exploration tab: Visual summaries
             tabPanel("Data Exploration",
                      
                      h3("Explore Bank Marketing Data"),
                      
                      p("Explore customer characteristics and campaign outcomes using the applied sidebar filters."),
                      
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
                      
                      tableOutput("categorical_summary"),
                      
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
                      
                      tableOutput("numerical_summary"),
                      
                      hr(),
                      
                      # Visualization
                      h4("Customer Age Distribution"),
                      plotOutput("age_plot"))
           )
        )
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
    req(
      input$numeric_var1,
      input$numeric_var2,
      input$range1,
      input$range2
    )
    
    # Filter by Job
    if (input$job != "All") {
      data <- data[data$job == input$job, , drop = FALSE]
    }
    
    # Filter by Education
    if (input$education != "All") {
      data <- data[data$education == input$education, , drop = FALSE]
    }
    
    # Filter by the first numeric range
    data <- data[
      !is.na(data[[input$numeric_var1]]) &
        data[[input$numeric_var1]] >= input$range1[1] &
        data[[input$numeric_var1]] <= input$range1[2],
      ,
      drop = FALSE
    ]
    
    # Filter by the second numeric range
    data <- data[
      !is.na(data[[input$numeric_var2]]) &
        data[[input$numeric_var2]] >= input$range2[1] &
        data[[input$numeric_var2]] <= input$range2[2],
      ,
      drop = FALSE
    ]
    
    # Save the newly filtered dataset
    filtered_data(data)
  })
  
    # Display interactive dataset
    output$bank_table <- DT::renderDataTable({
        
      data <- filtered_data()
      
      data
      
    },
    options = list(
      pageLength = 10,
      scrollX = TRUE
    ),
    rownames = FALSE
    )
    
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
        result <- table(
          data[[var1]],
          data[[var2]],
          useNA = "ifany"
        )
       
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
          Mean = if (length(x) > 0) mean(x) else NA_real_,
          Median = if (length(x) > 0) median(x) else NA_real_,
          SD = if (length(x) > 1) sd(x) else NA_real_,
          Min = if (length(x) > 0) min(x) else NA_real_,
          Max = if (length(x) > 0) max(x) else NA_real_
        )
        
      } 
        
      if (group_var == "none") {
        
        #Calculate overall descriptive statistics
        result <- as.data.frame(
          t(calculate_summary(data[[numeric_var]]))
        )
        
      } else {
        
        # Split values by the selected categorical variable
        grouped_values <- split(
          data[[numeric_var]],
          data[[group_var]]
        )
        
        # Calculate statistics for each group
        result <- do.call(rbind, lapply(grouped_values, calculate_summary))
        
        # Add the group names as a column
        result <- data.frame(
          Group = rownames(result),
          as.data.frame(result),
          row.names = NULL
        )
      }
      
      # Round numerical statistics to two decimal places
      numeric_columns <- sapply(result, is.numeric)
      
      result[numeric_columns] <- lapply(
        result[numeric_columns],
        round,
        digits = 2
      )
      
      result
    }, rownames = FALSE
    
    )

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
        
        if (input$download_type == "full"){
          data_to_download <- bank_data
        } else {
          data_to_download <- filtered_data()
        }
        
        write.csv(
          data_to_download, file, row.names = FALSE
        )
      }
    )
    
    # Generate a histogram of customer ages
    output$age_plot <- renderPlot({
      
      # Retrieve the applied filtered data
      data <- filtered_data()
      
      # Display a message if no observations match
      validate_data(data)

      hist(
        data$age,
        main = "Distribution of Customer Age",
        xlab = "Age",
        col = "lightblue",
        border = "white"
      )
    })
}

# ---------------------------------------------------------
# Run the application 
# ---------------------------------------------------------
shinyApp(ui = ui, server = server)
