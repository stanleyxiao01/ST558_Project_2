
# ST 558 Project 2: Bank Marketing Analytics Dashboard
# Interactive exploration of customer characteristics and term deposit subscription outcomes

library(shiny)
library(DT)

#Load the Bank Marketing dataset
bank_data <- read.csv("Dataset/bank_clean.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Dashboard title
    titlePanel("Bank Marketing Analytics Dashboard"),

    # Sidebar layout with filters and main content
    sidebarLayout(
      
        #Sidebar: User-selected data filters
        sidebarPanel(
          
          #Categorical filter 1: Job  
          selectInput("job",
                        "Select Job:",
                        choices = c("All", unique(bank_data$job))
        ),
        
        #Categorical filter 2: Education
        
        selectInput(
          "education",
          "Select Education:",
          choices = c("All", sort(unique(bank_data$education)))
          ),
        
        hr(),
        
        # Numeric variable 1
        selectInput(
          "numeric_var1",
          "Select First Numeric Variable:",
          choices = c(
            "Age" = "age",
            "Account Balance" = "balance",
            "Campaign Contacts" = "campaign",
            "Call Duration" = "duration",
            "Previous Contacts" = "previous"
          ),
          selected = "age"
        ),
        
        #Dynamic Slider for numeric variable 1
        uiOutput("numeric_slider1"),
        
        #Numeric variable 2
        selectInput(
          "numeric_var2",
          "Select Second Numeric Variable:",
          choices = c(
            "Age" = "age",
            "Account Balance" = "balance",
            "Campaign Contacts" = "campaign",
            "Call Duration" = "duration",
            "Previous Contacts" = "previous"
          ),
          selected = "balance"
        ),
        
        #Dynamic slider for numeric variable 2
        uiOutput("numeric_slider2"),
        
        hr(),
        
        #Apply filters only when clicked
        actionButton(
          "apply_filters",
          "Apply Filters",
          class = "btn-primary"
        )
    ),
    
        #Main panel: Dashboard tabs
        mainPanel(
          tabsetPanel(
            # About tab: Project description
             tabPanel("About",
                      h3("About This Dashboard"),
                      p("Explore bank marketing campaign data.")),
             
             #Data Download tab
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
                    
             
             #Data Exploration tab: Visual summaries
             tabPanel("Data Exploration",
                      plotOutput("age_plot"))
           )
        )
    )
)


#---------------------------------------------------------
# Server Logic
#---------------------------------------------------------
server <- function(input, output, session) {

  # Dynamic Numeric Sliders

  #Generate slider for the first numeric variable
  output$numeric_slider1 <- renderUI({
    
    #Wait until a numeric variable is selected
    req(input$numeric_var1)
    
    #Extract the selected numeric variable
    variable <- bank_data[[input$numeric_var1]]
    
    #Create a range slider based on the variable's values
    sliderInput(
      inputId = "range1",
      label = paste("Select", input$numeric_var1, "Range:"),
      min = min(variable, na.rm = TRUE),
      max = max(variable, na.rm = TRUE),
      value = range(variable, na.rm = TRUE)
    )
  })
  
  #Generate slider for the second numeric variable
  output$numeric_slider2 <- renderUI({
    
    #Wait until the first numeric variable is selected
    req(input$numeric_var2)
    
    #Extract the selected numeric variable
    variable <- bank_data[[input$numeric_var2]]
    
    #Create a range slider based on the variable's values
    sliderInput(
      inputId = "range2",
      label = paste("Select", input$numeric_var2, "Range:"),
      min = min(variable, na.rm = TRUE),
      max = max(variable, na.rm = TRUE),
      value = range(variable, na.rm = TRUE)
    )
  })
  
  #Reactive Data Filtering
  
  #Update filtered data only when Apply Filters is clicked
  filtered_data <- eventReactive(input$apply_filters, {
    
    #Begin with the complete dataset
    data <- bank_data
    
    #Filter by Job
    if (input$job != "All") {
      data <- data[data$job == input$job, , drop = FALSE]
    }
    
    #Filter by Education
    if (input$education != "All") {
      data <- data[data$education == input$education, , drop = FALSE]
    }
    
    #Ensure both numeric variables and sliders are available
    req(
      input$numeric_var1,
      input$numeric_var2,
      input$range1,
      input$range2
    )
    
    #Filter by the first numeric range
    data <- data[
      !is.na(data[[input$numeric_var1]]) &
        data[[input$numeric_var1]] >= input$range1[1] &
        data[[input$numeric_var1]] <= input$range1[2],
      ,
      drop = FALSE
    ]
    
    #Filter by the second numeric range
    data <- data[
      !is.na(data[[input$numeric_var2]]) &
        data[[input$numeric_var2]] >= input$range2[1] &
        data[[input$numeric_var2]] <= input$range2[2],
      ,
      drop = FALSE
    ]
    
    #Return the filtered dataset
    data
  }, ignoreNULL = FALSE)
      
  #Display interactive dataset
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
    
    #Download either the full or filtered dataset
    output$download_data <- downloadHandler(
      
      filename = function() {
        
        if (input$download_type == "full") {
          "bank_marketing_full.csv"
        } else {
            "bank_marketing_filtered.csv"
          }
        },
      
      #Write the selected dataset to a CSV file
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
    
    #Generate a histogram of customer ages
    output$age_plot <- renderPlot({
      
      #Retrieve the applied filtered data
      data <- filtered_data()
      
      #Display a message if no observations match
      validate(
        need(nrow(data) > 0,
             "No observatinos match the selected filters.")
      )

      hist(
        data$age,
        main = "Distribution of Customer Age",
        xlab = "Age",
        col = "lightblue",
        border = "white"
      )
    })
}

#---------------------------------------------------------
# Run the application 
#---------------------------------------------------------
shinyApp(ui = ui, server = server)
