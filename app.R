
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
            selectInput("job",
                        "Select Job:",
                        choices = c("All", unique(bank_data$job)))
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
    
  #Display the first 6 observations
    output$bank_table <- DT::renderDataTable({
        bank_data
    },
    options = list(
      pageLength = 10,
      scrollX = TRUE
    ),
    rownames = FALSE
    )
    
    #Download the complete dataset as CSV
    output$download_data <- downloadHandler(
      
      filename = function() {
        "bank_marketing_full.csv"
      },
      
      content = function(file) {
        write.csv(bank_data, file, row.names = FALSE)
      }
    )
    
    #Generate a histogram of customer ages
    output$age_plot <- renderPlot({
      hist(
        bank_data$age,
        main = "Distribution of Customer Age",
        xlab = "Age"
      )
    })
}

#---------------------------------------------------------
# Run the application 
#---------------------------------------------------------
shinyApp(ui = ui, server = server)
