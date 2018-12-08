#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage("East Group 4 Final Project",
                tabPanel("Steve",
                         sidebarLayout(
                           sidebarPanel(
                             numericInput(inputId = "tableNrow",
                                          label = "Number of Rows to Display",
                                          value = 5,
                                          min = 1)
                           ),
                           mainPanel(tableOutput("Table1") )
                         )
                ),
                
                tabPanel("Mike",
                         sidebarLayout(
                           sidebarPanel(),
                           mainPanel()
                         )
                )
   

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Table1 <- renderTable({ head(park.spatial@data, input$tableNrow)}) 
}

# Run the application 
shinyApp(ui = ui, server = server)

