


##############################################################
##############################################################
### Start App

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Parks By Type"),

  # Sidebar with a input 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "districtnum",
                  label = "Choose a district?",
                  choices = sort(unique(parks_census_dist@data$Num)),
                  selected = "1"),
      selectInput(inputId = "amenity",
                  label = "Choose a park amenity",
                  choices = names(parks_census_dist@data[4:47]),
                  selected = 'Open_Turf')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(h4(textOutput("ParkMapTitle")),
             leafletOutput("ParkMap"),
             h4(textOutput("PopDensPlotTitle")),
             plotOutput("PopDensPlot"),
             h4(textOutput("demoTableTitle")),
             DT::dataTableOutput("CensusTable"))
    )
)

pal <- colorFactor(c("cyan2", 
                     "coral2", 
                     "green2", 
                     "darkgreen", 
                     "mediumorchid", 
                     "mediumblue", 
                     "magenta", 
                     "darkmagenta"), 
                   domain = unique(parks_census_dist@data$Park_Type))

pal_districts <- colorFactor(palette = c("#999999", "#4d4d4d", "#808080", "#bfbfbf", 
                               "#666666", "#b3b3b3"), 
                            domain = c("1", "2" ,"3", "4", "5", "6"))

pal_districts_TF <- colorFactor(palette = c("magenta", "mediumblue"),
                                domain = c("TRUE", "FALSE"))

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$ParkMapTitle <- renderText({
    "Parks with the selected amenity"
  })
 # parks_spatial[!is.na(parks_spatial@data[,'Open_Turf']),] 
  
  parks_subset <- reactive({
    parks_census_dist[!is.na(parks_census_dist@data[,input$amenity]),]
  })
  
  output$ParkMap <- renderLeaflet({
      leaflet(parks_subset())  %>%
      addTiles()  %>%
      addPolygons(data = districts,
                  popup = paste("<b>District ", districts@data$Num, "</b><br>", 
                                "Council Member: ", districts@data$Council_Me),
                  fillColor = ~pal_districts(districts@data$Num), #polygon fill
                  color = "black", #stroke color
                  stroke = 1,  #stroke width
                  fillOpacity = .6) %>% 
      addCircleMarkers(data = parks_subset(),
                       radius = 8,
                       color = ~pal_districts_TF(as.character(input$districtnum != parks_subset()@data$Num | 
                                                   is.na(parks_subset()@data$Num))),
                       #color = ~pal(parks_subset()@data$Park_Type),
                       stroke = FALSE, 
                       fillOpacity = 0.7, 
                       popup = paste("<b>",parks_subset()@data$Park_Name, "</b><br>", 
                                     parks_subset()@data$Address, "<br>", 
                                     parks_subset()@data$NAMELSAD, "<br>", 
                                     "<em>",parks_subset()@data$Park_Type, "</em><br>",
                                     "Area population density: ", 
                                     parks_subset()@data$Population_Density_per_sq_mile)) %>%
      addLegend('bottomleft', 
                 colors = c("magenta", "mediumblue"),
                 labels = c("In District", "Out of District"), #show the whole range
                 title = 'South Bend Park Location',
                 opacity = 0.7) %>%
      setView(-86.2520, 41.6764, zoom = 11)
    
    
  #      addPolygons(fillColor = ~pal2(elections@data$SUB_REGION), 
  #                color = ~pal3(elections@data[,input$elect]), 
  #                weight = 2)
  })
  
  output$PopDensPlotTitle <- renderText({
    "Population density near those parks"
  })
  
  output$PopDensPlot <- renderPlot({
# Set up the plot
    ggplot(
      data = parks_subset()@data,
      aes(x = Park_Type, y = Population_Density_per_sq_mile, 
          #color by in/out of district
          color = as.character(input$districtnum != Num | is.na(Num)), size = 2) 
    ) + geom_point() + 
      guides(size = FALSE, color = FALSE) +
    #color = guide_legend(override.aes = list(size = 5, shape = 15))) + #don't show the size legend
      theme_minimal() + 
      theme(text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 14)) +
      scale_color_manual("District", values = c("TRUE" = "mediumblue", "FALSE" = "magenta"), 
                         labels = c("FALSE" = "In District", "TRUE" = "Out of District")) +
      xlab("\nPark Type") +
      ylab("Population density per sq. mile \n")
  })
  
  
  output$demoTableTitle <- renderText({
    "Demographics and population near those parks"
  })


    output$CensusTable <- DT::renderDataTable({
    DT::datatable(parks_subset()@data[, c(1:3, 57, 63, 87:93)], 
                  
                  options = list(pageLength = 5, scrollX = T)) %>%
        formatStyle(
          'District',
          target = 'row',
        backgroundColor = styleEqual(input$districtnum, '#ffe6ff'))
    })
}


# Run the application 
shinyApp(ui = ui, server = server)

