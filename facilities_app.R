#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(leaflet)
library(tidyverse)
library(rgeos)
library(maptools)
library(stringr)
library(geosphere)

# Data processing
districts <- readOGR(dsn = "~/Notre Dame courses/Data Visualization/Final Project", 
                     layer = "City_Council_Districts", 
                     stringsAsFactors = FALSE)

load("~/Notre Dame courses/Data Visualization/Final Project/Public_Facilities_SHP.RData")

#get the geo encoding from districts
common_crs <- CRS(proj4string(districts))

#apply coding to facilities
facilities <- spTransform(public_facilities_spatial, common_crs)

district_pal <- colorFactor(palette = c("red", "orange", "yellow", "green", 
                                        "blue", "violet"), 
                            domain = c("1", "2", "3", "4", "5", "6")
                            )

facility_pal <- colorFactor(palette = c("firebrick", "salmon3", "navy"), 
                            domain = c("FIRE STATION", "LIBRARY", 
                                       "POLICE STATION"))

ov <- over(facilities, districts)
facilities@data$district <- ov$Num
facilities@data$in_district <- "Yes"

districts.center <- SpatialPointsDataFrame(gCentroid(districts, byid = TRUE), 
                                           districts@data, match.ID = FALSE)

for (i in 1:nrow(facilities@data)) {
  if(is.na(facilities@data[i, ]$district)) {
    distances <- distHaversine(facilities@coords[i, ], districts.center)
    nearest <- which.min(distances)
    facilities@data[i, ]$district <- nearest
    facilities@data[i, ]$in_district <- "No"
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("South Bend Dashboard"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         radioButtons(inputId = "facility_type", 
                      label = h3("Type of Facility"), 
                      choices = list("Fire Station" = "FIRE STATION", 
                      "Library" = "LIBRARY", 
                      "Police Station" = "POLICE STATION")
                      )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("facilityMap"), 
         plotOutput("barPlot"), 
         h4(textOutput("inCityExplain"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  selected_facility <- reactive({
    facilities[facilities@data$POPL_TYPE == input$facility_type,]
    })
  
   output$facilityMap <- renderLeaflet({
     leaflet() %>% 
       addTiles() %>% 
       addPolygons(data = districts, 
                   popup = paste("<b>District ", districts@data$Num, "</b><br>", 
                                 "Council Member: ", districts@data$Council_Me), 
                   color = ~district_pal(districts@data$Num)) %>% 
       addCircleMarkers(data = selected_facility(), 
                        radius = 5, 
                        opacity = 1, 
                        fillOpacity = 1, 
                        color = ~facility_pal(selected_facility()@data$POPL_TYPE), 
                        popup = paste("<b>", selected_facility()@data$POPL_NAME, 
                                      "</b><br>", str_extract(selected_facility()@data$POPL_ADDR1, "[0-9A-z[:blank:]\\.,]*"), 
                                      "<br>", selected_facility()@data$POPL_CITY, 
                                      ", IN ", selected_facility()@data$POPL_ZIP, 
                                      "<br>District: ", 
                                      ifelse(selected_facility()@data$in_district == "Yes", 
                                             selected_facility()@data$district, 
                                             paste("Nearest to ", 
                                                   selected_facility()@data$district, 
                                                   sep = "")), 
                                      sep = ""))
   })
   
   output$barPlot <- renderPlot({
     plot_data <- selected_facility()@data %>% 
       count(district, in_district) %>% 
       complete(district = c("1", "2", "3", "4", "5", "6"), 
                in_district = c("Yes", "No"), 
                fill = list(n = 0))
     
     ggplot(plot_data, 
            aes(x = district, y = n, fill = in_district)
            ) + 
       geom_col(position = "stack") + 
       labs(x = "District", y = ifelse(input$facility_type == "FIRE STATION", 
                                       "Number of fire stations", 
                                       ifelse(input$facility_type == "LIBRARY", 
                                              "Number of libraries", 
                                              "Number of police stations")), 
            fill = "In city?") + 
       theme_classic() + 
       theme(text = element_text(size = 16), 
             legend.position = "bottom")
   })
   
   output$inCityExplain <- renderText({
     "Facilities outside the city are associated with the nearest district."
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

