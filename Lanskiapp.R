# Alison Lanski Project Contribution
# Data Viz Shiny App component 
#    http://shiny.rstudio.com/
#


###################################
## Load packages and check data

library(shiny)
library(rgdal)
library(leaflet)
library(tidyverse)
library(DT)
options(scipen = 99)

#get the data set up right

#parks
load("C:/Users/Lanski/Documents/GitHub/Group1DataVizProject/Parks_SHP.Rdata")

#districts
districts <- readOGR(dsn = "C:/Users/Lanski/Documents/GitHub/Group1DataVizProject", 
                                  layer = "City_Council_Districts", 
                                  stringsAsFactors = FALSE)
#census
census <- readOGR(dsn = "C:/Users/Lanski/Documents/GitHub/Group1DataVizProject", 
                  layer = "2010_CensusData", 
                  stringsAsFactors = FALSE)

#work to combine census with park data

#get coding from districts
common_crs <- CRS(proj4string(districts))

#redo census
census_redo <- spTransform(census, common_crs)
CRS(proj4string(census_redo))

#redo parks
parks_redo <- spTransform(parks_spatial, common_crs)
CRS(proj4string(parks_redo))

#get the census data for the park points
library(maptools)
parks_census_df <- over(parks_redo, census_redo)

#recombine with parks info
parks_census <- spCbind(parks_redo, parks_census_df)

#set up colnames to real things
census_fields <- c("State_FIPS", "County_FIPS", "Census_Tract", "Geo_ID", 
                   "Tract_Number", "Tract_Name", "MTFCC", "FUNC_Stat", 
                   "Area_Land", "Area_Water", "Full_Name", "Total_Population", 
                   "Total_Population_2", "Population_Density_per_sq_mile", 
                   "Area_Land", "Area_Total", "Area_Total_Area_Land", 
                   "Area_Total_Area_Water", "Total_Population_3", "Males", 
                   "Females", "Total_Population_4", "Age_Under_5", 
                   "Age_5_to_9", "Age_10_to_14", "Age_15_to_17", 
                   "Age_18_to_24", "Age_25_to_34", "Age_35_to_44", 
                   "Age_45_to_54", "Age_55_to_64", "Age_65_to_74", 
                   "Age_75_to_84", "Age_85_years_plus", 
                   "Total_population_5", "White", "Black_African_American", 
                   "American_Indian_Alaska_Native", "Asian", 
                   "Native_Hawaiian_Other_Pacific_Islander", 
                   "Some_Other_Race", "Two_or_More_Races", "Households", 
                   "Family_households", "Households_Married_couple_family", 
                   "Households_Other_family", 
                   "Households_Male_householder_no_wife_present", 
                   "Households_Female_householder_no_husband_present", 
                   "Households_Nonfamily_households", 
                   "Households_Householder_living_alone", 
                   "Households_Householder_not_living_alone", 
                   "Total_population_6", "Total_population_In_households", 
                   "Total_population_In_family_households", 
                   "Total_population_in_households_Householder", 
                   "Total_population_in_households_Spouse", 
                   "Total_population_in_households_Child", 
                   "Total_population_in_households_Grandchild", 
                   "Total_population_in_households_Brother_or_sister", 
                   "Total_population_in_households_Parent", 
                   "Total_population_in_households_Other_relatives", 
                   "Total_population_in_households_Nonrelatives", 
                   "Total_population_in_nonfamily_households", 
                   "Total_population_in_households_Living_alone", 
                   "Total_population_in_households_Not_living_alone", 
                   "Total_population_in_households_Nonrelatives", 
                   "Total_population_in_group_quarters", 
                   "Total_population_Institutionalized", 
                   "Total_population_Noninstitutionalized_group_quarters", 
                   "Housing_units", "Occupied_housing_units", 
                   "Occupied_housing_units_Owners", 
                   "Occupied_housing_units_Renters")

colnames(parks_census@data)[51:123] <- census_fields

#add district info
parks_districts_df <- over(parks_census, districts)


#recombine with parks info
parks_census_dist <- spCbind(parks_census, parks_districts_df)


#########################################################
### Start App

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Parks By Type"),

  # Sidebar with a input 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "amenity",
                  label = "Choose a park amenity",
                  choices = names(parks_census_dist@data[4:47]),
                  selected = 'Open_Turf'),
      selectInput(inputId = "districtnum",
                  label = "Which district?",
                  choices = sort(unique(parks_census_dist@data$Num)),
                  selected = "1")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(leafletOutput("ParkMap"), plotOutput("DistPlot"), DT::dataTableOutput("CensusTable"))
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

#pal2 <- colorFactor(palette = c("blue", "red", "yellow", "green", "orange",
#                                "gray", "forestgreen", "cornflowerblue", "darkorange"), 
#                    domain = elections@data$SUB_REGION)

#pal3 <- colorFactor(palette = c("blue", "red"), 
#                    domain = c("D", "R"))

# Define server logic required to draw a histogram
server <- function(input, output) {

  
 # parks_spatial[!is.na(parks_spatial@data[,'Open_Turf']),] 
  
  parks_subset <- reactive({
    parks_census_dist[!is.na(parks_census_dist@data[,input$amenity]),]
  })
  
  output$ParkMap <- renderLeaflet({
      leaflet(parks_subset())  %>%
      addTiles()  %>%
      addPolygons(data = districts,
                  popup = paste("District ", districts@data$Num)) %>% 
      addCircleMarkers(data = parks_subset(),
                       radius = 8,
                       color = ~pal(parks_subset()@data$Park_Type),
                       stroke = FALSE, 
                       fillOpacity = 0.7, 
                       popup = paste(parks_subset()@data$Park_Name, "<br>",
                                     parks_subset()@data$Address, "<br>", 
                                     parks_subset()@data$NAMELSAD)) %>%
     addLegend('bottomleft', 
                pal = pal, 
                values = parks_census_dist@data$Park_Type, #show the whole range
                title = 'Parks by Type: \nSouth Bend',
                opacity = 0.7) %>%
      setView(-86.2520, 41.6764, zoom = 11)
    
    
  #      addPolygons(fillColor = ~pal2(elections@data$SUB_REGION), 
  #                color = ~pal3(elections@data[,input$elect]), 
  #                weight = 2)
  })
  
  output$DistPlot <- renderPlot({
    # Render a barplot
    ggplot(
      data = parks_subset()@data,
#      data = parks_census_dist@data, #works
      aes(x = Park_Type, y = Population_Density_per_sq_mile, 
          color = (input$districtnum != Num), size = 2)
    ) + geom_point() + 
      guides(size = FALSE) + 
      theme_minimal() + 
      scale_color_manual("District", values = c("lightblue", "black", "gray"), 
                         labels = c("In District", "Out of District", "Out of City"))

    #) + geom_histogram(stat = "count")
    # plot(x = parks_census_dist@data$Lat, 
    #      y = parks_census_dist@data$SE_T002_02, 
    #         main=input$amenity,
    #         ylab="Number of Telephones",
    #         xlab="Year")
  })
  
  output$CensusTable <- DT::renderDataTable({
    DT::datatable(parks_subset()@data[, c(1,2,56, 62, 86:91)], fillContainer = TRUE)})
}


# Run the application 
shinyApp(ui = ui, server = server)