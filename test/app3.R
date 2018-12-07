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
library(geosphere) # Ken
library(DT)        # Marisa

options(scipen = 99)

# Data processing

#KEN

districts <- readOGR(dsn = "City_Council_Districts", 
                     layer = "City_Council_Districts", 
                     stringsAsFactors = FALSE)

load("Public_Facilities_SHP.RData")

#get the geo encoding from districts
common_crs <- CRS(proj4string(districts))


#apply coding to parks
facilities <- spTransform(public_facilities_spatial, common_crs)
CRS(proj4string(facilities))


district_pal <- colorFactor(palette = c("red", "orange", "yellow", "green", 
                                        "blue", "violet"), 
                            domain = c("1", "2", "3", "4", "5", "6")
)

facility_pal <- colorFactor(palette = c("firebrick", "salmon3", "navy"), 
                            domain = c("FIRE STATION", "LIBRARY", 
                                       "POLICE STATION"))

ov <- over(facilities, districts)
facilities@data$district <- ov$Num
#KEN

#ALISON
load("Parks_SHP.RData")

census <- readOGR(dsn = "2010_CensusData", 
                  layer = "2010_CensusData", 
                  stringsAsFactors = FALSE)

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
                   "Area_Land", "Area_Water", "Full_Name", "Total_Population_1", 
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

#ALISON

#MARISA
districts$popup = paste("<b>District ",districts$Dist,"</b><br>",
                        "Council Member: ",districts$Council_Me,sep ="")

prop = readOGR(dsn = "Abandoned_Property_Parcels", layer = "Abandoned_Property_Parcels")
prop$popup = paste("<b>",prop$Property_S,"</b><br>",
                   prop$Address_Nu,prop$Street_Nam,sep = " ")
#MARISA

#RUSS
schools <-  readOGR(dsn="School_Boundaries",
                    "School_Boundaries",
                    stringsAsFactors = FALSE)

schools.center <- SpatialPointsDataFrame(gCentroid(schools, byid=TRUE), 
                                         schools@data, match.ID=FALSE)
#RUSS

# Define UI for application that draws a histogram
ui <- navbarPage(title = "East Group 1 Final Project -TEST",
                tabPanel("Ken",
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
                              plotOutput("barPlot")
                           )
                         )
                ),
                
                tabPanel("Alison",
                         sidebarLayout(
                           sidebarPanel(),
                           mainPanel()
                         )
                ),
                
                tabPanel("Marisa",
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput(inputId = "propertyStatus", 
                                                label = "Choose one or more property status options",
                                                choices = unique(prop@data$Property_S),
                                                selected = c("Emergency Demo"))
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             leafletOutput("abandonedPropertiesPlot"),
                             fluidRow(DT::dataTableOutput("abandonedPropertiesTable"))
                           )
                         )
                ),
                
                tabPanel("Russ",
                         sidebarLayout(
                           
                           # Sidebar with a slider input
                           sidebarPanel(
                             radioButtons(inputId = "school.type", label= "School Type",choices = c(unique(schools@data$SchoolType),"All"), selected = "All"),
                             uiOutput(outputId="sub.school.list")
                           ),
                           
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             tabsetPanel(type = "tabs", 
                                         tabPanel("Map", leafletOutput("map"))
                             )
                           )
                         )
                )
   

)#end navbarPage

# Define server logic required to draw a histogram
server <- function(input, output) {
#KEN
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
                                     sep = ""))
  })
  
  output$barPlot <- renderPlot({
    ggplot(selected_facility()@data %>% 
             count(district) %>% 
             complete(district = c("1", "2", "3", "4", "5", "6", NA), fill = list(n = 0)), 
           aes(x = district, y = n)
    ) + 
      geom_col(fill = ifelse(input$facility_type == "FIRE STATION", 
                             "firebrick", 
                             ifelse(input$facility_type == "LIBRARY", 
                                    "salmon3", 
                                    "navy"))) + 
      labs(x = "District", y = ifelse(input$facility_type == "FIRE STATION", 
                                      "Number of fire stations", 
                                      ifelse(input$facility_type == "LIBRARY", 
                                             "Number of libraries", 
                                             "Number of police stations"))) + 
      theme_classic() + 
      theme(text = element_text(size = 16))
  })
#KEN
  
#MARISA
  selected_prop = reactive({
    prop[prop$Property_S %in% input$propertyStatus,]
  })
  
  prop_center = reactive({
    SpatialPointsDataFrame(gCentroid(selected_prop(), byid=TRUE), 
                           selected_prop()@data, match.ID=FALSE)
  })
  
  pal = colorFactor(palette = 'Set1', domain =districts$Dist)
  
  output$abandonedPropertiesPlot <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = districts, popup = ~popup, color = ~pal(Dist)) %>% 
      addCircleMarkers(data = prop_center(), popup = ~popup, radius = 5, opacity = 1,fillOpacity = 1)
  })
  
  output$abandonedPropertiesTable <- DT::renderDataTable({
    DT::datatable(prop_center()@data)
  })
#MARISA
  
#RUSS
  output$sub.school.list <- renderUI({
    print(input$school.type)
    
    if(input$school.type == "All"){
      school.list <- schools@data[,1]
    }else{
      school.list <- schools@data[schools@data$SchoolType == input$school.type,1]
    }
    # checkboxGroupInput(
    #   inputId = "School",
    #   label = "School",
    #   choices = school.list)
  })
  
  bound.subset <- reactive({
    schools[schools@data$School == input$School,]
  })
  
  output$map <-  renderLeaflet({
    leaflet(bound.subset())%>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addPolygons(data = districts)%>%
      addCircleMarkers(data = schools.center, popup = ~School, radius = 5, opacity = 1, fillOpacity = 1)
  })
#RUSS

}

# Run the application 
shinyApp(ui = ui, server = server)

