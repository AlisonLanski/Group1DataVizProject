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
library(scales)    # Ken
library(DT)        # Marisa
library(scales) # Russ for pretty_breaks() in ggplot

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

facilities@data$marker <- paste(facilities@data$POPL_TYPE, 
                                facilities@data$in_district, 
                                sep = " - ")

facility_pal <- colorFactor(palette = c("tomato", "firebrick", "salmon", 
                                        "salmon3", "royalblue", "navy"), 
                            domain = c("FIRE STATION - No", 
                                       "FIRE STATION - Yes", 
                                       "LIBRARY - No", 
                                       "LIBRARY - Yes", 
                                       "POLICE STATION - No", 
                                       "POLICE STATION - Yes"))

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

pal_districts <- colorFactor(palette = c("#999999", "#4d4d4d", "#808080", "#bfbfbf", 
                                         "#666666", "#b3b3b3"), 
                             domain = c("1", "2" ,"3", "4", "5", "6"))

#ALISON

#MARISA
districts$popup = paste("<b>District ",districts$Dist,"</b><br>",
                        "Council Member: ",districts$Council_Me,sep ="")

prop = readOGR(dsn = "Abandoned_Property_Parcels", layer = "Abandoned_Property_Parcels")
prop$popup = paste("<b>",prop$Property_S,"</b><br>",
                   prop$Address_Nu,prop$Street_Nam,sep = " ")
#MARISA

#RUSS
# Old Code
# schools <-  readOGR(dsn="School_Boundaries",
#                     "School_Boundaries",
#                     stringsAsFactors = FALSE)
# 
# schools.center <- SpatialPointsDataFrame(gCentroid(schools, byid=TRUE), 
#                                          schools@data, match.ID=FALSE)
school_pal <- colorFactor(palette = c("firebrick", "salmon3"), 
                          domain = c("Public", "Private"))

# Read in Schools data
schools <-  readOGR(dsn="School_Boundaries",
                    "School_Boundaries",
                    stringsAsFactors = FALSE)

# Get School points
schools.center <- SpatialPointsDataFrame(gCentroid(schools,
                                                   byid=TRUE),
                                         schools@data, match.ID=FALSE)
# combine school and district info
ov_schools <- over(schools.center, districts)
ov_schools2 <- spCbind(schools.center, ov_schools)

# add district column and info to Schools data
schools.center@data$district <- ov_schools2$Num

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
                              plotOutput("barPlot"), 
                              h4(textOutput("inCityExplain"))
                           )
                         )
                ),
                
                tabPanel("Alison",
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
                  fillColor = ~pal_districts(districts@data$Num), #polygon fill
                  color = "black", #stroke color
                  stroke = 1,  #stroke width
                  fillOpacity = .6) %>% 
      addCircleMarkers(data = selected_facility(), 
                       radius = 5, 
                       opacity = 1, 
                       fillOpacity = 1, 
                       color = ~facility_pal(selected_facility()@data$marker), 
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
    
    plot_colors <- c()
    
    if (input$facility_type == "FIRE STATION") {
      plot_colors <- c("tomato", "firebrick")
    } else if (input$facility_type == "LIBRARY") {
      plot_colors <- c("salmon", "salmon3")
    } else {
      plot_colors <- c("royalblue", "navy")
    }
    
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
            legend.position = "bottom") + 
      scale_fill_manual(values = c(plot_colors)) + 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
  })
  
  output$inCityExplain <- renderText({
    "Facilities outside the city are associated with the nearest district."
  })

#KEN
  
#ALISON
  
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
#ALISON
  
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
  selected_schooltype <- reactive({
    schools.center[schools.center@data$SchoolType == input$school_type,]
  })
  
  output$schoolMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # addPolygons(data = districts,
      #             popup = paste("<b>District ", districts@data$Num, "</b><br>",
      #                           "Council Member: ", districts@data$Council_Me),
      #             color = ~district_pal(districts@data$Num)) %>%
      addPolygons(data = districts,
                  popup = paste("<b>District ", districts@data$Num, "</b><br>", 
                                "Council Member: ", districts@data$Council_Me),
                  fillColor = ~pal_districts(districts@data$Num), #polygon fill
                  color = "black", #stroke color
                  stroke = 1,  #stroke width
                  fillOpacity = .6) %>% 
      
      addCircleMarkers(data = selected_schooltype(),
                       radius = 5,
                       opacity = 1,
                       fillOpacity = 1,
                       color = ~school_pal(selected_schooltype()@data$SchoolType),
                       popup = paste("<b>", selected_schooltype()@data$School,
                                     "</b><br>",
                                     sep = "")
      )
  })
  
  output$barPlotschool <- renderPlot({
    ggplot(selected_schooltype()@data %>% 
             count(district) %>% 
             complete(district = c("1", "2", "3", "4", "5", "6", NA), fill = list(n = 0)), aes(x = district, y = n) ) + 
      geom_col(fill = ifelse(input$school_type == "Private",
                             "firebrick", "salmon3")) +
      labs(x = "District", y = ifelse(input$school_type == "Private",
                                      "Number of Private Schools", "Number of Public Schools")) + 
      theme_classic() + 
      theme(text = element_text(size = 16)) +
      scale_y_continuous(breaks=pretty_breaks()) # need to add library(scales) for pretty_breaks
    # scale_y_continuous(breaks=c(1:10))
  })
#RUSS
}


# Run the application 
shinyApp(ui = ui, server = server)

