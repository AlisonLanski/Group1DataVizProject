#
# Group 1 East
# Data Visualization Final Project
# Shiny App Code
# 
# Alison Lanski, Ken Nagle, Marisa Roman, Russ Thomas
#
#

#############################
#Load Packages
#############################

library(shiny)
library(rgdal)
library(leaflet)
library(tidyverse)
library(rgeos)
library(maptools)
library(stringr)
library(geosphere) # Ken
library(scales)    # Ken
library(DT)        # Marisa & Alison
library(scales) # Russ for pretty_breaks() in ggplot

options(scipen = 99) #turn off scientific notation

##############################################
# Data processing (geocoding done ahead of time and saved locally)
###############################################

#KEN

# Load districts data
districts <- readOGR(dsn = "City_Council_Districts", 
                     layer = "City_Council_Districts", 
                     stringsAsFactors = FALSE)

# Load facilities data
load("Public_Facilities_SHP.Rdata", envir=.GlobalEnv)

#get the geo encoding from districts
common_crs <- CRS(proj4string(districts))

#apply coding to parks
facilities <- spTransform(public_facilities_spatial, common_crs)

# Determine which facilities are in which districts
ov <- over(facilities, districts)

# Store the district in the facilities@data data frame
facilities@data$district <- ov$Num

# Create a variable indicating whether the facility is in a district,
# defaulting to "Yes"
facilities@data$in_district <- "Yes"

# Calculate the centroid of each district
districts.center <- SpatialPointsDataFrame(gCentroid(districts, byid = TRUE), 
                                           districts@data, match.ID = FALSE)

# Loop through the facilities
for (i in 1:nrow(facilities@data)) {
  # Check if the facility is not in a district
  if(is.na(facilities@data[i, ]$district)) {
    # Calculate the distance from the facility to the centroid of each
    # district
    distances <- distHaversine(facilities@coords[i, ], districts.center)
    # Find the nearest centroid
    nearest <- which.min(distances)
    # Set the district to the one with the nearest centroid
    facilities@data[i, ]$district <- nearest
    # Indicate that the facility is not in a district
    facilities@data[i, ]$in_district <- "No"
  }
}

# Create a variable indicating the type of facility and whether it is in a
# district
facilities@data$marker <- paste(facilities@data$POPL_TYPE, 
                                facilities@data$in_district, 
                                sep = " - ")

# Create color palette for markers for facilities
facility_pal <- colorFactor(palette = c("tomato", "firebrick", "salmon", 
                                        "salmon3", "royalblue", "navy"), 
                            domain = c("FIRE STATION - No", 
                                       "FIRE STATION - Yes", 
                                       "LIBRARY - No", 
                                       "LIBRARY - Yes", 
                                       "POLICE STATION - No", 
                                       "POLICE STATION - Yes"))

#KEN end

#ALISON
load("Parks_SHP.Rdata", envir=.GlobalEnv)

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

#add a nice looking districts column for later
parks_census_dist@data <- parks_census_dist@data %>%
  mutate(District = Num) %>% dplyr::select(Park_Name, Park_Type, District, everything())

pal_districts <- colorFactor(palette = c("#999999", "#4d4d4d", "#808080", "#bfbfbf", 
                                         "#666666", "#b3b3b3"), 
                             domain = c("1", "2" ,"3", "4", "5", "6"))

pal_districts_TF <- colorFactor(palette = c("magenta", "mediumblue"),
                                domain = c("TRUE", "FALSE"))


#ALISON end

#MARISA

# Create a popup column to be used as the tooltip for each district polygon
districts$popup = paste("<b>District ",districts$Num,"</b><br>",
                        "Council Member: ",districts$Council_Me,sep ="")

# Read in the abandoned properties data
prop = readOGR(dsn = "Abandoned_Property_Parcels", layer = "Abandoned_Property_Parcels")

# Retain prop records with non-NA Council_Di and non-NA Property_S (status); 
# we use the district and status on all components of the map, so all components
# must have these two values
prop@data = filter(prop@data, !is.na(prop@data$Council_Di) & !is.na(prop@data$Outcome_St))

# Create a Full_Address column to be used in the property popup and datatable
prop$Full_Address = paste(prop$Address_Nu,prop$Street_Nam,"South Bend, IN",prop$Zip_Code,sep = " ")

# Create a popup column to be used as the tooltip for each property marker
prop$popup = paste("<b>","Status: ",prop$Property_S,"</b><br>","<br>",
                   prop$Full_Address)

# Rename the Outcome_St and Code_Enfor columns so they're not cut off in the datatable
prop$Outcome = prop$Outcome_St
prop$Code_Enforcement = prop$Code_Enfor

#MARISA end

#RUSS

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

#RUSS end


#######################################################
#####  Set up the page
#######################################################

# Define UI for application that draws a histogram
ui <- navbarPage(title = "District Dashboard",

                 #Ken's Section
                 tabPanel("Public Facilities",
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
                
                #Alison's Section
                tabPanel("Parks",
                         sidebarLayout(
                           sidebarPanel(
                             selectInput(inputId = "districtnum",
                                         label = "Choose a district",
                                         choices = sort(unique(parks_census_dist@data$Num)),
                                         selected = "1"),
                             selectInput(inputId = "amenity",
                                         label = "Choose a park amenity",
                                         choices = names(parks_census_dist@data[5:48]),
                                         selected = 'Open_Turf')
                             ),
                           
                          
                           mainPanel(h4(textOutput("ParkMapTitle")),
                                     leafletOutput("ParkMap"),
                                     h4(textOutput("PopDensPlotTitle")),
                                     plotOutput("PopDensPlot"),
                                     h4(textOutput("CensusTableTitle")),
                                     DT::dataTableOutput("CensusTable"))
                         )
                ),
                
                # Marisa's Section
                tabPanel("Abandoned Properties",
                         sidebarLayout(
                           sidebarPanel(
                             # Pre-select the In Compliance option since it has the most data points
                             radioButtons(inputId = "propertyStatus", 
                                          label = "Choose a property status",
                                          choices = sort(unique(prop@data$Property_S)),
                                          selected = "In Compliance: Outcome Complete")
                           ),
                           
                           mainPanel(
                             # Create a tab panel on this tab so that tab components aren't stacked
                             # and unnecessarily grouped together
                             tabsetPanel(
                               # Map tab:
                               # Show a plot of the abandoned properties on the district map
                               # Show a plot of abandoned properties by district beneath the map
                               tabPanel("Map", 
                                        leafletOutput("abandonedPropertiesPlot"),
                                        plotOutput("abandonedPropertiesByDistrictPlot")),
                               # Table tab:
                               # Show a datatable of the abandoned properties matching the filtered status
                               tabPanel("Table", fluidRow(DT::dataTableOutput("abandonedPropertiesTable")))
                             )
                           )
                         )
                ),
                
                #Russ' section
                tabPanel("Schools",
                         sidebarLayout(
                           sidebarPanel(
                             radioButtons(inputId = "school_type", 
                                          label = h3("Type of School"), 
                                          choices = list("Public" = "Public", 
                                                         "Private" = "Private")
                             )
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             leafletOutput("schoolMap"), 
                             plotOutput("barPlotschool")
                           )
                         ) 
                )
   

)#end navbarPage

###########################################################
## Server function
############################################################

server <- function(input, output) {

  ######
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

#KEN end
  ######################################
#ALISON
  
  output$ParkMapTitle <- renderText({
    "Parks with the selected amenity"
  })
  
  parks_subset <- reactive({
    parks_census_dist[!is.na(parks_census_dist@data[,input$amenity]),]
  })
  
  output$ParkMap <- renderLeaflet({
    leaflet(parks_subset())  %>%
      addTiles()  %>%
      #districts
      addPolygons(data = districts,
                  popup = paste("<b>District ", districts@data$Num, "</b><br>", 
                                "Council Member: ", districts@data$Council_Me),
                  fillColor = ~pal_districts(districts@data$Num), #polygon fill
                  color = "black", #stroke color
                  stroke = 1,  #stroke width
                  fillOpacity = .6) %>% 
      #parks
      addCircleMarkers(data = parks_subset(),
                       radius = 8,
                       color = ~pal_districts_TF(as.character(input$districtnum != parks_subset()@data$Num | 
                                                                is.na(parks_subset()@data$Num))),
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
      guides(size = FALSE, color = FALSE) + #don't show the size legend
      theme_minimal() + 
      theme(text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 14)) +
      scale_color_manual("District", values = c("TRUE" = "mediumblue", "FALSE" = "magenta"), 
                         labels = c("FALSE" = "In District", "TRUE" = "Out of District")) +
      xlab("\nPark Type") +
      ylab("Population density per sq. mile \n")
  })
  
  
  output$CensusTableTitle <- renderText({
    "Demographics and population near those parks"
  })
  
  
  output$CensusTable <- DT::renderDataTable({
    DT::datatable(parks_subset()@data[, c(1:3, 57, 63, 87:93)], #pick the right columns
                  
                  options = list(pageLength = 5, scrollX = T)) %>%
      formatStyle(
        'District',
        target = 'row',
        backgroundColor = styleEqual(input$districtnum, '#ffe6ff')) #highlight selected district
  })
#ALISON end
  ###########################################
#MARISA
  
  # Properties are filtered by status, so create a dataframe that is the subset
  # of abandoned properties with a status matching the filter selection
  selected_prop = reactive({
    prop[prop$Property_S %in% input$propertyStatus,]
  })
  
  # Derive the centroid of each property polygon since we want to display points
  # for properties instead of polygons
  prop_center = reactive({
    SpatialPointsDataFrame(gCentroid(selected_prop(), byid=TRUE), 
                           selected_prop()@data, match.ID=FALSE)
  })
  
  # abandonedPropertiesPlot is the plot of property markers overlaid on the 
  # district map
  output$abandonedPropertiesPlot <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = districts, 
                  popup = ~popup, 
                  fillColor = ~pal_districts(districts@data$Num), #polygon fill
                  color = "black", #stroke color
                  stroke = 1,  #stroke width
                  fillOpacity = .6) %>% 
      addCircleMarkers(data = prop_center(), popup = ~popup, radius = 5, opacity = 1,fillOpacity = 1)
  })
  
  # abandonedPropertiesbyDistrictPlot is the bar chart of the count of properties
  # by district
  output$abandonedPropertiesByDistrictPlot <- renderPlot({
    ggplot(data = prop_center()@data,
           aes(x = Council_Di)) + geom_bar(stat="count") +
      theme_minimal() + 
      theme(text = element_text(size = 15),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 14)) +
      xlab("District") +
      ylab("Count of Abandoned Properties")
  })
  
  # Datatable of the abandonedProperties filtered by the selected status
  output$abandonedPropertiesTable <- DT::renderDataTable({
    DT::datatable(prop_center()@data[,c("Outcome","Code_Enforcement","Full_Address")],
                  options = list(pageLength = 10, scrollX = T))
  })
#MARISA end
  #############################################
#RUSS
  selected_schooltype <- reactive({
    schools.center[schools.center@data$SchoolType == input$school_type,]
  })
  
  output$schoolMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
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
             complete(district = c("1", "2", "3", "4", "5", "6"), fill = list(n = 0)), aes(x = district, y = n) ) + 
      geom_col(fill = ifelse(input$school_type == "Private",
                             "firebrick", "salmon3")) +
      labs(x = "District", y = ifelse(input$school_type == "Private",
                                      "Number of Private Schools", "Number of Public Schools")) + 
      theme_classic() + 
      theme(text = element_text(size = 16)) +
      scale_y_continuous(breaks=pretty_breaks()) # need to add library(scales) for pretty_breaks
  })
#RUSS end
}

#####################################
# Run the application 
#####################################
shinyApp(ui = ui, server = server)

