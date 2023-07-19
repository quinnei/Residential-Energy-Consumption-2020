################################################################################
#################### I. Installation and loading of the dataset ################
################################################################################

pacman::p_load(tidyverse, # for data wrangling
               shiny, shinydashboard, shinythemes, bslib, rsconnect, # for shiny app
               sf, tigris, tmap) # for spatial mapping




##### I. Create a user defined function that wrangles the data -----------------
merge_the_data <- function(file_name) {
  
### 1) Read in the RECS summary data
# Define the path directory
  directory <-
    "https://raw.githubusercontent.com/quinnei/Residential-Energy-Consumption-2020/main/RECS-Shiny-App/1_Dataset/"
# Specify the location of the csv file
  file_path <- paste0(directory, file_name, ".csv")
# Read the csv dataset
  RECS <- read.csv(file_path)
  
### 2) Read in the geospatial data of the US, using the tigris package
  US <- states(cb = TRUE, year = 2020) %>%
# Position Alaska & Hawaii below the continental US
    shift_geometry(position = "below") %>%
# For a conic representation of the U.S.
    sf::st_transform("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96")
# Capitalize the state names
  US$NAME <- toupper(US$NAME)
  
### 3) Join the 2 datasets, by the state variable
  merged_data <- left_join(RECS, US, by = c("STATE" = "NAME"))
# Make sure that the merged dataframe is recognized as a spatial object
  st_geometry(merged_data) <- merged_data$geometry
  
  
  return(merged_data)
}



##### II. Call the function and store the merged, spatial dataset --------------
energy_insecurity <- merge_the_data("energy_insecurity")
energy_consumption <- merge_the_data("energy_consumption")



##### III. Define a function that plots an interactive map --------------------

create_map <- function(data, colored_variable, legend_title, popup_labels) {
  tm_shape(data) +
    tm_borders(col = 'black', alpha = 0.5) +
    tm_fill(col = colored_variable, title = legend_title,
            popup.vars = popup_labels, # Add popup with relevant info
            id = "STATE", # The first piece of info to display in the popup
            alpha = 0.85) + # Set the transparency of the colors
    tm_layout(frame = FALSE, # Remove the black frame that surrounds the map
              legend.outside = TRUE, # Move the legend 
              legend.title.size = 1, legend.text.size = 0.7) +
# Plot the interactive map in the original CRS
    tm_view(projection = 0) + tm_basemap(NULL)
}



##### IV. Create an interactive map of 
##### 1) energy insecurity 2) Energy Use Intensity (EUI) -----------------------
map_energy_insecurity <- create_map(
  energy_insecurity, 'PERCENTAGE', "Energy Insecure Households (%)",
# Indicate % and absolute values of energy insecurity & total number of households in each state
  c("PERCENTAGE OF ENERGY INSECURE HOUSEHOLDS (%)" = "PERCENTAGE",
    "NUMBER OF ENERGY INSECURE HOUSEHOLDS" = "NUM_ENERGY_INSECURE_HOUSEHOLDS",
    "TOTAL NUMBER OF HOUSEHOLDS" = "TOTAL_HOUSEHOLDS"))


map_energy_consumption <- create_map(
  energy_consumption, 'ENERGY_CONSUMPTION_PER_SQFT', "Energy Use Intensity (Btu/square ft)",
# For each state, indicate EUI, total average expenditures on energy, and climate region
  c("EUI (BTU/SQFT)" = "ENERGY_CONSUMPTION_PER_SQFT",
    "EXPENDITURES ON ENERGY ($)" = "ENERGY_EXPENDITURE_PER_SQFT",
    "CLIMATE" = "CLIMATE"))



################################################################################
#################### II. Develop a Shiny Dashboard #############################
################################################################################


################################################################################
### II-a. Create a user interface that defines the visual elements of the app ##
################################################################################ 

ui <- fluidPage(
  
##### 1) Apply a minimalist theme ----------------------------------------------
  theme = bs_theme(bootswatch = "lux"),
  
##### 2) Create a navigation bar at the top of the page ------------------------  
  navbarPage(
# 2-1) Specify the title of the web page   
    title = "A STATE-LEVEL SUMMARY OF RESIDENTIAL ENERGY CONSUMPTION IN 2020",
    
    
# 2-2) Create 2 main tabs ------------------------------------------------------
    
# 2-2-a) title of tab 1
    tabPanel("ENERGY INSECURITY",
# 2-2-b) title of the landing page
             h4("HOUSEHOLDS THAT STRUGGLE TO PAY ENERGY BILLS OR LIVE IN POTENTIALLY DANGEROUS THERMAL CONDITIONS"),
# 2-2-c) Define the size of the map
             tmapOutput("energy_insecurity_map", width = "100%", height = "865px")
    ),
    
    
# 2-2-a') title of tab 2
    tabPanel("ENERGY USE INTENSITY (EUI)",
# 2-2-b') title of the landing page
             h4("ANNUAL HOUSEHOLD ENERGY CONSUMPTION (PER SQUARE FOOTAGE)"),
# 2-2-c') Define the size of the map
             tmapOutput("energy_consumption_map", width = "100%", height = "865px")
    )
  )
)



################################################################################
###### II-b. Create a server, which consists of an 1) input and 2) output ###### 
################################################################################ 


# Tells the server how to generate outputs from the inputs provided ------------
server <- function(input, output) {
  
##### 1) Generate the 'energy_insecurity_map' output ---------------------------
  output$energy_insecurity_map <- renderTmap({
# 1-a) Turn on interactive feature of the map
    tmap_mode("view")
# 1-b) Return the output that was defined in Part I.
    map_energy_insecurity
  })
  
  
  
##### 2) Generate the 'energy_consumption_map' output --------------------------
  output$energy_consumption_map <- renderTmap({
# 2-a) Turn on interactive feature of the map
    tmap_mode("view")
# 2-b) Return the output that was defined in Part I.
    map_energy_consumption
  })
  
}



################################################################################
########################### II-c. Run the Shiny app ############################
################################################################################ 

shinyApp(ui = ui, server = server)