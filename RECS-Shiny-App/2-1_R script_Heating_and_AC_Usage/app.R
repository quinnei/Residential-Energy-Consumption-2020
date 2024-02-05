################################################################################
#################### I. Installation and loading of the dataset ################
################################################################################

pacman::p_load(tidyverse,
               shinythemes, bslib,
               sf, tigris, leaflet, leaflet.extras,
               viridisLite, RColorBrewer, plotly)



### 1. Read the geodata
# Position Alaska & Hawaii underneath the continental US
US <- states(cb = TRUE, year = 2020) %>% shift_geometry(position = "below") %>% sf::st_transform("+proj=longlat +datum=WGS84")
US$NAME <- toupper(US$NAME)

### 2. Define a function that reads in and merges datasets
get_spatial_data <- function(file_name, variable = NULL, new_value = NULL) {
  ### 1) Read the csv dataset
  # Define the path directory
  directory <- "https://raw.githubusercontent.com/quinnei/Residential-Energy-Consumption-2020/main/RECS-Shiny-App/1_Dataset/"
  # Specify the path where the csv file is located
  file_path <- paste0(directory, "LONG_", file_name, ".csv")
  # Read the CSV data
  csv_data <- read.csv(file_path)
  
  ### 2) Create a geo-spatial dataset
  merged_data <- left_join(csv_data, US, by = c("STATE" = "NAME"))
  ### 3) Make sure that the merged df is recognized as a spatial object
  st_geometry(merged_data) <- merged_data$geometry
  
  ### 4) Modify the value labels for 'OTHER' (*optional*)
  if (!is.null(variable) && !is.null(new_value)) {
    merged_data[[variable]][merged_data[[variable]] == "OTHER"] <- new_value
  }
  
  return(merged_data)
}

### 3. Save as dataframes
heater_type <- get_spatial_data("heater_type", "HEATING_EQUIPMENT_TYPE", "OTHER TYPES OF HEATING EQUIPMENT")
heater_fuel <- get_spatial_data("heater_fuel", "HEATER_FUEL_TYPE", "OTHER TYPES OF FUEL")
AC_type <- get_spatial_data("AC_type")




################################################################################
#################### II. DEFINE FUNCTIONS FOR DATA VISUALIZATION ###############
################################################################################

### 1. Define a function that creates interactive bar plots
create_bar_plot <- function(number, data, type, title_txt, palette_colors, palette_style) {
  renderPlotly({
    # For reproducibility of colors  
    set.seed(number)
    
    
    ggplot_object <- ggplot(data, aes(y = PERCENTAGE, x = STATE, fill = reorder(type, PERCENTAGE),
                                      text = paste("TYPE:", type))) +
      geom_bar(stat = "identity", color = "black") +
      labs(title = title_txt, y = "PERCENTAGE OF U.S. HOUSEHOLDS (%)", fill = "") +
      scale_fill_manual(values = sample(brewer.pal(palette_colors, palette_style))) + 
      theme(
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold", size = 10),
        legend.title = element_text(face = "bold")) +
      coord_flip()
    
    # Convert to an interactive plot & assign names to the pop-up labels  
    ggplotly(ggplot_object, tooltip = c("STATE", "PERCENTAGE", "text"))
  })
}



### 2. Define a function that subsets the data according to fuel/equipment types
filter_the_data <- function(data, variable, input_value) {
  reactive({
    data %>% filter({{variable}} == {{input_value}})})
}



### 3. Define a function that defines the color palette of the spatial map
define_palette <- function(variable) {
  reactive({
    # Show in shaded colors, the % of households in each state    
    colorNumeric(palette = rev(viridis(50)), domain = variable()$PERCENTAGE)
  })
}



### 4. Define a function that plots an interactive map, using the filtered dataset
create_map <- function(filtered_data, gradation, variable, condition) {
  leaflet() %>%
    setView(lng = -115.5795, lat = 33.8282, zoom = 3.9) %>%
    addPolygons(
      data = filtered_data(),
      fillColor = ~gradation()(PERCENTAGE),
      fillOpacity = 1,
      color = "black",
      weight = 2,
      popupOptions = popupOptions(maxWidth = "550"),
# Display the 1) state name 2) climate 3) % values 4) absolute values 5) number of households
      popup = paste(
        "<div style='max-width: 550px;'>", # Set the maximum width for the text inside the pop-up
        "<b>STATE :</b>", filtered_data()$STATE, "<br>",
        "<b>CLIMATE :</b>", filtered_data()$CLIMATE, "<br>",
# Modify the labels based on the drop down menu that was selected 
        ifelse(variable == condition, paste("<b>HOUSEHOLDS THAT", variable, ":</b>"),
               paste("<b>USAGE OF", variable, ":</b>")),
        filtered_data()$PERCENTAGE, "%", "<br>",
# Add commas to the digits            
        "<b>CORRESPONDING NUMBER OF HOUSEHOLDS :</b>", format(filtered_data()$NUM_HOUSEHOLDS, big.mark = ",", scientific = FALSE), "<br>",
        "<b>TOTAL NUMBER OF HOUSEHOLDS :</b>", format(filtered_data()$TOTAL_HOUSEHOLDS, big.mark = ",", scientific = FALSE))) %>% 
    addLegend(
      pal = gradation(),
      values = filtered_data()$PERCENTAGE,
      title = ifelse(variable == condition, paste("HOUSEHOLDS THAT", variable, "(%)"),
                     paste("HOUSEHOLDS THAT USE", variable, "(%)")),
      opacity = 1,
      position = "topleft") %>%
    setMapWidgetStyle(list(background = "white"))
}




################################################################################
#################### III. Build a shiny app ####################################
################################################################################

###### 1. Create a UI (how the app will look like)
ui <- fluidPage(
### Apply a minimalistic theme
  theme = bs_theme(bootswatch = "zephyr"),
  
###### 2. Create tabs
  navbarPage("A STATE-LEVEL OVERVIEW OF HOUSEHOLD ENERGY CONSUMPTION", # Title of the webpage
### 2-a. Create the upper-most tab 1
      navbarMenu("HEATER USAGE",
### 2-b. Create tab 1.1           
          tabPanel("1.1 HEATING EQUIPMENT",
              tabsetPanel(

################################################################################ 
##### 4. DEFINE OUTPUT FOR Tab 1.1. BARPLOT ####################################
################################################################################                   
                  
                  tabPanel("BAR PLOT",
                        mainPanel(br(),
                            plotlyOutput(outputId = 'barplot_HEATER', height = "780px", width = "1300px"))
                           ),

################################################################################ 
##### 6. DEFINE OUTPUT FOR Tab 1.1. MAP ########################################
################################################################################ 
                  tabPanel("INTERACTIVE MAP", br(), # Add a line break
                           
################################################################################
##### 6-a) Create a drop-down menu for Tab 1.1 #################################
################################################################################                           
                        sidebarLayout(
                            sidebarPanel(
                               width = 12,
                               selectInput(
                                 inputId = 'HEATING_EQUIPMENT_TYPE',  # 5-a) variable that will be used
                                 label = 'SELECT HEATING EQUIPMENT TYPE :', # 5-b) text to display
                                 choices = c( # 5-c) options to choose from, in that order
                                   'STEAM OR HOT WATER SYSTEM WITH RADIATORS OR PIPES', 'CENTRAL FURNACE', 'CENTRAL HEAT PUMP',
                                   'BUILT-IN ELECTRIC UNITS IN WALLS/CEILINGS/BASEBOARDS/FLOORS', 'BUILT-IN ROOM HEATER BURNING GAS OR OIL', 'WOOD OR PELLET STOVE',
                                   'PORTABLE ELECTRIC HEATER', 'MINISPLIT', 'OTHER TYPES OF HEATING EQUIPMENT', 'DO NOT USE HEATER')
                               )
                             ),
                             mainPanel(br(), br(),
                               leafletOutput(outputId = 'map_HEATER', height = "600px", width = "1600px")
                             )
                           )
                  )
               )
      ),
             
### 2-c. Create tab 1.2           
          tabPanel("1.2 FUEL USED FOR HEATING",
              tabsetPanel(
                
################################################################################ 
##### 4'. DEFINE OUTPUT FOR Tab 1.2. BARPLOT ###################################
################################################################################                 
                  tabPanel("BAR PLOT",
                        mainPanel(br(),
                            plotlyOutput(outputId = 'barplot_HEATER_FUEL', height = "780px", width = "1200px"))
                           ),


################################################################################ 
##### 6'. DEFINE OUTPUT FOR Tab 1.2. MAP #######################################
################################################################################ 
                  tabPanel("INTERACTIVE MAP", br(), # Add a line break
                           
################################################################################
##### 6'-a) Create a drop-down menu for Tab 1.2 ################################
################################################################################                           
                        sidebarLayout(
                            sidebarPanel(
                               width = 12,
                               selectInput(
                                 inputId = 'HEATER_FUEL_TYPE',  # 5-a) variable that will be used
                                 label = 'SELECT HEATER FUEL TYPE :', # 5-b) text to display
                                 choices = c( # 5-c) options to choose from, in that order
                                   'NATURAL GAS', 'ELECTRICITY', 'FUEL OIL', 'PROPANE',
                                   'WOOD OR PELLETS', 'OTHER TYPES OF FUEL', 'DO NOT USE HEATER')
                                 
                               )
                             ),
                             mainPanel(br(), br(),
                               leafletOutput(outputId = 'map_FUEL', height = "600px", width = "1600px")
                                       )
                             )
                           )
                  )
               )
      ),
             
### 2-a'. Create the upper-most tab 2
      navbarMenu("A/C USAGE",
             
### 2-b'. Create tab 2.1           
          tabPanel("2.1 A/C EQUIPMENT",
               tabsetPanel(
                  
################################################################################ 
##### 4''. DEFINE OUTPUT FOR Tab 2.1. BARPLOT ##################################
################################################################################                  
                  tabPanel("BAR PLOT",
                        mainPanel(br(),
                            plotlyOutput(outputId = 'barplot_AC', height = "780px", width = "1300px"))
                           ),
################################################################################ 
##### 6''. DEFINE OUTPUT FOR Tab 2.1. MAP ######################################
################################################################################ 
                  tabPanel("INTERACTIVE MAP", br(), # Add a line break
                           
################################################################################
##### 6''-a) Create a drop-down menu for Tab 2.1 ###############################
################################################################################                            
                        sidebarLayout(
                            sidebarPanel(
                               width = 12,
                               selectInput(
                                 inputId = 'AC_EQUIPMENT_TYPE',
                                 label = 'SELECT A/C EQUIPMENT TYPE :',
                                 choices = c(
                                   'CENTRAL AC AND CENTRAL HEAT PUMP', 'WINDOW OR WALL AC', 'PORTABLE AC',
                                   'MINISPLIT', 'EVAPORATIVE COOLER', 'DO NOT USE AC')
                               )
                             ),
                             mainPanel(br(), br(),
                               leafletOutput(outputId = 'map_AC', height = "600px", width = "1600px")
                               
                             )
                           )
                  )
               )
      ),
                        
### 2-c'. Create tab 2.2           
          tabPanel("2.2 FUEL USED FOR A/C",
              tabsetPanel(
                  tabPanel("BAR PLOT",  br(), br(), br(),
                           "The types of fuel used by A/C equipment have been omitted for the purpose of this analysis, as most A/Cs run on electricity.",
                           br()),
                  tabPanel("INTERACTIVE MAP",  br(), br(), br(),
                           "The types of fuel used by A/C equipment have been omitted for the purpose of this analysis, as most A/Cs run on electricity.",
                           br())
                   )
                  )
      )
  )
)



### 3. Define the Server, which consists of 1) input and 2) output
server <- function(input, output) {

################################################################################ 
##### 5. RENDER BARPLOT FOR Tab 1.1. ###########################################
################################################################################  
  
# Call the user defined function for plotting a stacked, horizontal bar plot    
  output$barplot_HEATER <- create_bar_plot(
    number = 99,
    data = heater_type, type = heater_type$HEATING_EQUIPMENT_TYPE,
    title_txt = "HEATERS USED IN THE HOME: BY EQUIPMENT TYPE",
    palette_colors = 10, palette_style = "Set3")
  
################################################################################ 
##### 5'. RENDER BARPLOT FOR Tab 1.2. ##########################################
################################################################################  
  
  output$barplot_HEATER_FUEL <- create_bar_plot(
    number = 25,
    data = heater_fuel, type = heater_fuel$HEATER_FUEL_TYPE,
    title_txt = "HEATERS USED IN THE HOME: BY FUEL TYPE",
    palette_colors = 7, palette_style = "Set2")

################################################################################ 
##### 5''. RENDER BARPLOT FOR Tab 2.1. #########################################
################################################################################  

  output$barplot_AC <- create_bar_plot(
    number = 347,
    data = AC_type, type = AC_type$AC_EQUIPMENT_TYPE,
    title_txt = "A/C USED IN THE HOME: BY EQUIPMENT TYPE",
    palette_colors = 6, palette_style = "Set3")




################################################################################ 
##### 7. RENDER INTERACTIVE MAP FOR TAB 1.1 ####################################
################################################################################  

### a) Filter the data based on the selected drop-down option - tab 1.1
  filtered_HEATER <- filter_the_data(heater_type, HEATING_EQUIPMENT_TYPE, input$HEATING_EQUIPMENT_TYPE)
### b) Define the color palette based on the filtered data 
  gradation_1 <- define_palette(filtered_HEATER)
### c) Create an interactive map of the US, based on filtered data 
  observe({
    output$map_HEATER <- renderLeaflet({
      create_map(filtered_HEATER, gradation_1, input$HEATING_EQUIPMENT_TYPE, "DO NOT USE HEATER")})
  })

################################################################################ 
##### 7'. RENDER INTERACTIVE MAP FOR TAB 1.2 ###################################
################################################################################    
 
### a) Filter the data based on the selected drop-down option - tab 1.2 
  filtered_HEATING_FUEL <- filter_the_data(heater_fuel, HEATER_FUEL_TYPE, input$HEATER_FUEL_TYPE)
### b) Define the color palette based on the filtered data 
  gradation_2 <- define_palette(filtered_HEATING_FUEL)
### c) Create an interactive map of the US, based on filtered data 
  observe({
    output$map_FUEL <- renderLeaflet({
      create_map(filtered_HEATING_FUEL, gradation_2, input$HEATER_FUEL_TYPE, "DO NOT USE HEATER")})
  })

################################################################################ 
##### 7''. RENDER INTERACTIVE MAP FOR TAB 2.1 ##################################
################################################################################  
  
### a) Filter the data based on the selected drop-down option - tab 2.1
  filtered_AC <- filter_the_data(AC_type, AC_EQUIPMENT_TYPE, input$AC_EQUIPMENT_TYPE)   
### b) Define the color palette based on the filtered data 
  gradation_3 <- define_palette(filtered_AC)
### c) Create an interactive map of the US, based on filtered data 
  observe({
    output$map_AC <- renderLeaflet({
      create_map(filtered_AC, gradation_3, input$AC_EQUIPMENT_TYPE, "DO NOT USE AC")})
  })

}



### 8. Run the Shiny app
shinyApp(ui, server)