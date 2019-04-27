#I loaded the shiny library and shinythemes library to set up basic app and customize the layout.

library(shiny)
library(shinythemes)

#The tidyverse allows for basic processing of data like filtering, the janitor library makes all variables lowercase, and the readxl allows me to convert excel spreadsheet into readable format.

library(tidyverse)
library(janitor)
library(readxl)

#The lubridate library provides code to allow for manipulations to dates and months, which I will need to process departure times.

library(lubridate)

#The leaflet library creates the maps I will use to visualize the airport destinations.
library(leaflet)

#From the raw excel spreadsheet, I clean the variable names and am only interested in outgoing flights from Newark Airport.

newark <- read_excel("airline_data_NJ.xlsx") %>%
  clean_names() %>%
  filter(origin=="EWR") %>%
  
#Departure times that are missing are removed.
  
  filter(dep_time!="NA")

#In order to find the coordinates of each destination airport, I found an online database with all airline codes and their respective coordinates. 
#The dest, lat, and long columns are located and names are cleaned.

location <- read_csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",col_names= FALSE) %>%
  transmute(DEST= X5, 
            LAT= X7, 
            LONG= X8) %>%
  clean_names()

#Then, I added the lat and long variables to the existing Newark flight dataset, with the joiner being destination.

newark<-left_join(newark,location, by="dest") %>%

#Unneeded variable columns are eliminated.
  
  select(-year,-month,-origin_city_name,-origin) %>%

#I coded a departure delay of more than 15 minutes to signify delayed, while anything under a +15 minute mark was considered not delayed.
  
  mutate(dep_del15 = case_when(
    dep_del15 == "0" ~ "Not Delayed",
    dep_del15 == "1" ~ "Delayed",
  )) 


#count <-newark %>% group_by(DEST) %>% count()

#newark <-left_join(newark,count, by="DEST")

#pal <- 
  #colorFactor(palette = c("green", "red"), 
              #levels = c("Not Delayed", "Delayed"))

# Define UI for application that draws a map, with a shinytheme of superhero

ui <- fluidPage(theme = shinytheme("superhero"),
   
   # Application title
   
   titlePanel("Newark Airport Flights, Jan. 2018"),
   
   # Sidebar with a date Range input for customizing departure date, as well as an airline selector.
   
   sidebarLayout(
      sidebarPanel(
        dateRangeInput("fl_date", 
                       "Date", 
                       start = "2018-01-01", end = "2018-01-31"),
         selectInput("op_unique_carrier",
                     "Choose an Airline:",
                     c("United Airlines" = "UA",
                       "ExpressJet" = "EV",
                       "American Airlines"= "AA",
                       "Republic Airline" = "YX",
                       "Delta Airlines" = "DL",
                       "Southwest Airlines" ="WN",
                       "JetBlue"="B6",
                       "Spirit Airlines" = "NK"),
                        selected=NULL)
      ),
                      
      # Show a plot of the generated map
      
      mainPanel(
         leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  output$map <- renderLeaflet({
    map <- newark %>% 
      filter(fl_date == input$fl_date, op_unique_carrier == input$op_unique_carrier) %>%
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB") %>%
      addCircleMarkers(radius = 3,
                       #color = ~nrow(dep_del15),
                       popup = ~dest_city_name)
                         #~paste0(dest_city_name, "-", n,"", "flights")) 
      #addLegend(position = "bottomright",
                #pal = pal, 
                #values = c("Not Delayed", "Delayed"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
