library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(lubridate)
library(leaflet)
newark <- read_excel("airline_data_NJ.xlsx") %>%
  filter(ORIGIN=="EWR") %>%
  filter(DEP_TIME!="NA")

location <- read_csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",col_names= FALSE) %>%
  transmute(DEST= X5, 
            LAT= X7, 
            LONG= X8)

newark<-left_join(newark,location, by="DEST") %>%
  select(-YEAR,-MONTH,-ORIGIN_CITY_NAME,-ORIGIN) %>%
  mutate(DEP_DEL15 = case_when(
    DEP_DEL15 == "0" ~ "Not Delayed",
    DEP_DEL15 == "1" ~ "Delayed",
  )) 

count <-newark %>%
  group_by(DEST) %>%
  count()

newark <-left_join(newark,count, by="DEST")

pal <- 
  colorFactor(palette = c("green", "red"), 
              levels = c("Not Delayed", "Delayed"))

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
   
   # Application title
   titlePanel("Newark Airport Flights, Jan. 2018"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        dateRangeInput("FL_DATE", 
                       "Date", 
                       start = "2018-01-01", end = "2018-01-31"),
         selectInput("OP_UNIQUE_CARRIER",
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
                      
      # Show a plot of the generated distribution
      
      mainPanel(
         leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  output$map <- renderLeaflet({
    map <- newark %>% 
      filter(FL_DATE == input$FL_DATE, OP_UNIQUE_CARRIER == input$OP_UNIQUE_CARRIER) %>%
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB") %>%
      addCircleMarkers(radius = 3,
                       #color = ~nrow(DEP_DEL15),
                       popup = ~paste0(DEST_CITY_NAME, "-", n,"", "flights")) 
      #addLegend(position = "bottomright",
                #pal = pal, 
                #values = c("Not Delayed", "Delayed"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
