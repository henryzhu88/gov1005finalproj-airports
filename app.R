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

#The DT library allows me to create datatables.
library(DT)

#From the raw excel spreadsheet, I clean the variable names and am only interested in outgoing flights from Newark Airport.

newark <- read_excel("airline_data_NJ.xlsx") %>%
  clean_names() %>%
  filter(origin=="EWR") %>%
  
#Departure times that are missing are removed.
  
  filter(dep_time!="NA") %>%
  replace_na(list(dep_delay ="0", dep_del15 ="0", dep_delay_group="0"))
  

#In order to find the coordinates of each destination airport, I found an online database with all airline codes and their respective coordinates. 
#The dest, lat, and long columns are located and names are cleaned.

location <- read_csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat",col_names= FALSE) %>%
  transmute(DEST= X5, 
            LAT= X7, 
            LONG= X8) %>%
  clean_names()

#I isolated the newark airport coordinate, to be used to draw connecting line.


#Then, I added the lat and long variables to the existing Newark flight dataset, with the joiner being destination.

newark<-left_join(newark,location, by="dest") %>%

#Unneeded variable columns are eliminated.
  
  select(-year,-month,-origin_city_name,-origin) %>%

#I coded a departure delay of more than 15 minutes to signify delayed, while anything under a +15 minute mark was considered not delayed.
  
  mutate(dep_del15 = case_when(
    dep_del15 == "0" ~ "Not Delayed",
    dep_del15 == "1" ~ "Delayed")) %>%
  
  mutate(op_unique_carrier = case_when(
  op_unique_carrier == "UA" ~ "United Airlines",
  op_unique_carrier == "EV" ~ "ExpressJet",
  op_unique_carrier == "MQ" ~ "American Airlines",
  op_unique_carrier == "AA" ~ "American Airlines",
  op_unique_carrier == "OH" ~ "American Airlines",
  op_unique_carrier == "DL" ~ "Delta Airlines",
  op_unique_carrier == "9E" ~ "Delta Airlines",
  op_unique_carrier == "WN" ~ "Southwest Airlines",
  op_unique_carrier == "B6" ~ "JetBlue",
  op_unique_carrier == "NK" ~ "Spirit Airlines",
  op_unique_carrier == "EV" ~ "ExpressJet",
  op_unique_carrier == "AS" ~ "Alaska Airlines",
  op_unique_carrier == "G4" ~ "Allegiant Air",
  op_unique_carrier == "OO" ~ "Skywest Airlines",
  op_unique_carrier == "YX" ~ "Republic Airline",
  op_unique_carrier == "VX" ~ "Virgin America")) 


#count <-newark %>% group_by(DEST) %>% count()

#newark <-left_join(newark,count, by="DEST")

pal <- 
  colorFactor(palette = c("green", "red"), 
              levels = c("Not Delayed", "Delayed"))

# Define UI for application that draws a map, with a shinytheme of superhero

ui <- navbarPage("Newark Flights, Jan 2018",theme = shinytheme("sandstone"),
  
#ABOUT                     
      tabPanel("About",
  
      fluidPage(
   
   # Application title
   
   titlePanel("Newark Airport Flights, Jan. 2018"),
   
   p(paste("Where are flights from Newark Airport headed? Check out some visualizations and data from all flights in January 2018."))
   )),
#ABOUT
   

#MAP
   # Sidebar with a date Range input for customizing departure date, as well as an airline selector.
   tabPanel("Destination Flight Map",
            
            fluidPage(
              
            titlePanel("Newark Airport Flight Map, Jan. 2018"),
            
   sidebarLayout(
      sidebarPanel(
        dateRangeInput("fl_date", 
                       "Choose a Date Range:", 
                       start = "2018-01-01", end = "2018-01-31"),
        sliderInput("crs_dep_time", 
                    "Select a Departure Time Range (Scheduled):", 
                    min = min(unique(newark$crs_dep_time)), 
                    max = max(unique(newark$crs_dep_time)),
                    value = c(0, 2400),
                    sep = ""), 
        selectInput("op_unique_carrier",
                     "Choose an Airline:",
                     c("United Airlines",
                       "ExpressJet",
                       "American Airlines",
                       "Delta Airlines",
                       "Southwest Airlines",
                       "JetBlue",
                       "Spirit Airlines",
                       "Alaska Airlines",
                       "Allegiant Air",
                       "Virgin America",
                       "Republic Airline",
                       "Skywest Airlines"),
                        selected=NULL)
                      ),
                      
      # Show a plot of the generated map
      
      mainPanel(
          leafletOutput("map"))
   ))

),

#GRAPHS
tabPanel("Graphs",
         
         fluidPage(
           
           titlePanel("Newark Airport Graphs"),
           
             
             # Show a plot of the generated map
             
             mainPanel(
               plotOutput("hist"))
           )
         
),

#GRAPHS

#TABLE
tabPanel("Full Flight Data",
         
         fluidPage(
           titlePanel("Flight Data"),
           
           mainPanel(
             DTOutput("full_table"))   
           

))

#TABLE
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    map <- newark %>% 
      filter(fl_date >= input$fl_date[1] & fl_date <= input$fl_date[2], crs_dep_time >= input$crs_dep_time[1] & crs_dep_time <= input$crs_dep_time[2],op_unique_carrier == input$op_unique_carrier) %>%
      mutate(total=n()) %>%
      group_by(dest_city_name) %>%
      mutate(count= n()) %>%
      ungroup() %>%
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB") %>%
      addCircleMarkers(radius = 3,
                      
                       #color = ~100*(count/total)),
                       popup = ~paste0(dest_city_name, ":",sep=" ", count,sep=" ","flights")) %>%
      addLegend(position = "bottomright",
                pal = pal, 
                values = c("Not Delayed", "Delayed"))
   })
  
  output$hist <-renderPlot({
    newark$dep_time <- as.numeric(newark$dep_time)
    delay<- newark %>% 
      filter(dep_del15 =="Delayed") %>%
      arrange(fl_date) %>%
      ggplot(aes(x=dep_time)) + geom_histogram(fill="#005DAA") +
      theme_classic() +
      xlab("Time of Day") +
      ylab("Number of Delayed Flights") +
      labs(title="Distribution of Delayed Flights Based on Time of Day", subtitle="Newark Airport, January 2018, Bureau of Transportation Statistics", caption= "Delayed Flight: Actual Departure 15 Minutes or More After Scheduled Dep. Time")+
      scale_x_continuous(limits=c(0,2400),
                         breaks=c(0,400,800,1200,1600,2000,2400),
                         labels=c("12:00 AM", "4:00 AM", "8:00 AM","12:00 PM","4:00 PM","8:00 PM","12:00 AM")) +
      scale_y_continuous(breaks=c(0,25,50,75,100,125),
                         labels=c("0","25","50","75","100","125"))
    delay
  })
  
  output$full_table <- renderDT(
      
      datatablenewark<-newark %>%
        
        
        transmute("Day of Month"= day_of_month,
                  "Airline" = op_unique_carrier,
                  "Scheduled Departure Time" =crs_dep_time,
                  "Actual Departure Time"= dep_time,
                  "Delay Status" = dep_del15,
                  "Destination City"= dest_city_name),
      rownames = FALSE,
      options = list(
        order = 
          list(list(0, 'asc')))
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
