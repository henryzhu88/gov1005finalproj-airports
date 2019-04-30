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
  
#Departure times that are missing are removed. NA delay times are designated as zero.
  
  filter(dep_time!="NA") %>%
  replace_na(list(dep_delay ="0", dep_del15 ="0", dep_delay_group="0"))
  

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
    dep_del15 == "1" ~ "Delayed")) %>%
  
#I recoded the carrier codes to equate to each respective airline. Some regional companies(i.e. American Eagle) are subsidaries of larger airline companies and were grouped together.
  
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

# Define UI for application that draws a map, with a shinytheme of superhero

ui <- navbarPage("Newark Flights, Jan 2018",theme = shinytheme("sandstone"),
  
#ABOUT                     
      tabPanel("About",
  
      fluidPage(
   
   # Application title
   
   titlePanel(h1("Welcome to Newark Liberty International Airport!")),
   
   fluidRow(
     
     # Second header in a smaller font size
     
     h3("Curious where in the U.S. you can fly to from Newark, NJ? How likely will my flight get delayed?"),
     
     # Description of my project 
     
     p("Maybe looking at historical data from Jan 2018 will help! Scroll through these tabs to see for yourself!"),
     
     br(),
     
     h3("App Info:"),
     
     # Gives the user the chance to look at the data I used themselves
     
     p("I obtained my data through the Bureau of Transportation Statistics, which can be accessed",
       tags$a(href = "https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236",
              "here.")),

     # Now you can take a look at my R code and replicate my project!
     
     p("The code for this project can be accessed through my",
       tags$a(href = "https://github.com/henryzhu88/gov1005finalproj-airports",
              "GitHub."))),
   
  
   
   mainPanel(
     
                imageOutput("pic"))
  
   
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
          leafletOutput("map"),
          DTOutput("full_table"))
   ))

),

#GRAPHS
tabPanel("Graphs",
         
         fluidPage(
           
           titlePanel("Newark Airport Graphs"),
           
           sidebarLayout(
             sidebarPanel(
               dateRangeInput("fl_date2", 
                              "Choose a Date Range:", 
                              start = "2018-01-01", end = "2018-01-31"),
               sliderInput("crs_dep_time2", 
                           "Select a Departure Time Range (Scheduled):", 
                           min = min(unique(newark$crs_dep_time)), 
                           max = max(unique(newark$crs_dep_time)),
                           value = c(0, 2400),
                           sep = ""), 
               selectInput("op_unique_carrier2",
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
               plotOutput("hist"))
           ))
         
))

#GRAPHS


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    
    mapdata <- newark %>% 
      filter(fl_date >= input$fl_date[1] & fl_date <= input$fl_date[2], crs_dep_time >= input$crs_dep_time[1] & crs_dep_time <= input$crs_dep_time[2],op_unique_carrier == input$op_unique_carrier) %>%
      mutate(total=n()) %>%
      group_by(dest_city_name) %>%
      mutate(count= n()) %>%
      mutate(dep_delay = as.numeric(dep_delay)) %>%
      mutate(avgdel= sum(dep_delay)/count) %>%
      mutate(avgdel = case_when(
        avgdel < 0 ~ "A: Departed Early on Avg.",
        avgdel >=0 & avgdel <5 ~ "B: Less than 5-Minute Avg.Delay",
        avgdel >=5 & avgdel<15 ~ "C: 5-Minute to 15-Minute Avg.Delay",
        avgdel >=15 & avgdel<=30 ~ "D: 15 Minute to 30-Minute Avg.Delay",
        avgdel >=30 ~ "E: More Than 30-Minute Avg.Delay")) %>%
      ungroup() 
    
    pal2 <-
      colorFactor(palette = c("#006400","#90EE90","#FADA5E","#FF8C00","red"), 
                  levels = c("A: Departed Early on Avg.", "B: Less than 5-Minute Avg.Delay",
                             "C: 5-Minute to 15-Minute Avg.Delay","D: 15 Minute to 30-Minute Avg.Delay",
                             "E: More Than 30-Minute Avg.Delay"))    
    
    map<- mapdata %>%
      leaflet() %>% 
      addProviderTiles(provider = "CartoDB") %>%
      addCircleMarkers(radius = 3,
                       color = ~pal2(avgdel),
                       popup = ~paste0(dest_city_name, ":",sep=" ", count,sep=" ","total flights",sep=" ")) %>%
      addLegend(position = "bottomright",
                pal = pal2, 
                values = c("A: Departed Early on Avg.", "B: Less than 5-Minute Avg.Delay",
                           "C: 5-Minute to 15-Minute Avg.Delay","D: 15 Minute to 30-Minute Avg.Delay",
                           "E: More Than 30-Minute Avg.Delay"))
   })
  
#I generated a histogram that shows distribution of delays according to time of day.
  
  output$hist <-renderPlot({
  
#Departure time is made numeric to allow for it to be represented as a continuous variable across the x-axis.
    
    newark$dep_time <- as.numeric(newark$dep_time)

    delay<- newark %>% 

#The same filter function based on the input value is used here, adjusting for changes to date, departure time, and airline.
      filter(fl_date >= input$fl_date2[1] & fl_date <= input$fl_date2[2], crs_dep_time >= input$crs_dep_time2[1] & crs_dep_time <= input$crs_dep_time2[2],op_unique_carrier == input$op_unique_carrier2) %>%
      arrange(fl_date) %>%
      
#I draw the ggplot for a histogram, with departure time distributed across the x-axis. I colored the plot dark blue.
      
      ggplot(aes(x=dep_time)) + geom_histogram(fill="#005DAA") +

#A classic theme is selected.
      theme_classic() +

#The axis titles are labeled, with clarification that a delay here is signified as 15 minutes after scheduled departure time.
      
      xlab("Time of Day") +
      ylab("Number of Delayed Flights") +
      labs(title="Distribution of Delayed Flights Based on Time of Day", subtitle="Newark Airport, January 2018, Bureau of Transportation Statistics", caption= "Delayed Flight: Actual Departure 15 Minutes or More After Scheduled Dep. Time")+

#The x-axis is scaled to represent ticks at every four hours and encompasses the span of one 24-day.
      
      scale_x_continuous(limits=c(0,2400),
                         breaks=c(0,400,800,1200,1600,2000,2400),
                         labels=c("12:00 AM", "4:00 AM", "8:00 AM","12:00 PM","4:00 PM","8:00 PM","12:00 AM")) +

#The y-axis is scaled to represent counts of flight, by 25.
      
      scale_y_continuous(breaks=c(0,25,50,75,100,125),
                         labels=c("0","25","50","75","100","125"))
    delay
  })
  
#The output for the datatable below the map is represented. Since it is interactive, a change on the sidebar panel will also change the results of the table.
  
  output$full_table <- renderDT(

      datatablenewark<-newark %>%

#The same filter function based on the input value is used here, adjusting for changes to date, departure time, and airline.
        
      filter(fl_date >= input$fl_date[1] & fl_date <= input$fl_date[2], crs_dep_time >= input$crs_dep_time[1] & crs_dep_time <= input$crs_dep_time[2],op_unique_carrier == input$op_unique_carrier) %>%
  
#Only the date(YYYY-MM-DD) is needed in the Date, so I removed the extraneous output.
  
       mutate(fl_date = str_remove_all(fl_date, "T00:00:00Z")) %>%
        
#I renamed each of the columns into readable format and contained my table to only these values.
        
        transmute("Date"= fl_date,
                  "Airline" = op_unique_carrier,
                  "Scheduled Departure Time" =crs_dep_time,
                  "Actual Departure Time"= dep_time,
                  "Delay Time(min)" = dep_delay,
                  "Destination City"= dest_city_name),

#There is no need to show the rownames. Additionally, I made sure the first date and first time are listed first through organizing those two columns in ascending order.
      
        rownames = FALSE,
        options = list(
        order = 
          list(list(0, 'asc'),list(2, 'asc')))
    )
  
output$pic <- renderImage({  
  list(src="airportpic.jpg",
       contentType="image/gif")
}, deleteFile = FALSE)

}

# Run the application 
shinyApp(ui = ui, server = server)
