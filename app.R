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
    dep_del15 == "0" ~ "Not Delayed for More than 15 Minutes",
    dep_del15 == "1" ~ "Delayed for More than 15 Minutes")) %>%
  
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
     
     # Header with more specific description of project
     
     h3("Curious where in the U.S. you can fly to from Newark, NJ? How likely will my flight get delayed?"),
     
     # More details about my project 
     
     p("Maybe looking at historical data from Jan 2018 will help! Scroll through these tabs to see for yourself!"),
     
     #Line Break for spacing
     
     br(),
     
     #Details about app
     
     h3("App Info:"),
     
     # Hyperlinked the stats website.
     
     p("I obtained my data through the Bureau of Transportation Statistics, which can be accessed",
       tags$a(href = "https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236",
              "here.")),

     # Access to my Github.
     
     p("The code for this project can be accessed through my",
       tags$a(href = "https://github.com/henryzhu88/gov1005finalproj-airports",
              "GitHub."))),
   
#Inserted a picture.
   
   mainPanel(
                imageOutput("pic"))
  
    )),
#ABOUT
   

#MAP
  #Title of tab and title of panel.

   tabPanel("Destination Flight Map",
            
            fluidPage(
              
            titlePanel("Flight Map Visualization"),
            
            fluidRow(
              
              # Header with more specific description of project
              
              h4("Step 1: Customize your range and airline!"),                 
              h4("Step 2: View results on map! Click on the cities!"),
              h4("Step 3: See detailed results in the table below.")
              
              ),
              
  
# Sidebar with a date Range input for customizing departure date, time of day, as well as an airline selector.          
   
    sidebarLayout(
      sidebarPanel(

#This widget adjusts for the date range of interest, which I restricted to the month of January in 2018.
        #id of widget given
        
        dateRangeInput("fl_date", 
                       
        #title of widget, instructing users to choose a date range.
        
                       "Choose a Date Range:", 
        
        #start and end point of date range.
        
                       start = "2018-01-01", end = "2018-01-31"),

#This widget adjusts for the time range of the departure flight, using military time.
          #id of widget given
        sliderInput("crs_dep_time",
                    
          #title of widget, instructing users to choose a time range
          
                    "Select a Departure Time Range (Scheduled):", 
          
          #the minimum(earliest) flight level is preset, as well as the maximum.
          
                    min = min(unique(newark$crs_dep_time)), 
                    max = max(unique(newark$crs_dep_time)),
          
          #The range of values extends from 0 to 2400(military time)
          
                    value = c(0, 2400),
                    sep = ""), 

#This widget adjusts for the choice of your airline. Airline options are listed below
      #input id is selected
        selectInput("op_unique_carrier",
                    
      #title of widget, instructing individuals to select an airline.
      
                     "Choose an Airline:",
      
      #list of airlines to select
      
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
          
      #I also included the table with all of the flights that correspond to the selected parameters.
      
          DTOutput("full_table"))
   ))

),

#GRAPHS
tabPanel("Graphs",
         
         fluidPage(
           
           titlePanel("Newark Airport Graphs"),
           
           fluidRow(
             
             # Header with more specific description of project
             
             h4("Graph 1: Flight Distribution by Time of Day")
             
           ),
           
           sidebarLayout(
             sidebarPanel(
               #This widget adjusts for the date range of interest, which I restricted to the month of January in 2018.
               #id of widget given
               
               dateRangeInput("fl_date2", 
                              
                              #title of widget, instructing users to choose a date range.
                              
                              "Choose a Date Range:", 
                              
                              #start and end point of date range.
                              
                              start = "2018-01-01", end = "2018-01-31"),
               
               #This widget adjusts for the time range of the departure flight, using military time.
               #id of widget given
               sliderInput("crs_dep_time2",
                           
                           #title of widget, instructing users to choose a time range
                           
                           "Select a Departure Time Range (Scheduled):", 
                           
                           #the minimum(earliest) flight level is preset, as well as the maximum.
                           
                           min = min(unique(newark$crs_dep_time)), 
                           max = max(unique(newark$crs_dep_time)),
                           
                           #The range of values extends from 0 to 2400(military time)
                           
                           value = c(0, 2400),
                           sep = ""), 
               
               #This widget adjusts for the choice of your airline. Airline options are listed below
               #input id is selected
               selectInput("op_unique_carrier2",
                           
                           #title of widget, instructing individuals to select an airline.
                           
                           "Choose an Airline:",
                           
                           #list of airlines to select
                           
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
                           selected=NULL),
               
               #This checkbox was created to differentiate between delayed and non-delayed flights
               
               checkboxGroupInput("delaycheck","Flight Status:",
                                  c("Not Delayed for More than 15 Minutes","Delayed for More than 15 Minutes"),
                                  selected = c("Not Delayed for More than 15 Minutes","Delayed for More than 15 Minutes"))
        
             ),  
             # Show a plot of the generated map
             
             mainPanel(
               plotOutput("hist"),
               plotOutput("bar"))
           ))
         
))

#GRAPHS


# Define server logic required to draw a histogram
server <- function(input, output) {
  
#This map shows the different destinations from newark airport as well as information about delayed flights.
  #A leaflet is generated.
  output$map <- renderLeaflet({
    
  #I first created a variable specifically to allow me to manipulate the data needed for the map.
    
    mapdata <- newark %>% 
      
      #After selecting my newark data(already modified above), the first step is to instruct shiny to filter out the data based on user choice.
      #the input range of date is restricted to option 1 and option 2. Same for scheduled departure time.
      #the input of the airline is also made flexible based on user choice and only the data of the selected airline is preserved.
      #the input of flight status is also added.
      
      filter(fl_date >= input$fl_date[1] & fl_date <= input$fl_date[2], crs_dep_time >= input$crs_dep_time[1] & crs_dep_time <= input$crs_dep_time[2],op_unique_carrier == input$op_unique_carrier) %>%
      
      #I then grouped by the destination city to allow for a count of how many flights are incoming in that city.
      
      group_by(dest_city_name) %>%
      
      #This per-city flight count is represented in a separate column called count, which will be shown dynamically in each pop-up.
      
      mutate(count= n()) %>%
      
      #Next, I wanted to create an average delay time for each city.
      
      mutate(dep_delay = as.numeric(dep_delay)) %>%
      
      #The formula I used was to add up the departure delay times for each city before dividing by the number of flights to each city.
      
      mutate(avgdel= sum(dep_delay)/count) %>%
      
      #This number was then grouped into a new avgdel variable which categorized the delay based on its average length.
      
      mutate(avgdel = case_when(
        
        #Earlier than 0 means the departed time was earlier than the scheduled time.
        
        avgdel < 0 ~ "A: Departed Early on Avg.",
        
        #Each subsequent age range is then listed out and then reworded.
        
        avgdel >=0 & avgdel <5 ~ "B: Less than 5-Minute Avg.Delay",
        avgdel >=5 & avgdel<15 ~ "C: 5-Minute to 15-Minute Avg.Delay",
        avgdel >=15 & avgdel<=30 ~ "D: 15 Minute to 30-Minute Avg.Delay",
        avgdel >=30 ~ "E: More Than 30-Minute Avg.Delay")) %>%
      ungroup() 
    
#I created a color palette to allow for a visualization of average delay time, with early departures in green to more than 30-minute average delays in red.
    
    pal2 <-
      colorFactor(palette = c("#006400","#90EE90","#FADA5E","#FF8C00","red"), 
                  levels = c("A: Departed Early on Avg.", "B: Less than 5-Minute Avg.Delay",
                             "C: 5-Minute to 15-Minute Avg.Delay","D: 15 Minute to 30-Minute Avg.Delay",
                             "E: More Than 30-Minute Avg.Delay"))    
 
#Using the adjusted map data, a leaflet map is created.   
    map<- mapdata %>%
      leaflet() %>% 
      
  #The theme of CartoDB is selected. 
      
      addProviderTiles(provider = "CartoDB") %>%
      
  #Circlemarkers are added with a stable radius of 3.
      addCircleMarkers(radius = 3,
                       
  #the color varies based on the avg delay time of flights in the selected parameters, using the palette created above.
  
                       color = ~pal2(avgdel),
  
  #An interactive pop-up is created which lists the city chosen as well as how many flights are incoming to that city from Newark under the given parameters.
  #This flight number is flexible and can change.
  
                       popup = ~paste0(dest_city_name, ":",sep=" ", count,sep=" ","total flights",sep=" ")) %>%

#A legend is created that is based on a green-red color pallette, with green signifying early departures while red signifies extremely-late departures.
      #Legend positioned in the bottom right corner
      addLegend(position = "bottomright",
          
      #palette selected
                pal = pal2, 
      
      #values listed out. Needed to put the alphabetical listing to preserve ordering.
      
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
      filter(fl_date >= input$fl_date2[1] & fl_date <= input$fl_date2[2], crs_dep_time >= input$crs_dep_time2[1] & crs_dep_time <= input$crs_dep_time2[2],op_unique_carrier == input$op_unique_carrier2, dep_del15 == input$delaycheck) %>%
      arrange(fl_date) %>%
      
#I draw the ggplot for a histogram, with departure time distributed across the x-axis. I colored the plot dark blue.
      
      ggplot(aes(x=dep_time)) + geom_histogram(binwidth=50, fill="#005DAA") +

#A classic theme is selected.
      theme_classic() +

#The axis titles are labeled, with clarification that a delay here is signified as 15 minutes after scheduled departure time.
      
      xlab("Time of Day") +
      ylab("Number of Flights") +
      labs(title="Distribution of Flights Based on Time of Day", subtitle="Newark Airport, January 2018, Bureau of Transportation Statistics", caption= "Delayed Flight: Actual Departure 15 Minutes or More After Scheduled Dep. Time")+

#The x-axis is scaled to represent ticks at every four hours and encompasses the span of one 24-day.
      
      scale_x_continuous(limits=c(0,2400),
                         breaks=c(0,400,800,1200,1600,2000,2400),
                         labels=c("12:00 AM", "4:00 AM", "8:00 AM","12:00 PM","4:00 PM","8:00 PM","12:00 AM"))

    delay
  })
  
  #I created a second graph that looks at distribution of delayed flights across all of the airlines, using a bar chart.
  output$bar <-renderPlot({
    
    #Departure time is made numeric to allow for it to be represented as a continuous variable across the x-axis.
    
    mapdata$dep_time <- as.numeric(mapdata$dep_time)
    
    airline <- mapdata %>% 
      
      #The same filter function based on the input value is used here, adjusting for changes to date and departure time.
      filter(fl_date >= input$fl_date2[1] & fl_date <= input$fl_date2[2], crs_dep_time >= input$crs_dep_time2[1] & crs_dep_time <= input$crs_dep_time2[2]) %>%
      
      group_by(op_unique_carrier) %>%
      
      #I wanted only the number of delayed flights, taking the length.
      mutate(delcount= length(dep_del15[dep_del15 == "Delayed"])) %>%
      
      #The total number of glihts is calculated.
      mutate(aircount= n()) %>%
      
      #To sort by airline, I grouped by carrier.
      
      group_by(op_unique_carrier) %>%
    
      #The frequency of delay is calculated through the percentage of delayed over total flights.
      mutate(freqdel= delcount/aircount*100) %>%
      
      #To represent through one value, I took the mean, although it is the same value.
      
      summarize(freqdel=mean(freqdel)) %>%
      
      #I draw the ggplot for a histogram, with departure time distributed across the x-axis. I colored the plot dark blue.
      
      ggplot(aes(x=op_unique_carrier, y=freqdel)) + geom_col(fill="#C0C0C0") +
      
      #A classic theme is selected.
      theme_classic() +
      
      #The axis titles are labeled.
      
      xlab("") +
      ylab("Percentage of Flights Delayed by 15 Minutes or More(%)") +
      labs(title="Delayed Flights by Airline", subtitle="Newark Airport, January 2018, Bureau of Transportation Statistics") +
      theme(axis.text.x = element_text(angle=60, hjust=1))
  airline
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
