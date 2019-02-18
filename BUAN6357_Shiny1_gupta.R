# R Shiny assignment
# Shashwat Gupta
# SXG180019



library(tidyverse)
library(shiny)
library(leaflet)
library(DT)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path ))
print( getwd() )


############# Reading and cleaning data  ##################

vehicle_Accident <- read.csv("Vehicle_Accident_Data.csv")

vehicle_Accident <- tidyr::separate(data=vehicle_Accident,
                      col=Crash.Date.Time,
                      into=c("Date", "Time"),
                      sep=" ",
                      remove=FALSE)
vehicle_Accident <- tidyr::separate(data=vehicle_Accident,
                                    col=Location,
                                    into=c("Latitude", "Longitude"),
                                    sep=",",
                                    remove=FALSE)
vehicle_Accident$Latitude <- stringr::str_replace_all(vehicle_Accident$Latitude, "[(]", "")
vehicle_Accident$Longitude <- stringr::str_replace_all(vehicle_Accident$Longitude, "[)]", "")

typeof(vehicle_Accident$Latitude)
typeof(vehicle_Accident$Date)

vehicle_Accident$Latitude <- as.numeric(vehicle_Accident$Latitude)
vehicle_Accident$Longitude <- as.numeric(vehicle_Accident$Longitude)
vehicle_Accident<-vehicle_Accident[!(vehicle_Accident$Location=="(0, 0)") ,]
vehicle_Accident$Date <- as.Date(vehicle_Accident$Date, "%m/%d/%Y")
vehicle_Accident <-   vehicle_Accident[vehicle_Accident$Latitude >  30, ]


# R SHiny Application

############### UI Function #################
ui <- fluidPage(
  
      sidebarLayout(
        
        sidebarPanel(
          
          # Select variable date
          dateRangeInput('dateRange',
                         label = 'Fatality by Date:',
                         start = as.Date('2018-01-01') , end = as.Date('2019-06-01')
          ),
          
          # Select variable Fatality
          selectInput(inputId = "fatality",
                      label = "Fatality: True / False",
                      choices = c("TRUE","FALSE"," "),
                      selected = " "),

           # Select variable Hit and Run
          selectInput(inputId = "hit",
                      label = "Hit and Run: True / False",
                      choices = c("TRUE", "FALSE", " "),
                      selected = " ")
          
          
          ),
        
        mainPanel(
          
          verticalLayout(

        leafletOutput(outputId = "mymap"),
        DT::dataTableOutput(outputId = "mytable")
       

       )
     )
  )
)

################## server function ################
server <- function(input,output,session){
  
  
  data <- reactive({
     filter( 
       vehicle_Accident, between(vehicle_Accident$Date ,input$dateRange[1], input$dateRange[2])
     & vehicle_Accident$Fatality == input$fatality 
     & vehicle_Accident$Hit.And.Run == input$hit
  )
})
  
 

  
  output$mymap <- renderLeaflet({

    leaflet(data = data()) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude,
                 lat = ~Latitude)
})
  
    
    
    
    output$mytable <- DT::renderDataTable({
      DT::datatable(data = data(),
                    options = list(scrollX = TRUE)
      )  
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

