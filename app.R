#### Libraries ####

library(shiny)
library(shinyjs)
library(shinyTime)
library(plotly)
library(shinyWidgets)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(viridis)
library(maps)
library(leaflet)
library(darksky)
library(shinyMatrix)
library(lubridate)

#### Setup ####

Sys.setenv(DARKSKY_API_KEY= "dc4edf99f802619773ee304daeea74ff")

df.loc <- read_excel(
    path = "Final Project Data.xlsx",
    sheet = "Locations")

df.counts <- read_excel(
    path = "Final Project Data.xlsx",
    sheet = "Counts")

df=merge(df.counts, df.loc, by="LocationID")
US <- map_data("world") %>% filter(subregion=="Washington")

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )}


ui <- dashboardPagePlus(
#### Top Bar ####
        dashboardHeaderPlus(title = "CET521 Project",enable_rightsidebar = TRUE,rightSidebarIcon = "gears"),
#### Sidebar ####     
        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
                menuItem("Survey",tabName = "survey",icon = icon("poll-h")))
            ),
#### Body ####
       dashboardBody(
            tabItems(
                #First Tab
                tabItem(tabName = "dashboard",h2("Hello World"),
                    fluidRow(
                        box(plotOutput("plot1")),
                        box(leafletOutput("plot2"))
                        )
                    ),
                #Second Tab
                tabItem(tabName = "survey",h2("foo bar"),
                    mainPanel(
                        
                        shinyjs::useShinyjs(),# state using shinyjs based on Javascript
                        
                        wellPanel(
                                   titlePanel("A Survey Demo"),
                                   div(id = "form",
                                       selectInput("location",labelMandatory("Location"),
                                                   unique(df$Intersection)),
                                       dateInput("date",labelMandatory("Date")),
                                       timeInput("time_start","Start Time:"),
                                       timeInput("time_end","End Time:"),
                                       textInput("name", labelMandatory("Name"), ""),
                                       textInput("favourite_pkg", labelMandatory("Favourite R package")),
                                       # checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
                                       sliderInput("r_num_years", "Number of years using R", 0, 10, 2, ticks = FALSE),
                                       matrixInput("turns",
                                                   value = matrix(0, 4, 3,
                                                                  dimnames = list(c("Northbound", "Southbound","Eastbound","Westbound"),
                                                                                  c("Left","Straight","Right"))),
                                                
                                                   rows=list(names=TRUE),
                                                   col=list(names=TRUE),
                                                   class = "numeric",
                                                   paste = TRUE,
                                                   copy = TRUE),
                                       actionButton("submit", "Submit", class = "btn-primary")
                                   ),
                                   shinyjs::hidden(
                                       # create an hidden div
                                       div(
                                           id = "thankyou_msg",
                                           h3("Thanks, your response was submitted successfully!"),
                                           actionLink("submit_another", "Submit another response")
                                       )
                                   )
                               )
                        )
                    )
                )),

        rightsidebar = rightSidebar(
            background = "dark",
            rightSidebarTabContent(
                id = 1,
                title = "Tab 1",
                icon = "desktop",
                active = TRUE,
                sliderInput(
                    "obs",
                    "Number of observations:",
                    min = 0, max = 1000, value = 500
                ),
                sliderInput("bins",
                            "Number of bins:",
                            min = 1,
                            max = 50,
                            value = 30),

                pickerInput(inputId = "int", 
                            label = strong("Intersection"),
                            choices = unique(df$Intersection),
                            selected = unique(df$Intersection),
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE,
                                           `selected-text-format` = "count > 1",
                                           `count-selected-text` = "{0} Intersection Selected",
                                           `live-search`=TRUE)
                            ),


                selectInput(inputId = "tclass",
                            label = "Traffic Class",
                            choices=unique(df$TrafficClass),
                            selected = unique(df$TrafficClass),
                            multiple = TRUE,
                            selectize = TRUE,
                            width = NULL,
                            size = NULL)
            ),
            rightSidebarTabContent(
                id = 2,
                title = "Tab 2",
                textInput("caption", "Caption", "Data Summary")
            ),
            rightSidebarTabContent(
                id = 3,
                icon = "paint-brush",
                title = "Tab 3",
                numericInput("obs", "Observations:", 10, min = 1, max = 100)
            )
        ),
        title = "Right Sidebar"
)


server <- function(input, output) {

    output$plot1 <- renderPlot( {
        df %>%
            filter(Intersection %in% input$int) %>%
            filter(TrafficClass %in% input$tclass) %>%
            ggplot() +
            aes(x = Latitude, y = Longitude, color = TrafficClass) +
            geom_point(size = 2L) +
            labs(x = "Age (years)", 
                 y = "Commute Time (mins)", 
                 title = "[2] Commute Time v. [43] Age")
    })

    map_data_react <- reactive({
        df %>%
            filter(Intersection %in% input$int)%>%
            filter(TrafficClass %in% input$tclass)
    })
    
    output$plot2 <- renderLeaflet({
        map_data <- map_data_react()
        
        print(nrow(map_data))
        
        if(nrow(map_data)==0) {map_data<-df}
        
        leaflet() %>% 
            addTiles()  %>% 
            addProviderTiles("Wikimedia") %>% 
            addCircleMarkers(lng = map_data$Longitude, lat = map_data$Latitude)
       
    })
    
    
    # define the header
    fieldsAll <- colnames(df)
    
    # get the system time
    timestamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS")
    
    # get the forecast
    weather <- function() {
        
        forcast_lat=df.loc$Latitude[df.loc$Intersection == input$location]
        forcast_lon=df.loc$Longitude[df.loc$Intersection == input$location]
        forcast_time=paste0(
            str_pad(hour(input$time_start),2,"left","0"),
            ":00:00")
        
        f=get_forecast_for(
            latitude = forcast_lat,
            longitude = forcast_lon,
            timestamp = paste0(input$date,"T",forcast_time))
        filtered=f$hourly[,c("time","temperature","summary")]
        indexed=filtered[which(filtered$time == paste0(input$date," ",forcast_time)),]
        print(indexed)
        return(indexed)
    }


    # gather form data into a format
    formData <- reactive({
        data <- sapply(fieldsAll, function(x) input[[x]])
        data <- c(data, timestamp = timestamp())
        data <- t(data)
        data <- as.data.frame(data)

    })
   
    # action to take when submit button is pressed
    observeEvent(input$submit, {
        print(weather())
        print(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
    })

}


# Run the application 
shinyApp(ui = ui, server = server)

