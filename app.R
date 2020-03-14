#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
library(owmr)

Sys.setenv(OWM_API_KEY = "3569a7591bf810f312c81a5ba06b78ea")

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

# Define UI for application that draws a histogram
ui <- dashboardPagePlus(
        header = dashboardHeaderPlus(
            enable_rightsidebar = TRUE,
             rightSidebarIcon = "gears"
            ),
        sidebar = dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", 
                         tabName = "dashboard", 
                         icon = icon("dashboard")),
                menuItem("Survey", 
                         tabName = "survey", 
                         icon = icon("th"))
            )
        ),
        body = dashboardBody(
            tabItems(
                tabItem(
                    tabName = "dashboard",
                    h2("Hello World"),
                    fluidRow(box(plotOutput("plot1"))),
                    fluidRow(box(leafletOutput("plot2")))
                    ),
                tabItem(
                    tabName = "survey",
                    h2("foo bar"),
                    fluidRow(
                        # state using shinyjs based on Javascript
                        shinyjs::useShinyjs(),
                        
                        column(6,
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
                ))),

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

                selectInput(inputId = "int", label = strong("Intersection"),
                            choices = unique(df$Intersection),
                            selected = "Travel",
                            multiple = TRUE),


                selectInput(inputId = "tclass",
                            label = "Traffic Class",
                            choices=unique(df$TrafficClass),
                            selected = NULL,
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


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot1 <- renderPlot( {
        # generate bins bed on input$bins from ui.R
        
        # x    <- df.loc$Latitude[df.loc$Intersection==input$type]
        # y    <- df.loc$Longitude[df.loc$Intersection==input$type]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',ylim = 0)
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

    
    output$plot2 <- renderLeaflet( {
        # generate bins bed on input$bins from ui.R
        
        # x    <- df.loc$Latitude[df.loc$Intersection==input$type]
        # y    <- df.loc$Longitude[df.loc$Intersection==input$type]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',ylim = 0)
          leaflet(df %>%
                filter(Intersection %in% input$int) %>%
                filter(TrafficClass %in% input$tclass)) %>% 
            addTiles()  %>% 
            addProviderTiles("Esri.WorldImagery") %>%
            addCircleMarkers(~Longitude, ~Latitude)
    })
    
    # define the header
    fieldsAll <- colnames(df)
    
    # get the system time
    timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS")
    
    # get the forecast
    weather <- function() {
        forcast_lat=df.loc$Latitude[df.loc$Intersection == input$location]
        forcast_lon=df.loc$Longitude[df.loc$Intersection == input$location]
        f=get_current(
            lat=forcast_lat,
            lon=forcast_lon,
            units="Imperial")
        t<-paste(round(f$main$temp,0),"°F",f$weather$main)
        return(t)
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

