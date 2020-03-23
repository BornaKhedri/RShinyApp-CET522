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
library(viridis)
library(maps)
library(leaflet)
library(darksky)
library(shinyMatrix)
library(lubridate)
library(shinythemes)
library(bs4Dash)
library(odbc)
library(NLP)

#### Query Data ####

conn <- DBI::dbConnect(odbc::odbc(),
                       Driver   = "SQLServer",
                       Server   = "128.95.29.72",
                       Database = "Team09_W20",
                       UID      = "Username",       #insert db username here
                       PWD      = "Password",       #insert db password here
                       Port     = 1433)

ProjSurvey <- "SELECT * FROM ProjSurvey"
ProjCounts <- "SELECT * FROM ProjCounts"
ProjHelmet <- "SELECT * FROM ProjHelmet"
ProjLocations <- "SELECT * FROM ProjLocations"
ProjSurveyOD <- "SELECT * FROM ProjSurveyOD"

ProjSurvey_Data <- dbGetQuery(conn, ProjSurvey)
ProjCounts_Data <- dbGetQuery(conn, ProjCounts)
ProjHelmet_Data <- dbGetQuery(conn, ProjHelmet)
ProjLocations_Data <- dbGetQuery(conn, ProjLocations)
ProjSurveyOD_Data <- dbGetQuery(conn, ProjSurveyOD)

df=merge(ProjSurvey_Data, ProjLocations_Data, by="LocationID")
df=merge(df,ProjHelmet_Data,by="SurveyID")
df=merge(df,ProjCounts_Data,by="SurveyID")

MaxIDQuery <- "SELECT MAX(LocationID) as 'Max_LocationID' FROM ProjLocations"
MaxID <- dbGetQuery(conn, MaxIDQuery)

#### Setup ####

Sys.setenv(DARKSKY_API_KEY= "dc4edf99f802619773ee304daeea74ff")

US <- map_data("world") %>% filter(subregion=="Washington")

labelMandatory <- function(label) {
    tagList(
        label,
        span("*", class = "mandatory_star")
    )}


ui <- bs4DashPage( 
    old_school = FALSE,
    sidebar_min = TRUE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = FALSE,
    controlbar_overlay = TRUE,
    title = "CET521 Project",
    navbar = bs4DashNavbar(),
    sidebar = bs4DashSidebar(
        skin = "light",
        status = "primary",
        title = "Bicycle Survey",
        brandColor = "primary",
        elevation = 3,
        opacity = 0.8,
        bs4SidebarUserPanel(),
        bs4SidebarMenu(
            bs4SidebarHeader(""),
            bs4SidebarMenuItem(
                "Dashboard",
                tabName = "Dashboard",
                icon = "dashboard"
            ),
            bs4SidebarMenuItem(
                "Form",
                tabName = "Form",
                icon = "poll-h"
            )
        )
    ),
    controlbar = bs4DashControlbar(
        skin = "light",
        title = "My right sidebar",

        pickerInput(
            inputId = "helmet_choice",
            label = "Has Helmet?",
                    choices=c("TRUE","FALSE"),
            selected = c("TRUE","FALSE"),
            multiple = TRUE,
        ),
        pickerInput(
            inputId = "int", 
            label = strong("Intersection"),
            choices = unique(df$Intersection),
            selected = unique(df$Intersection),
                multiple = TRUE,
                options = list(`actions-box` = TRUE,
                   `selected-text-format` = "count > 1",
                   `count-selected-text` = "{0} Intersection Selected",
                   `live-search`=TRUE)
            ),
        pickerInput(
            inputId = "tclass",
            label = "Traffic Class",
            choices=unique(df$TrafficClass),
            selected = unique(df$TrafficClass),
            multiple = TRUE,
        ),
        pickerInput(
            inputId = "gender",
            label = "Gender",
            choices=c("Male","Female"),
            selected = c("Male","Female"),
            multiple = TRUE,
        )
    ),
    footer = bs4DashFooter(),
    body = bs4DashBody(
        bs4TabItems(
            bs4TabItem(tabName = "Dashboard",
               bs4Card(leafletOutput("plot1"),closable = FALSE,maximizable = TRUE,width = 500),
               bs4Card(plotOutput("plot2"),closable = FALSE,maximizable = TRUE, width = 500),
               bs4Card(plotOutput("plot3"),closable = FALSE,maximizable = TRUE, width=500)

               
           ),
           
        
            bs4TabItem(tabName = "Form",sidebar_collapsed=TRUE,controlbar_collapsed=FALSE,
                useShinyjs(),
                bs4Card(
                    inputId = "survey1",
                    title = strong("Annual Bicycle Count Survey"),
                    collapsible = FALSE,
                    collapsed = FALSE,
                    closable = FALSE,
                    column(width = 12,align="center",selectInput("location",labelMandatory("Location"),unique(df$Intersection))),
                    
                    fluidRow(
                        column(width=6,offset = 0,align="center",textInput("name", labelMandatory("Name"))),
                        
                        column(width=6,offset = 0,align="center",dateInput("date",labelMandatory("Date")))
                    ),
                    column(width=12,align='center',strong("24 Hour Time")),
                    fluidRow(
                        column(width=5,offset = 0,align="center",timeInput("time_start","Start",seconds = FALSE)),
                        column(width=5,offset = 2,align="center",timeInput("time_end","End",seconds = FALSE))
                    )
                    
                ),
                useShinyjs(),
                bs4Card(
                    inputId = "survey2",
                    title = strong("Helmet Observations"),
                    collapsible = FALSE,
                    collapsed = FALSE,
                    closable = FALSE,
                    matrixInput(
                        "helmet",
                        value = matrix(0, 2, 2,
                            dimnames = list(c("With Helmet", "Without Helmet"),c("Male","Female"))
                        ),
                        rows=list(names=TRUE),
                        col=list(names=TRUE),
                        class = "numeric",
                        paste = TRUE,
                        copy = TRUE)
                    ),
                useShinyjs(),
                bs4Card(
                    inputId = "survey3",
                    title = strong("Direction Observations"),
                    collapsible = FALSE,
                    collapsed = FALSE,
                    closable = FALSE,
                    matrixInput(
                        "turns",
                        value = matrix(0, 4, 3,
                            dimnames = list(c("North", "South","East","West"),
                            c("Left","Straight","Right"))
                            ),
                        rows=list(names=TRUE),
                        col=list(names=TRUE),
                        class = "numeric",
                        paste = TRUE,
                        copy = TRUE
                    )
                ),
                useShinyjs(),
                bs4Card(inputId="survey4",
                        title = strong("Trip Details"),
                        collapsible = TRUE,
                        collapsed = TRUE,
                        closable = FALSE,
                        textAreaInput("purpose", "Trip Purpose", value = "", width = '100%', rows = 5, resize = "both"),
                        textAreaInput("origin", "Trip Origin", value = "", width = '100%', rows = 5, resize = "both"),
                        textAreaInput("destination", "Trip Destination", value = "", width = '100%', rows = 5, resize = "both"),
                        textAreaInput("frequency", "Frequency of Bicycle Travel", value = "", width = '100%', rows = 5, resize = "both")
                       
                       ),
                useShinyjs(),# state using shinyjs based on Javascript                
                actionButton("submit", "Submit", 
                             class = "btn-primary",
                             width = "50%",
                             align="center",
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                shinyjs::hidden(
                    div(id="thank",
                    bs4Box(
                        id = "hidden",
                        title = strong("Thank you for your response"),
                        actionButton("return", "Return to Form", 
                                     class = "btn-primary",
                                     width = "50%",
                                     align="center",
                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                    )
                )
                               
                )
            )
        )
    )


server <- function(input, output) {

data_react <- reactive({
        filtered <- df %>%
            filter(Intersection %in% input$int)%>%
            filter(Helmet %in% input$helmet_choice)%>%
            filter(Gender %in% input$gender)%>%
            filter(TrafficClass %in% input$tclass)
        if(nrow(filtered)==0) {return(df)}
        else{return(filtered)}
    })
    
    output$plot1 <- renderLeaflet({
        map_data <- data_react()
        
        print(nrow(map_data))
        
        leaflet() %>% 
            addTiles()  %>% 
            addProviderTiles("Wikimedia") %>% 
            addCircleMarkers(lng = map_data$Long, lat = map_data$Lat)
       
    })
    
    output$plot2 <- renderPlot({
        plot2_data <- data_react()
        ggplot(plot2_data) +
        aes(x = Date, fill = ApproachDir, group = TurnDir, weight = TurnCounts) +
        geom_bar() +
        scale_fill_viridis_d(option = "inferno") +
        labs(x = "Months Of Bicycle Ridership", y = "Bicyclist Count", title = "Monthly Bicycle Ridership Data", caption = "All movement directions represented via group") +
        theme_gray() +
        facet_wrap(vars(ApproachDir))
    })
    
    output$plot3 <- renderPlot({
        plot3_data <- data_react()
        ggplot(plot3_data) +
            aes(x = Date, y = CountHelmet, colour = Gender, group = Helmet) +
            geom_line(size = 0.9) +
            scale_color_brewer(palette = "Dark2") +
            labs(x = "Date", y = "Helmets Worn", title = "Helmet Usage Data", caption = "Data is grouped by Date", color = "Sex") +
            theme_gray()
    })
    
    # define the header
    fieldsAll <- colnames(df)
    
    # get the system time
    timestamp <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%OS")
            
    # get the forecast
    weather <- function() {
        
        forcast_lat=df$Lat[df$Intersection == input$location]
        forcast_lon=df$Long[df$Intersection == input$location]
        forcast_time=paste0(str_pad(hour(input$time_start),2,"left","0"),":00:00")
        
        forecast=get_forecast_for(
            latitude = forcast_lat,
            longitude = forcast_lon,
            timestamp = paste0(input$date,"T",forcast_time))
        forecast=forecast$hourly
        forecast=forecast[which(
            forecast$time == paste0(input$date," ",forcast_time)
            ),]
       forecast=forecast[,c("temperature","summary")]
        print(forecast)
        return(forecast)
    }


    # gather form data into a format
    formData <- reactive({

        ProjHelmet<-data.frame(
            "Gender"=c("Male","Female","Male","Female"),
            "Helmet"=c("TRUE","TRUE","FALSE","FALSE"),
            "CountHelmet"=c(input$helmet[1,1],input$helmet[1,2],input$helmet[2,1],input$helmet[2,2])
        )
        ProjHelmet$SurveyID<-MaxID+1
    # ProjHelmet <- function() {
    #     
    #     ProjHelmet<-data.frame(
    #         "Gender"=c("Male","Female","Male","Female"),
    #         "Helmet"=c(TRUE,TRUE,FALSE,FALSE),
    #         "CountHelmet"=c(input$helmet[1,1],input$helmet[1,2],input$helmet[2,1],input$helmet[2,2])
    #     )
    #     ProjHelmet$SurveyID<-MaxID+1
    #     return(ProjHelmet)
        
        counts<-c(input$turns["North",],input$turns["South",],input$turns["East",],input$turns["West",])
        tdir<-c(rep("North",3),rep("South",3),rep("East",3),rep("West",3))
        turn<-c("Left","Straight","Right","Left","Straight","Right","Left","Straight","Right","Left","Straight","Right")
        
        
        ProjCounts<-data.frame(
            "ApproachDir"=tdir,
            "TurnDir"=turn,
            "TurnCounts"=counts
        )
        ProjCounts$SurveyID<-MaxID+1
        
        ProjSurveyOD<-data.frame(
            "Purpose"=input$purpose,
            "Origin"=input$origin,
            "Destination"=input$destination,
            "BicycleFreq"=input$frequency
        )
        ProjSurveyOD$SurveyID<-MaxID+1
        
        weather = weather()
        
        ProjSurvey<-data.frame(
            "SurveyID" = MaxID+1,
            "LocationID" =df$LocationID[df$Intersection == input$location],
            "Name" = input$name,
            "Date" = input$date,
            "StartTime" = input$time_start,
            "EndTime" = input$time_end,
            "Temperature" = weather$temperature,
            "Weather"=weather$summary,
            "Timestamp"=timestamp()
        )
        
    })
   
    # action to take when submit button is pressed
    observeEvent(input$submit, {
        #print(weather())
        shinyjs::reset(id="survey1")
        shinyjs::reset(id="survey2")
        shinyjs::reset(id="survey3")
        shinyjs::reset(id="survey4")
        shinyjs::toggle(id="survey1")
        shinyjs::toggle(id="survey2")
        shinyjs::toggle(id="survey3")
        shinyjs::toggle(id="survey4")
        shinyjs::toggle("submit")
        shinyjs::toggle("thank")
       
        # projSurveyQuery <- "INSERT INTO ProjSurvey (SurveyID, LocationID, Name, Date, StartTime, EndTime, Temperature, Weather, Timestamp)
        #                     VALUES ("+String(MaxID+1)+","+String(df$LocationID[df$Intersection == input$location])+","+String(input$name)+","+
        #                     String(input$date)+","+String(input$time_start)+","+String(input$time_end)+","+String(weather$temperature)+","+
        #                     String(weather$summary)+","+ String(timestamp())+")"
        # a<-renderText({"INSERT INTO ProjSurvey (SurveyID, LocationID, Name, Date, StartTime, EndTime, Temperature, Weather, Timestamp)
                             # VALUES ("+String(MaxID+1)+","+String(df$LocationID[df$Intersection == input$location])+","+String(input$name)+","+
                             # String(input$date)+","+String(input$time_start)+","+String(input$time_end)+","+String(weather$temperature)+","+
                             # String(weather$summary)+","+ String(timestamp())+")"})
        a <- reactive({ query = "INSERT INTO ProjSurvey (SurveyID, LocationID, Name, Date, StartTime, EndTime, Temperature, Weather, Timestamp)
                             VALUES ("+String(MaxID+1)+","+String(df$LocationID[df$Intersection == input$location])+","+String(input$name)+","+
            String(input$date)+","+String(input$time_start)+","+String(input$time_end)+","+String(weather$temperature)+","+
            String(weather$summary)+","+ String(timestamp())+")"
        dbsendquery(conn, query)})
        
        # dbSendQuery(conn, projSurveyQuery)
        
        # dbWriteTable(conn, "ProjSurvey", formData$ProjSurvey,append=TRUE)
        # dbWriteTable(conn, "ProjSurveyOD", formData$ProjSurveyOD,append=TRUE)
        # dbWriteTable(conn, "ProjCounts", formData$ProjCounts,append=TRUE)
        # dbWriteTable(conn = conn, name = "ProjHelmet", data = ProjHelmet(),append=TRUE)

    })
    
    observeEvent(input$return, {
        shinyjs::toggle(id="survey1")
        shinyjs::toggle(id="survey2")
        shinyjs::toggle(id="survey3")
        shinyjs::toggle(id="survey4")
        shinyjs::toggle("submit")
        shinyjs::toggle("thank")
    })
    

}


# Run the application 
shinyApp(ui = ui, server = server)

