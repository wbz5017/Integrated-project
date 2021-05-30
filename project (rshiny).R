library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(ggmap)
library(tidygeocoder)
library(VFS)
library(tseries)

df <- fread("US_stations.csv")

# get target stations
station_within <- function(lat, long, dist){
  df$DISTANCE_km <- acos(sin(df$LATITUDE * pi /180) * sin(lat * pi / 180) + cos(df$LATITUDE * pi /180) * cos(lat * pi / 180) * cos(df$LONGITUDE * pi / 180 - long * pi / 180)) * 6371.009
  df %>%
    filter(DISTANCE_km <= dist)%>%
    arrange(DISTANCE_km)%>%
    return()
}

# get weather information
get_weather <- function(stations){
  weather <- list()
  for(i in 1:nrow(stations)){
    weather[[i]] <- cbind(ID = stations[[i,1]], read.dly(paste("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/", stations[[i,1]], ".dly", sep = "")))
  }
}


# build user interface
ui <- fluidPage(
  tags$style("#myNumericInput {font-size:25px;height:50px;}"),
  navbarPage("Climate Summary",theme = shinytheme("flatly"),
             
             tabPanel("Stations locator",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          span(tags$i(h6("Target US stations within x-km of the address entered")), style="color:#045a8d"),
                          textInput(inputId = "address", label = "Address", value = "", width = '100%', placeholder = NULL),
                          numericInput(inputId = "dis", label = "distance within -km", value = "", min = 1, max = 100),
                          actionButton(inputId = "stat", label = "Show target stations")
                        ),
                        mainPanel(dataTableOutput("stations"), width = 9),
                        position = c("left","right"),
                        fluid = TRUE)
             ),
             
             tabPanel("Climate",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          dateRangeInput(inputId = "date", label = "Date range", start = NULL, end = NULL, min = NULL, max = NULL, 
                                         format = "yyyy-mm", startview = "month", weekstart = 0, language = "en", separator = "to", width = NULL),
                          radioButtons("type", "Aggregation method:",
                                       c("Simple average" = "simavg",
                                         "Distance-weighted" = "disavg")
                                       )
                        ),
                        mainPanel(width = 9,
                          tabsetPanel(type = "tabs",
                                      tabPanel("Precipitation", plotOutput("prcp")),
                                      tabPanel("Snowfall", plotOutput("snow")),
                                      tabPanel("Snow depth", plotOutput("snwd")),
                                      tabPanel("Maximum temperature", plotOutput("tmax")),
                                      tabPanel("Minimum temperature", plotOutput("tmin")))
                        ),
                        position = c("left","right"),
                        fluid = TRUE)
             ),
             
             tabPanel("Raw data",
                      actionLink(inputId = "download", label = "Download GHCN data",class="btn btn-info"),
                      span(tags$i(h6("List of stations and their coordinated can be found by downloading 'ghcnd-stations.txt'.", style="font-size:20px"))),
                      span(tags$i(h6("Climation information for all stations can be extracted from 'ghcnd-all.tar.gz' file.", style="font-size:20px")))
             )
  )
)


# build server
server <- function(input,output){
  # open GHCN website if click on download button
  observeEvent(input$download,{
    browseURL("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/")
  })
  
  # show the list of target stations
  statif <- eventReactive(input$stat,{
    as.character(input$address)
  })
  output$stations <- renderDataTable({
      lat <- as.numeric(geo(statif(), method = 'osm', lat = lat, long = long)[2])
      long <- as.numeric(geo(statif(), method = 'osm', lat = lat, long = long)[3])
      stations <- station_within(lat, long, input$dis)
  })
  
  
}

shinyApp(ui = ui, server = server)
