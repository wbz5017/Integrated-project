# Load R packages
library(shiny)
library(shinythemes)
library(tidyr)
library(dplyr)
library(geosphere)
library(tidyverse)
library(data.table)
library(VFS)
library(tseries)
library(ggplot2)
library(tidygeocoder)
library(reshape)

# lines = readLines("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt")
# 
# country = 'US'
# nstations = length(lines)
# 
# country_stations = NULL
# 
# for (i in seq(1, nstations)){
#   if(substr(lines[i],1,2) != country) {
#     next
#   } else {
#     line_parsed = c(substr(lines[i],1,11), substr(lines[i],13,20), substr(lines[i],22,30), substr(lines[i],32,37), substr(lines[i],39,40), substr(lines[i],42,71), substr(lines[i],73,75), substr(lines[i],77,79), substr(lines[i],81,85))
#   }
#   country_stations = rbind(country_stations, line_parsed)
# }  
# 
# df = data.table(country_stations)
# colnames(df) = c("ID", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE", "NAME", "GSN_FLAG", "HCN_CRN_FLAG", "WMO_ID")
# df$LATITUDE = as.numeric(df$LATITUDE)
# df$LONGITUDE = as.numeric(df$LONGITUDE)

df <- fread("US_stations.csv")
df <- df[,-1]

# get target stations
station_within <- function(lat, long, dist){
  df$DISTANCE_km <- acos(sin(df$LATITUDE * pi /180) * sin(lat * pi / 180) + cos(df$LATITUDE * pi /180) * cos(lat * pi / 180) * cos(df$LONGITUDE * pi / 180 - long * pi / 180)) * 6371.009
  df %>%
    filter(DISTANCE_km <= dist)%>%
    arrange(DISTANCE_km)%>%
    return()
}

# Define UI
ui <- fluidPage(
  navbarPage("GHCN Climate Data", theme = shinytheme("flatly"),
             
             tabPanel("Stations locator",
                      sidebarLayout(
                        sidebarPanel(width = 5,
                                     #span(tags$i(h6("Target US stations within x-km of the address entered")), style="color:#045a8d"),
                                     textInput(inputId = "address", label = "Address", value = "2960 Broadway, New York, NY 10027", width = '100%', placeholder = NULL),
                                     numericInput(inputId = "dis", label = "distance within -km", value = "10", min = 1, max = 100),
                                     actionButton(inputId = "stat", label = "Show target stations")
                        ),
                        mainPanel(dataTableOutput("stations"), width = 9),
                        position = c("left","right"),
                        fluid = TRUE)
             ),
             
             tabPanel("Weather Data Availiability",
                      mainPanel(h1("Weather Data Availiability"),
                                verbatimTextOutput("txtout2"))
             ),
             
             tabPanel("Weather Data Aggregation",
                      sidebarLayout(
                        sidebarPanel(width = 5,
                                     dateRangeInput(inputId = "date", label = "Date range", start = NULL, end = NULL, min = NULL, max = NULL, 
                                                    format = "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", separator = "to", width = 300),
                                     # uiOutput("date_slider"),           
                          checkboxGroupInput(inputId = "key", label = "Features:",
                                             choices = list("Precipitation (mm)" = "PRCP.VALUE", "Maximum temperature (degrees C)" = "TMAX.VALUE", "Minimum temperature (degrees C)" = "TMIN.VALUE", "Snowfall (mm)" = "SNOW.VALUE", "Snow depth (mm)" = "SNWD.VALUE", "Average cloudiness (percent)" = "ACMC.VALUE"), 
                                             selected = "PRCP.VALUE")
                          ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Weather Summary Table",column(12, div(DT::dataTableOutput("txtout3"), style = "font-size:70%"))),
                            tabPanel("Time-series Plot", plotOutput("tsplots")))
                        ),

                        position = c("left","right"),
                        fluid = TRUE)
             ),
                      
                      # sidebarPanel(
                      #   tags$h3("Input:"),
                      #   selectInput("Key1", label = "Key1:", 
                      #               choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                      #               selected = "None"),
                      #   selectInput("Key2", label = "Key2:", 
                      #               choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                      #               selected = "None"),
                      #   selectInput("Key3", label = "Key3:", 
                      #               choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                      #               selected = "None"),
                      #   selectInput("Key4", label = "Key4:", 
                      #               choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                      #               selected = "None"),
                      #   selectInput("Key5", label = "Key5:",
                      #               choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                      #               selected = "None")
                      # ),
                      
             #          "Table", column(12, div(DT::dataTableOutput("txtout3"), style = "font-size:70%"))
             # ),
             
             tabPanel("Reference",
                      actionLink(inputId = "download", label = "View Original GHCN data",class="btn btn-info")
             )
  )
)



# Define server function  
server <- function(input, output) {
  # for reference to GHCN website if click on download button
  observeEvent(input$download,{
    browseURL("https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/")
  })
  
  #datasetInput1 <- reactive({
  #temp <- data.frame(
  #  Name = c("Latitude",
  #          "Longitude",
  #          "Distance"),
  #  Value = c(input$n1,
  #            input$n2,
  #            input$n3)
  #  )
  #df <- Stations
  #df$DISTANCE_km <- acos(sin(df$LATITUDE * pi /180) * sin(input$n1 * pi / 180) + cos(df$LATITUDE * pi /180) * cos(input$n1 * pi / 180) * cos(df$LONGITUDE * pi / 180 - input$n2 * pi / 180)) * 6371.009
  #target <- arrange(filter(df, DISTANCE_km <= input$n3), DISTANCE_km)
  #
  #return(target)
  #})
  
  
  #output$txtout1 <- DT::renderDataTable({
  #    datasetInput1()
  #})
  
  
  # show the list of target stations
  statif <- eventReactive(input$stat,{
    as.character(input$address)
  })
  
  datasetInput1 <- reactive({
    lat <- as.numeric(geo(statif(), method = 'osm', lat = lat, long = long)[2])
    long <- as.numeric(geo(statif(), method = 'osm', lat = lat, long = long)[3])
    target <- station_within(lat, long, input$dis)
    return(target)
  })
  output$stations <- renderDataTable({
    datasetInput1()
  })
  
  # show the weather data availability
  datasetInput2 <- reactive({
    target <- datasetInput1()
    weather <- list()
    
    for(i in 1:nrow(target)){
      name <- paste("ghcnd_all/", target[[i,1]], ".dly", sep = "")
      weather[[i]] = cbind(ID = target[[i,1]], read.dly(name))
    }
    j = 1
    test = list()
    for(i in 1:length(weather)){
      test[j] = paste("Station ---", weather[[i]][1,1])
      j = j + 1
      for(k in 1 + 4 * (1:(length(names(weather[[i]])) / 4 - 1))){
        if(sum(!is.na(weather[[i]][k])) > 0){
          index <- which(!is.na(weather[[i]][k]))
          test[j] = paste(names(weather[[i]])[k], " ", weather[[i]][min(index), 2], "_", weather[[i]][min(index), 3], " ", weather[[i]][max(index), 2], "_", weather[[i]][max(index), 3], sep = "")
          j = j + 1
        }
      }
    }
    
    return(list(test, weather))
  })
  
  output$txtout2 <- renderPrint({
    for(i in 1:length(datasetInput2()[[1]])){
      print(datasetInput2()[[1]][[i]])
    }
  })
  
  # show the weather data aggregation table
  datasetInput3 <- reactive({
    weather <- datasetInput2()[[2]]
    target = input$key
    
    result <- data.frame(matrix(ncol=1,nrow=0, dimnames=list(NULL, c("DATE"))))
    result$DATE <- as.Date(result$DATE)
    name <- rep("DATE", 1 + 2 * length(target))
    
    for (j in target) {
      df <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("YEAR", "MONTH", "DAY"))))
      for(i in 1:length(weather)){
        df <- df %>%
          full_join(weather[[i]][(names(weather[[i]]) %in% c("YEAR", "MONTH", "DAY", j))], by = c("YEAR", "MONTH", "DAY"))
      }
      
      DATE = as.Date(paste(df$YEAR, "/", df$MONTH, "/", df$DAY, sep = ""))
      ELEMENT_AVG = rowMeans(df[,-c(1, 2, 3)], na.rm = TRUE)
      N_STATIONS = rowSums(!is.na(df[,-c(1, 2, 3)]))
      result <- result%>%
        full_join(data.table(DATE, ELEMENT_AVG, N_STATIONS), by = c("DATE"))
    }
    
    
    for (j in 1:length(target)) {
      name[2 * j] = paste(target[j], "_AVG", sep = "")
      name[1 + 2 * j] = paste(target[j], "_NSTATIONS", sep = "")
    }
    
    colnames(result) = name
    result <- data.table(result)
    
    return(result)
  })
  output$txtout3 <- DT::renderDataTable({
    datasetInput3()
  })
  
  # DATE
  output$date_slider <- renderUI({
    req(datasetInput3()$DATE)
    date_min=min(datasetInput3()$DATE)
    date_max=max(datasetInput3()$DATE)
    sliderInput("date_slider", "Date range:", min=date_min, max=date_max, value=c(date_min,date_max))
  })
  
  datasetInput4 <- reactive({
    reactive_objects <- datasetInput3()
    reactive_objects <- reactive_objects[
      DATE>=input$date[1] &
        DATE<=input$date[2]
    ]
    reactive_objects <- melt(reactive_objects[, c(1, 2 * (1:((length(datasetInput3()) - 1) / 2)))], id.vars = "DATE")
    
    pp <- ggplot(reactive_objects, aes(x = DATE, y = value, colour = variable)) + geom_line()
    return(pp)
  })
  
  output$tsplots <- renderPlot({
    datasetInput4()
  })
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
