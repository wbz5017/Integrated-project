
# Load R packages
library(shiny)
library(shinythemes)
library(tidyr)
library(geosphere)
library(tidyverse)
library(data.table)
library(VFS)
library(tseries)
library(ggplot2)

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
Stations <- fread("US_stations.csv")
Stations <- Stations[,-1]

  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "GHCN Weather Data",
      tabPanel("Stations",
               sidebarPanel(
                 tags$h3("Input:"),
                 numericInput("n1", "Latitude:", 40.8, min = -90, max = 90),
                 numericInput("n2", "Longitude:", -73.96174, min = -180, max = 180),
                 numericInput("n3", "Distance:", 10, min = 0, max = 20),
                 
               ), # sidebarPanel
               tabsetPanel(id="ui_tab",
                           tabPanel("Map",
                                    column(12,h4("Click a site"))
                           ),
                           tabPanel("Table",
                                    column(12, div(DT::dataTableOutput("txtout1"), style = "font-size:70%"))
                           )
               )
               
      ), # Navbar 1, tabPanel
      tabPanel("Weather Data Availiability", 
               mainPanel(h1("Weather Data Availiability"),
                         verbatimTextOutput("txtout2"),
                         )),
      tabPanel("Aggregation",
        sidebarPanel(
        tags$h3("Input:"),
        selectInput("Key1", label = "Key1:", 
                    choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                    selected = "None"),
        selectInput("Key2", label = "Key2:", 
                    choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                    selected = "None"),
        selectInput("Key3", label = "Key3:", 
                    choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                    selected = "None"),
        selectInput("Key4", label = "Key4:", 
                    choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                    selected = "None"),
        selectInput("Key5", label = "Key5:", 
                    choices = list("None" = "None", "PRCP.VALUE" = "PRCP.VALUE", "TMAX.VALUE" = "TMAX.VALUE", "TMIN.VALUE" = "TMIN.VALUE", "SNOW.VALUE" = "SNOW.VALUE", "SNWD.VALUE" = "SNWD.VALUE"), 
                    selected = "None"),
      ),
      
      "Table", column(12, div(DT::dataTableOutput("txtout3"), style = "font-size:70%")))
  
    ) # navbarPage
  ) # fluidPage

  
  
  # Define server function  
  server <- function(input, output) {
    datasetInput1 <- reactive({
    temp <- data.frame(
      Name = c("Latitude",
               "Longitude",
               "Distance"),
      Value = c(input$n1,
                input$n2,
                input$n3)
      )
    df <- Stations
    df$DISTANCE_km <- acos(sin(df$LATITUDE * pi /180) * sin(input$n1 * pi / 180) + cos(df$LATITUDE * pi /180) * cos(input$n1 * pi / 180) * cos(df$LONGITUDE * pi / 180 - input$n2 * pi / 180)) * 6371.009
    target <- arrange(filter(df, DISTANCE_km <= input$n3), DISTANCE_km)
    
    return(target)
    })
    
    
    output$txtout1 <- DT::renderDataTable({
        datasetInput1()
    })
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
    
    datasetInput3 <- reactive({
      weather <- datasetInput2()[[2]]
      target = as.character(c(input$Key1, input$Key2, input$Key3, input$Key4, input$Key5))
      target = target[target != "None"]
      
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
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
