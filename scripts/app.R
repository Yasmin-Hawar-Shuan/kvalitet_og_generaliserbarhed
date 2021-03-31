library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(leaflet)


deselect <- c("turngl", "TurtypeHelrejse", "StartStopPointNr", "SlutStopPointNr", "RejseAar", "RejseMd", "KortNr")

df <- read.csv("../2020/Helrejser_Rejsekortet_januar_2020.csv", sep = ";")[1:10000, ] %>% 
  select(-all_of(deselect))

coords <- list.files(".") %>% # setwd to a folder with data from just stops, bicycles, tier and voi scooters
  lapply(read.csv)

names(coords) <- c("stops", "cykl", "tier", "voi")

vars <- c(
  "Stoppesteder" = "stops",
  "Lånecykler" = "cykl",
  "Tier løbehjul" = "tier",
  "Voi løbehjul" = "voi"
)

ui <- navbarPage("Fynbus",
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(
                              varSelectInput(inputId = "dropdownX", label = "X-axis", data = df),
                              varSelectInput(inputId = "dropdownY", label = "Y-axis", data = df),
                              varSelectInput(inputId = "dropdownC", label = "Colour", data = df)
                          ),
                          mainPanel(
                            plotlyOutput(outputId = "chart")
                          )
                        )
                        ),
                 tabPanel("Maps", 
                          mainPanel(
                            leafletOutput("map", width = "100%", height = "800px"),
                            absolutePanel(top = 10, right = 10,
                                          checkboxGroupInput("point", "Transportmiddel", vars))
                                          # selectInput("point", "Transportmiddel", vars))
                          ))
)

server <- function(input, output) {
  output$chart <- renderPlotly({
    col_x <- sym(input$dropdownX)
    col_y <- sym(input$dropdownY)
    col_c <- sym(input$dropdownC)
    
    p <- ggplot(df, aes(x = !!col_x, y = !!col_y, color = !!col_c)) +
      geom_point() +
      labs(title = input$inputTitle)
    ggplotly(p)
  })
  
  output$map <- renderLeaflet({
    leaflet(coords) %>% 
      addTiles() %>% 
      setView(lng = 10.39, lat = 55.4, zoom = 14)
  })
  
  observe({

    trans <- ifelse(vars %in% input$point, T, F)
    
    ifelse(trans[1] == T,
           leafletProxy("map", data = coords) %>% 
             addCircles(
               lng = ~stops$S_LONG,
               lat = ~stops$S_LAT,
               label = ~stops$Stopnavn,
               color = "#5f9713",
               radius = 10,
               weight = 3,
               group = "stops"),
           leafletProxy("map", data = coords) %>% 
             clearGroup("stops"))
    
    ifelse(trans[2] == T,
           leafletProxy("map", data = coords) %>%
             addCircles(
               lng = ~cykl$long,
               lat = ~cykl$lat,
               label = ~cykl$name,
               color = "#006992",
               radius = 10,
               weight = 3,
               group = "cykl"),
           leafletProxy("map", data = coords) %>% 
             clearGroup("cykl"))
    
    ifelse(trans[3] == T, 
    leafletProxy("map", data = coords) %>%
      addCircles(
          lng = ~tier$long,
          lat = ~tier$lat,
          label = ~tier$name,
          color = "#00708D",
          radius = 10,
          weight = 3,
          group = "tier"), 
     leafletProxy("map", data = coords) %>% 
        clearGroup("tier"))
    
    ifelse(trans[4] == T,
      leafletProxy("map", data = coords) %>%
        addCircles(
          lng = ~voi$long,
          lat = ~voi$lat,
          label = ~voi$name,
          color = "#E6177A",
          radius = 10,
          weight = 3,
          group = "voi"),
      leafletProxy("map", data = coords) %>% 
        clearGroup("voi"))
  })
}

shinyApp(ui, server)
