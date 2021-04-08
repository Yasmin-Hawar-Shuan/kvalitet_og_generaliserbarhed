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

names(coords) <- c("board_2020", "board_2021", "boardDay_2020", "boardDay_2021", "stops", "cykl", "tier", "voi")

points <- c(
  "Stoppesteder" = "stops",
  "Lånecykler" = "cykl",
  "Tier løbehjul" = "tier",
  "Voi løbehjul" = "voi"
)

years <- c(
  "none" = "none",
  "Gennemsnit påstigere 2020" = "2020_p",
  "Gennemsnit påstigere 2021" = "2021_p",
  "Gennemsnit afstigere 2020" = "2020_i",
  "Gennemsnit afstigere 2021" = "2021_i"
)

maptiler <- Sys.getenv("MAPTILER_API_KEY")
thunder <- Sys.getenv("THUNDERFOREST_API_KEY")

tiles <- c(
  "Standard" = "Standard",
  "Streets" = paste0("https://api.maptiler.com/maps/streets/{z}/{x}/{y}.png?key=", maptiler),
  "Satellite" = paste0("https://api.maptiler.com/maps/hybrid/{z}/{x}/{y}.jpg?key=", maptiler),
  "Pastel" = paste0("https://api.maptiler.com/maps/pastel/{z}/{x}/{y}.png?key=", maptiler),
  "Topo" = paste0("https://api.maptiler.com/maps/topo/{z}/{x}/{y}.png?key=", maptiler),
  "Transport" = paste0("https://tile.thunderforest.com/transport/{z}/{x}/{y}.png?apikey=", thunder),
  "Transport Dark" = paste0("https://tile.thunderforest.com/transport-dark/{z}/{x}/{y}.png?apikey=", thunder),
  "Atlas" = paste0("https://tile.thunderforest.com/atlas/{z}/{x}/{y}.png?apikey=", thunder),
  "Neighbourhood" = paste0("https://tile.thunderforest.com/neighbourhood/{z}/{x}/{y}.png?apikey=", thunder)
)

ui <- navbarPage("Fynbus",
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(
                              varSelectInput(inputId = "dropdownX", label = "X-axis", data = df),
                              varSelectInput(inputId = "dropdownY", label = "Y-axis", data = df),
                              varSelectInput(inputId = "dropdownC", label = "Colour", data = df),
                              width = 2
                          ),
                          mainPanel(
                            plotlyOutput(outputId = "chart")
                          )
                        )
                        ),
                 tabPanel("Maps", 
                          sidebarLayout(
                            sidebarPanel(checkboxGroupInput("point", "Transportmiddel", points),
                                         selectInput(inputId = "year", label = "Scaling", years),
                                         selectInput(inputId = "tile", "Kort type", tiles),
                                         width = 2
                          ),
                          mainPanel(
                            leafletOutput("map", width = "100%", height = "800px"),
                          ))
))

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
    proxy <- leafletProxy("map", data = coords)
    tile <- input$tile
    
    ifelse(tile != "Standard", proxy %>% addTiles(urlTemplate = tile, group = "custom_tiles"), proxy %>% clearGroup("custom_tiles"))

    trans <- ifelse(points %in% input$point, T, F)
    
    ifelse(trans[1] == T,
           proxy %>% 
             addCircles(
               lng = ~stops$S_LONG,
               lat = ~stops$S_LAT,
               label = ~stops$Stopnavn,
               color = "#5f9713",
               radius = 10,
               weight = 3,
               group = "stops"),
           proxy %>%  
             clearGroup("stops"))
    
    ifelse(trans[2] == T,
           proxy %>%
             addCircles(
               lng = ~cykl$long,
               lat = ~cykl$lat,
               label = ~cykl$name,
               color = "#006992",
               radius = 10,
               weight = 3,
               group = "cykl"),
           proxy %>% 
             clearGroup("cykl"))
    
    ifelse(trans[3] == T, 
   proxy %>%
      addCircles(
          lng = ~tier$long,
          lat = ~tier$lat,
          label = ~tier$name,
          color = "#00708D",
          radius = 10,
          weight = 3,
          group = "tier"), 
     proxy %>% 
        clearGroup("tier"))
    
    ifelse(trans[4] == T,
      proxy %>%
        addCircles(
          lng = ~voi$long,
          lat = ~voi$lat,
          label = ~voi$name,
          color = "#E6177A",
          radius = 10,
          weight = 3,
          group = "voi"),
      proxy %>% 
        clearGroup("voi"))
    
    scaled_points <- function(dat) {
      proxy %>% 
        clearGroup("stops") %>%
        clearGroup("stops_scaled") %>% 
        addCircles(
          lng = ~dat$S_LONG,
          lat = ~dat$S_LAT,
          popup = ~paste(dat$Stopnavn, "Gennemsnit påstigerantal: ", dat$occupancy_boarding),
          color = "#5f9713",
          radius = dat$occupancy_boarding * 15,
          weight = 3,
          group = "stops_scaled")
    }
    
    scaled_points_2 <- function(dat) {
      proxy %>% 
        clearGroup("stops") %>%
        clearGroup("stops_scaled") %>% 
        addCircles(
          lng = ~dat$S_LONG,
          lat = ~dat$S_LAT,
          popup = ~paste(dat$Stopnavn, "Gennemsnit afstigerantal: ", dat$occupancy_deboarding),
          color = "#5f9713",
          radius = dat$occupancy_deboarding * 15,
          weight = 3,
          group = "stops_scaled")
    }
    
    ifelse(input$year != "none", ifelse(substr(input$year, 6, 6) == "p", 
                                        ifelse(substr(input$year, 1, 4) == "2020", scaled_points(coords$board_2020), scaled_points(coords$board_2021)), 
                                        ifelse(substr(input$year, 1, 4) == "2020", scaled_points_2(coords$board_2020), scaled_points_2(coords$board_2021))), 
           proxy %>% 
             clearGroup("stops_scaled"))
  })
}

shinyApp(ui, server)
