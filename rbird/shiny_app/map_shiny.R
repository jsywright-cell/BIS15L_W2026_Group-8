library(shiny)
library(tidyverse)
library(shinydashboard)
library(ggmap)
library(ggdensity)
library(shinydashboard)

total_seasons_data <- read_csv("total_data_seasons.csv")

register_stadiamaps("c7554538-a17b-488f-9eb4-62494d62e9e5", write = FALSE)

lat_yolo <- c(38.32, 38.93)
long_yolo <- c(-122.4, -121.5)
bbox_yolo <- make_bbox(long_yolo, lat_yolo, f=0.03)
map_yolo <- get_stadiamap(bbox_yolo, maptype = "stamen_terrain", zoom = 11)

lat_davis <- c(38.525, 38.575)
long_davis <- c(-121.678, -121.792)
bbox_davis <- make_bbox(long_davis, lat_davis, f=0.03)
map_davis <- get_stadiamap(bbox_davis, maptype = "stamen_terrain", zoom = 14)

ui <- fluidPage(
  
  dashboardHeader(title="Map of Yolo County and Davis by Season and Year", titleWidth = 500),
  
  dashboardSidebar(disable=T),
  
  dashboardBody(
    
    selectInput("x",
                "Select Year",
                choices=c("2018",
                          "2019",
                          "2020",
                          "2021",
                          "2022",
                          "2023"),
                selected="2018"),
    
    selectInput("y",
                "Select Season",
                choices=c("Winter",
                          "Spring",
                          "Summer",
                          "Fall"),
                selected="Winter"),
    
    fluidRow(
      # Left Map (width 6 out of 12 columns)
      column(6,
             h3("Map 1: Yolo County"),
             plotOutput("map1")
      ),
      
      # Right Map (width 6 out of 12 columns)
      column(6,
             h3("Map 2: Davis"),
             plotOutput("map2")
      )
    )))





server <- function(input, output, session) {
  
  
  
  output$map1 <- renderPlot({
    
    dynamic_title <- paste("Density of Checklists in", input$y, "of", input$x)
    
    filtered_selection <- total_seasons_data %>%
      filter(year_y==input$x & season==input$y) 
    
    ggmap(map_yolo) +
      geom_density_2d_filled(data=filtered_selection, aes(longitude_y, latitude_y), size=1, alpha=0.5)+
      labs(title = dynamic_title, x="Longitude", y="Latitude")+
      theme(
        axis.title.x = element_text(vjust = -1), # Negative vjust moves it down/outward
        axis.title.y = element_text(vjust = 3)  # Positive vjust moves it up/outward
      )})
  
  output$map2 <- renderPlot({
    
    dynamic_title <- paste("Density of Checklists in", input$y, "of", input$x)
    
    filtered_selection <- total_seasons_data %>%
      filter(year_y==input$x & season==input$y)
    
    ggmap(map_davis) +
      geom_density_2d_filled(data=filtered_selection, aes(longitude_y, latitude_y), size=1, alpha=0.5)+
      labs(title = dynamic_title, x="Longitude", y="Latitude")+
      theme(
        axis.title.x = element_text(vjust = -1), # Negative vjust moves it down/outward
        axis.title.y = element_text(vjust = 3)  # Positive vjust moves it up/outward
      ) 
    
    
  })
  
  
  
}





shinyApp(ui, server)