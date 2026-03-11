library(shiny)
library(paletteer)
library(shinydashboard)
library(tidyverse)
library(ggmap)
library(ggdensity)

#setwd("~/Desktop/BIS15L_W2026_Group-8/rbird")
total_seasons_data <- read_csv("total_data_seasons.csv")

#**setting map parameters**

register_stadiamaps("c7554538-a17b-488f-9eb4-62494d62e9e5", write = FALSE)

lat_yolo <- c(38.32, 38.93)
long_yolo <- c(-122.4, -121.5)
bbox_yolo <- make_bbox(long_yolo, lat_yolo, f=0.03)
map_yolo <- get_stadiamap(bbox_yolo, maptype = "stamen_terrain", zoom = 11)

lat_davis <- c(38.525, 38.575)
long_davis <- c(-121.678, -121.792)
bbox_davis <- make_bbox(long_davis, lat_davis, f=0.03)
map_davis <- get_stadiamap(bbox_davis, maptype = "stamen_terrain", zoom = 14)

ui <- tagList(

  tags$div(
    style = "text-align:center; margin-bottom:20px;",
    tags$img(src = "birds2.png", height = "100%", width= "100%")
    ),
  
dashboardPage(
  
  dashboardHeader(title = 
    "Explore Bird Observations"),
  
  dashboardSidebar(
    
    sidebarMenu(id="tabs",
                
      menuItem("Top Birds", tabName = "top_birds", icon = icon("feather")),
      menuItem("Hotspot Maps", tabName = "hotspot_maps", icon = icon("map"))
    ),
    
    conditionalPanel(
      condition = "input.tabs == 'top_birds'",
        radioButtons("season", "Season", 
                     choices = c("Winter", 
                                 "Spring", 
                                 "Summer", 
                                 "Fall")),
        radioButtons("year", "Year",
                     choices = c("2019", 
                                 "2020", 
                                 "2021"))
      ),
    
    conditionalPanel(
      condition = "input.tabs == 'hotspot_maps'",
        selectInput("x",
                    "Select Year",
                    choices = c("2018",
                                "2019",
                                "2020",
                                "2021",
                                "2022",
                                "2023"),
                    selected = "2018"),
        selectInput("y",
                    "Select Season",
                    choices = c("Winter",
                                "Spring",
                                "Summer",
                                "Fall"))
      )
    ), #closes dashboardSidebar

 dashboardBody(
   
   tabItems(
     tabItem(tabName = "top_birds",
             plotOutput("plot", width = "550px", height = "525px")
     ),
     tabItem(tabName = "hotspot_maps",
             fluidRow(
               column(6,
                      h3("Map 1: Yolo County"),
                      plotOutput("map1")
               ),
               column(6,
                      h3("Map 2: Davis"),
                      plotOutput("map2")
               )
             )
     )
   ) #closes tabItems
    
) #closes dashboardBody
) #closes dashboardPage
) #closes tagList


server <- function(input, output, session) {
  

  output$plot <- renderPlot({
    total_seasons_data %>% 
      filter(season == input$season, 
             year_y == as.numeric(input$year)) %>% 
      group_by(common_name) %>% 
      summarize(count = n()) %>% 
      arrange(desc(count)) %>%
      slice_head(n = 5) %>%
      ggplot(aes(x = common_name, y = count, fill = common_name)) +
      scale_fill_brewer(palette = "Set3")+
      geom_col() +
      theme_minimal() +
      labs(title = paste("Top 5 Birds in", input$season, "of", input$year), x = "Bird Species", y = "Count") +
      theme(legend.position = "none")
  
  })
  
  output$map1 <- renderPlot({
    
    dynamic_title <- paste("Density of Checklists in", input$y, "of", input$x)
    
    filtered_selection <- total_seasons_data %>%
      filter(year_y == as.numeric(input$x) & season == input$y) 
    
    ggmap(map_yolo) +
      geom_density_2d_filled(data = filtered_selection, aes(longitude_y, latitude_y), size = 1, alpha = 0.5) +
      labs(title = dynamic_title, x = "Longitude", y = "Latitude") +
      theme_minimal()
  })
  
  output$map2 <- renderPlot({
    
    dynamic_title <- paste("Density of Checklists in", input$y, "of", input$x)
    
    filtered_selection <- total_seasons_data %>%
      filter(year_y == as.nuneric(input$x) & season == input$y)
    
    ggmap(map_davis) +
      geom_density_2d_filled(data = filtered_selection, aes(longitude_y, latitude_y), size = 1, alpha = 0.5) +
      labs(title = dynamic_title, x = "Longitude", y = "Latitude") +
      theme_minimal()
    
  })
  
}

shinyApp(ui = ui, server = server, 
         # options = list(launch.browser = TRUE) for now
         )
