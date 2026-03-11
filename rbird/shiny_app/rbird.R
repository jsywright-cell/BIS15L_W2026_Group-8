library(shiny)
library(paletteer)
library(patchwork)

setwd("~/Desktop/BIS15L_W2026_Group-8/rbird")
total_seasons_data <- read.csv("total_data_seasons.csv")

ui <- fluidPage(

titlePanel("Filter"),

    dashboardBody(
      radioButtons("x", "Season", choices = c("Winter", "Spring", "Summer", "Fall")
    ),
    
      radioButtons("y", "Year", choices = c("2019", "2020", "2021")),
    
    plotOutput("plot", width = "550px", height = "525px")
    
)
)
server <- function(input, output) {
  
  
  output$plot <- renderPlot({
    total_seasons_data %>% 
      filter(season == input$x, year_y == input$y) %>% 
      group_by(common_name) %>% 
      summarize(count = n()) %>% 
      arrange(desc(count)) %>%
      slice_head(n = 5) %>%
      ggplot(aes(x = common_name, y = count, fill = common_name)) +
      geom_col() +
      theme_minimal() +
      labs(title = paste("Top 5 Birds in", input$x, "of", input$y), x = "Bird Species", y = "Count") +
      theme(legend.position = "none")
    
  

  
  })

}

shinyApp(ui = ui, server = server, 
         # options = list(launch.browser = TRUE) for now
         )
