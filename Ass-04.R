# Load libraries

library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)

# import data ========================================

population <- read_csv("WPP2019_TotalPopulationBySex.csv")


# UI User Interface ==================================


ui <- fluidPage(
  titlePanel("This interactive plot provides population versus time in years. You could select one of five continents or one of five countries to see its population in your desired years. For selecting location (continent/country) use the slide-down menu and select years span by slider input from 1950 to 2100. Population after the last sensus in the location is a projection."),
  
  titlePanel("Data provided by United Nations website at:"),
  titlePanel("https://population.un.org/wpp2019/Download/Standard/CSV/"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        inputId = "location",
        label = "Select country/continent",
        choices = c("Asia", "Europe", "Africa", "South America", "Oceania", "India", "United States of America", "China", "Canada", "Iran")
      ),
      
      
      
      sliderInput(inputId = "year",
                  label = "Range:",
                  min = 1950, max = 2100,
                  value = c(1950,2100), step = 5),
      
    ),
    mainPanel(
      plotlyOutput(outputId = "population_plot")
    )
  )
)


# Server =============================================

server <- function(input, output) {
  output$population_plot <- renderPlotly({
    
    
    p <- population %>%
      mutate(Location = recode(Location, "Iran (Islamic Republic of)"="Iran")) %>%
      select(Location, Time, PopTotal, Variant) %>%
      filter(Location == input$location, Variant=="Medium") %>%
      
      filter(between(Time, input$year[1], input$year[2])) %>%
      
      group_by(Location) %>%
      
      
      ggplot(aes(x=Time,y=PopTotal))+
      geom_line()+ labs(title = "Population of country/continent in span of years", x= "year", y="population (/1000)")
    
    
    ggplotly(p, tooltip = c("x", "y"))
    
  })
}



# create Shiny App ==================================
shinyApp(ui = ui, server = server)





