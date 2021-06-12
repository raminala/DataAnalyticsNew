# Load libraries

library(shiny)
library(tidyverse)
library(tidytext)
library(glue)
library(plotly)

# import data ========================================

population <- read_csv("WPP2019_TotalPopulationBySex.csv")


# ====================================================

ppp <- population %>%
  select(Location, Time, PopTotal, Variant) %>%
  filter(Location == "Asia" | Location =="Europe"| Location =="Africa"| Location =="North America"| Location =="South America"| Location =="Oceania"| Location =="India"| Location =="United States of America"| Location =="China"| Location =="Canada"| Location == "Iran (Islamic Republic of)", Variant=="Medium") %>%
  
  # change name of some cells. "Iran (Islamic Republic of)" to "Iran"
  
  mutate(Location = recode(Location, "Iran (Islamic Republic of)"="Iran")) %>%
  
  group_by(Location)


# Plot Sentiment Chart ===============================


ggplot(ppp, aes(Time,PopTotal, color=Location))+
  geom_line()



# UI User Interface ==================================


ui <- fluidPage(
  titlePanel("Population of some countries and continents"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "location",
        label = "Select country/continent",
        choices = c("Asia", "Europe", "Africa", "South America", "Oceania", "India", "United States of America", "China", "Canada", "Iran")
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "sentiment_plot")
    )
  )
)


# Server =============================================

server <- function(input, output) {
  output$sentiment_plot <- renderPlotly({
    
    
    p <- population %>%
      mutate(Location = recode(Location, "Iran (Islamic Republic of)"="Iran")) %>%
      select(Location, Time, PopTotal, Variant) %>%
      filter(Location == input$location, Variant=="Medium") %>%
      
      group_by(Location) %>%
    
    
    ggplot(aes(x=Time,y=PopTotal))+
      geom_line()+ labs(title = "Population of country/continent in span of years")+xlab("year")+ylab("population (/1000)")
    
    
    ggplotly(p, tooltip = c("x", "y"))
    
  })
}



# create Shiny App ===================================================
shinyApp(ui = ui, server = server)





