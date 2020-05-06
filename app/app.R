#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
rm(list = ls())
# When in RStudio, dynamically sets working directory to path of this script
if ("rstudioapi" %in% installed.packages()[, "Package"] & rstudioapi::isAvailable() & interactive())
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
videoGameSales <- read.csv("videogamesales.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Videogame analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
        ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- videoGameSales%>% group_by(Platform) %>% tally() %>% arrange(desc(n))
        
        ggplot(x, aes(x=Platform, y=n))+
            geom_count()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
