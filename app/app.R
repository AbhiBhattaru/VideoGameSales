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
platform_key<- data.frame(Platform = c('PS2', 'DS', 'PS3', 'Wii', 'X360', 'PSP', 'PS', 'PC', 'XB', 'GBA', 'GC', '3DS', 'PSV', 'PS4', 'N64', 'XOne', 'SNES', 'SAT', 'WiiU', '2600', 'GB', 'NES', 'DC', 'GEN', 'NG', 'SCD', 'WS', '3DO', 'TG16'),
                          Platform_full = c('Play Station 2', 'Nintendo DS', 'Playstation 3', 'Wii', 'Xbox 360', 'Play Station Portable', 'Play Station', 'PC', 'Xbox', 'Game boy advanced', 'Game Cube', 'Nintendo 3DS', 'Playstation Vita', 'Playstation 4', 'Nintendo 64', 'Xbox One', 'SNES', 'Sega Saturn', 'WiiU', 'Atari 2600', 'Gameboy', 'NES', 'Dreamcast', 'Sega Genesis', 'Neo Geo', 'Sega CD', 'WonderSwan', '3DO', 'TurboGrafx-16'))
videoGameSales<- videoGameSales%>%left_join(platform_key, by="Platform")%>%filter(!is.na(Platform_full))
videoGameSales[is.na(videoGameSales)]="Not Avaliable"

 # Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Videogame analysis"),

    p("Video games are often a great equalizer for the world. Whether it be your favorite first person shooter game or exploring an unknown world in Minecraft, 
      there is a videogame for everyone. That is why it becomes such an intersting topic to explore! In this app we can learn a lot about recent trends of video game
      sales to see what games are popular and bring in the best acclaim"),
    
    br(),
    
    h2("Video game consoles"),
    
    p("To this day, numerous gaming consoles have been created by many companies, which often make newer generation consoles and continue to upgrade their games. Lets see 
      which consoles gave the most games by producer"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            selectInput("Consoles", "Select a console", choices = videoGameSales$Platform_full, selected = "Wii")
        ),
        mainPanel = mainPanel(
            plotOutput("ConsolePlot")
        )
    ),
    
    
    
    
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

    output$ConsolePlot <- renderPlot({
        graph_data<- videoGameSales%>%
            filter(Platform_full== paste(input$Consoles))%>%
            group_by(Publisher)%>% 
            tally(n)
        ggplot(graph_data, aes(x=Publisher, y=n))+
            geom_bar()
            
    })
    
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- videoGameSales%>% group_by(Platform) %>% tally() %>% arrange(desc(n))
        
        ggplot(x, aes(x=Platform, y=n))+
            geom_count()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
