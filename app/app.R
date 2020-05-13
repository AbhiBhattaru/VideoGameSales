#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(taRifx)
rm(list = ls())
# When in RStudio, dynamically sets working directory to path of this script
if ("rstudioapi" %in% installed.packages()[, "Package"] & rstudioapi::isAvailable() & interactive())
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
videoGameSales <- read.csv("videogamesales.csv")
platform_key<- data.frame(Platform = factor(c('2600',
                                              '3DO',
                                              '3DS',
                                              'DC',
                                              'DS',
                                              'GB',
                                              'GBA',
                                              'GC',
                                              'GEN',
                                              'GG',
                                              'N64',
                                              'NES',
                                              'NG',
                                              'PC',
                                              'PCFX',
                                              'PS',
                                              'PS2',
                                              'PS3',
                                              'PS4',
                                              'PSP',
                                              'PSV',
                                              'SAT',
                                              'SCD',
                                              'SNES',
                                              'TG16',
                                              'Wii',
                                              'WiiU',
                                              'WS',
                                              'X360',
                                              'XB',
                                              'XOne')),
                         Platform_full = factor(c("Atari 2600",
                                                  "3DO",
                                                  "Nintendo 3DS",
                                                  "Dreamcast",
                                                  "Nintendo DS",
                                                  "Gameboy",
                                                  "Game boy advanced",
                                                  "Game Cube",
                                                  "Sega Genesis",
                                                  "Game Gear",
                                                  "Nintendo 64",
                                                  "NES",
                                                  "Neo Geo",
                                                  "PC",
                                                  "PC-FX",
                                                  "Play Station",
                                                  "Play Station 2",
                                                  "Playstation 3",
                                                  "Playstation 4",
                                                  "Play Station Portable",
                                                  "Playstation Vita",
                                                  "Sega Saturn",
                                                  "Sega CD",
                                                  "SNES",
                                                  "TurboGrafx-16",
                                                  "Wii",
                                                  "WiiU",
                                                  "WonderSwan",
                                                  "Xbox 360",
                                                  "Xbox",
                                                  "Xbox One")))


videoGameSales<- videoGameSales%>%left_join(platform_key, by="Platform")%>%filter(!is.na(Platform_full))

 # Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Videogame analysis"),

    p("Video games are often a great equalizer for the world. Whether it be your favorite first person shooter game or exploring an unknown world in Minecraft, 
      there is a videogame for everyone. That is why it becomes such an intersting topic to explore! In this app we can learn a lot about recent trends of video game
      sales to see what games are popular and bring in the best acclaim"),
    
    br(),
    
    h3("Platforms"),
    
    p("To this day, numerous gaming consoles have been created by many companies, which often make newer generation consoles and continue to upgrade their games. 
      Let's see the top ten producers per platform"),
    sidebarLayout(
        sidebarPanel = sidebarPanel(
            selectInput("Consoles1", "Select a platform", choices = videoGameSales$Platform_full, selected = "Wii", multiple = F)
        ),
        mainPanel = mainPanel(
            plotOutput("ConsolePlot1")
        )
    ),
    
    br(),
    p("Another interesting thing to study is how sales vary internationally per platform"),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        selectizeInput("Consoles2", "Select up to 10 platforms", choices = videoGameSales$Platform_full, selected = "Wii", multiple = T, options=list(maxItems = 10)),
        radioButtons("Country1", "Select a region", 
                     choices = c("North America" = "NA_Sales", "Europe"="EU_Sales", "Japan"="JP_Sales", "Other"="Other_Sales"))
      ),
      mainPanel = mainPanel(
        plotOutput("ConsolePlot2")
      )
    ),
    br(),
    p("Everyone likes different types of games. Lets explore different genres of games and how they vary by console"),
    
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        selectizeInput("Consoles3", "Select up to 3 platforms", choices = videoGameSales$Platform_full, selected = "Wii", multiple = T, options=list(maxItems = 3)),
        selectizeInput("Genre1", "Select up to 8 genres", choices = videoGameSales$Genre[videoGameSales$Genre!=''], selected = "Action", multiple = T, options = list(maxItems = 8))
      ),
      mainPanel = mainPanel(
        plotOutput("ConsolePlot3")
      )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ConsolePlot1 <- renderPlot({
      req(input$Consoles1)
      graph_data<- videoGameSales%>%
        filter(Platform_full== input$Consoles1)%>%
        group_by(Publisher)%>% 
        tally()%>%
        arrange(desc(n))%>%
        top_n(10)
      ggplot(graph_data, aes(x=factor(Publisher, levels = graph_data$Publisher), y=n))+
        geom_col()
    })
    
    output$ConsolePlot2 <-renderPlot({
      req(input$Consoles2, input$Country1)
      
      graph_data<- videoGameSales%>%
        filter(Platform_full %in% input$Consoles2)%>%
        select(Platform_full,NA_Sales, EU_Sales, JP_Sales, Other_Sales)
        
      ggplot(graph_data, aes(x=Platform_full, y=unlist(graph_data[input$Country1])))+
        geom_violin()
    })
    
    output$ConsolePlot3 <-renderPlot({
      req(input$Consoles3, input$Genre1)
      
      graph_data<- videoGameSales%>%
        filter(Platform_full %in% input$Consoles3, Genre %in% input$Genre1)%>%
        select(Platform_full, Genre)%>%
        group_by(Platform_full,Genre)%>%
        tally()
      ggplot(graph_data, aes(x=Genre, y=n, fill = Platform_full))+
        geom_col(position = "dodge")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
