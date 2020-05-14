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
library(gganimate)
library(gifski)
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
videoGameSales$Year_of_Release<- as.numeric(as.character(videoGameSales$Year_of_Release))

 # Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Videogame analysis"),

    p("Video games are often a great equalizer for the world. Whether it be your favorite first person shooter game or exploring an unknown world in Minecraft, 
      there is a videogame for everyone. That is why it becomes such an intersting topic to explore! In this app we can learn a lot about recent trends of video game
      sales to see what games are popular and bring in the best acclaim"),
    
    br(),
    
    
    #section 1
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
    ),
    br(),
    #section 2
    h3("Individual Games"),
    p("Though each platform has its nuances, they all share one thing in common: the games themselves. Lets look at certain games and study their performance cross-platform"),
    p("First , lets look at which games were the most successful."),
    
    verticalLayout(
      selectizeInput("GameSelect1", "Type in your favorite games to compare them. Choose up to 10", choices = subset(videoGameSales, Global_Sales>0 & Critic_Score>0, User_Score>0)$Name, selected = "Wii", multiple = T, options=list(maxItems = 10)),
      selectInput("Filter1", "Choose a sale location", choices = c("North America" = "NA_Sales", "Europe"="EU_Sales", "Japan"="JP_Sales", "Other"="Other_Sales", "Global Sales"="Global_Sales"), selected = "Global Sales"),
      mainPanel(
        plotOutput("GamePlot1")
      ),
      selectInput("Filter2", "Choose a rating type", choices = c("Critic Score" = "Critic_Score", "User Score"="User_Score"), selected = "Critic Score"),
      mainPanel(
        plotOutput("GamePlot2")
      )
    ),
    
    br(),
    h3("Time"),
    p("Perhaps one of the most interesting things to think about is how videogame sales change overtime. First, lets see how many games got released each year. Note that
      this dataset was published in 2014 and has been slightly been updated since then. Thus, the figure shows less games released in recent years but this may not be
      factually correct. Nonetheless, we will study the data"),
    verticalLayout(
      sliderInput("Year1", "Choose the years you want to study", min = 1980, max = 2016, value = c(1980,2016)),
      selectInput("Year2", "Select a genre", choices = videoGameSales$Genre[videoGameSales$Genre!=''], selected = "Action"),
      checkboxGroupInput("Year3", "Choose a sale location", choices = c("North America" = "genreNAsale", "Europe"="genreEUsale", "Japan"="genreJPsale", "Other"="genreothersale")),
      plotOutput("YearPlot1"),
      br(),
      plotOutput("YearPlot2")
    ),
    br(),
    
    h4("Concluding thoughts"),
    p("I have shown you a lot of plots to tell you about video game sales. To end things, lets look at this final plot! Here is an animation of your favorite game
      sales."),
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        selectizeInput("GameSelect1", "Type in your favorite games to compare them. Choose up to 10", choices = subset(videoGameSales, Global_Sales>0 & Critic_Score>0, User_Score>0)$Name, selected = "Wii", multiple = T, options=list(maxItems = 10)),
      ),
      mainPanel = mainPanel(
        imageOutput("Animation")
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
    
    output$GamePlot1 <- renderPlot({
      req(input$GameSelect1, input$Filter1)
      
      graph_data <- videoGameSales %>%
        filter(Name %in% input$GameSelect1)%>%
        select(Name, Platform_full, Global_Sales, NA_Sales, EU_Sales, JP_Sales, Other_Sales)%>%
        group_by(Name, Platform_full)
      
      ggplot(graph_data, aes(x=Name, y=unlist(graph_data[input$Filter1]), fill = Platform_full))+
        geom_col(position = "stack")
        
    })
    
    output$GamePlot2 <- renderPlot({
      req(input$GameSelect1, input$Filter2)
      
      graph_data <- videoGameSales %>%
        filter(Name %in% input$GameSelect1)%>%
        select(Name, Platform_full, Critic_Score, Critic_Count, User_Score, User_Count)%>%
        group_by(Name, Platform_full)
      
      ggplot(graph_data, aes(x=Name, y=unlist(graph_data[input$Filter2]), fill = Platform_full))+
        geom_col(position = "dodge")+
        geom_text(aes(label = Critic_Count), position = position_dodge(1.0))
    })
    
    output$YearPlot1 <- renderPlot({
      graph_data <- videoGameSales%>%
        filter(Year_of_Release>=input$Year1[1],Year_of_Release<=input$Year1[2])%>%
        group_by(Year_of_Release)%>%
        tally()

      ggplot(graph_data, aes(x=Year_of_Release,y=n))+
        geom_histogram(stat="identity")
    })
    
    output$YearPlot2 <- renderPlot({
      req(input$Year2, input$Year3)
      graph_data <- videoGameSales%>%
        filter(Year_of_Release>=input$Year1[1],Year_of_Release<=input$Year1[2], Genre %in% input$Year2)%>%
        group_by(Year_of_Release,Genre)%>%
        mutate(genretotsale = sum(Global_Sales), genreNAsale = sum(NA_Sales), genreEUsale = sum(EU_Sales), genreJPsale = sum(JP_Sales), genreothersale = sum(Other_Sales))%>%
        pivot_longer(cols = c("genretotsale", "genreNAsale", "genreEUsale", "genreJPsale", "genreothersale"), names_to = "Location", values_to = "Sale")%>%
        filter(Location==input$Year3 | Location == "genretotsale")
      
      ggplot(graph_data, aes(x=Year_of_Release,y=Sale, col = Location))+
        geom_line()
    })
    
    output$Animation <- renderImage({
      req(input$GameSelect1)
      
      progress <- shiny::Progress$new(max = 100)
      progress$set(message = "Rendering", value = 0)
      on.exit(progress$close())
      
      updateShinyProgress <- function(detail) {    
        progress$inc(1, detail = detail)
      }
      
      outfile <- tempfile(fileext='.gif')
      
      graph_data<- videoGameSales%>%
        filter(Name %in% input$GameSelect1)%>%
        select(Name, Platform_full, Global_Sales)
        
        
      
      p <- ggplot(graph_data, aes(x=Platform_full, y=Global_Sales))+
        geom_bar()+
        transition_states(Name)
        
      anim_save("outfile.gif", animate(p, update_progress = updateShinyProgress))
      list(src = "outfile.gif",
           contentType = 'image/gif'
      )
    })
    
    
    
      
      
}

# Run the application 
shinyApp(ui = ui, server = server)
