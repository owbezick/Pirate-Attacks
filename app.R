#
# This is a template Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Owen Bezick
# 

# Source Libraries
# Libraries 

# Shiny
library(shinydashboard)
library(shinyWidgets)
library(quanteda)
library(leaflet)
library(plotly)
library(htmltools)

# Data
library(dplyr)
library(lubridate)
library(tidyverse)

# UI ----
ui <- dashboardPage(
    dashboardHeader(title = "Pirate Attacks" 
    )
    # Sidebar ----
    , dashboardSidebar( 
        sidebarMenu(
            menuItem(tabName = "welcome", text = "Welcome", icon = icon("info")) 
            , menuItem(tabName = "dataExploration", text = "Data Exploration", icon = icon("data"))
            , menuItem(tabName = "report", text = "Report", icon = icon("data"))
        )
    )
    # Body ----
    , dashboardBody( 
        tabItems(
            # Welcome ----
            tabItem(
                tabName = "welcome"
                , fluidRow(
                    box(width = 12, title = "About our Project", status = "primary"
                        , column(width = 6
                                 , HTML("<b> About </b>")
                                 , uiOutput("aboutText")
                                 
                        )
                        , column(width = 6
                                 , HTML("<b> Modern Piracy </b>")
                                 , uiOutput("modernPiracy")
                        )
                    )
                    
                )
                , fluidRow(
                    box(width = 12, title = "Attack Narration Map: Explore the Data!", status = "primary" 
                        , fluidRow(
                            column(width = 6
                                   , HTML("<b> Filter By: </b>")
                                   , uiOutput("regionfilter")
                            )
                            , column(width = 6
                                     , uiOutput("timeFilter")
                            )
                        )
                        , box(width = 12
                              , leafletOutput("map", height = "750")
                        )
                    )
                )
            )
            # Data Viz ---- 
            , tabItem(
                tabName = "dataExploration"
                , fluidRow(
                    column(width = 6
                           , plotlyOutput("time_plotly")
                    )
                    , column(width = 6
                             , plotlyOutput("island")
                    )
                )
            )
            , tabItem(
                tabName = "report"
                , fluidRow(
                    box(width = 12, title = "Our Findings", status = "primary"
                        , column(width = 6
                                 , HTML("<b> Time of Day Results </b>")
                                 
                        )
                        , column(width = 6
                                 , HTML("<b> Island Nation Results </b>")
                        )
                    )
                    
                )
            )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    # Data Import ----
    pirate <- read_rds("df_pirate.RDS")
    
    # Welcome ----
    output$aboutText <- renderText("For the Pirate Attack Project, we chose to look at the International Maritime Bureau’s 
                                   data on piracy world from 2015-2019, focusing on 2019. Misconceptions about modern piracy 
                                   flood our imaginations with pictures of eye patches, skull & crossbones, and scruffy men 
                                   yelling “arrrrgh”. This is not reality, however. The Pirate Attack Project seeks to dispel 
                                   these misconceptions and shed light on the trends and issues surrounding theft on the high 
                                   seas in 2020. Through interactive maps, charts, and authentic attack narrations, we explore 
                                   questions like “Are ships from island nations more likely to experience attacks?” or “What 
                                   time of day should crews be most on their guard against intruders?”.  We are intrigued as to
                                   how the Pirate Attack Project will change our (and hopefully your) thinking about piracy.")
    
    output$modernPiracy <- renderText("A partial definition according to the International Maritime Bureau says “piracy” is 
    “any illegal acts of violence or detention, or any act of depredation, committed for private ends by the crew or the 
    passengers of a private ship or a private aircraft, and directed on the high seas, against another ship or aircraft, 
    or against persons or property on board such ship or aircraft.”Modern pirates are not usually carefree adventurers looking
    for some treasure and a good time. Often, pirates are poor men using rafts, old boats, and a variety of simple weapons to 
    carry out amateur attacks. For example, when international fishing vessels began encroaching on Somali waters, depleting 
    fish stocks and forcing fishermen out of business, Somali pirate groups began to form. In the Gulf of Aden, Somali pirates 
    soon became a high-profile issue. Next, did you know that the “gold” for modern pirates is not a heavy yellow metal? Ransoms 
    paid to recover hostage sailors are the true modern “treasures” in the world of piracy. Sailors face this continual threat in 
    areas like the Gulf of Guinea, the Strait of Malacca, the Indian Ocean, and the Singapore Straits. Have you ever thought of 
    insurance costs involved with a 21st century pirate attack? Many ships refrain from reporting incidents to avoid higher insurance
    costs. Several other factors influence the likelihood of piracy today, such as the flag your ship flies, the time of day, or the 
    city where your ship is berthed.")
    
    # MAP
    # DATA
    # List of regions
    ls_region <- unique(pirate$region)
    
    # Filter
    output$regionfilter <- renderUI({
        pickerInput(inputId = "region", label ="Region", choices = ls_region, multiple = T, selected = ls_region, options = list(`actions-box` = TRUE))
    })
    
    # Filter
    output$timeFilter <- renderUI({
        sliderInput("time", "Hour of Day:", min = 0, max = 2400, value = c(0,2400), step = 100)
    })
    
    # Reactive Dataframe
    pirate_R <- reactive({
        req(input$region, input$time)
        pirate %>%
            filter(region %in% input$region) %>%
            filter(time > input$time[1] & time < input$time[2])
    })
    
    # Viz
    # Icon
    shipIcon <- makeIcon(
        iconUrl = "historic_ship.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 22, iconAnchorY = 94,
    )
    
    #TODO  add "ship_name" and "flag" variables in the popup. still gotta figure out how to. 
    # Have the ship_name as a label right now.
    # Leaflet
    output$map <- renderLeaflet({
        df <- pirate_R()
        df %>% 
            leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (default)") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            addMarkers(pirate$longitude, pirate$latitude, 
                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F)
                       , popup =  paste0(pirate$ship_name
                                         , "<br>"
                                         , pirate$flag
                                         , "<br>"
                                         , pirate$narration, "<br>")
                       , label = ~htmlEscape(pirate$ship_name)
                       , icon = shipIcon) %>%
            addLayersControl(baseGroups = c( "World Imagery (default)", "Toner Lite"),
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    # Data Exploration ----
    # Time Graph
    # Plot
    time_plot <- pirate %>% 
        ggplot(aes(x = time)) +
        geom_density(aes(color = region)
                     , binwidth = 100
                     , boundary = 0)+
        scale_x_continuous(breaks = seq(0, 2359, by = 200)) +
        labs(title = "Attacks Per Hour"
             , subtitle = "What time of day was a ship more likely to be attacked?"
             , caption = "Source: International Maritime Bureau"
             , x = "Hour"
             , y = "Attacks") +
        theme(axis.text.x = element_text(angle = 45))
    
    # Plotly
    output$time_plotly <- renderPlotly({
        ggplotly(time_plot, tooltip = "text") %>% 
            layout(title = list(text = paste0('Attacks Per Hour',
                                              '<br>',
                                              '<sup>',
                                              'What time of day was a ship more likely to be attacked?',
                                              '</sup>')))
    })
    
    # Island Graph
    # Plot
    island_plot <- pirate %>% 
        group_by(flag) %>% 
        count(sort = TRUE) %>%
        mutate(frequency = (n / 163)
               , typeC = case_when(
                   flag %in% islands ~ "Island Nation", TRUE ~ "Mainland Nation")
               , percentage = frequency * 100) %>% 
        head(10) %>%
        ggplot()+
        geom_point(aes(x=reorder(flag, desc(frequency)), y = frequency, color = typeC, text = sprintf("Frequency: %.2f%% <br>Number of Ships Attacked: %.0f<br> ", percentage, n)
        )
        ) +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Frequency of Pirate Attacks For Island Nations Versus Mainland Nations", subtitle = "Are island nations’ ships more likely to experience attacks?", caption = "Source: International Maritime Bureau", x = "Origin Country of Ship", y = "Frequency") +
        theme(legend.title = element_blank()) +
        theme (axis.text.x = element_text(angle = 45)
        )
    
    # Plotly
    output$island <- renderPlotly({
        ggplotly(island_plot, tooltip = "text") %>% 
            layout(title = list(text = paste0('Frequency of Pirate Attacks For Island Nations Versus Mainland Nations',
                                              '<br>',
                                              '<sup>',
                                              'Are island nations’ ships more likely to experience attacks?',
                                              '</sup>')
            )
            )
    })
    # Report Server
    
}

# Run the application 
shinyApp(ui = ui, server = server)