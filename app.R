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
            , menuItem(tabName = "data1", text = "Data Exploration", icon = icon("data"))
            , menuItem(tabName = "data2", text = "More Data Exploration", icon = icon("data"))
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
                tabName = "data1"
                , fluidRow(
                    column(width = 12
                           , plotlyOutput("time")
                    )
                )
            )
            , tabItem(
                tabName = "data2"
                , fluidRow(
                    column(width = 12
                           , plotlyOutput("island")
                    )
                )
            )
            # , tabItem(
            #     tabName = "results"
            #     , fluidRow(
            #         box(width = 12, title = "Our Findings", status = "primary"
            #             , column(width = 6
            #                      , HTML("<b> Time of Day Results </b>")
            #                      , uiOutput("aboutText")
            #                      
            #             )
            #             , column(width = 6
            #                      , HTML("<b> Island Nation Results </b>")
            #                      , uiOutput("modernPiracy")
            #             )
            #         )
            #         
            #     )
            # )
            # , tabItem(
            #     tabName = "report"
            #     , fluidRow(
            #         box(width = 12, title = "Our Findings", status = "primary"
            #             , column(width = 6
            #                      , HTML("<b> Time of Day Results </b>")
            #                      , uiOutput("helloWorld")
            # 
            #             )
            #             , column(width = 6
            #                      , HTML("<b> Island Nation Results </b>")
            #                      , uiOutput("helloWorld")
            #             )
            #         )
            # 
            #     )
            # )
        )
    )
)


# Define server logic 
server <- function(input, output) {
    # Data Import ----
    pirate <- read_rds("pirate.RDS")
    
    # Welcome ----
    output$aboutText <- renderText("For the Pirate Attack Project, we chose to look at the International Maritime Bureau’s 
                                   data on piracy world from 2015-2019, focusing on 2019. Misconceptions about modern piracy 
                                   flood our imaginations with pictures of eye patches, skull & crossbones, and scruffy men 
                                   yelling “arrrrgh”. This is not reality, however. The Pirate Attack Project seeks to dispel 
                                   these misconceptions and shed light on the trends and issues surrounding theft on the high 
                                   seas in 2020. Through interactive maps, charts, and authentic attack narrations, we explore 
                                   questions like “Are ships from island nations more likely to experience attacks?” or “What 
                                   time of day should crews be most on their guard against intruders?”.  We are intrigued as 
                                   to how the Pirate Attack Project will change our (and hopefully your) thinking about piracy.") 

    output$modernPiracy <- renderText("A partial definition according to the International Maritime Bureau says “piracy” is 
    “any illegal acts of violence or detention, or any act of depredation, committed for private ends by the crew or the 
    passengers of a private ship or a private aircraft, and directed on the high seas, against another ship or aircraft, 
    or against persons or property on board such ship or aircraft.” Modern pirates are not usually carefree adventurers
                                      looking for some treasure and a good time. Often, pirates are poor men using rafts, 
                                      old boats, and a variety of simple weapons to carry out amateur attacks. For example,
                                      when international fishing vessels began encroaching on Somali waters, depleting fish 
                                      stocks and forcing fishermen out of business, Somali pirate groups began to form. In the 
                                      Gulf of Aden, Somali pirates soon became a high-profile issue. Next, did you know that the
                                      “gold” for modern pirates is not a heavy yellow metal? Ransoms paid to recover hostage 
                                      sailors are the true modern “treasures” in the world of piracy. Sailors face this continual 
                                      threat in areas like the Gulf of Guinea, the Strait of Malacca, the Indian Ocean, and the
                                      Singapore Straits. Have you ever thought of insurance costs involved with a 21st century pirate
                                      attack? Many ships refrain from reporting incidents to avoid higher insurance costs. Several
                                      other factors influence the likelihood of piracy today, such as the flag your ship flies, the
                                      time of day, or the city where your ship is berthed.")
    
    # Map Graphics ----
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
    
    # Pirate Dataframe
    pirate_R <- reactive({
        req(input$region, input$time)
        pirate %>%
            filter(region %in% input$region) %>%
            filter(time > input$time[1] & time < input$time[2])
    })
    
    # Map 
    # Icon
    shipIcon <- makeIcon(
        iconUrl = "historic_ship.png",
        iconWidth = 30, iconHeight = 30,
        iconAnchorX = 22, iconAnchorY = 94,
    )
    
    # Leaflet
    output$map <- renderLeaflet({
        df <- pirate_R()
        df %>% 
            leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (default)") %>%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
            addMarkers(pirate$longitude, pirate$latitude, 
                       clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F)
                       , popup =  ~htmlEscape(pirate$narration)
                       , label = ~htmlEscape(pirate$ship_name)
                       , icon = shipIcon) %>%
            addLayersControl(baseGroups = c( "World Imagery (default)", "Toner Lite"),
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    #
    #time of day
    chart1 <- pirate %>% 
        ggplot(aes(x = time)) +
        geom_density(aes(color = region), binwidth = 100, boundary = 0)+
        scale_x_continuous(breaks = seq(0, 2359, by = 200)) +
        labs(title = "Attacks Per Hour", subtitle = "What time of day was a ship more likely to be attacked?", caption = "Source: International Maritime Bureau", x = "Hour", y = "Attacks") +
        theme (axis.text.x = element_text(angle = 45))
    
    
    # Radial stacked bar chart
    output$time <- renderPlotly({
        ggplotly(chart1, tooltip = "text") %>% 
            layout(title = list(text = paste0('Attacks Per Hour',
                                              '<br>',
                                              '<sup>',
                                              'What time of day was a ship more likely to be attacked?',
                                              '</sup>')))
    })
    
    #Create islands list
    islands <- c("Antigua and Barbuda", "Bahamas","Bahrain", "Barbados","Brunei","Cape Verde","Comoros","Cook Islands","Cuba","Cyprus","Dominica","Dominican Republic","East Timor","Federated States of Micronesia","Fiji","Grenada, Carriacou and Petite Martinique","Haiti","Iceland","Indonesia","Republic of Ireland","Jamaica","Japan","Kiribati","Madagascar", "Maldives","Malta","Marshall Islands","Mauritius","Nauru","New Zealand","Niue", "Northern Cyprus","Palau","Papua New Guinea","Philippines","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","São Tomé and Príncipe", "Seychelles","Singapore","Solomon Islands","Sri Lanka","Taiwan","Tonga","Trinidad and Tobago","Tuvalu","United Kingdom","Vanuatu", "Cayman Island", "Isle of Man", "Hong Kong")
    
    #add a column in pirates dataset on whether or not the nation is an island
    pirate <- pirate %>%
        mutate(typeC = case_when(
            flag %in% islands ~ "Island Nation", TRUE ~ "Mainland Nation"))
    
    #create a new df with info on frequencies of atackt for each country
    counts <- pirate %>% 
        group_by(flag) %>% 
        count(sort = TRUE)
    
    #format the frequency
    counts <- counts %>%
        mutate(frequency = (n / 163), typeC = case_when(
            flag %in% islands ~ "Island Nation", TRUE ~ "Mainland Nation") , percentage = frequency * 100)
    
    #make a plot with the info
    chart2 <- counts %>% 
        head(10) %>%
        ggplot()+
        geom_point(aes(x=reorder(flag, desc(frequency)), y = frequency, color = typeC, text = sprintf("Frequency: %.2f%% <br>Number of Ships Attacked: %.0f<br> ", percentage, n)
        ))+
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Frequency of Pirate Attacks For Island Nations Versus Mainland Nations", subtitle = "Are island nations’ ships more likely to experience attacks?", caption = "Source: International Maritime Bureau", x = "Origin Country of Ship", y = "Frequency") +
        theme(legend.title = element_blank()) +
        theme (axis.text.x = element_text(angle = 45))
    
    #make the graph plotly
    output$island <- renderPlotly({
        ggplotly(chart2, tooltip = "text") %>% 
            layout(title = list(text = paste0('Frequency of Pirate Attacks For Island Nations Versus Mainland Nations',
                                              '<br>',
                                              '<sup>',
                                              'Are island nations’ ships more likely to experience attacks?',
                                              '</sup>')))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)