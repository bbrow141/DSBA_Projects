#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
library(rsconnect)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(shinythemes)
library(RColorBrewer)
library(tidytext)
library(dplyr)
library(tidyverse)
library(janitor)
library(knitr)
library(leaflet)
library(rworldmap)
library(wordcloud2)
library(tm)
library(plotly)
library(treemap)
library(hrbrthemes)
library(viridis)
#library(d3treeR)
library(googleVis)

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

#basic data cleaning 
astronauts  %>% 
    clean_names() %>% 
    filter(!is.na(number)) %>%  # remove last row (all values are NA)
    mutate(
        sex = if_else(sex == "M", "male", "female"),
        military_civilian = if_else(military_civilian == "Mil", "military", "civilian")
    )

#nationalities to be chosen in astronauts table 
uniquevalues<-astronauts %>%
    select(nationality) %>%
    unique()

sex <-astronauts %>%
    select(sex) %>%
    unique()

#sexes in pie chart 
sex1 <- astronauts %>%
    group_by(sex)%>%
    summarise(count=n())

#split last names for word cloud
x<-sapply(strsplit(astronauts$name, ","), head, 1)

#line plot (year vs mission hrs)
line1 <- aggregate(astronauts['hours_mission'], by=astronauts['year_of_mission'], sum)

#tree map
occupation <- astronauts %>% group_by(occupation)%>% summarise(count=n())

#for vehicle type bubble chart:
astronauts$ascend_shuttle_type <- NA
astronauts$ascend_shuttle_type[grep("Vostok", astronauts$ascend_shuttle)] <- "Vostok"
astronauts$ascend_shuttle_type[grep("STS", astronauts$ascend_shuttle)] <- "Space Shuttle"
astronauts$ascend_shuttle_type[grep("Mercury-Atlas|MA", astronauts$ascend_shuttle)] <- "Mercury-Atlas"
astronauts$ascend_shuttle_type[grep("Gemini|gemini", astronauts$ascend_shuttle)] <- "Gemini"
astronauts$ascend_shuttle_type[grep("Soyuz|soyuz", astronauts$ascend_shuttle)] <- "Soyuz"
astronauts$ascend_shuttle_type[grep("Apollo|apollo", astronauts$ascend_shuttle)] <- "Apollo"
astronauts$ascend_shuttle_type[grep("Voskhod", astronauts$ascend_shuttle)] <- "Voskhod"
astronauts$ascend_shuttle_type[grep("Shenzhou", astronauts$ascend_shuttle)] <- "Shenzhou"

astronauts$ascend_shuttle_type <- as.factor(astronauts$ascend_shuttle_type)

shuttle_summary <- astronauts %>% 
    group_by(ascend_shuttle_type, ascend_shuttle) %>% 
    mutate(n = row_number()) %>%
    summarise(astronauts_on_board = max(n), 
              year = max(year_of_mission),
              missions = max(n), 
              astronauts_ferried = sum(astronauts_on_board)) %>%
    filter(ascend_shuttle_type != "NA")

# Define UI for application that draws a histogram
ui <- 
    
    dashboardPage(
        skin =c( "purple"),
        dashboardHeader(title= "Astronauts Dashboard"),
        
        #creating tabs in sidebar
        dashboardSidebar(
            sidebarMenu(
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Table", icon = icon("table"), tabName = "table"),
                menuItem("Charts", icon = icon("bar-chart-o"), tabName = "charts"),
                menuItem("Animation", tabName = "animate", icon = icon("cog"))
                
            ), #formatting headers 
            tags$head(tags$style(HTML('.main-sidebar{width: 130px;}
                                   .main-header > .navbar {margin-left: 130px;}
                                   .main-header .logo { width: 130px;}      
                                   .content-wrapper, .main-footer, 
                                   .right-side {margin-left: 130px;}')))
            
            
        ), 
        
        dashboardBody(
            tabItems(
                tabItem(tabName = "dashboard",
                        h2("Dashboard"),
                        box(
                            title = "Word Cloud", width = NULL, solidHeader = TRUE,
                            background = "light-blue"
                        ),
                        h5("This visual shows the last names of the astronouts 
                           and provides a representation of the frequency 
                           of their mission travels."),
                        fluidRow(
                            column(8,plotOutput("word1")),
                            
                            column(4,
                                   "Selections", br(),
                                   
                                   #sliderInput("freq",
                                   #           "Minimum Frequency:",
                                   #          min = 1,  max = 200, value = 15),
                                   sliderInput("max",
                                               "Maximum Number of Words:",
                                               min = 1,  max = 100,  value = 50),
                                   actionButton("freq", "Rearrange")
                              
                            )
                        ),
                        box( 
                            title = "Bubble Chart", width = NULL,solidHeader = TRUE, 
                            background= "light-blue"
                            
                        ),
                        h5("This visual shows the year versus number of missions for each ascent vehicle class, 
                           with the size of the bubbles varying based on the number of astronauts ferried."),
                        fluidRow(
                            column(10,plotOutput("BubblePlot")),
                            column(2, min = 1961, max = 2019,
                                   value = 1961, step = 1,animate = TRUE,
                                   # hr(),
                                   checkboxGroupInput("vehicle", "Choose Vehicle Class",
                                                      c("Apollo", "Gemini", "Mercury-Atlas", "Shenzhou", "Soyuz", "Space Shuttle", "Voskhod", "Vostok"),
                                                      selected= c("Apollo", "Gemini", "Mercury-Atlas", "Shenzhou", "Soyuz", "Space Shuttle", "Voskhod", "Vostok")),
                                   
                            ),
                        )
                        
                ),
                
                tabItem(tabName = "table",
                        box(
                            title = "Astronauts table", width = NULL, solidHeader = TRUE,
                            background = "light-blue"
                        ),
                        h5("This table provides an overview of the data and allows you to 
                           select certain filters to narrow or group the data."),
                        fluidRow(
                            column(9,dataTableOutput('table')),
                            column(3,selectInput("nationality","Choose a nationality:",choices = uniquevalues)),
                        ),
                        
                        
                ),
                tabItem(tabName = "charts",
                        box(
                            title = "Static Charts", width = NULL, solidHeader = TRUE,
                            background = "light-blue"
                        ),
                        h5("The tree map is used to visualize the proportions of the count of the occupations for each astronaut in the database.\n 
                            The pie chart represents the number of male versus female astronauts. "),
                        fluidRow(
                            box(title = "Tree Map",status = "info",solidheader = TRUE ,plotOutput("tree")),
                            box(title = "Pie Chart",status = "info",solidheader = TRUE ,plotOutput("pie"))
                        ),
                        h5("This interactive line chart shows the total number of mission hours completed for each year."),
                        fluidRow(
                            box(title = "Line Chart",width = 10, height = NULL,status = "info",solidheader = TRUE, plotlyOutput("line"))
                        )
                ),
                
                tabItem(tabName = "animate",
                        box(
                            title = "Animations", width = NULL, solidHeader = TRUE,
                            background = "light-blue"
                        ),
                        h5("This visual shows the birth year of each of the astronauts along with the total number of missions they have completed to date."),
                        fluidRow(
                            box( title = "Birth Year V.S. Number of Total Missions",width = 10, height = NULL, plotlyOutput("birth"))
                            
                        ),
                        h5("This visual shows mission hours for each country throughout the years."),
                        fluidRow(
                            box( title = "Mission hours by Country", width =20, height =NULL, column(8,plotOutput("HourbyCountry")),
                                 column(4, sliderInput("year", "Year:",
                                                       min = 1961, max = 2019,
                                                       value = 1960, step = 3,animate = TRUE)
                                 )
                            )
                        )
                )
                
            )
        )
    )



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$table <- renderDataTable({ 
        astronauts%>%
            select(name,year_of_selection,year_of_mission,nationality)%>%
            filter(nationality==input$nationality)
    })
    
    output$word1 <- renderPlot({
        
        wordcloud(x, data = x,
                  min.freq = input$freq, max.words=input$max,
                  grid_size = 30, sizeRange = c(20, 50),
                  width=12,height=8,
                  color = brewer.pal(8,"Dark2"), backgroundColor = "black")
        
    })
    
    output$tree <- renderPlot({
        treemap(occupation,
                index = "occupation",
                vSize="count"
                
        )
        
    })
    
    output$line <- renderPlotly({
        plot_ly(data=line1, x=line1$year_of_mission,  y = line1$hours_mission,
                type = 'scatter', mode = 'lines', legendgroup = "1" ,
                color = "orange"
        )%>%
            layout(xaxis = list(title = "Year_of_Mission"),
                   yaxis = list(title = "Total_Number_of_Mission_Hours"))
    })
    output$birth <- renderPlotly({
        plot_ly(x = astronauts$year_of_birth, y = astronauts$total_number_of_missions, #size = ~pop, 
                text = astronauts$name, hoverinfo = "text", type = 'scatter', color=astronauts$total_number_of_missions)%>%
            layout(xaxis = list(title = "Year_of_Birth"),
                   yaxis = list(title = "Total_Number_of_Missions"))%>%
            add_markers(color = "red", showlegend = FALSE, frame = astronauts$year_of_birth,
                        id = astronauts$total_number_of_missions
            ) %>%
            animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
            animation_button(
                x = 1, xanchor = "right", y = 0, yanchor = "bottom"
            ) %>%
            animation_slider(
                currentvalue = list(prefix = "YEAR ", font = list(color="red"))
            )
    })    
    
    output$HourbyCountry <-renderPlot ({
        
        p<-astronauts%>%filter(year_of_mission>=input$year,total_hrs_sum>=1000)%>%
            select(nationality,total_hrs_sum)%>%
            group_by(nationality)%>%
            summarise(total_hrs_sum=sum(total_hrs_sum))%>%
            ggplot(aes(nationality, total_hrs_sum/1000, fill = nationality)) +
            geom_bar(stat = "identity", show.legend = FALSE) +
            coord_flip()+
            labs(y='Hours(1000s)',x='')+
            theme_minimal()
        plot(p)
    })
    
    output$BubblePlot <- renderPlot({
        mission_summaries <- shuttle_summary %>% 
            group_by(ascend_shuttle_type, year) %>%
            mutate(n = row_number()) %>% 
            summarise(missions = max(n), astronauts_ferried = sum(astronauts_on_board))%>%
            filter(ascend_shuttle_type %in% input$vehicle)%>%
            ggplot(aes(x=year, y=missions, size=astronauts_ferried, color = ascend_shuttle_type)) +
            geom_point(alpha=0.5) +
            scale_fill_brewer(palette = "RdYlBu") +
            labs(x='Year',y='# of Missions')+
            scale_size(range = c(2, 11), name="Astronauts Ferried") +
            theme_ipsum()
        plot(mission_summaries)
    })
    
    output$pie <-renderPlot ({
        
        pie(sex1$count , labels = c("Female" , 'Male'))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)