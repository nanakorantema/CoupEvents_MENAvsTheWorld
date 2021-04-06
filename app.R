library(shiny)
library(primer.data)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(ggplot2)
library(janitor)
library(rworldmap)
library(highcharter)
library(dplyr)
library(maps)
source("Plots.R")
source("Coup_Maps.R")

# Define UI for application 
ui <- navbarPage(theme = shinytheme("lumen"),
                 "Coups and Democracy in the Middle East",
                 tabPanel("Discussion",
                          titlePanel("Project Data"),
                          h3("Sourcing My Data"),
                          p("For my project I plan to look at two data sets. I will first look at coup data collected 
                            by The Cline Center for Advanced Social Resaerch. Their data set includes information
                            on coups, attempted coups, and coup plots/conspiracies in 136 countries from 1945-2019.
                            that data specifies the type of actor(s) who initiated the coup plots and information 
                            regarding what happened to the deposed leader in each case. Being that the so called 
                            Arab Spring has served as a catalyst for civil resistance and political reform in the Middle
                            East, I am hoping to use this data to compare coup attempts in the region and the 
                            rest of the world. 
        
                            Next, I plan to use an Arab barometer data set entitled Democracy in the Middle East 
                            and North Africa: Five Years after the Arab Uprisings which explores opinions towards
                            democracy in selected MENA countries. Essentially, I hope to look for some type of relationship
                            between coup attempts in the region and feelings toward democracy. I will deinfitely need to take
                            my time to go through the codebook for this set as the raw survey data uses many codes and scales.
                            The direction of my project might change (and probably will as I take a closer look at the data)
                            but I am excited about using two different types of data sets that will provide a meta and micro 
                            level study of a region I am deeply interested in.")),
                 tabPanel("Graphs", 
                          fluidPage(
                              titlePanel("Preliminary Coup Data"),
                              sidebarLayout(
                                    sidebarPanel(
                                        selectInput(
                                                "plot_type",
                                                "Plot Type",
                                                c("Top 10 Successful" = "a", "Top 10 Unsuccessful" = "b")
                                            )),
                                    mainPanel(plotOutput("plots")))
                          )),
                 tabPanel("Maps",
                          fluidPage(
                                    titlePanel("Mapping Event Types"),
                                    h3("Understanding the Event Types"),
                                    p("Coups are a definitive event in the history of a nation-state. In this data set, it is best to understand
                                    the included events as being part of two key categories: realized and unrealized. Coups are the only 
                                    events that are realized (ie. succesful), meaning that it results in the incumbent's loss of power. Unrealized events 
                                    include conspiracies and attempted coups which do not remove the targeted incumbent. A conspiracy is defined as a plot 
                                    to execute a coup that is discovered and thwarted before it can be initiated. An attempted coup is an event when a coup plan
                                    is initiated but fails to achieve its goal.
                                      
                                    This page includes density maps that show how common each of these event types are all over the world.
                                      
                                      I am also trying to create a highcharter map but cannot integrate it into my shinyapp just yet."
                                  ),
                                
                                         ),
                                        mainPanel(plotOutput("map1", height="560px", width="950px"),
                                                  highchartOutput("map_1",height = '500px'),
                                                  plotOutput("map2", height="560px", width="950px"),
                                                  plotOutput("map3", height="560px", width="950px"))),
                 tabPanel("Coup Predictor", 
                          titlePanel("Coup Predictor"),
                          h3("Plan for this page"),
                          p("As the last component of my project I hope to create a predictive model for the coup data based on parameters
                            in the set. I would like users to be able to select a country and type of coup ( Military, dissident, rebel, palace,
                            Foreign-backed, Auto, popular revolt, forced resignations). I think I would need to use posterior_epred ")),
                 tabPanel("About", 
                          titlePanel("About"),
                          h3("Project Background and Motivations"),
                          p("Hello, this is a shiny app that I have created for milestone # of my big data course"),
                          h3("About Me"),
                          p("My name is Nana-Korantema Koranteng and I study the Middle East. 
                            You can reach me at nanakorantema_koranteng@g.harvard.edu.")))




server <- function(input, output) {
    
    output$plots <- renderPlot({
        
        if(input$plot_type == "a"){            
            plot_1
        }                                        
        else if(input$plot_type == "b"){
            plot_2
        }
    })
    
    output$map1 <- renderPlot({
        
            
            Clean_coup <- coup_data %>% 
                select( - c(realized, unrealized, conspiracy, attempt, coup_id)) %>% 
                group_by(year) %>% 
                arrange(desc(year))
            
            
            grouped_coup <- Clean_coup %>% 
                group_by(country, event_type) %>% 
                summarise(Coups = n()) %>% 
                filter(event_type == "coup")
            
            
            joined_data <-joinCountryData2Map(grouped_coup,
                                              joinCode = "NAME",
                                              nameJoinColumn = "country")
            
            thecoupMap <- mapPolys( joined_data, nameColumnToPlot="Coups",
                                          missingCountryCol='dark grey',
                                          oceanCol="light blue",
                                          addLegend=TRUE )
            mtext("[Grey Color: No Data Available]",side=1,line=-1)
    })
  
    output$map_1 <- renderHighchart(
      
      
        map_1 <- hcmap(
       map = "custom/world-highres3", # high resolution world map
       data = map_info, # name of dataset
       joinBy = c("iso-a3"),
       name = "coups",
       value = "n",
       showInLegend = TRUE, # hide legend
       nullColor = "#DADADA",
       download_map_data = TRUE,
       dataLabels = list(enabled = TRUE, format = "{point.country}",
       tooltip = list(
       valueDecimals = 2)))%>%
       hc_mapNavigation(enabled = FALSE) %>%
       hc_legend("none") %>%
       hc_title(text = "Coup Map from 1949 - 2019"))

        
    output$map2 <- renderPlot({   
        
      
           
           Clean_coup <- coup_data %>% 
               select( - c(realized, unrealized, conspiracy, attempt, coup_id)) %>% 
               group_by(year) %>% 
               arrange(desc(year))
        
            grouped_attempted <- Clean_coup %>% 
                group_by(country, event_type) %>% 
                summarise(Attempts = n()) %>% 
                filter(event_type == "attempted")
            
            
            joined_data <-joinCountryData2Map(grouped_attempted,
                                              joinCode = "NAME",
                                              nameJoinColumn = "country")
            
            theattemptedMap <- mapPolys(joined_data, nameColumnToPlot="Attempts",
                                               missingCountryCol='dark grey',
                                               oceanCol="light blue",
                                               addLegend=TRUE )
            mtext("[Grey Color: No Data Available]",side=1,line=-1)  
        
        
    })
    
    output$map3 <- renderPlot({
    
            
            Clean_coup <- coup_data %>% 
                select( - c(realized, unrealized, conspiracy, attempt, coup_id)) %>% 
                group_by(year) %>% 
                arrange(desc(year))
            
            grouped_conspiracy <- Clean_coup %>% 
                group_by(country, event_type) %>% 
                summarise(Conspiracies = n()) %>% 
                filter(event_type == "conspiracy")
            
            
            joined_data <-joinCountryData2Map(grouped_conspiracy,
                                              joinCode = "NAME",
                                              nameJoinColumn = "country")
            
            theconspiracyMap <- mapCountryData( joined_data, 
                                                nameColumnToPlot="Conspiracies",
                                                missingCountryCol='dark grey',
                                                oceanCol="light blue",
                                                addLegend=TRUE )
            mtext("[Grey Color: No Data Available]",side=1,line=-1)
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
