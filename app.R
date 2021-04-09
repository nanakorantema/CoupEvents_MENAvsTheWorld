library(shiny)
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
        
                            ")),
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
                                      
                                    This page includes interactive density maps that show how common each of these event types are all over the world."
                                  ),
                                
                                         ),
                                        mainPanel(
                                                  highchartOutput("map_1",height = '500px'),
                                                  highchartOutput("map_2",height = '500px'),
                                                  highchartOutput("map_3",height = '500px'))),
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
    
  
  
output$map_1 <- renderHighchart({
  
  map_1  
  
})
      


        
output$map_2 <- renderHighchart({   
        
map_2      
        
    })
    
output$map_3 <- renderHighchart({
    
map_3        
            
        
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
