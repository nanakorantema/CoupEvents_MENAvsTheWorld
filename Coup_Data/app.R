library(shiny)
library(primer.data)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(ggplot2)
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
        
                Next, I plan to use an Arab barometer data set entitled Democracy in the Middle East 
                and North Africa: Five Years after the Arab Uprisings which explores opinions towards
                democracy in selected MENA countries. Essentially, I hope to look for some type of relationship
                between coup attempts in the region and feelings toward democracy. I will deinfitely need to take
                my time to go through the codebook for this set as the raw survey data uses many codes and scales.
                The direction of my project might change (and probably will as I take a closer look at the data)
                but I am excited about using two different types of data sets that will provide a meta and micro 
                level study of a region I am deeply interested in.")),
                 tabPanel("Visualing the Data", 
                          fluidPage(
                              titlePanel("Preliminary Coup Data"),
                              sidebarLayout(
                                    sidebarPanel(
                                        selectInput(
                                                "plot_type",
                                                "Plot Type",
                                                c("Top 10 Successful" = "a", "Top 10 Unsuccessful" = "b"),
                                            )),
                                    mainPanel(plotOutput("plots")))
                          )),
                 tabPanel("About", 
                          titlePanel("About"),
                          h3("Project Background and Motivations"),
                          p("Hello, this is a shiny app that I have created for milestone #4 of my big data course"),
                          h3("About Me"),
                          p("My name is Nana-Korantema Koranteng and I study the Middle East. 
                            You can reach me at nanakorantema_koranteng@g.harvard.edu.")))


# Define server logic required to draw a histogram

server <- function(input, output) {
    
    output$plots <- renderPlot({
        
        if(input$plot_type == "a"){            
            plot_1
        }                                        
        else if(input$plot_type == "b"){
            plot_2
        }
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
