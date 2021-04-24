library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(highcharter)
library(dplyr)
library(maps)
library(tidyverse)
library(ggplot2)
library(janitor)
library(skimr)
library(tidybayes)
library(gtsummary)
library(Rcpp)
library(rstanarm)
library(gt)
library(patchwork)
source("Plots.R")
source("Models.R")


# Define UI for application 
ui <- navbarPage(theme = shinytheme("lumen"),
                 "Coups Events and How the Middle East Compares to the Rest of the World",
                 tabPanel("Introduction", 
                          fluidPage(
                          titlePanel("Introduction"),
                          h3("Why and Where do Coups Occur?"),
                            p(" The Middle East and North Africa (MENA) are known for many things-- its rich oil resources, unique cultures, religious significance 
                            for Abrahamic religions, and its seemingly constant tumult. In recent years, headlines reporting on the region have concentrated on the war and violence that 
                            has plagued the region. If one were to merely go by headlines, it would be easy to believe that the Middle East and North Africa
                            is more unstable than other regions, but is that true? How does the Middle East and North Africa compare to the rest of the world when it comes to destablizing
                            activities such as coup d'etats?"),
                          p("Britannica defines a coup d'etat as 'the sudden, violent overthrow of an existing government by a small group'. In this project, I use the Coup 
                            d'etat data set compiled by the Cline Center for Advanced Social Research at the University of Illinois, Urabana-Champaign. The data is comprised of
                            943 coup events, including 426 realized coups, 336 attempted coups, and 181 coup conspiracies that occurred globally between 1945 and 2019.
                            A realized (ie. succesful) event results in the incumbent's loss of power. Unrealized events include conspiracies and attempted coups,
                            which do not remove the targeted incumbent. A conspiracy is defined as a plot to execute a coup that is discovered and thwarted before 
                            it can be initiated. An attempted coup is an event when a coup plan is initiated but fails to achieve its goal. There are also a variety
                            of methods and groups that initiate a coup event. The CLine Center's set includes the following : military coups, auto coups, palace coups, 
                            popular revolt coups, dissident coups,rebel coups, foreign-backed, counter coups, and other coups that do not fit other types."),
                          p("Looking through all of the data, it quickly became clear that since 1949, countries in South America has seen the most successful coups."),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(
                                  "plot_type",
                                  "Plot Type",
                                  c("Top 10 Successful Coups" = "a", "Top 10 Unsuccessful Coup Events" = "b")
                                )),
                              mainPanel(plotOutput("plots")))
                          )),
                 tabPanel("Maps",
                          fluidPage(
                          titlePanel("Mapping Event Types"),
                          p("This page includes interactive density maps that show how common each of these event types are all over the world."),
                                        mainPanel(
                                                  highchartOutput("map_1", height = '500px'),
                                                  highchartOutput("map_2", height = '500px'),
                                                  highchartOutput("map_3", height = '500px')))),
                 tabPanel("Coup Analysis",
                          fluidPage(
                          titlePanel("Coup Events Analysis"),
                          h3("Comparing the Probabilities of the Success of Specific Coup Types in the MENA vs. The Rest of the World "),
                            p("As the last component of my project I sought to compare the probability of the 4 most popular coup types in
                            the Middle East in comparison to the rest of the world. To do this, I created a MENA variable in the data which
                            categorized each country in the set by either 'TRUE' meaning within the Middle East or 'FALSE' located somewhere 
                            else in the world. I then created a regression model that captured the interactions between the MENA parameter and 
                            the selected coup types."),
                              gt_output("Table_1"),
                             p("These plots show that for the most part, these coup types do not have a higher rate of success in MENA countries
                             in comparison to the rest of the world."),
                             mainPanel(        
                                                plotOutput("mena_plots")))),
                 tabPanel("About", 
                          titlePanel("About"),
                          h1("Project Background and Motivations"),
                          h3("About Me"),
                          p("My name is Nana-Korantema Koranteng and I am a second year AM candidate in Regional Studies - Middle East at Harvard University.
                          I am interested in conflict, gender, and political developments in the region. This project was inspired by the frequent claims that
                          fill Western media which often frame the Middle East as a region of constant instability. While the region has dealt with many wars
                          and conflicts, I know that there had to be more to these stories. I settled on this coup data set as coup events (coups, conspiracies,
                          and attempts all have the potential to destabilize key structures of governance in any society.)
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

output$Table_1 <-  render_gt({

  Table_1
  
                              
  })

output$mena_plots <- renderPlot({
  
mena_plots

})
}


# Run the application 
shinyApp(ui = ui, server = server)
