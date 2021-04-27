library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(highcharter)
library(dplyr, warn.conflicts = FALSE)
library(maps)
library(ggplot2)
library(janitor)
library(skimr)
library(tidybayes)
library(gtsummary)
library(rstanarm)
library(Rcpp)
library(ggdist)
library(gt)
library(broom.mixed)
source("Plots.R")
source("Models.R")


# Define UI for application 
ui <- navbarPage(theme = shinytheme("lumen"),
      "Coups Events and How the Middle East Compares to the Rest of the World",
        tabPanel("Introduction", 
          fluidPage(h3("Understanding Coups"),
                    p("The Middle East and North Africa (MENA) region is known
                      for many things-- its rich oil resources, unique cultures,
                      religious significance for Abrahamic religions, and its 
                      seemingly constant tumult. In recent years, headlines 
                      reporting on the region have concentrated on the war and 
                      violence that has plagued the region. If one were to go by 
                      headlines, it would be easy to believe that the Middle East 
                      and North Africa are the most unstable region globally,
                      but is that true? While this is a big question, one of the
                      ways that one can begin to answer it is by looking at 
                      governance, in particular coup d'etats. How do Middle 
                      East and North African countries compare to the rest of 
                      the world when it comes to destabilizing activities such 
                      as coup d'etats?"),
                      p("Britannica defines a coup d'etat as 'the sudden,
                        violent overthrow of an existing government by a small 
                        group.' In this project, I use the Coup d'etat data set 
                        compiled by the Cline Center for Advanced Social Research
                        at the University of Illinois, Urbana-Champaign. 
                        The data contains 943 coup events, including 426
                        realized coups, 336 attempted coups, and 181 coup 
                        conspiracies globally between 1945 and 2019. A realized 
                        (i.e., successful) event results in the incumbent's loss 
                        of power. Unrealized events include conspiracies and 
                        attempted coups, which do not remove the targeted 
                        incumbent. A conspiracy is a plot to execute a coup that
                        is discovered and thwarted before it occurs. An 
                        attempted coup is an event when a coup plan is initiated 
                        but fails to achieve its goal. There are also a variety 
                        of methods and groups that initiate a coup event. 
                        The Cline Center's set includes the following: military 
                        coups, auto coups, palace coups, popular revolt coups, 
                        dissident coups, rebel coups, foreign-backed coups, 
                        counter-coups, and other coups that did not fit into any 
                        category."),
                        p("Looking through all of the data, it quickly became 
                          clear that since 1949, countries in South America has
                          seen the most successful coups."),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput(
                                          "plot_type",
                                          "Plot Type",
                                c("Top 10 Successful Coups" = "a", 
                                  "Top 10 Unsuccessful Coup Events" = "b", "
                                  Coup Occurrences from 1949 - 2019" = "c")
                                              )),
                                  mainPanel(plotOutput("plots"))),
                        p("You can find an experts take on instability in
                          the MENA Through this link:"),
                          a("Stability in the Middle East:
                            The Range of Short and Long-Term Causes", 
                            href = "https://www.csis.org/analysis/stability-
                            middle-east-range-short-and-long-term-causes"))),
      tabPanel("Maps",
              fluidPage(titlePanel("Mapping Coup Events"),
                p("Hover over the countries in these interactive 
                  density maps to see how many coup events have
                  occured in each country!"),
                  mainPanel(
                            highchartOutput("map_1", height = '500px'),
                            highchartOutput("map_2", height = '500px'),
                            highchartOutput("map_3", height = '500px')))),
      tabPanel("Coup Analysis",
              fluidPage(titlePanel("Coup Events Analysis"),
                h3("Comparing the Probabilities of the Success
                    of Combinations in the MENA vs. The Rest of the World "),
                p("As the last component of my project,
                  I focused on the four most popular coup
                  types in the Middle East. To do this, I
                  created a MENA variable in the data, which
                  categorized each country in the set by
                  either 'TRUE' meaning within the Middle
                  East or 'FALSE' located somewhere else
                  in the world. I then created a posterior
                  distribution that includes estimated
                  values of success for various combinations
                  for the four coup types while considering
                  their location (MENA/Non-MENA). To make
                  things interesting, I created another
                  variable combining popular and
                  foreign-backed coups, which are included
                  in the posterior distribution provided
                  estimated values."),
                p("The graph below represents a scenario
                  where neither a military nor palace coup
                  has occurred. Each line then shows the
                  predicted probabilities of coup success
                  for different combinations. This model
                  predicts that foreign-backed coups have
                  the lowest probability of success in the
                  MENA when no other coup type within this
                  predictive model occurs. It also
                  indicates that Foreign-backed coups
                  should have about a 5% - 20% probability
                  of success. Due to the spread of the
                  distribution, we can be less sure about
                  the exact value. Coups that do not fit
                  the types explored in this model are
                  least likely to succeed outside of the
                  MENA, with a probability of success
                  between 26% - 30%. Most interestingly,
                  the likelihood of coup success is always
                  greater both in and outside the MENA when
                  backed by popular resistance. According
                  to this graph, the MENA's most successful
                  coup scenario is a popular coup, with a
                  probability of 90% -100% success. While
                  this is merely one model, this does seem
                  to support the power of popular resistance."
                ),
                plotOutput("coup_plot"),
              h3("Regression Table and Data Generating Model"),
                h4("Data Generating Model"),
                  p("This is the mathematical expression
                    representing the linear regression below"),
                    fluidPage(uiOutput("equation1")),
                  p("I used the linear regression below as the
                    basis for my predictive model. This table
                    shows that being located outside the MENA
                    has a positive impact on coup success. The
                    fact that the MENA has a smaller sample
                    size may impact the results of the model."),
                    gt_output("Table_1"))), 
      tabPanel("About", 
        titlePanel("About"),
          h1("Project Background and Motivations"),
             h3("About Me"),
                p("My name is Nana-Korantema Koranteng, and I am a second-year 
                  AM candidate in Regional Studies - Middle East at Harvard 
                  University. I am interested in conflict, gender, and political
                  developments in the region. This project was inspired by the
                  frequent claims that fill Western media, which often frame the
                  Middle East as a region of constant instability. While the 
                  region has dealt with many wars and conflicts, I know that 
                  there had to be more to these stories. I settled on this coup
                  data set as coup events (coups, conspiracies, and attempts 
                  all have the potential to destabilize fundamental structures 
                  of governance in any society.)"),
                p("Contact me at: nanakorantema_koranteng@g.harvard.edu"),
              h2("Data Set"),
                p("Peyton, Buddy, Joseph Bajjalieh, Dan Shalmon, 
                  Michael Martin, and Jonathan Bonaguro. 2020. Cline Center 
                  Coup D’état Project Dataset. 
                  Cline Center for Advanced Social Research. V.2.0.0. 
                  November 16. University of Illinois Urbana-Champaign. 
                  doi: 10.13012/B2IDB-9651987_V"),
                  a("Check out my github repository to see how my shiny app works!", 
                    href = "https://github.com/nanakorantema/Final_Project")))




server <- function(input, output) {
    
output$plots <- renderPlot({
        
        if(input$plot_type == "a"){            
            plot_1
        }                                        
        else if(input$plot_type == "b"){
                plot_2
        }
        else if(input$plot_type == "c"){
                plot_3
        }
    })
    
#my interactive maps

output$map_1 <- renderHighchart({
  
 map_1  
  
})
      
        
output$map_2 <- renderHighchart({   
        
map_2      
        
 })
    
output$map_3 <- renderHighchart({
    
map_3        
            
    })

# I had a lot of trouble getting the equation to show properly, so I looked at 
#another student's project and copied her formatting

output$equation1 <- renderUI({
  withMathJax(helpText('$$ realized_i =  \\beta_1 popular_{i}
                        + \\beta_2 military_{i} + \\beta_3 foreign_{i} 
                        + \\beta_4 palace_{i} + \\beta_5 menaTRUE_{i} 
                        + \\beta_6 popular*menaTRUE_{i}
                        + \\beta_7 military*menaTRUE_{i} + \\beta_8 foreign*menaTRUE_{i} 
                        + \\beta_9 palace*menaTRUE_{i} + \\epsilon_{i} $$'))
  
})
  
output$Table_1 <-  render_gt({

  Table_1
  
                              
  })

output$coup_plot <- renderPlot({
  
coup_plot

})
}


# Run the application 
shinyApp(ui = ui, server = server)
