library(shiny)
library(primer.data)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(ggplot2)
library(janitor)
library(rworldmap)

coup_data <- read_csv(file = "Coup_Data_v2.0.0.csv")

top_10_coups <- coup_data %>% 
  group_by(country) %>% 
  filter(event_type == "coup") %>% 
  select(country, event_type, realized) %>% 
  summarise(total_coup = sum(realized)) %>% 
  arrange(desc(total_coup)) %>% 
  slice(1:10) 

plot_1 <- top_10_coups %>% 
  ggplot(aes(x = fct_reorder(country, total_coup),
             y = total_coup)) +
  labs(title = "Top 10 Countries with the Most Successful Coups from 1945- 2019",
       subtitle = "South and Central American countries have had the most successful coups",
       caption = "Source: Cline Center Coup D’état Project Dataset",
       x = "Country",
       y = "Number of Successful Coups") +
  coord_flip() +
  theme_minimal() +
  geom_col(fill = "darkolivegreen4")

attempted_coups <-coup_data %>% 
  group_by(country) %>% 
  filter(attempt == 1) %>% 
  select(country, attempt) %>% 
  summarise(failed_attempts = sum(attempt)) %>% 
  arrange(desc(failed_attempts)) %>% 
  slice(1:10) 

plot_2 <- attempted_coups %>% 
  ggplot(aes(x = fct_reorder(country, failed_attempts),
             y = failed_attempts)) +
  labs(title = "Top 10 Countries with the Most Unsuccessful Coups from 1945- 2019",
       subtitle = "Coups fail all over the world",
       caption = "Source: Cline Center Coup D’état Project Dataset",
       x = "Country",
       y = "Number of Unsuccessful Coups") +
  coord_flip() +
  theme_minimal() +
  geom_col(fill = "darkolivegreen4")

#Mapping info

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

thecoupMap <- mapCountryData( joined_data, nameColumnToPlot="Coups",
                              missingCountryCol='dark grey',
                              oceanCol="light blue",
                              addLegend=TRUE )
mtext("[Grey Color: No Data Available]",side=1,line=-1)


grouped_attempted <- Clean_coup %>% 
  group_by(country, event_type) %>% 
  summarise(Attempts = n()) %>% 
  filter(event_type == "attempted")


joined_data <-joinCountryData2Map(grouped_attempted,
                                  joinCode = "NAME",
                                  nameJoinColumn = "country")

theattemptedMap <- mapCountryData( joined_data, nameColumnToPlot="Attempts",
                                   missingCountryCol='dark grey',
                                   oceanCol="light blue",
                                   addLegend=TRUE )
mtext("[Grey Color: No Data Available]",side=1,line=-1)


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


