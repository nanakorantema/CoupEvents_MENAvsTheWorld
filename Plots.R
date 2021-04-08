library(shiny)
library(primer.data)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(highcharter)
library(dplyr)
library(maps)
library(tidyverse)
library(ggplot2)
library(janitor)

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




