library(shiny)
library(primer.data)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(ggplot2)

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

