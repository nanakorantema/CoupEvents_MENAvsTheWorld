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
       subtitle = "South and Central American countires have had the most successful coups",
       caption = "Source: Cline Center Coup D’état Project Dataset",
       x = "Country",
       y = "Number of Successful Coups") +
  coord_flip() +
  theme_minimal() +
  geom_col(fill = "darkolivegreen4")

