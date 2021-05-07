library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(highcharter)
library(dplyr)
library(maps)
library(ggplot2)
library(janitor)
library(skimr)

# added column types to remove error message

coup_data <- read_csv(file = "Coup_Data/Coup_Data_v2.0.0.csv", col_types = cols(
  .default = col_double(),
  coup_id = col_character(),
  country = col_character(),
  month = col_character(),
  day = col_character(),
  event_type = col_character()
))

# Iremoved variables that were not relevant to creating this plot. I made sure to
# group by country before summarizing so that I could use it to plot the total
# occurrences

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
            labs(title = "Top 10 Countries with the Most Successful Coups from 
                 1945- 2019",
                 subtitle = "South and Central American countries have had the
                 most successful coups",
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
            labs(title = "Top 10 Countries with the Most Unsuccessful Coups from
                 1945- 2019",
                 subtitle = "Coups fail all over the world, but especially
                 Central & Latin America",
                 caption = "Source: Cline Center Coup D’état Project Dataset",
                 x = "Country",
                 y = "Number of Unsuccessful Coups") +
            coord_flip() +
            theme_minimal() +
            geom_col(fill = "darkolivegreen4")

# I used the same clean data that I late use for the maps here, mostly because
# this map was added on after I had completed everything else

Clean_coup <- readRDS("Coup_Data/clean_coup_data.rds")

plot_3  <- Clean_coup %>% 
            group_by(year) %>% 
            filter(realized == 1) %>% 
            ggplot(aes(x = year)) +
            geom_histogram(fill = "darkolivegreen4",
                           bins = 75,
                           binwidth = .5) +
            labs(title = "Coup Occurences from 1949 - 2019",
                 subtitle = "Coup occurrences peaked in the late 70- early '80s",
                 caption = "Source: Cline Center Coup D’état Project Dataset",
                 x = "Country",
                 y = "Number of Successful Coups") +
            theme_minimal()

#Mapping info

coup_data <- read_csv(file = "Coup_Data/Coup_Data_v2.0.0.csv")

#I cleaned the data a bit in preparation for creating these maps


# In order to use highcharter to create my maps, I needed to join a a specific
# mapping data set 
map_info <- Clean_coup %>% 
  group_by(country) %>% 
  select(country, event_type, realized) %>% 
  filter(event_type == "coup") %>% 
  rename(mapname = "country")  %>% 
  
  group_by(event_type, mapname) %>%
  summarise(n = n(),
            .groups = "drop") %>% 
  inner_join(iso3166,
             by = "mapname") %>% 
  rename("iso-a3" = a3)


map_1 <- hcmap(
  map = "custom/world-highres3", # high resolution world map
  data = map_info, # name of dataset
  joinBy = c("iso-a3"),
  name = "Coups",
  value = "n",
  showInLegend = TRUE, # hide legend
  nullColor = "#DADADA",
  download_map_data = TRUE,
  dataLabels = list(enabled = TRUE, format = "{point.country}",
                    tooltip = list(
                      valueDecimals = 2)))%>%
  hc_mapNavigation(enabled = FALSE) %>%
  hc_legend("none") %>%
  hc_title(text = "Map of Coups from 1949 - 2019") 


map_info_attempt <- coup_data %>% 
  group_by(country) %>% 
  select(country, event_type, unrealized) %>% 
  filter(event_type == "attempted") %>% 
  rename(mapname = "country") %>% 
  group_by(event_type, mapname) %>%
  summarise(n = n(),
            .groups = "drop") %>% 
  inner_join(iso3166,
             by = "mapname") %>% 
  rename("iso-a3" = a3)


map_2 <- hcmap(
  map = "custom/world-highres3", 
  data = map_info_attempt, 
  joinBy = c("iso-a3"),
  name = "Attempts",
  value = "n",
  showInLegend = TRUE, 
  nullColor = "#DADADA",
  download_map_data = TRUE,
  dataLabels = list(enabled = TRUE, format = "{point.country}",
                    tooltip = list(
                      valueDecimals = 2)))%>% 
  hc_mapNavigation(enabled = FALSE) %>%
  hc_legend("none") %>%
  hc_title(text = "Map of Coup Attempts from 1949 - 2019") 

map_info_consp <- coup_data %>% 
  group_by(country) %>% 
  select(country, event_type, unrealized) %>% 
  filter(event_type == "conspiracy") %>% 
  rename(mapname = "country") %>% 
  group_by(event_type, mapname) %>%
  summarise(n = n(),
            .groups = "drop") %>% 
  inner_join(iso3166,
             by = "mapname") %>% 
  rename("iso-a3" = a3)


map_3 <- hcmap(
  map = "custom/world-highres3", 
  data = map_info_consp, 
  joinBy = c("iso-a3"),
  name = "Conspiracies",
  value = "n",
  showInLegend = TRUE, 
  nullColor = "#DADADA",
  download_map_data = TRUE,
  dataLabels = list(enabled = TRUE, format = "{point.country}",
                    tooltip = list(
                      valueDecimals = 2)))%>% 
  hc_mapNavigation(enabled = FALSE) %>%
  hc_legend("none") %>%
  hc_title(text = "Map of Coup Conspiracies from 1949 - 2019") 


