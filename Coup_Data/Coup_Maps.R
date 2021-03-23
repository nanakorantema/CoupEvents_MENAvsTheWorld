library(janitor)
library(rworldmap)
library(tidyverse)

coup_data <- read_csv(file = "Coup_Data_v2.0.0.csv")

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