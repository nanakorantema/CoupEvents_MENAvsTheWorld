library(tidyverse)
library(tidybayes)
library(gtsummary)
library(Rcpp)
library(rstanarm)
library(gt)
library(patchwork)


#I have selected these variables because they represent the top 4 most common
#coup types as well as other factors that may be significant to the success of a
#coup (loss of life). As I would like to concentrate on the implications of
#these effects in the Middle East, I have created a dummy variable specific to
#the Middle East and seek to explore its interaction with the coup types to
#determine if they have as significant causal effect.

fit_1 <- readRDS("Coup_Data/fit_1.rds")

Table_1 <- tbl_regression(fit_1, 
                          intercept = TRUE, 
                          estimate_fun = function(x) style_sigfig(x, digits = 4)) %>%
              as_gt()
#render as gt

pe_1 <- readRDS("Coup_Data/pe_1.rds")
#information for posterior_epred

palace_pe <- pe_1 %>% 
                filter(palace == 1,
                       popular == 0,
                       military == 0,
                       foreign == 0) %>% 
                ggplot(aes(x = .value, y = as.character(palace), fill = mena)) +
                stat_slab(alpha =.5) +
                labs(x = "Probability of Realization",
                     y = "Palace Coup",
                     fill = "MENA") +
                theme(legend.title = element_text(face = "bold")) +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                theme_tidybayes() 

popular_pe <- pe_1 %>% 
                filter(palace == 0,
                       popular %in% 1,
                       military == 0,
                       foreign == 0) %>% 
                ggplot(aes(x = .value, y = as.character(popular == 1), fill = mena)) +
                stat_slab(alpha =.5) +
                labs(x = "Probability of Realization",
                     y = "Popular Coup",
                     fill = "MENA") +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                theme(legend.title = element_text(face = "bold")) +
                theme_tidybayes()

foreign_pe <- pe_1 %>% 
                filter(palace == 0,
                       popular == 0,
                       military == 0,
                       foreign == 1) %>% 
                ggplot(aes(x = .value, y = as.character(foreign == 1), fill = mena)) +
                stat_slab(alpha =.5) +
                labs(x = "Probability of Realization",
                     y = "Foreign Backed Coup",
                     fill = "MENA") +
                theme(legend.title = element_text(face = "bold")) +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
                theme_tidybayes()

military_pe <- pe_1 %>% 
                filter(palace == 0,
                       popular == 0,
                       military == 1,
                       foreign == 0) %>% 
  ggplot(aes(x = .value, y = as.character(military == 1), fill = mena)) +
  labs(x = "Probability of Realization",
       y = "Military Coup",
       fill = "MENA") +
  theme(legend.title = element_text(face = "bold")) +
  stat_slab(alpha =.5) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_tidybayes()


mena_interaction <- palace_pe + popular_pe + foreign_pe + military_pe


mena_plots <- mena_interaction +
  plot_annotation(title = "Posterior Probability Distribution for Top 4 Coup Types in MENA",
                  subtitle = "For the most part they are more popular outside of the MENA",
                  caption = "Source: Hopkins et al (2021)") &
  coord_cartesian(xlim = c(0,1.0)) 

