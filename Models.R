library(tidyverse)
library(tidymodels)
library(rstanarm)
library(tidybayes)
library(gtsummary)
library(Rcpp)
library(gt)
library(patchwork)

load("Coup_Data/Model_Data")

#I have selected these variables because they represent the top 4 most common
#coup types as well as other factors that may be significant to the success of a
#coup (loss of life). As I would like to concentrate on the implications of
#these effects in the Middle East, I have created a dummy variable specific to
#the Middle East and seek to explore its interaction with the coup types to
#determine if they have as significant causal effect.

Table_1 <- tbl_regression(fit_1, 
                          intercept = TRUE, 
                          estimate_fun = function(x) style_sigfig(x, digits = 4)) %>%
              as_gt()

popular <- unique(model_data$popular)
military <- unique(model_data$military)
foreign <- unique(model_data$foreign)
palace <- unique(model_data$palace)
killed <- unique(model_data$killed)
mena <- unique(model_data$mena)

newobs_1 <- expand_grid( killed, popular, military, foreign, palace, mena) 

pe_1 <- add_fitted_draws(newobs_1, fit_1) 


palace_pe <- pe_1 %>% 
  ggplot(aes(x = .value, y = as.character(palace == 1), fill = mena)) +
  stat_slab(alpha =.5) +
  labs(x = "Probability of Realization",
       y = "Palace Coup",
       fill = "MENA") +
  theme(legend.title = element_text(face = "bold")) +
  theme_tidybayes() 

popular_pe <- pe_1 %>% 
  ggplot(aes(x = .value, y = as.character(popular == 1), fill = mena)) +
  stat_slab(alpha =.5) +
  labs(x = "Probability of Realization",
       y = "Popular Coup",
       fill = "MENA") +
  theme(legend.title = element_text(face = "bold")) +
  theme_tidybayes()

foreign_pe <- pe_1 %>% 
  ggplot(aes(x = .value, y = as.character(foreign == 1), fill = mena)) +
  stat_slab(alpha =.5) +
  labs(x = "Probability of Realization",
       y = "Foreign Backed Coup",
       fill = "MENA") +
  theme(legend.title = element_text(face = "bold")) +
  theme_tidybayes()

military_pe <- pe_1 %>% 
  ggplot(aes(x = .value, y = as.character(military), fill = mena)) +
  labs(x = "Probability of Realization",
       y = "Military Coup",
       fill = "MENA") +
  theme(legend.title = element_text(face = "bold")) +
  stat_slab(alpha =.5) +
  theme_tidybayes()




