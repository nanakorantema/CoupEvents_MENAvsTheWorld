library(tidyverse)
library(tidybayes)
library(gtsummary)
library(Rcpp)
library(gt)
library(broom.mixed)
library(rstanarm)
library(ggdist)



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
                as_gt() %>% 
                tab_header(title = md("**Likelihood of Coup Realization**"),
                           subtitle = "How Coup Type and Location Predict Likelihood of Success") %>%
                tab_source_note(md("Source: The Cline Center (2021)")) %>% 
                cols_label(estimate = md("**Parameter**"))
#render as gt

pe_1 <- readRDS("Coup_Data/pe_1.rds")
#information for posterior_epred, I created a new variable that is a combination
#of popular and foreign backed coups

coup_plot <- pe_1 %>% 
  filter(military == 0,
         palace == 0) %>% 
  ggplot(aes(x = .value, y = as.character(pop_for), fill = mena)) +
  stat_slab(alpha =.5) +
  labs(title = "Posterior Probability Distributions of Palace and Foreign Backed Coups",
       subtitle = "The probability of a coup succeeding increases with popular resistance",
       x = "Probability of Success",
       y = "Popular and Foriegn Backed Coup Combinations",
       fill = "MENA") +
  theme(legend.title = element_text(face = "bold")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_tidybayes() 

