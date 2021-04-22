library(tidyverse)
library(tidymodels)
library(rstanarm)
library(tidybayes)
library(gtsummary)
library(Rcpp)
library(gt)
library(patchwork)

read_rds("Coup_Data/Model_Data")

#I have selected these variables because they represent the top 4 most common
#coup types as well as other factors that may be significant to the success of a
#coup (loss of life). As I would like to concentrate on the implications of
#these effects in the Middle East, I have created a dummy variable specific to
#the Middle East and seek to explore its interaction with the coup types to
#determine if they have as significant causal effect.

fit_1 <- stan_glm(realized ~ popular + military + foreign + palace + killed + mena - 1 + mena*popular + mena*military +  mena*foreign + mena*palace,
                  family = "binomial",
                  model_data,
                  refresh = 0,
                  seed = 13)
