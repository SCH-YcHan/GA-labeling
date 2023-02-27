rm(list=ls())

library(dplyr)
library(ggplot2)
library(gridExtra)

#load trading result
paper_result <- read.csv("../data/paper_result_IS_commission_60.csv")
UD_result <- read.csv("../data/UD_commission_result_60.csv")
lr_nn <- read.csv("../data/Ensemble_LR_NN.csv")
lr_xgb <- read.csv("../data/Ensemble_LR_XGB.csv")
nn_xgb <- read.csv("../data/Ensemble_NN_XGB.csv")
lr_nn_xgb <- read.csv("../data/Ensemble_LR_NN_XGB.csv")
en_result <- rbind(
  lr_nn %>% mutate(Model = "lr + nn"),
  lr_xgb %>% mutate(Model = "lr + xgb"),
  nn_xgb %>% mutate(Model = "nn + xgb"),
  lr_nn_xgb %>% mutate(Model = "lr + nn + xgb")
)

MEAN_SD <- function(x){
  M <- round(mean(x, na.rm=T), 2)
  S <- round(sd(x, na.rm=T), 2)
  return(paste0(M,"(",S,")"))
}

paper_result %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) %>% 
  filter(Model != "svm")

UD_result %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) %>% 
  filter(Model != "svm")

en_result %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)
