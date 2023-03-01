rm(list=ls())

library(dplyr)
library(ggplot2)
library(gridExtra)

#load trading result
paper_result <- read.csv("./data/paper_result_IS_commission_60.csv")
UD_result <- read.csv("./data/UD_commission_result_60.csv")
lr_nn <- read.csv("./data/Ensemble_LR_NN.csv")
lr_xgb <- read.csv("./data/Ensemble_LR_XGB.csv")
nn_xgb <- read.csv("./data/Ensemble_NN_XGB.csv")
lr_nn_xgb <- read.csv("./data/Ensemble_LR_NN_XGB.csv")
en_result <- rbind(
  lr_nn %>% mutate(Model = "lr + nn"),
  lr_xgb %>% mutate(Model = "lr + xgb"),
  nn_xgb %>% mutate(Model = "nn + xgb"),
  lr_nn_xgb %>% mutate(Model = "lr + nn + xgb")
)

remove_symbol <- c()
rds_path <- "./data/GA_RDS/"
for(s in list.files(rds_path)){
  ga_rds <- readRDS(paste0(rds_path,s))
  if(ga_rds@fitnessValue==0){
    remove_symbol <- c(remove_symbol, str_split(s, "_")[[1]][1])
  }
}

MEAN_SD <- function(x){
  M <- round(mean(x, na.rm=T), 2)
  S <- round(sd(x, na.rm=T), 2)
  return(paste0(M,"(",S,")"))
}

paper_result %>% 
  filter(Model != "svm") %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) 

UD_result %>% 
  filter(Model != "svm") %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

en_result %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

paper_result %>% 
  filter(Model != "svm") %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) 

UD_result %>% 
  filter(Model != "svm") %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

en_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)
