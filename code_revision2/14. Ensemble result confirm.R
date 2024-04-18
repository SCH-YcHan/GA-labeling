rm(list=ls())

library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

#load trading result
NASDAQ_UD_result <- read.csv("../data/NASDAQ_UD_commission_result_60.csv")
KOSPI_UD_result <- read.csv("../data/KOSPI_UD_commission_result_60.csv")

NASDAQ_paper_result <- read.csv("../data/NASDAQ_paper_result_IS_commission_60.csv")
KOSPI_paper_result <- read.csv("../data/KOSPI_paper_result_IS_commission_60.csv")

NASDAQ_lr_nn <- read.csv("../data/NASDAQ_Ensemble_LR_NN.csv")
NASDAQ_lr_xgb <- read.csv("../data/NASDAQ_Ensemble_LR_XGB.csv")
NASDAQ_nn_xgb <- read.csv("../data/NASDAQ_Ensemble_NN_XGB.csv")
NASDAQ_lr_nn_xgb <- read.csv("../data/NASDAQ_Ensemble_LR_NN_XGB.csv")

NASDAQ_en_result <- rbind(
  NASDAQ_lr_nn %>% mutate(Model = "lr + nn"),
  NASDAQ_lr_xgb %>% mutate(Model = "lr + xgb"),
  NASDAQ_nn_xgb %>% mutate(Model = "nn + xgb"),
  NASDAQ_lr_nn_xgb %>% mutate(Model = "lr + nn + xgb")
)

KOSPI_lr_nn <- read.csv("../data/KOSPI_Ensemble_LR_NN.csv")
KOSPI_lr_xgb <- read.csv("../data/KOSPI_Ensemble_LR_XGB.csv")
KOSPI_nn_xgb <- read.csv("../data/KOSPI_Ensemble_NN_XGB.csv")
KOSPI_lr_nn_xgb <- read.csv("../data/KOSPI_Ensemble_LR_NN_XGB.csv")

KOSPI_en_result <- rbind(
  KOSPI_lr_nn %>% mutate(Model = "lr + nn"),
  KOSPI_lr_xgb %>% mutate(Model = "lr + xgb"),
  KOSPI_nn_xgb %>% mutate(Model = "nn + xgb"),
  KOSPI_lr_nn_xgb %>% mutate(Model = "lr + nn + xgb")
)

remove_symbol <- c()
rds_path <- "../data/GA_RDS/"
for(s in list.files(rds_path)){
  ga_rds <- readRDS(paste0(rds_path,s))
  if(ga_rds@fitnessValue<0){
    remove_symbol <- c(remove_symbol, str_split(s, "_")[[1]][1])
  }
}
remove_symbol <- c(remove_symbol, "004940.KS")

MEAN_SD <- function(x){
  M <- round(mean(x, na.rm=T), 2)
  S <- round(sd(x, na.rm=T), 2)
  return(paste0(M,"(",S,")"))
}

NASDAQ_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

NASDAQ_paper_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) 

NASDAQ_en_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

KOSPI_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

KOSPI_paper_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) 

KOSPI_en_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>%
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)
