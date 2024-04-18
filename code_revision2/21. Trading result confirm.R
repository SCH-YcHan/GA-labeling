rm(list=ls())

library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

NASDAQ_UD_LR <- read.csv("../data/NASDAQ_UD_LR.csv")
NASDAQ_UD_NN <- read.csv("../data/NASDAQ_UD_NN.csv")
NASDAQ_UD_XGB <- read.csv("../data/NASDAQ_UD_XGB.csv")

NASDAQ_SGA_LR <- read.csv("../data/NASDAQ_SGA_LR.csv")
NASDAQ_SGA_NN <- read.csv("../data/NASDAQ_SGA_NN.csv")
NASDAQ_SGA_XGB <- read.csv("../data/NASDAQ_SGA_XGB.csv")

NASDAQ_LR_NN <- read.csv("../data/NASDAQ_Ensemble_LR_NN.csv")
NASDAQ_LR_XGB <- read.csv("../data/NASDAQ_Ensemble_LR_XGB.csv")
NASDAQ_NN_XGB <- read.csv("../data/NASDAQ_Ensemble_NN_XGB.csv")
NASDAQ_LR_NN_XGB <- read.csv("../data/NASDAQ_Ensemble_LR_NN_XGB.csv")

NASDAQ_UD <- rbind(
  NASDAQ_UD_LR %>% mutate(Model = "LR"),
  NASDAQ_UD_NN %>% mutate(Model = "NN"),
  NASDAQ_UD_XGB %>% mutate(Model = "XGB")
)

NASDAQ_SGA <- rbind(
  NASDAQ_SGA_LR %>% mutate(Model = "LR"),
  NASDAQ_SGA_NN %>% mutate(Model = "NN"),
  NASDAQ_SGA_XGB %>% mutate(Model = "XGB")
)

NASDAQ_Ensemble <- rbind(
  NASDAQ_LR_NN %>% mutate(Model = "LR + NN"),
  NASDAQ_LR_XGB %>% mutate(Model = "LR + XGB"),
  NASDAQ_NN_XGB %>% mutate(Model = "NN + XGB"),
  NASDAQ_LR_NN_XGB %>% mutate(Model = "LR + NN + XGB")
)

KOSPI_UD_LR <- read.csv("../data/KOSPI_UD_LR.csv")
KOSPI_UD_NN <- read.csv("../data/KOSPI_UD_NN.csv")
KOSPI_UD_XGB <- read.csv("../data/KOSPI_UD_XGB.csv")

KOSPI_SGA_LR <- read.csv("../data/KOSPI_SGA_LR.csv")
KOSPI_SGA_NN <- read.csv("../data/KOSPI_SGA_NN.csv")
KOSPI_SGA_XGB <- read.csv("../data/KOSPI_SGA_XGB.csv")

KOSPI_LR_NN <- read.csv("../data/KOSPI_Ensemble_LR_NN.csv")
KOSPI_LR_XGB <- read.csv("../data/KOSPI_Ensemble_LR_XGB.csv")
KOSPI_NN_XGB <- read.csv("../data/KOSPI_Ensemble_NN_XGB.csv")
KOSPI_LR_NN_XGB <- read.csv("../data/KOSPI_Ensemble_LR_NN_XGB.csv")

KOSPI_UD <- rbind(
  KOSPI_UD_LR %>% mutate(Model = "LR"),
  KOSPI_UD_NN %>% mutate(Model = "NN"),
  KOSPI_UD_XGB %>% mutate(Model = "XGB")
)

KOSPI_SGA <- rbind(
  KOSPI_SGA_LR %>% mutate(Model = "LR"),
  KOSPI_SGA_NN %>% mutate(Model = "NN"),
  KOSPI_SGA_XGB %>% mutate(Model = "XGB")
)

KOSPI_Ensemble <- rbind(
  KOSPI_LR_NN %>% mutate(Model = "LR + NN"),
  KOSPI_LR_XGB %>% mutate(Model = "LR + XGB"),
  KOSPI_NN_XGB %>% mutate(Model = "NN + XGB"),
  KOSPI_LR_NN_XGB %>% mutate(Model = "LR + NN + XGB")
)

MEAN_SD <- function(x){
  M <- round(mean(x, na.rm=T), 2)
  S <- round(sd(x, na.rm=T), 2)
  return(paste0(M,"(",S,")"))
}

NASDAQ_UD %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

NASDAQ_SGA %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) 

NASDAQ_Ensemble %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) %>% 
  .[c(1,3,4,2),]

KOSPI_UD %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

KOSPI_SGA %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) 

KOSPI_Ensemble %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold, -Mean_gain, -Mean_loss) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD) %>% 
  .[c(1,3,4,2),]
