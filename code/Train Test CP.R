rm(list=ls())

setwd("C:/Users/user/ga-labeling/code")

library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(zoo)

symbol <- "AAPL"

ud <- read.csv(paste0("../data/UD_label/", symbol, "_UD.csv"))
ga <- readRDS(paste0("../data/GA_RDS/", symbol, "_paper.rds"))
ti <- read.csv(paste0("../data/Stock_TI/", symbol, "_TI.csv"))

ud$label2 <- ga@solution[1,]+1 %>% as.vector
train <- left_join(ud, ti %>% select(Date, Open), by="Date")

train$label <- na.locf(train$label)

ensemble_fun <- function(ensem_v, symbol, type=F){

  v_len <- length(ensem_v)
  
  if(type==F){
    for (i in 1:v_len){
      f <- read.csv(paste0("../data/Pred_result/", symbol, "_UD_", ensem_v[i], ".csv"))
      assign(ensem_v[i], f)
    }
  }
  if(type==T){
    for (i in 1:v_len){
      f <- read.csv(paste0("../data/Pred_result/", symbol, "_PAPER_", ensem_v[i], ".csv"))
      assign(ensem_v[i], f)
    }    
  }
  
  ensemble <- get(ensem_v[1]) %>% 
    mutate(label = ifelse(label<0.5, 1, 2))
  
  if(v_len==1){
  }else{
    for (i in 2:v_len){
      label2 <- ifelse(get(ensem_v[i])$label<0.5, 1, 2)
      ensemble$label <- ifelse(ensemble$label==label2, ensemble$label, 0)
    }
  }
  return(ensemble)
}

ud_ensemble <- ensemble_fun(c("XGB"), symbol, F)
ga_ensemble <- ensemble_fun(c("LR", "NN", "XGB"), symbol, T)

ud_ensemble$label2 <- ga_ensemble$label
test <- ud_ensemble %>% select(-train_size)

test$label[test$label==0] <- NA
test$label <- na.locf(test$label)

test$label2[test$label2==0] <- NA
test$label2 <- na.locf(test$label2)

p1 <- ggplot(test, aes(x=as.Date(Date), y=Open)) +
  geom_line() +
  labs(x=NULL, y="Open") +
  theme_bw()

p2 <- ggplot(test, aes(x=as.Date(Date), y=label)) +
  geom_step() +
  labs(x=NULL, y="Updown") +
  theme_bw()

p3 <- ggplot(test, aes(x=as.Date(Date), y=label2)) +
  geom_step() +
  labs(x=NULL, y="SGA") +
  theme_bw()

trading <- test

trading$label[nrow(trading)]=2
trading$label2[nrow(trading)]=2

trading_label <- trading %>% 
  dplyr::select(Date, label) %>%
  dplyr::mutate(label = ifelse(label-dplyr::lag(label)!=0 | is.na(label-dplyr::lag(label)), label, 0)) %>%
  dplyr::filter(label != 0) %>% 
  merge(trading %>% dplyr::mutate(Open = lead(Open)) %>% dplyr::select(Date, Open), by="Date", all=T) %>% 
  dplyr::filter(!is.na(label)) %>% 
  dplyr::filter(!is.na(Open))

if(trading_label$label[1]==2){trading_label$label[1]=NA}

trading_label <- trading_label %>%
  dplyr::filter(!is.na(label)) %>%
  dplyr::mutate(profit = Open-dplyr::lag(Open,1)) %>% 
  dplyr::filter(label == 2)

trading_label2 <- trading %>% 
  dplyr::select(Date, label2) %>%
  dplyr::mutate(label2 = ifelse(label2-dplyr::lag(label2)!=0 | is.na(label2-dplyr::lag(label2)), label2, 0)) %>%
  dplyr::filter(label2 != 0) %>% 
  merge(trading %>% dplyr::mutate(Open = lead(Open)) %>% dplyr::select(Date, Open), by="Date", all=T) %>% 
  dplyr::filter(!is.na(label2)) %>% 
  dplyr::filter(!is.na(Open))

if(trading_label2$label2[1]==2){trading_label2$label2[1]=NA}

trading_label2 <- trading_label2 %>%
  dplyr::filter(!is.na(label2)) %>%
  dplyr::mutate(profit2 = Open-dplyr::lag(Open,1)) %>% 
  dplyr::filter(label2 == 2)

CP_test <- test %>% 
  left_join(trading_label %>% select(Date, profit), by="Date") %>% 
  left_join(trading_label2 %>% select(Date, profit2), by="Date") %>% 
  select(Date, profit, profit2)

CP_test$cumsum <- cumsum(ifelse(is.na(CP_test$profit), 0, CP_test$profit))
CP_test$cumsum2 <- cumsum(ifelse(is.na(CP_test$profit2), 0, CP_test$profit2))

df_long <- CP_test %>% 
  select(Date, cumsum, cumsum2) %>% 
  pivot_longer(cols = c("cumsum", "cumsum2"), names_to = "Labeling", values_to = "cumulative_profit") %>% 
  mutate(Labeling = ifelse(Labeling=="cumsum", "Updown", "SGA"))

# 누적수익 그래프를 그립니다
p4 <- ggplot(df_long, aes(x = as.Date(Date), y = cumulative_profit, color = Labeling, linetype = Labeling)) +
  geom_step() +
  labs(x = NULL, y = "Cumulative Profit") +
  scale_color_manual(values = c("green", "red")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

grid.arrange(
  p1, p2, p3, p4,
  nrow=4
)


