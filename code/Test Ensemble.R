rm(list=ls())

library(dplyr)

trading <- function(Pred_data){
  Pred_data2 <- Pred_data %>% 
    select(Date, label) %>%
    mutate(label = ifelse(label-lag(label)!=0 | is.na(label-lag(label)), label, 0)) %>%
    filter(label != 0) %>% 
    merge(Pred_data %>% mutate(Open = lead(Open)) %>% select(Date, Open), by="Date", all=T) %>% 
    filter(!is.na(label)) %>% 
    filter(!is.na(Open))
  
  if(Pred_data2$label[1]==2){Pred_data2$label[1]=NA}
  
  Pred_data3 <- Pred_data2 %>%
    filter(!is.na(label)) %>%
    mutate(profit = Open*0.9981-lag(Open,1)*1.0019) %>% 
    filter(label == 2)
  
  Pred_data_N_trade <- nrow(Pred_data3)
  Pred_data_Nw <- sum(Pred_data3$profit>0)
  Pred_data_Wr <- Pred_data_Nw/Pred_data_N_trade
  Pred_data_mean_W <- mean(Pred_data3$profit[Pred_data3$profit>0])
  Pred_data_mean_L <- mean(abs(Pred_data3$profit[Pred_data3$profit<0]))
  Pred_data_Pwl <- Pred_data_mean_W/Pred_data_mean_L
  Pred_data_Pf <- Pred_data_Pwl*((Pred_data_Nw/(Pred_data_N_trade-Pred_data_Nw)))
  Pred_data_cum_profit <- sum(Pred_data3$profit)
  Pred_data_buy_hold <- Pred_data$Open[nrow(Pred_data)]-Pred_data$Open[1]
  
  Pred_data_result <- data.frame(
    Train_size = Pred_data$train_size[1],
    N_trade = Pred_data_N_trade,
    Win_ratio = Pred_data_Wr,
    Mean_gain = Pred_data_mean_W,
    Mean_loss = Pred_data_mean_L,
    Payoff_ratio = Pred_data_Pwl,
    Profit_factor = Pred_data_Pf,
    Cum_Profit = Pred_data_cum_profit,
    Buy_hold = Pred_data_buy_hold
  )
  
  return(Pred_data_result)
}

model_ensemble <- function(ensem_v, symbol){
  v_len <- length(ensem_v)
  
  for (i in 1:v_len){
    f <- read.csv(paste0("../data/Pred_result/", symbol, "_PAPER_", ensem_v[i], ".csv"))
    assign(ensem_v[i], f)
  }
  
  ensemble <- get(ensem_v[1]) %>% 
    mutate(label = 0)
  
  for (i in 1:v_len){ensemble$label <- ensemble$label + get(ensem_v[i])$label}
  
  ensemble$label <- ensemble$label/v_len
  ensemble$label <- ifelse(ensemble$label<0.5, 1, 2)
  
  trade_result <- trading(ensemble)
  
  return(cbind(Symbol = symbol, trade_result))
}

model_ensemble2 <- function(ensem_v, symbol){
  v_len <- length(ensem_v)
  
  for (i in 1:v_len){
    f <- read.csv(paste0("../data/Pred_result/", symbol, "_PAPER_", ensem_v[i], ".csv"))
    assign(ensem_v[i], f)
  }
  
  ensemble <- get(ensem_v[1]) %>% 
    mutate(label = ifelse(label<0.5, 1, 2))
  
  for (i in 2:v_len){
    label2 <- ifelse(get(ensem_v[i])$label<0.5, 1, 2)
    ensemble$label <- ifelse(ensemble$label==label2, ensemble$label, 0)
  }
  
  ensemble <- ensemble %>% filter(label!=0)
  
  trade_result <- trading(ensemble)
  
  return(cbind(Symbol = symbol, trade_result))
}

Symbols <- read.csv("../data/NASDAQ_Marketcap60.csv")$Symbol

ensemble_lr_nn <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble2(c("LR", "NN"), symbol)
  
  ensemble_lr_nn <- rbind(ensemble_lr_nn, row)
}

write.csv(ensemble_lr_nn, "../data/Ensemble_LR_NN.csv", row.names=F)

ensemble_lr_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble2(c("LR", "XGB"), symbol)
  
  ensemble_lr_xgb <- rbind(ensemble_lr_xgb, row)
}

write.csv(ensemble_lr_xgb, "../data/Ensemble_LR_XGB.csv", row.names=F)

ensemble_nn_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble2(c("NN", "XGB"), symbol)
  
  ensemble_nn_xgb <- rbind(ensemble_nn_xgb, row)
}

write.csv(ensemble_nn_xgb, "../data/Ensemble_NN_XGB.csv", row.names=F)

ensemble_lr_nn_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble2(c("LR", "NN", "XGB"), symbol)
  
  ensemble_lr_nn_xgb <- rbind(ensemble_lr_nn_xgb, row)
}

write.csv(ensemble_lr_nn_xgb, "../data/Ensemble_LR_NN_XGB.csv", row.names=F)


