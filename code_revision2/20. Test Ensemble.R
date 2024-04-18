rm(list=ls())

library(dplyr)
library(stringr)

trading <- function(Pred_data, Dividend_dates){
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
    mutate(grp = cumsum(label == 1)) %>%
    group_by(grp) %>%
    filter(n() == 2) %>%
    summarise(Start_Date = first(Date) %>% as.Date,
              End_Date = last(Date) %>% as.Date,
              profit = last(Open)*0.9981 - first(Open)*1.0019) %>%
    select(-grp) %>%
    anti_join(Dividend_dates, by = c("Start_Date" = "Ex.Dividend.Date")) %>%
    anti_join(Dividend_dates, by = c("End_Date" = "Ex.Dividend.Date"))
  
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

Updown <- function(model_name, symbol){
  f <- read.csv(paste0("../data/Pred_result/", symbol, "_UD_", model_name, ".csv"))
  pred <- f %>% mutate(label = ifelse(label<0.5, 1, 2))
  
  Dividend_dates <- dividend %>% 
    filter(Symbol==symbol)
  
  trade_result <- trading(pred, Dividend_dates)
  
  return(cbind(Symbol = symbol, trade_result))
}

SGA <- function(model_name, symbol){
  f <- read.csv(paste0("../data/Pred_result/", symbol, "_paper_", model_name, ".csv"))
  pred <- f %>% mutate(label = ifelse(label<0.5, 1, 2))
  
  Dividend_dates <- dividend %>% 
    filter(Symbol==symbol)
  
  trade_result <- trading(pred, Dividend_dates)
  
  return(cbind(Symbol = symbol, trade_result))
}

model_ensemble <- function(ensem_v, symbol){
  v_len <- length(ensem_v)
  
  for (i in 1:v_len){
    f <- read.csv(paste0("../data/Pred_result/", symbol, "_paper_", ensem_v[i], ".csv"))
    assign(ensem_v[i], f)
  }
  
  ensemble <- get(ensem_v[1]) %>% 
    mutate(label = ifelse(label<0.5, 1, 2))
  
  for (i in 2:v_len){
    label2 <- ifelse(get(ensem_v[i])$label<0.5, 1, 2)
    ensemble$label <- ifelse(ensemble$label==label2, ensemble$label, 0)
  }
  
  ensemble <- ensemble %>% filter(label!=0)
  
  Dividend_dates <- dividend %>% 
    filter(Symbol==symbol)
  
  trade_result <- trading(ensemble, Dividend_dates)
  
  return(cbind(Symbol = symbol, trade_result))
}

Symbols <- c(
  "AAPL", "ADBE", "ALGN", "ALNY", "AMAT", "AMZN", "DLTR", "DXCM", "ENTG", "EXAS", "IDXX",
  "ILMN", "INTU", "ISRG", "KLAC", "LRCX", "LULU", "MASI", "MELI", "MKTX", "MPWR", "MRVL",
  "MSFT", "NFLX", "NTAP", "NVDA", "ORLY", "PODD", "POOL", "REGN", "ROST", "SBAC", "SGEN",
  "TTWO", "ULTA", "VRSN", "VRTX", "ZBRA"
)

dividend <- read.csv("../data/Stock_dividend/NASDAQ_dividends_date.csv") %>% 
  mutate(Ex.Dividend.Date = Ex.Dividend.Date %>% as.Date)

UD_lr <- data.frame()
for (symbol in Symbols){
  row <- Updown("LR", symbol)
  UD_lr <- rbind(UD_lr, row)
}
write.csv(UD_lr, "../data/NASDAQ_UD_LR.csv", row.names=F)

UD_nn <- data.frame()
for (symbol in Symbols){
  row <- Updown("NN", symbol)
  UD_nn <- rbind(UD_nn, row)
}
write.csv(UD_nn, "../data/NASDAQ_UD_NN.csv", row.names=F)

UD_xgb <- data.frame()
for (symbol in Symbols){
  row <- Updown("XGB", symbol)
  UD_xgb <- rbind(UD_xgb, row)
}
write.csv(UD_xgb, "../data/NASDAQ_UD_XGB.csv", row.names=F)

SGA_lr <- data.frame()
for (symbol in Symbols){
  row <- SGA("LR", symbol)
  SGA_lr <- rbind(SGA_lr, row)
}
write.csv(SGA_lr, "../data/NASDAQ_SGA_LR.csv", row.names=F)

SGA_nn <- data.frame()
for (symbol in Symbols){
  row <- SGA("NN", symbol)
  SGA_nn <- rbind(SGA_nn, row)
}
write.csv(SGA_nn, "../data/NASDAQ_SGA_NN.csv", row.names=F)

SGA_xgb <- data.frame()
for (symbol in Symbols){
  row <- SGA("XGB", symbol)
  SGA_xgb <- rbind(SGA_xgb, row)
}
write.csv(SGA_xgb, "../data/NASDAQ_SGA_XGB.csv", row.names=F)

ensemble_lr_nn <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble(c("LR", "NN"), symbol)
  ensemble_lr_nn <- rbind(ensemble_lr_nn, row)
}
write.csv(ensemble_lr_nn, "../data/NASDAQ_Ensemble_LR_NN.csv", row.names=F)

ensemble_lr_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble(c("LR", "XGB"), symbol)
  ensemble_lr_xgb <- rbind(ensemble_lr_xgb, row)
}
write.csv(ensemble_lr_xgb, "../data/NASDAQ_Ensemble_LR_XGB.csv", row.names=F)

ensemble_nn_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble(c("NN", "XGB"), symbol)
  ensemble_nn_xgb <- rbind(ensemble_nn_xgb, row)
}
write.csv(ensemble_nn_xgb, "../data/NASDAQ_Ensemble_NN_XGB.csv", row.names=F)

ensemble_lr_nn_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble(c("LR", "NN", "XGB"), symbol)
  ensemble_lr_nn_xgb <- rbind(ensemble_lr_nn_xgb, row)
}
write.csv(ensemble_lr_nn_xgb, "../data/NASDAQ_Ensemble_LR_NN_XGB.csv", row.names=F)



Symbols <- c(
  "000100.KS", "000120.KS", "000240.KS", "000270.KS", "000660.KS", "001740.KS",
  "003550.KS", "004170.KS", "005930.KS", "006360.KS", "006400.KS", "006800.KS",
  "009150.KS", "009540.KS", "010060.KS", "010620.KS", "010950.KS", "011200.KS",
  "012450.KS", "012630.KS", "016360.KS", "032640.KS", "034220.KS", "035250.KS",
  "035420.KS", "036460.KS", "042660.KS", "042670.KS", "047040.KS", "051900.KS",
  "051910.KS", "078930.KS", "096770.KS"
)

dividend <- read.csv("../data/Stock_dividend/KOSPI_dividends_date.csv") %>% 
  mutate(Ex.Dividend.Date = Ex.Dividend.Date %>% as.Date)

UD_lr <- data.frame()
for (symbol in Symbols){
  row <- Updown("LR", symbol)
  UD_lr <- rbind(UD_lr, row)
}
write.csv(UD_lr, "../data/KOSPI_UD_LR.csv", row.names=F)

UD_nn <- data.frame()
for (symbol in Symbols){
  row <- Updown("NN", symbol)
  UD_nn <- rbind(UD_nn, row)
}
write.csv(UD_nn, "../data/KOSPI_UD_NN.csv", row.names=F)

UD_xgb <- data.frame()
for (symbol in Symbols){
  row <- Updown("XGB", symbol)
  UD_xgb <- rbind(UD_xgb, row)
}
write.csv(UD_xgb, "../data/KOSPI_UD_XGB.csv", row.names=F)

SGA_lr <- data.frame()
for (symbol in Symbols){
  row <- SGA("LR", symbol)
  SGA_lr <- rbind(SGA_lr, row)
}
write.csv(SGA_lr, "../data/KOSPI_SGA_LR.csv", row.names=F)

SGA_nn <- data.frame()
for (symbol in Symbols){
  row <- SGA("NN", symbol)
  SGA_nn <- rbind(SGA_nn, row)
}
write.csv(SGA_nn, "../data/KOSPI_SGA_NN.csv", row.names=F)

SGA_xgb <- data.frame()
for (symbol in Symbols){
  row <- SGA("XGB", symbol)
  SGA_xgb <- rbind(SGA_xgb, row)
}
write.csv(SGA_xgb, "../data/KOSPI_SGA_XGB.csv", row.names=F)

ensemble_lr_nn <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble(c("LR", "NN"), symbol)
  ensemble_lr_nn <- rbind(ensemble_lr_nn, row)
}
write.csv(ensemble_lr_nn, "../data/KOSPI_Ensemble_LR_NN.csv", row.names=F)

ensemble_lr_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble(c("LR", "XGB"), symbol)
  ensemble_lr_xgb <- rbind(ensemble_lr_xgb, row)
}
write.csv(ensemble_lr_xgb, "../data/KOSPI_Ensemble_LR_XGB.csv", row.names=F)

ensemble_nn_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble(c("NN", "XGB"), symbol)
  ensemble_nn_xgb <- rbind(ensemble_nn_xgb, row)
}
write.csv(ensemble_nn_xgb, "../data/KOSPI_Ensemble_NN_XGB.csv", row.names=F)

ensemble_lr_nn_xgb <- data.frame()
for (symbol in Symbols){
  row <- model_ensemble(c("LR", "NN", "XGB"), symbol)
  ensemble_lr_nn_xgb <- rbind(ensemble_lr_nn_xgb, row)
}
write.csv(ensemble_lr_nn_xgb, "../data/KOSPI_Ensemble_LR_NN_XGB.csv", row.names=F)
