rm(list=ls())

library(dplyr)
library(xgboost)
library(e1071)
library(caret)
library(nnet)
library(stringr)
source("./code/Trading plot.R")

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

prediction <- function(train, test){
  #XGB
  set.seed(20207188)
  xgb <- xgboost(data = train %>% select(-Date, -label, -Open, -Close) %>% data.matrix,
                 label = train$label-1,
                 objective = "binary:logistic",
                 eval_metric = "error",
                 nrounds = 30,
                 verbose = F)
  xgb_test_pre <- predict(xgb, test %>% select(-Date, -Open, -Close) %>% data.matrix)
  
  xgb_test_result <- cbind(test %>% select(Date, Open),
                           label=ifelse(xgb_test_pre<0.5, 1, 2),
                           train_size=nrow(train))
  
  #SVM
  set.seed(20207188)
  svm <- svm(label-1~., data=train %>% select(-Date, -Open, -Close))
  
  svm_test_pre <- predict(svm, test %>% select(-Date, -Open, -Close)) %>% as.vector
  
  svm_test_result <- cbind(test %>% select(Date, Open),
                           label=ifelse(svm_test_pre<0.5, 1, 2),
                           train_size=nrow(train))
  
  #LR
  set.seed(20207188)
  lr <- glm(label-1~., family="binomial", data=train %>% select(-Date, -Open, -Close))
  
  lr_test_pre <- predict(lr, test %>% select(-Date, -Open, -Close), type="response") %>% as.vector
  
  lr_test_result <- cbind(test %>% select(Date, Open),
                          label=ifelse(lr_test_pre<0.5, 1, 2),
                          train_size=nrow(train))
  
  #NN
  set.seed(20207188)
  nn <- nnet(x=train %>% select(-Date, -Open, -Close, -label),
             y=train$label-1,
             size=100,
             MaxNWts=5000,
             trace=F)
  
  nn_test_pre <- predict(nn, test %>% select(-Date, -Open, -Close)) %>% as.vector
  
  nn_test_result <- cbind(test %>% select(Date, Open),
                          label=ifelse(nn_test_pre<0.5, 1, 2),
                          train_size=nrow(train))
  
  result <- list(
    "xgb_test_result" = xgb_test_result,
    "svm_test_result" = svm_test_result,
    "lr_test_result" = lr_test_result,
    "nn_test_result" = nn_test_result
  )
  
  return(result)
}

pre_and_trade <- function(TI_file, UD_label, symbol, plotting=F){
  
  train <- TI_file %>%
    filter(Date < "2019-01-01") %>% 
    merge(., UD_label, by="Date", all.x=T) %>% 
    na.omit
  row.names(train) <- NULL
  
  test <- TI_file %>%
    filter(Date > "2019-01-01" & Date < "2022-01-01")
  
  scale_col <- setdiff(names(train), c("Date", "Open", "Close", "label"))
  prepro <- preProcess(train[scale_col], method=c("center", "scale"))
  
  train[scale_col] <- predict(prepro, train[scale_col])
  test[scale_col] <- predict(prepro, test[scale_col])
  
  pred <- prediction(train, test)
  
  if(plotting){
    lapply(
      seq_along(pred),
      trading_plot,
      Pred_data=pred,
      plot_name=paste0(symbol, "_UD_commission_", names(pred))
    )
  }
  
  result <- pred %>% 
    lapply(trading) %>% 
    do.call(rbind.data.frame, .)
  
  if(!file.exists("./data/UD_result_commission")){
    dir.create("./data/UD_result_commission")
  }
  
  write.csv(result, paste0("./data/UD_result_commission/", symbol, "_UD.csv"))
  
  return(result)
}

Symbols <- read.csv("./data/NASDAQ_Marketcap60.csv")$Symbol
#Symbols <- Symbols[31:60]

for (symbol in Symbols){
  stock <- read.csv(paste0("./data/Stock_TI/",symbol ,"_TI.csv"))
  stock$Date <- as.Date(stock$Date)
  
  ud_label <- read.csv(paste0("./data/UD_label/", symbol, "_UD.csv"))
  ud_label$Date <- as.Date(ud_label$Date)
  
  pre_and_trade(stock, ud_label, symbol)
}

