rm(list=ls())

library(dplyr)
library(xgboost)
library(e1071)
library(caret)
library(nnet)
library(stringr)

prediction <- function(train, test, symbol){
  #XGB
  set.seed(20207188)
  xgb <- xgboost(data = train %>% select(-Date, -label, -Open, -Close) %>% data.matrix,
                 label = train$label-1,
                 objective = "binary:logistic",
                 eval_metric = "error",
                 nrounds = 30,
                 verbose = F)
  xgb_test_pre <- predict(xgb, test %>% select(-Date, -Open, -Close) %>% data.matrix)
  
  xgb_pred_result <- cbind(test %>% select(Date, Open),
                           label=round(xgb_test_pre,4),
                           train_size=nrow(train))
  
  write.csv(xgb_pred_result, paste0("../data/Pred_result/", symbol, "_UD_XGB.csv"), row.names = F)
  
  #LR
  set.seed(20207188)
  lr <- glm(label-1~., family="binomial", data=train %>% select(-Date, -Open, -Close))
  
  lr_test_pre <- predict(lr, test %>% select(-Date, -Open, -Close), type="response") %>% as.vector
  
  lr_pred_result <- cbind(test %>% select(Date, Open),
                          label=round(lr_test_pre,4),
                          train_size=nrow(train))
  
  write.csv(lr_pred_result, paste0("../data/Pred_result/", symbol, "_UD_LR.csv"), row.names = F)
  
  #NN
  set.seed(20207188)
  nn <- nnet(x=train %>% select(-Date, -Open, -Close, -label),
             y=train$label-1,
             size=100,
             MaxNWts=5000,
             trace=F)
  
  nn_test_pre <- predict(nn, test %>% select(-Date, -Open, -Close)) %>% as.vector
  
  nn_pred_result <- cbind(test %>% select(Date, Open),
                          label=round(nn_test_pre,4),
                          train_size=nrow(train))
  
  write.csv(nn_pred_result, paste0("../data/Pred_result/", symbol, "_UD_NN.csv"), row.names = F)
}

pre_and_trade <- function(TI_file, UD_label, symbol){
  
  train <- TI_file %>%
    filter(Date < "2019-01-01") %>% 
    merge(., UD_label, by="Date", all.x=T) %>% 
    na.omit
  row.names(train) <- NULL
  
  test <- TI_file %>%
    filter(Date > "2019-01-01")
  
  scale_col <- setdiff(names(train), c("Date", "Open", "Close", "label"))
  prepro <- preProcess(train[scale_col], method=c("center", "scale"))
  
  train[scale_col] <- predict(prepro, train[scale_col])
  test[scale_col] <- predict(prepro, test[scale_col])
  
  prediction(train, test, symbol)
  print(symbol)
}

Symbols <- read.csv("../data/NASDAQ_Marketcap60.csv")$Symbol

for (symbol in Symbols){
  stock <- read.csv(paste0("../data/Stock_TI/", symbol ,"_TI.csv"))
  stock$Date <- as.Date(stock$Date)
  
  ud_label <- read.csv(paste0("../data/UD_label/", symbol, "_UD.csv"))
  ud_label$Date <- as.Date(ud_label$Date)
  
  pre_and_trade(stock, ud_label, symbol)
}

Symbols <- read.csv("../data/KOSPI_Marketcap60.csv")$종목코드

for (symbol in Symbols){
  symbol <- str_remove(symbol, "X")
  stock <- read.csv(paste0("../data/Stock_TI/", symbol ,"_TI.csv"))
  stock$Date <- as.Date(stock$Date)
  
  ud_label <- read.csv(paste0("../data/UD_label/", symbol, "_UD.csv"))
  ud_label$Date <- as.Date(ud_label$Date)
  
  pre_and_trade(stock, ud_label, symbol)
}
