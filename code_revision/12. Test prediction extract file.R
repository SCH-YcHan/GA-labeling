rm(list=ls())

library(dplyr)
library(xgboost)
library(e1071)
library(caret)
library(nnet)
library(stringr)

prediction <- function(train, test, symbol, obj_name){
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
                           label=round(xgb_test_pre,4),
                           train_size=nrow(train))
  
  write.csv(xgb_test_result, paste0("../data/Pred_result/", symbol, "_", obj_name, "_XGB.csv"), row.names = F)
  
  # #SVM
  # set.seed(20207188)
  # svm <- svm(label-1~., data=train %>% select(-Date, -Open, -Close))
  # 
  # svm_test_pre <- predict(svm, test %>% select(-Date, -Open, -Close)) %>% as.vector
  # 
  # svm_test_result <- cbind(test %>% select(Date, Open),
  #                          label=round(svm_test_pre,4),
  #                          train_size=nrow(train))
  # 
  # write.csv(svm_test_result, paste0("./data/Pred_result/", symbol, "_", obj_name, "_SVM.csv"), row.names = F)
  # 
  # #LR
  # set.seed(20207188)
  # lr <- glm(label-1~., family="binomial", data=train %>% select(-Date, -Open, -Close))
  # 
  # lr_test_pre <- predict(lr, test %>% select(-Date, -Open, -Close), type="response") %>% as.vector
  # 
  # lr_test_result <- cbind(test %>% select(Date, Open),
  #                         label=round(lr_test_pre,4),
  #                         train_size=nrow(train))
  # 
  # write.csv(lr_test_result, paste0("./data/Pred_result/", symbol, "_", obj_name, "_LR.csv"), row.names = F)
  # 
  # #NN
  # set.seed(20207188)
  # nn <- nnet(x=train %>% select(-Date, -Open, -Close, -label),
  #            y=train$label-1,
  #            size=100,
  #            MaxNWts=5000,
  #            trace=F)
  # 
  # nn_test_pre <- predict(nn, test %>% select(-Date, -Open, -Close)) %>% as.vector
  # 
  # nn_test_result <- cbind(test %>% select(Date, Open),
  #                         label=round(nn_test_pre,4),
  #                         train_size=nrow(train))
  # 
  # write.csv(nn_test_result, paste0("./data/Pred_result/", symbol, "_", obj_name, "_NN.csv"), row.names = F)
  # 
  # result <- list(
  #   "xgb_test_result" = xgb_test_result,
  #   "svm_test_result" = svm_test_result,
  #   "lr_test_result" = lr_test_result,
  #   "nn_test_result" = nn_test_result
  # )
  # 
  # return(result)
}

train_test_split <- function(Data, Label, inSelect=T){
  if(class(Label)=="ga"){
    train <- Data %>% 
      filter(Date < "2019-01-01") %>% 
      cbind(., label = as.vector(Label@solution[1,])+1) %>% 
      na.omit
    row.names(train) <- NULL
    
    if(inSelect==T){
      train <- train %>% 
        dplyr::mutate(label = ifelse(label-dplyr::lag(label)!=0 | is.na(label-dplyr::lag(label)), label, 0)) %>% 
        dplyr::filter(label != 0)
    }
  }else{
    train <- Data %>%
      filter(Date < "2019-01-01") %>% 
      merge(., Label, by="Date", all.x=T) %>% 
      na.omit
    row.names(train) <- NULL
  }
  test <- Data %>% 
    filter(Date > "2019-01-01")
  
  scale_col <- setdiff(names(train), c("Date", "Open", "Close", "label"))
  prepro <- preProcess(train[scale_col], method=c("center", "scale"))
  
  train[scale_col] <- predict(prepro, train[scale_col])
  test[scale_col] <- predict(prepro, test[scale_col])
  
  return(list(train=train, test=test))
}

pred_result <- function(TI_file, label_file, symbol, obj_name){
  
  ttl <- train_test_split(TI_file, label_file)
  
  pred <- prediction(ttl$train, ttl$test, symbol, obj_name)
  
  return(pred)
}

Symbols <- read.csv("../data/NASDAQ_Marketcap60.csv")$Symbol

if(!file.exists("../data/Pred_result")){
  dir.create("../data/Pred_result")
}

for (symbol in Symbols){
  stock <- read.csv(paste0("../data/Stock_TI/",symbol ,"_TI.csv"))
  stock$Date <- as.Date(stock$Date)
  
  ud_label <- read.csv(paste0("../data/UD_label/", symbol, "_UD.csv"))
  ud_label$Date <- as.Date(ud_label$Date)
  
  pred_result(stock, ud_label, symbol, "UD")
}

Symbols <- read.csv("../data/KOSPI_Marketcap60.csv")$종목코드

for (symbol in Symbols){
  symbol <- str_remove(symbol, "X")
  stock <- read.csv(paste0("../data/Stock_TI/",symbol ,"_TI.csv"))
  stock$Date <- as.Date(stock$Date)
  
  ud_label <- read.csv(paste0("../data/UD_label/", symbol, "_UD.csv"))
  ud_label$Date <- as.Date(ud_label$Date)
  
  pred_result(stock, ud_label, symbol, "UD")
}



