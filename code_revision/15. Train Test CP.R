rm(list=ls())

library(GA)
library(zoo)
library(grid)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)

ensemble_fun <- function(ensem_v, symbol, type=F){
  v_len <- length(ensem_v)
  
  if(type==T){
    for (i in 1:v_len){
      f <- read.csv(paste0("./data/Pred_result/", symbol, "_paper_", ensem_v[i], ".csv"))
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

figure <- function(symbol, p1_margin, p4_margin){
  ud <- read.csv(paste0("./data/UD_label/", symbol, "_UD.csv"))
  ga <- readRDS(paste0("./data/GA_RDS/", symbol, "_paper.rds"))
  ti <- read.csv(paste0("./data/Stock_TI/", symbol, "_TI.csv"))
  ud$label2 <- ga@solution[1,]+1 %>% as.vector
  train <- left_join(ud, ti %>% select(Date, Open), by="Date")
  train$label <- na.locf(train$label)
  
  ud_ensemble <- ensemble_fun(c("XGB"), symbol, F)
  ga_ensemble <- ensemble_fun(c("LR", "NN", "XGB"), symbol, T)
  if(ga_ensemble$label[1]==0){ga_ensemble$label[1]=2}
  ud_ensemble$label2 <- ga_ensemble$label
  test <- ud_ensemble %>% select(-train_size)
  test$label[test$label==0] <- NA
  test$label <- na.locf(test$label)
  
  test$label2[test$label2==0] <- NA
  test$label2 <- na.locf(test$label2)

  p1_test <- test %>%
    select(Date, label2) %>%
    mutate(label2 = ifelse(label2-lag(label2)!=0 | is.na(label2-lag(label2)), label2, 0)) %>%
    filter(label2 != 0) %>% 
    merge(test %>% mutate(Open = lead(Open)) %>% select(Date, Open), by="Date", all=T) %>% 
    filter(!is.na(Open))
  
  if(p1_test$label2[1]==2){p1_test$label2[1]=NA}
    
  p1_test$label2 <- ifelse(p1_test$label2==1, "Buy", ifelse(p1_test$label2==2, "Sell", NA))
  
  a <- p1_test %>% 
    mutate(Label = factor(label2, levels=c("Buy", "Sell"))) %>% 
    select(-label2)
  
  p1 <- a %>%
    ggplot(aes(x=as.Date(Date), y=Open)) + 
    geom_line() + 
    theme_bw() +
    labs(x = NULL, y = "Stock Price") +
    (a %>% 
       filter(!is.na(Label)) %>% 
       geom_point(mapping = aes(x=as.Date(Date), y=Open, color=Label, shape=Label, fill=Label))) + 
    scale_color_manual(values = c("green", "red")) +
    scale_shape_manual(values = c(24,25)) +
    scale_fill_manual(values = c("green", "red")) + 
    theme(
      legend.text = element_text(size=10),
      legend.position = c(0.07, 0.75),
      legend.box.background = element_rect(colour = "black"),
      axis.title.y = element_text(size=12, margin=margin(r=p1_margin))
    )
  
  p2 <- ggplot(test, aes(x=as.Date(Date), y=label)) +
    geom_step() +
    labs(x=NULL, y="Updown") +
    theme_bw() +
    theme(
      axis.title.y = element_text(size=12, margin=margin(r=5))
    )
  
  p3 <- ggplot(test, aes(x=as.Date(Date), y=label2)) +
    geom_step() +
    labs(x=NULL, y="SGA") +
    theme_bw() +
    theme(
      axis.title.y = element_text(size=12, margin=margin(r=5))
    )
  
  trading <- test
  trading$label[nrow(trading)]=2
  trading$label2[nrow(trading)]=2
  
  trading_label <- trading %>% 
    select(Date, label) %>%
    mutate(label = ifelse(label-lag(label)!=0 | is.na(label-lag(label)), label, 0)) %>%
    filter(label != 0) %>% 
    merge(trading %>% mutate(Open = lead(Open)) %>% select(Date, Open), by="Date", all=T) %>% 
    filter(!is.na(label)) %>% 
    filter(!is.na(Open))
  
  if(trading_label$label[1]==2){trading_label$label[1]=NA}
  
  trading_label <- trading_label %>%
    filter(!is.na(label)) %>%
    mutate(profit = Open-lag(Open,1)) %>% 
    filter(label == 2)
  
  trading_label2 <- trading %>% 
    select(Date, label2) %>%
    mutate(label2 = ifelse(label2-lag(label2)!=0 | is.na(label2-lag(label2)), label2, 0)) %>%
    filter(label2 != 0) %>% 
    merge(trading %>% mutate(Open = lead(Open)) %>% select(Date, Open), by="Date", all=T) %>% 
    filter(!is.na(label2)) %>% 
    filter(!is.na(Open))
  
  if(trading_label2$label2[1]==2){trading_label2$label2[1]=NA}
  
  trading_label2 <- trading_label2 %>%
    filter(!is.na(label2)) %>%
    mutate(profit2 = Open-lag(Open,1)) %>% 
    filter(label2 == 2)
  
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
  
  p4 <- ggplot(df_long, aes(x = as.Date(Date), y = cumulative_profit, color = Labeling, linetype = Labeling)) +
    geom_step() +
    labs(x = NULL, y = "Cumulative Profit") +
    scale_color_manual(values = c("green", "red")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    theme_bw() +
    theme(
      legend.text = element_text(size=10),
      legend.position = c(0.07, 0.75),
      legend.box.background = element_rect(colour = "black"),
      axis.title.y = element_text(size=12, margin=margin(r=p4_margin))
    )
  
  result <- arrangeGrob(
    p1, p2, p3, p4,
    nrow=4
  )

  return(result)
}

AAPL <- figure("AAPL", 7, 8)
png("AAPL Test CP.png", width=3000, height=3000, res=300)
grid.draw(AAPL)
dev.off()

AMGN <- figure("AMGN", 7, 12.5)
png("AMGN Test CP.png", width=3000, height=3000, res=300)
grid.draw(AMGN)
dev.off()

VRTX <- figure("VRTX", 7, 5)
png("VRTX Test CP.png", width=3000, height=3000, res=300)
grid.draw(VRTX)
dev.off()
