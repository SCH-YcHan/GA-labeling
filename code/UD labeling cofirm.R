rm(list=ls())

library(dplyr)

NASDAQ <- read.csv("./data/Stock_data/pre_NASDAQ.csv")
NASDAQ$Date <- as.Date(NASDAQ$Date)

symbols <- read.csv("./data/NASDAQ_Marketcap60.csv")$Symbol

UD_path <- "./data/UD_label/"

confirm_df <- data.frame()

for (symbol in symbols){
  ud <- read.csv(paste0(UD_path, symbol, "_UD.csv"))
  stock_labels <- ud$label
  
  stock <- NASDAQ %>%
    select(Date, paste(symbol, "Open", sep="_")) %>% 
    filter(Date < "2019-01-01")
  
  names(stock) <- c("Date", "Open")

  data <- cbind(stock, label=stock_labels) 
  data$label[nrow(data)]=2
  
  data2 <- data %>% 
    dplyr::select(Date, label) %>%
    dplyr::mutate(label = ifelse(label-dplyr::lag(label)!=0 | is.na(label-dplyr::lag(label)), label, 0)) %>%
    dplyr::filter(label != 0) %>% 
    merge(data %>% dplyr::mutate(Open = lead(Open)) %>% dplyr::select(Date, Open), by="Date", all=T) %>% 
    dplyr::filter(!is.na(label)) %>% 
    dplyr::filter(!is.na(Open))
  
  if(data2$label[1]==2){data2$label[1]=NA}
  
  data3 <- data2 %>%
    dplyr::filter(!is.na(label)) %>%
    dplyr::mutate(profit = Open-dplyr::lag(Open,1)) %>% 
    dplyr::filter(label == 2)
  
  N_trade <- nrow(data3)
  Nw <- sum(data3$profit>0)
  Wr <- Nw/N_trade
  mean_W <- mean(data3$profit[data3$profit>0])
  mean_L <- mean(abs(data3$profit[data3$profit<0]))
  Pr <- mean_W/mean_L
  Pf <- Pr*((Nw/(N_trade-Nw)))
  
  row <- data.frame(
    Symbol = symbol,
    N_trade = N_trade,
    Win_ratio = round(Wr,3),
    Payoff_ratio = round(Pr,3),
    Profit_factor = round(Pf,3)
  )
  
  confirm_df <- rbind(confirm_df, row)
}

write.csv(confirm_df, "./data/UD_result.csv", row.names = F)


