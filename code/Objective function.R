library(GA)
library(dplyr)

#Kim & Enke (2017)
obj_paper <- function(stock_labels){
  data <- cbind(stock, label=as.vector(stock_labels)+1)
  
  data$label[nrow(data)]=2
  
  data2 <- data %>% 
    dplyr::select(Date, label) %>%
    dplyr::mutate(label = ifelse(label-dplyr::lag(label)!=0 | is.na(label-dplyr::lag(label)), label, 0)) %>%
    dplyr::filter(label != 0) %>% 
    merge(data %>% dplyr::mutate(Open = lead(Open)) %>% dplyr::select(Date, Open), by="Date", all=T) %>% 
    dplyr::filter(!is.na(label)) %>% 
    dplyr::filter(!is.na(Open))
  
  if((length(data2$label)==1 & data2$label[1]==2) |
     (length(data2$label)==2 & data2$label[1]==2)){return(min(data$Open)-max(data$Open))}
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
  
  if(N_trade>30 & Wr>=0.33 & Wr<=0.80 & Pr>=0.25 & Pr<=2.00 & Pf>=1){
    return((Wr*mean_W-(1-Wr)*mean_L)/mean_L*N_trade)
  }else{
    return(min(data$Open)-max(data$Open))
  }
}




