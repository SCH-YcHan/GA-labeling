library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

#Up Down 기본 모형
UpDown <- function(close_data, N=1, logging=F){
  lead1 <- lead(close_data, n=N)

  label <- ifelse(lead1-close_data>0, 1, 2)
  
  if(logging==T){
    log_df <- data.frame(close_data = close_data,
                         lead = lead1,
                         log_profit = log(lead1)-log(close_data),
                         label = label)
    write.csv(log_df,"./Logging/UpDown.csv", row.names=F)
  }
  
  return(label)
}

#instance selection
instance_selection <- function(data, Dep_Label, dup=T){
  if (dup==T){
    data2 <- data %>% 
      filter(get(Dep_Label) != 0) %>% 
      mutate(Train_N_before = nrow(.)) %>% 
      mutate(label2 = ifelse(get(Dep_Label)-lag(get(Dep_Label))!=0 |
                               is.na(get(Dep_Label)-lag(get(Dep_Label))), get(Dep_Label), 0)) %>% 
      filter(label2 != 0)
  } else{
    data2 <- data %>% 
      filter(get(Dep_Label) != 0) %>% 
      mutate(Train_N_before = nrow(.)) %>% 
      mutate(label2 = get(Dep_Label))
  }
  if(data2$label2[1]==2){data2$label2[1]=NA}
  if(data2$label2[nrow(data2)]==1){data2$label2[nrow(data2)]=NA}
  
  data3 <- data2 %>%
    filter(!is.na(label2)) %>%
    select(-label2) %>% 
    mutate(Train_N_after = nrow(.))
  
  return(data3)
}

#profit calculate
labeling_metrics <- function(data, merge_data, symbol, logging=F){
  data$label[nrow(data)]=2
  
  data2 <- data %>% 
    select(Date, label) %>%
    filter(label != 0) %>% 
    mutate(label = ifelse(label-lag(label)!=0 | is.na(label-lag(label)), label, 0)) %>%
    filter(label != 0) %>% 
    merge(merge_data, by="Date", all=T) %>% 
    filter(!is.na(label))
  
  if(length(data2$label)==1 & data2$label[1]==2){
    return(data.frame(Symbol = symbol,
                      Test_N_before = data %>% filter(label != 0) %>% nrow(),
                      Test_N_after = 0,
                      Win_rate = NaN,
                      Mean_gain = NaN,
                      Mean_loss = NaN,
                      Payoff_ratio = NaN,
                      Profit_factor = NaN,
                      cum_profit = 0))
  }
  if(data2$label[1]==2){data2$label[1]=NA}
  
  data3 <- data2 %>%
    filter(!is.na(label)) %>%
    mutate(profit = Open-lag(Open,1)) %>% 
    filter(label == 2) 
  
  if(logging==T){
    write.csv(merge(merge(data, data2, by="Date", all.x=T), data3, by="Date", all.x=T),
              paste0("./Logging/", symbol, "_Profit.csv"), row.names=F)
  }
  
  N_trade <- nrow(data3)
  Nw <- sum(data3$profit>0)
  Wr <- Nw/N_trade
  mean_W <- mean(data3$profit[data3$profit>0])
  mean_L <- mean(abs(data3$profit[data3$profit<0]))
  Pwl <- mean_W/mean_L
  Pf <- Pwl*((Nw/(N_trade-Nw)))

  return(data.frame(Symbol = symbol,
                    Test_N_before = data %>% filter(label != 0) %>% nrow(),
                    Test_N_after = N_trade,
                    Win_rate = Wr,
                    Mean_gain = mean_W,
                    Mean_loss = mean_L,
                    Payoff_ratio = Pwl,
                    Profit_factor = Pf,
                    cum_profit = sum(data3$profit)))  
}

#Data Split function
data_split <- function(Data, method="HoldOut", period="Y", TEST_Date="2018-01-01", logging=F){
  DDD <- as.Date(Data$Date)
  if(method=="HoldOut"){
    Data <- Data %>% 
      mutate(case1 = ifelse(as.Date(Date)<TEST_Date, "Train", "Test"))
  }else if(method=="TsCV"){
    n <- 1
    if(period=="Y"){
      tys <- unique(substr(DDD[DDD>="2018-01-01"],1 ,4))
      for(y in tys){
        Data <- Data %>%
          mutate("case{n}" := ifelse(as.Date(Date) < paste0(y, "-01-01"), "Train",
                                     ifelse(str_detect(Date, y), "Test", NA)))
        n <- n+1
      }
    }else if(period=="M"){
      tms <- unique(substr(DDD[DDD>="2018-01-01"],1 ,7))
      for(m in tms){
        Data <- Data %>%
          mutate("case{n}" := ifelse(as.Date(Date) < paste0(m, "-01"), "Train",
                                     ifelse(str_detect(Date, m), "Test", NA)))
        n <- n+1
      }
    }else if(str_detect(period, "[[:digit:]]M")){
      nm <- as.numeric(str_extract(period , "[[:digit:]]+"))
      tms <- unique(substr(DDD[DDD>="2018-01-01"],1 ,7))
      tms <- tms[seq(1, length(tms), nm)]
      tms <- c(tms, substr(as.character(as.Date(paste0(tms[length(tms)], "-01"))+months(nm)),1,7))

      for(m in 1:(length(tms)-1)){
        Data <- Data %>%
          mutate("case{n}" := ifelse(as.Date(Date) < paste0(tms[m], "-01"), "Train",
                                     ifelse(as.Date(Date) >= paste0(tms[m], "-01") &
                                              as.Date(Date) < paste0(tms[m+1], "-01"), "Test", NA)))
        n <- n+1
      }
    }else{
      tds <- DDD[DDD>="2018-01-01"]
      for(d in tds){
        Data <- Data %>% 
          mutate("case{n}" := ifelse(as.Date(Date) < d, "Train",
                                     ifelse(as.Date(Date)==d, "Test", NA)))
        n <- n+1
      }
    }
  }else if(method=="SWTsCV"){
    n <- 1
    nm <- as.numeric(str_extract(period , "[[:digit:]]+"))
    tms <- unique(substr(DDD[DDD>="2018-01-01"],1 ,7))
    tms <- tms[seq(1, length(tms), nm)]
    tms <- c(tms, substr(as.character(as.Date(paste0(tms[length(tms)], "-01"))+months(nm)),1,7))

    for(m in 1:(length(tms)-1)){
      Data <- Data %>%
        mutate("case{n}" := ifelse(as.Date(Date) >= as.Date(paste0(tms[m], "-01"))-years(9) &
                                     as.Date(Date) < paste0(tms[m], "-01"), "Train",
                                   ifelse(as.Date(Date) >= paste0(tms[m], "-01") &
                                            as.Date(Date) < paste0(tms[m+1], "-01"), "Test", NA)))
      n <- n+1
    }
  }else{
    print("Error : method = 'HoldOut' or 'TsCV' or 'SWTsCV'")
  }
  if(logging==T){
    write.csv(Data, paste0("./Logging/", method, "_Split_",  period, ".csv"), row.names=F)
  }
  return(Data)
}
