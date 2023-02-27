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

#N-day 변동성/수익률 모형
Nday_VDP <- function(close_data, u_sig=0.2, l_sig=0.2, N=5,
                     plotting=F, plot_name="", logging=F){
  Nlead <- lead(close_data, N)
  
  rtn <- log(Nlead)-log(close_data)
  
  rtn_mean <- mean(rtn, na.rm=T)
  rtn_sd <- sd(rtn, na.rm=T)

  upper <- rtn_mean+rtn_sd*u_sig
  lower <- rtn_mean-rtn_sd*l_sig
  label <- ifelse(rtn>upper, 1, ifelse(rtn<lower, 2, 0))
  
  if(logging==T){
    log_df <- data.frame(close_data = close_data,
                         lead = Nlead,
                         rtn = rtn,
                         rtn_mean = rtn_mean,
                         rtn_sd = rtn_sd,
                         upper = upper,
                         lower = lower,
                         label = label)
    write.csv(log_df,paste0("./Logging/",N,"day_VDP.csv"), row.names = F)
  }

  if(plotting==T){
    png(paste0(plot_name, ".png"), width=3000, height=1800, res=300)
    hist(
      rtn,
      breaks=30,
      main=plot_name,
      xlab="rtn(log)",
      cex.lab=1.5,
      cex.main=2
    )
    abline(v=rtn_mean, col="green", lwd=2)
    abline(v=upper, col="red", lwd=2, lty=2)
    abline(v=lower, col="blue", lwd=2, lty=3)
    legend(
      "topleft",
      legend=c("Lower","rtn_mean","Upper"),
      lty=c(3,1,2),
      col=c("blue","green","red"),
      cex=1.3
    )
    dev.off()
  }
  return(label)
}

#N-day Barrier 모형
Nday_Barrier <- function(close_data, u_sig=0.2, l_sig=0.2, N=20,
                         plotting=F, plot_name="", logging=F){
  Nlead <- lead(close_data, N)
  
  rtn <- log(Nlead)-log(close_data)
  upper <- sd(rtn, na.rm=T)*u_sig
  lower <- sd(rtn, na.rm=T)*-l_sig
  
  label <- c()
  
  touch_idx <- c()

  for(i in 1:(length(close_data)-N)){
    data <- close_data[i:(i+N)]
    rtn_i <- log(data[2:(N+1)])-log(data[1])
    
    labels <- ifelse(rtn_i>upper, 1, ifelse(rtn_i<lower, 2, 0))
    
    sul <- sum(unique(labels))
    if(sul==0){
      label <- c(label, 0)
      touch_idx <- c(touch_idx, NA)
    }else if(sul==1){
      label <- c(label, 1)
      touch_idx <- c(touch_idx, which(labels==1)[1])
    }else if(sul==2){
      label <- c(label, 2)
      touch_idx <- c(touch_idx, which(labels==2)[1])
    }else{
      first <- ifelse(which(labels==1)[1] < which(labels==2)[1], 1, 2)
      label <- c(label, first)
      touch_idx <- c(touch_idx, which(labels==first)[1])
    }
  }
  if(logging==T){
    log_df <- data.frame(close_data = close_data,
                         lead = Nlead,
                         rtn = rtn,
                         upper = upper,
                         lower = lower,
                         touch_idx = c(touch_idx, rep(NA, N)),
                         label = c(label, rep(NA, N)))
    write.csv(log_df,paste0("./Logging/",N,"day_Barrier.csv"), row.names = F)
  }
  return(c(label, rep(NA,N)))
}

#Trade action 모형
Trade_action <- function(close_data, window=11, logging=F){
  label <- c()
  m_i <- floor(window/2)+1
  min_idx <- c()
  max_idx <- c()
  for(i in 1:(length(close_data)-window+1)){
    data <- close_data[i:(i+window-1)]
    labels <- ifelse(which.min(data)==m_i, 1, ifelse(which.max(data)==m_i, 2, 0))
    min_idx <- c(min_idx, which.min(data))
    max_idx <- c(max_idx, which.max(data))
    label <- c(label, labels)
  }
  if(logging==T){
    log_df <- data.frame(close_data = close_data,
                         median_idx = m_i,
                         min_idx = c(rep(NA, m_i-1), min_idx, rep(NA, window-m_i)),
                         max_idx = c(rep(NA, m_i-1), max_idx, rep(NA, window-m_i)),
                         label = c(rep(NA, m_i-1), label, rep(NA, window-m_i)))
    write.csv(log_df,paste0("./Logging/",window,"Trade_action.csv"), row.names = F)
  }
  return(c(rep(NA, m_i-1), label, rep(NA, window-m_i)))
}

#Reverse Trade action 모형
Reverse_Trade_action <- function(close_data, window=11, logging=F){
  label <- c()
  m_i <- floor(window/2)+1
  min_idx <- c()
  max_idx <- c()
  for(i in 1:(length(close_data)-window+1)){
    data <- close_data[i:(i+window-1)]
    labels <- ifelse(which.min(data)==m_i, 2, ifelse(which.max(data)==m_i, 1, 0))
    min_idx <- c(min_idx, which.min(data))
    max_idx <- c(max_idx, which.max(data))
    label <- c(label, labels)
  }
  if(logging==T){
    log_df <- data.frame(close_data = close_data,
                         median_idx = m_i,
                         min_idx = c(rep(NA, m_i-1), min_idx, rep(NA, window-m_i)),
                         max_idx = c(rep(NA, m_i-1), max_idx, rep(NA, window-m_i)),
                         label = c(rep(NA, m_i-1), label, rep(NA, window-m_i)))
    write.csv(log_df,paste0("./Logging/",window,"Trade_action.csv"), row.names = F)
  }
  return(c(rep(NA, m_i-1), label, rep(NA, window-m_i)))
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
