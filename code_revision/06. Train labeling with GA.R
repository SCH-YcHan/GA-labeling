rm(list=ls())

library(stringr)
source("Objective function.R")

NASDAQ <- read.csv("../data/Stock_data/pre_NASDAQ.csv")
NASDAQ$Date <- as.Date(NASDAQ$Date)

symbols <- read.csv("../data/NASDAQ_Marketcap60.csv")$Symbol

if(!file.exists("../data/GA_RDS")){
  dir.create("../data/GA_RDS")
}

for(symbol in symbols){
  if(!file.exists(paste0("../data/GA_RDS/", symbol, "_paper.rds"))){
    print(symbol)
    stock <- NASDAQ %>%
      select(Date, paste(symbol, "Open", sep="_")) %>% 
      filter(Date < "2019-01-01")
    
    names(stock) <- c("Date", "Open")
    
    GA_paper <- ga(type="binary",
                   fitness = obj_paper,
                   nBits = nrow(stock),
                   popSize = 300,
                   maxiter = 1000,
                   elitism = 3,
                   seed = 20207188,
                   parallel = T)
    
    saveRDS(GA_paper, paste0("../data/GA_RDS/", symbol, "_paper.rds"))
    
    gc()
  }
}

rm(list=ls())

source("Objective function.R")

KOSPI <- read.csv("../data/Stock_data/pre_KOSPI.csv")
KOSPI$Date <- as.Date(KOSPI$Date)

symbols <- read.csv("../data/KOSPI_Marketcap60.csv")$�����ڵ�

if(!file.exists("../data/GA_RDS")){
  dir.create("../data/GA_RDS")
}

for(symbol in symbols){
  if(!file.exists(paste0("../data/GA_RDS/", str_remove(symbol, "X"), "_paper.rds"))){
    print(symbol)
    stock <- KOSPI %>%
      select(Date, paste(symbol, "Open", sep="_")) %>% 
      filter(Date < "2019-01-01")
    
    names(stock) <- c("Date", "Open")
    
    GA_paper <- ga(type="binary",
                   fitness = obj_paper,
                   nBits = nrow(stock),
                   popSize = 300,
                   maxiter = 1000,
                   elitism = 3,
                   seed = 20207188,
                   parallel = T)
    
    saveRDS(GA_paper, paste0("../data/GA_RDS/", str_remove(symbol, "X"), "_paper.rds"))
    
    gc()
  }
}
