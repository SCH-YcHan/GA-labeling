rm(list=ls())

source("Objective function.R")

NASDAQ <- read.csv("../data/Stock_data/pre_NASDAQ.csv")
NASDAQ$Date <- as.Date(NASDAQ$Date)

symbols <- read.csv("../data/NASDAQ_Marketcap60.csv")$Symbol

for(symbol in symbols){
  stock <- NASDAQ %>%
    select(Date, paste(symbol, "Open", sep="_")) %>% 
    filter(Date < "2019-01-01")
  
  names(stock) <- c("Date", "Open")
  
  GA_sharp <- ga(type="binary",
                 fitness = obj_sharp,
                 nBits = nrow(stock),
                 popSize = 300,
                 maxiter = 1000,
                 elitism = 10,
                 seed = 20207188,
                 parallel = T)
  
  saveRDS(GA_sharp, paste0("../data/GA_RDS/", symbol, "_sharp.rds"))
  
  GA_point <- ga(type="binary",
                 fitness = obj_point,
                 nBits = nrow(stock),
                 popSize = 300,
                 maxiter = 1000,
                 elitism = 10,
                 seed = 20207188,
                 parallel = T)
  
  saveRDS(GA_point, paste0("../data/GA_RDS/", symbol, "_point.rds"))
  
  GA_paper <- ga(type="binary",
                 fitness = obj_paper,
                 nBits = nrow(stock),
                 popSize = 300,
                 maxiter = 1000,
                 elitism = 10,
                 seed = 20207188,
                 parallel = T)
  
  saveRDS(GA_paper, paste0("../data/GA_RDS/", symbol, "_paper.rds"))
  
  gc()
}
