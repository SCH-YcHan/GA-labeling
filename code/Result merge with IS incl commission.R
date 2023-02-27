rm(list=ls())

library(dplyr)
library(stringr)

Symbols <- read.csv("../data/NASDAQ_Marketcap60.csv")$Symbol

paper_result <- data.frame()
for(symbol in Symbols){
  f <- read.csv(paste0("../data/Trading_result_IS_commission/", symbol, "_paper.csv"))
  f2 <- f %>% 
    filter(str_detect(X, "test")) %>% 
    mutate(Model = str_split(X, "_", simplify = T)[,1],
           Symbol = symbol) %>% 
    select(-X)
  
  paper_result <- rbind(paper_result, f2)
}

write.csv(paper_result, "../data/paper_result_IS_commission_60.csv", row.names=F)

