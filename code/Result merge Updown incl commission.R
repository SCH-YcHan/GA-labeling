rm(list=ls())

library(dplyr)
library(stringr)

Symbols <- read.csv("../data/NASDAQ_Marketcap60.csv")$Symbol

UD_result <- data.frame()
for(symbol in Symbols){
  f <- read.csv(paste0("../data/UD_result_commission/", symbol, "_UD.csv"))
  f2 <- f %>% 
    filter(str_detect(X, "test")) %>% 
    mutate(Model = str_split(X, "_", simplify = T)[,1],
           Symbol = symbol) %>% 
    select(-X)
  
  UD_result <- rbind(UD_result, f2)
}

write.csv(UD_result, "../data/UD_commission_result_60.csv", row.names=F)


