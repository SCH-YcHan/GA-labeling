rm(list=ls())

library(dplyr)
library(ggplot2)
library(stringr)

NASDAQ_Marketcap <- read.csv("../data/Stock_info/NASDAQ.csv")

NASDAQ <- read.csv("../data/Stock_data/pre_NASDAQ.csv")

remove_nasdaq <- c("CGNX", "CPRT", "CSGP", "FAST", "HSIC", "ODFL", "PEGA", "PLUG")
names2 <- NASDAQ %>%
  select(contains("_Open")) %>%
  colnames %>% 
  str_split("_", simplify=T) %>% 
  .[,1] %>% 
  setdiff(remove_nasdaq)

NM <- NASDAQ_Marketcap %>% 
  filter(Symbol %in% names2) %>% 
  arrange(desc(Market.Cap)) %>% 
  head(60)

write.csv(NM, "../data/NASDAQ_Marketcap60.csv", row.names = F)
