rm(list=ls())

library(dplyr)
library(ggplot2)
library(stringr)

NASDAQ_Marketcap <- read.csv("../data/Stock_info/NASDAQ.csv")
NASDAQ <- read.csv("../data/Stock_data/pre_NASDAQ.csv")

remove_nasdaq <- c(
  "CERN", "CGNX", "CPRT", "CSGP", "CTXS",
  "FAST", "FISV", "HSIC", "ODFL", "PEGA", 
  "PLUG", "XLNX")
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

rm(list=ls())

KOSPI_Marketcap <- read.csv("../data/Stock_info/KOSPI.csv",
                            fileEncoding = 'euc-kr')
KOSPI <- read.csv("../data/Stock_data/pre_KOSPI.csv")

remove_kospi <- c(
  'X015940.KS', 'X003450.KS', 'X000830.KS', 'X032390.KS', 'X053000.KS',
  'X003600.KS', 'X001300.KS', 'X103150.KS', 'X037620.KS')
names2 <- KOSPI %>%
  select(contains("_Open")) %>%
  colnames %>% 
  str_split("_", simplify=T) %>% 
  .[,1] %>% 
  setdiff(remove_kospi)

KOSPI_Marketcap$종목코드 <- paste0(
  "X",
  sprintf("%06d", KOSPI_Marketcap$종목코드),
  ".KS"
)

KM <- KOSPI_Marketcap %>% 
  filter(종목코드 %in% names2) %>% 
  arrange(desc(상장시가총액)) %>% 
  head(60)

write.csv(KM, "../data/KOSPI_Marketcap60.csv", row.names = F)


