rm(list=ls())

library(dplyr)
library(stringr)

#load trading result
NASDAQ_UD_result <- read.csv("../data/NASDAQ_UD_commission_result_60.csv")
KOSPI_UD_result <- read.csv("../data/KOSPI_UD_commission_result_60.csv")

NASDAQ_SGA_result <- read.csv("../data/NASDAQ_paper_result_IS_commission_60.csv")
KOSPI_SGA_result <- read.csv("../data/KOSPI_paper_result_IS_commission_60.csv")

remove_symbol <- c()
rds_path <- "../data/GA_RDS/"
for(s in list.files(rds_path)){
  ga_rds <- readRDS(paste0(rds_path,s))
  if(ga_rds@fitnessValue<0){
    remove_symbol <- c(remove_symbol, str_split(s, "_")[[1]][1])
  }
}
remove_symbol <- c(remove_symbol, "004940.KS")

NASDAQ_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="lr") %>% 
  arrange(Symbol) %>% 
  .$Symbol

KOSPI_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="lr") %>% 
  arrange(Symbol) %>% 
  .$Symbol

