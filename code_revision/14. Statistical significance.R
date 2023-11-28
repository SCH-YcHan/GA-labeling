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

NASDAQ_UD_lr_pf <- NASDAQ_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="lr") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

NASDAQ_SGA_lr_pf <- NASDAQ_SGA_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="lr") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

t.test(NASDAQ_UD_lr_pf, NASDAQ_SGA_lr_pf, paired = TRUE)

NASDAQ_UD_nn_pf <- NASDAQ_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="nn") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

NASDAQ_SGA_nn_pf <- NASDAQ_SGA_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="nn") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

t.test(NASDAQ_UD_nn_pf, NASDAQ_SGA_nn_pf, paired = TRUE)

NASDAQ_UD_xgb_pf <- NASDAQ_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="xgb") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

NASDAQ_SGA_xgb_pf <- NASDAQ_SGA_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="xgb") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

t.test(NASDAQ_UD_xgb_pf, NASDAQ_SGA_xgb_pf, paired = TRUE)




KOSPI_UD_lr_pf <- KOSPI_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="lr") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

KOSPI_SGA_lr_pf <- KOSPI_SGA_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="lr") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

t.test(KOSPI_UD_lr_pf, KOSPI_SGA_lr_pf, paired = TRUE)

KOSPI_UD_nn_pf <- KOSPI_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="nn") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

KOSPI_SGA_nn_pf <- KOSPI_SGA_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="nn") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

t.test(KOSPI_UD_nn_pf, KOSPI_SGA_nn_pf, paired = TRUE)

KOSPI_UD_xgb_pf <- KOSPI_UD_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="xgb") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

KOSPI_SGA_xgb_pf <- KOSPI_SGA_result %>% 
  filter(!(Symbol %in% remove_symbol)) %>% 
  filter(Model=="xgb") %>% 
  arrange(Symbol) %>% 
  .$Profit_factor

t.test(KOSPI_UD_xgb_pf, KOSPI_SGA_xgb_pf, paired = TRUE)



