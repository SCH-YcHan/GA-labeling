rm(list=ls())

source("./code/Labeling_code.R")

NASDAQ <- read.csv("./data/Stock_data/pre_NASDAQ.csv")
NASDAQ$Date <- as.Date(NASDAQ$Date)

symbols <- read.csv("./data/NASDAQ_Marketcap60.csv")$Symbol
#symbols <- symbols[31:60]

if(!file.exists("./data/UD_label")){
  dir.create("./data/UD_label")
}

for(symbol in symbols){
  stock <- NASDAQ %>%
    select(Date, paste(symbol, "Open", sep="_")) %>% 
    filter(Date < "2019-01-01")
  
  names(stock) <- c("Date", "Open")
  
  stock %>% 
    select(Open) %>% 
    lapply(function(x){data.frame(label = UpDown(x, N=1))}) %>% 
    do.call(cbind, .) %>% 
    cbind(Date=stock$Date, .) %>% 
    write.csv(paste0("./data/UD_label/", symbol, "_UD.csv"), row.names=F)
}
