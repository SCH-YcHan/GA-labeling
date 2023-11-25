rm(list=ls())

library(data.table)
library(dplyr)
library(stringr)

NASDAQ <- fread("../data/Stock_data/NASDAQ.csv") %>% data.frame

#replace colname
cn1 <- NASDAQ[1,] %>% unlist %>% as.vector
cn2 <- str_split(names(NASDAQ), "\\.", simplify = T)[,1]

col_names <- paste(cn1, cn2, sep="_")
col_names[1] <- "Date"

names(NASDAQ) <- col_names

#remove first row
NASDAQ <- NASDAQ[-1,]
rownames(NASDAQ) <- NULL

#replace var type
NASDAQ$Date <- as.Date(NASDAQ$Date)
NASDAQ[,-1] <- lapply(NASDAQ[,-1], as.numeric) %>% data.frame

#df to csv
write.csv(NASDAQ, "../data/Stock_data/pre_NASDAQ.csv", row.names=F)

rm(list=ls())

KOSPI <- fread("../data/Stock_data/KOSPI.csv") %>% data.frame

#replace colname
cn1 <- KOSPI[1,] %>% unlist %>% as.vector
cn2 <- str_split(names(KOSPI), "\\.", simplify = T)[,1]

col_names <- paste(cn1, cn2, sep="_")
col_names[1] <- "Date"

names(KOSPI) <- col_names

#remove first row
KOSPI <- KOSPI[-1,]
rownames(KOSPI) <- NULL

#replace var type
KOSPI$Date <- as.Date(KOSPI$Date)
KOSPI[,-1] <- lapply(KOSPI[,-1], as.numeric) %>% data.frame

#df to csv
write.csv(KOSPI, "../data/Stock_data/pre_KOSPI.csv", row.names=F)

