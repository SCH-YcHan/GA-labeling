rm(list=ls())

library(dplyr)
library(stringr)

#load trading result
NASDAQ_UD_LR <- read.csv("../data/NASDAQ_UD_LR.csv")
NASDAQ_UD_NN <- read.csv("../data/NASDAQ_UD_NN.csv")
NASDAQ_UD_XGB <- read.csv("../data/NASDAQ_UD_XGB.csv")

NASDAQ_SGA_LR <- read.csv("../data/NASDAQ_SGA_LR.csv")
NASDAQ_SGA_NN <- read.csv("../data/NASDAQ_SGA_NN.csv")
NASDAQ_SGA_XGB <- read.csv("../data/NASDAQ_SGA_XGB.csv")

KOSPI_UD_LR <- read.csv("../data/KOSPI_UD_LR.csv")
KOSPI_UD_NN <- read.csv("../data/KOSPI_UD_NN.csv")
KOSPI_UD_XGB <- read.csv("../data/KOSPI_UD_XGB.csv")

KOSPI_SGA_LR <- read.csv("../data/KOSPI_SGA_LR.csv")
KOSPI_SGA_NN <- read.csv("../data/KOSPI_SGA_NN.csv")
KOSPI_SGA_XGB <- read.csv("../data/KOSPI_SGA_XGB.csv")

NASDAQ_UD_LR_PF <- NASDAQ_UD_LR %>% arrange(Symbol) %>% .$Profit_factor
NASDAQ_SGA_LR_PF <- NASDAQ_SGA_LR %>% arrange(Symbol) %>% .$Profit_factor

shapiro.test(NASDAQ_SGA_LR_PF)
shapiro.test(NASDAQ_UD_LR_PF)

wilcox.test(NASDAQ_SGA_LR_PF, NASDAQ_UD_LR_PF, paired=T)
wilcox.test(NASDAQ_SGA_LR_PF, NASDAQ_UD_LR_PF, paired=T, alternative="greater")

NASDAQ_UD_NN_PF <- NASDAQ_UD_NN %>% arrange(Symbol) %>% .$Profit_factor
NASDAQ_SGA_NN_PF <- NASDAQ_SGA_NN %>% arrange(Symbol) %>% .$Profit_factor

shapiro.test(NASDAQ_SGA_NN_PF)
shapiro.test(NASDAQ_UD_NN_PF)

wilcox.test(NASDAQ_SGA_NN_PF, NASDAQ_UD_NN_PF, paired=T)
wilcox.test(NASDAQ_SGA_NN_PF, NASDAQ_UD_NN_PF, paired=T, alternative="greater")

NASDAQ_UD_XGB_PF <- NASDAQ_UD_XGB %>% arrange(Symbol) %>% .$Profit_factor
NASDAQ_SGA_XGB_PF <- NASDAQ_SGA_XGB %>% arrange(Symbol) %>% .$Profit_factor

shapiro.test(NASDAQ_SGA_XGB_PF)
shapiro.test(NASDAQ_UD_XGB_PF)

wilcox.test(NASDAQ_SGA_XGB_PF, NASDAQ_UD_XGB_PF, paired=T)
wilcox.test(NASDAQ_SGA_XGB_PF, NASDAQ_UD_XGB_PF, paired=T, alternative="greater")





KOSPI_UD_LR_PF <- KOSPI_UD_LR %>% arrange(Symbol) %>% .$Profit_factor
KOSPI_SGA_LR_PF <- KOSPI_SGA_LR %>% arrange(Symbol) %>% .$Profit_factor

shapiro.test(KOSPI_SGA_LR_PF)
shapiro.test(KOSPI_UD_LR_PF)

wilcox.test(KOSPI_SGA_LR_PF, KOSPI_UD_LR_PF, paired=T)
wilcox.test(KOSPI_SGA_LR_PF, KOSPI_UD_LR_PF, paired=T, alternative="greater")

KOSPI_UD_NN_PF <- KOSPI_UD_NN %>% arrange(Symbol) %>% .$Profit_factor
KOSPI_SGA_NN_PF <- KOSPI_SGA_NN %>% arrange(Symbol) %>% .$Profit_factor

shapiro.test(KOSPI_SGA_NN_PF)
shapiro.test(KOSPI_UD_NN_PF)

wilcox.test(KOSPI_SGA_NN_PF, KOSPI_UD_NN_PF, paired=T)
wilcox.test(KOSPI_SGA_NN_PF, KOSPI_UD_NN_PF, paired=T, alternative="greater")

KOSPI_UD_XGB_PF <- KOSPI_UD_XGB %>% arrange(Symbol) %>% .$Profit_factor
KOSPI_SGA_XGB_PF <- KOSPI_SGA_XGB %>% arrange(Symbol) %>% .$Profit_factor

shapiro.test(KOSPI_SGA_XGB_PF)
shapiro.test(KOSPI_UD_XGB_PF)

wilcox.test(KOSPI_SGA_XGB_PF, KOSPI_UD_XGB_PF, paired=T)
wilcox.test(KOSPI_SGA_XGB_PF, KOSPI_UD_XGB_PF, paired=T, alternative="greater")



