rm(list=ls())

library(dplyr)
library(ggplot2)
library(gridExtra)

paper_result <- read.csv("../data/paper_result_IS_commission_60.csv")
UD_result <- read.csv("../data/UD_commission_result_60.csv")

MEAN_SD <- function(x){
  M <- round(mean(x, na.rm=T), 2)
  S <- round(sd(x, na.rm=T), 2)
  return(paste0(M,"(",S,")"))
}

paper_result %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

UD_result %>% 
  select(-Symbol, -Cum_Profit, -Buy_hold) %>% 
  group_by(Model) %>% 
  summarise_all(.funs=MEAN_SD)

boxplot_plotting <- function(d_, x_, y_, s_, title_name){
  a <- ggplot(d_, aes(x=get(x_), y=get(y_), color=get(x_))) +
    ggtitle(title_name) +
    geom_boxplot(width=0.8) +
    geom_jitter(shape=16, position=position_jitter(0.2)) +
    theme_bw()+
    xlab("")+
    ylab("")+
    theme(plot.title = element_text(size=18, hjust=0.5),
          legend.title=element_blank(),
          legend.position = "none",
          panel.grid.major = element_line(size=1, color=alpha("black", 0.08)),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(size=13),
          axis.text.y = element_text(size=13)) +
    scale_y_continuous(limits=s_) + 
    scale_x_discrete(limits=c("lr", "svm", "xgb", "nn"))
  return(a)
}

w1 <- boxplot_plotting(paper_result, "Model", "Payoff_ratio", c(0,5), "Payoff ratio (Obj: paper)")
w2 <- boxplot_plotting(UD_result, "Model", "Payoff_ratio", c(0,5), "Payoff ratio (Obj: Updown)")

png("../data/Payoff ratio bp IS incl commission 60.png", width=2000, height=1500, res=300)
grid.arrange(w1,w2, ncol=2)
dev.off()

w3 <- boxplot_plotting(paper_result, "Model", "Profit_factor", c(0,5), "Profit factor (Obj: paper)")
w4 <- boxplot_plotting(UD_result, "Model", "Profit_factor", c(0,5), "Profit_factor (Obj: Updown)")

png("../data/Profit factor bp IS incl commission 60.png", width=2000, height=1500, res=300)
grid.arrange(w3,w4, ncol=2)
dev.off()





