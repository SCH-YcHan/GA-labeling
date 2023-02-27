library(ggplot2)

Plotting_up_down <- function(updown_data, png_name=""){
  png(paste0("../data/Trading_plot/", png_name, "_up_down.png"), width=2000, height=770, res=300)
  a <- updown_data %>% 
    mutate(Label = factor(label, levels=c("Up", "Down"))) %>% 
    select(-label)
  b <- a %>%
    ggplot(aes(x=Date, y=Open)) + 
    geom_line() + 
    theme_bw() +
    (a %>% 
       filter(!is.na(Label)) %>% 
       geom_point(mapping = aes(x=Date, y=Open, color=Label, shape=Label))) + 
    guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(legend.title.align=0.5,
          legend.text = element_text(size=10),
          legend.position = c(0.2,0.6),
          legend.box.background = element_rect(colour = "black"))
  print(b)
  dev.off()
}

Plotting_buy_sell <- function(buysell_data, png_name=""){
  png(paste0("../data/Trading_plot/", png_name, "_buy_sell.png"), width=2000, height=770, res=300)
  a <- buysell_data %>% 
    mutate(Label = factor(label, levels=c("Buy", "Sell"))) %>% 
    select(-label)
  b <- a %>%
    ggplot(aes(x=Date, y=Open)) + 
    geom_line() + 
    theme_bw() +
    (a %>% 
       filter(!is.na(Label)) %>% 
       geom_point(mapping = aes(x=Date, y=Open, color=Label, shape=Label))) + 
    guides(colour = guide_legend(override.aes = list(size=2))) +
    theme(legend.title.align=0.5,
          legend.text = element_text(size=10),
          legend.position = c(0.2,0.6),
          legend.box.background = element_rect(colour = "black"))
  print(b)
  dev.off()
}

trading_plot <- function(Pred_data, plot_name="", index){
  up_down_data <- Pred_data[[index]] %>% select(Date, Open, label)
  up_down_data$label <- ifelse(up_down_data$label==1, "Up", ifelse(up_down_data$label==2, "Down", NA))
  Plotting_up_down(up_down_data, plot_name[[index]])
  
  Pred_data2 <- Pred_data[[index]] %>% 
    select(Date, label) %>%
    mutate(label = ifelse(label-lag(label)!=0 | is.na(label-lag(label)), label, 0)) %>%
    filter(label != 0) %>% 
    merge(Pred_data[[index]] %>% mutate(Open = lead(Open)) %>% select(Date, Open), by="Date", all=T) %>% 
    filter(!is.na(label)) %>% 
    filter(!is.na(Open))
  
  buy_sell_data <- merge(Pred_data[[index]] %>% select(Date, Open), Pred_data2 %>% select(Date, label), by="Date", all.x=T)
  buy_sell_data$label <- ifelse(buy_sell_data$label==1, "Buy", ifelse(buy_sell_data$label==2, "Sell", NA))
  Plotting_buy_sell(buy_sell_data, plot_name[[index]])
}