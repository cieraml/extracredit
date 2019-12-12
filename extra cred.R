#Extra Credit Assignment
#SN: there was only the years 2012-2018 in the data set
#New responsitory was created

library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(ggmap)
library(osmdata)
library(stringr)
library(data.table)
aurelia <- read_xlsx(path = "~/Downloads/import_datasets/Aurelia_SEAMAP_2012-2018_30minCell.xlsx", 
               sheet = 1, col_names = T)
view(aurelia)                                                                      
aurelia <- arrange(aurelia, pop_mean_no_m3, year)
au <- aurelia[,c("year", "pop_mean_no_m3")]

g <- aurelia[aurelia$year == "2015",]
g2 <- subset(x = aurelia, year == "2015")
gg <- ggplot(data = g2, aes(x = year)) + geom_histogram(binwidth = 5) + facet_wrap(.~cell_id)

dir.create("a.map")

au.plot <- ggplot(au, x = year, y = pop_mean_no_m3) +
  geom_line(aes(x = year, y = pop_mean_no_m3)) +
  scale_color_continuous('viridis', guide = FALSE)+
  theme_bw() +
  ggtitle(label = aurelia) 

  ggsave(filename = paste0('a.map/',aurelia,'.png'),
         plot = au.plot, width = 4, height = 3, units = 'in', 
         dpi = 300)

GM <- getbb('Gulf of Mexico')
GM
GM.map = get_stamenmap(bbox = GM, zoom = 6, maptype = 'toner-background')
ggmap(GM.map)

a <- aurelia %>% group_by(year) %>% summarise(mean.density = mean(pop_mean_no_m3))


ddply(.data = g, .variables = c("pop_mean_no_m3"), function(x){
  
  p <- unique(x$pop_mean_no_m3)
  
  p.plot <- ggplot(data = x, aes(x = lat, y = lon)) +
    geom_point() +
    ggtitle(label = p)
  
  ggsave(filename = paste0('a.map/',p,'.png'),
         plot = p.plot, width = 4, height = 3, units = 'in',
         dpi = 300)
  
}, .inform = T, .progress = "text")




