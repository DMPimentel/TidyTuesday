#----------------------------
# Tidy Tuesday Challenge
# 2022 Week 7 (15-02-2022) 
# DuBois Challenge 22 
#----------------------------

#--- Load packages ----
library(tidyverse)
library(showtext)

#--- Load dataset ----
challenge06 <- readr::read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge06/data.csv')

#--- Fonts and colors ----
showtext_auto()
font_add_google("Open Sans Condensed",regular.wt = 300)
font <- "Open Sans Condensed"

dark<-'#191a15'
cafelate<-'#d4c4b4'

#--- Data transformation ----
challenge06 <- challenge06 %>%
  mutate(xx = `Iliteracy Rate` * -1) %>%
  rename(illiteracy = `Iliteracy Rate`)

#--- Labels ----
dates<-c('1880','1870','1880','1890','(1900?)')
pc<-c('99%','92.1%','81.6%','67.2%','(50%?)')

#--- Plot ----
ggplot(data = challenge06) +
  geom_bar(aes(x= xx, y = illiteracy), width=2, fill = dark, stat="identity") +
  geom_segment(aes(x=xx+1,y=illiteracy,xend=-102,yend=illiteracy),  size = 3, color = dark)+
  geom_segment(aes(x=xx+0.8,y=illiteracy,xend=-101.8,yend=illiteracy),  size = 2.5, color = cafelate)+
  scale_x_continuous(breaks= challenge06$xx, labels= pc, name = '') +
  scale_y_continuous(breaks = challenge06$illiteracy, labels = dates, name = '') +
  ggtitle('ILLITERACY.') +
  labs(caption = "#DuBoisChallenge2022 | Source: Anthony Starks")+
  theme_void() +
  theme(
    plot.background = element_rect(fill = cafelate),
    axis.text.x  = element_text(color = dark, size = 20, family = font, 
                                vjust=8),
    axis.text.y  = element_text(color = dark, size = 20, family = font, 
                                margin = margin(0,-15,0,0),
                                hjust = 1),
    plot.margin = unit(c(1,2,2,2), "cm"),
    plot.title=element_text(size=32, vjust=8, hjust = 0.5, face = 'bold'),
    plot.caption = element_text(size=10, hjust=0.5, vjust=-20)
    ) +
    annotate(geom = 'text', label= 'PERCENT OF\nILLITERACY.',x = -105, y = 0, 
             size = 5.5, family=font, lineheight= 0.3) 

ggsave("2022_W7_dubois06.png", width = 4.5, height = 6)  

 

