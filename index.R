library(tidyverse)
library(data.table)

df <- fread('data.csv')

f_size <- 10

t <- theme(plot.title = element_text(face="bold"),
           axis.text.x = element_text(size=f_size,color='#000000',angle=0),
           axis.text.y = element_text(size=f_size,color='#000000'),
           axis.title.x = element_text(face="bold", size=f_size,color='#000000'),
           axis.title.y = element_text(face="bold", size=f_size,color='#000000'),
           panel.background = element_rect(fill="#f1f1f1", color='#a5a5a5',size=0.5),
           panel.ontop = F,
           panel.grid.major.y = element_line(color='#5f7e89',size=0),
           panel.grid.minor.y = element_blank(),
           # panel.grid.major.x = element_blank(),
           panel.grid.minor.x = element_blank(),
           legend.text = element_text(size=f_size,color='#000000'),
           legend.title = element_text(face='bold',size=f_size,color='#000000'),
           legend.box.background = element_rect(fill='#ffffff', color='#ffffff', size=1.5),
           strip.text = element_text(size=f_size,color='#000000', face='bold'),
           strip.background = element_rect(colour = NA, fill = '#ffffff'))

pal <- c('Unqualified' = '#06adee', 'Qualified' = '#000000', 'Disclaimer' = '#b3b3b3')




df %>% 
  group_by(Objective, Practice, Maturity, Decision) %>% 
  summarise(n = n()) %>%
  filter(Objective == "DSS05") %>%
  ggplot(aes(x = Maturity, y = n, fill = Decision)) +
  geom_bar(stat = 'identity') +
  facet_grid(Objective ~ Practice) +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9)) +
  scale_fill_manual(values = pal) +
  t
