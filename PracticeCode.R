require(graphics)
mosaicplot(Titanic, main = "Survival on the Titanic")
apply(Titanic, c(3,4), sum)
view(Titanic)
summary(Titanic)
head(Titanic)
Titanic

starwars

us_rent_income
library(tidyr)
us_rent_income

penguins
View(penguins)
summary(penguins)
##-----Redesign Time-------

hw <- theme_gray()+ theme(
  plot.title=element_text(hjust=0.5),
  plot.subtitle=element_text(hjust=0.5),
  plot.caption=element_text(hjust=-.5),
  
  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
                                colour=gray(.5), linewidth =.2),
  
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.spacing.x = unit(0.10,"cm"),
  panel.spacing.y = unit(0.05,"cm"),
  
  # axis.ticks.y= element_blank()
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,3,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)

getwd()

nps_sar = read.csv('NPmostSARincidents.csv')
str(nps_sar)

library(tidyverse)
library(ggplot2)

ggplot(nps_sar, aes(x = reorder(NationalPark, NumberSARIncidents), y = NumberSARIncidents)) +
  geom_bar(stat = 'identity', fill = 'forestgreen', color = 'black') +
  geom_text(aes(label = NumberSARIncidents), vjust = 0.5, hjust = 1.5) +
  coord_flip() +
  labs(
    title = "Number of Search and Rescue Incidents by Park from 2018-2020",
    x = NULL,
    y = "Number of Incidents") + hw
