library(GGally)
library(ggplot2)
library(tidyverse)
library(gapminder)

theme_set(theme_bw(12))
variable = data$`WBC (11/10)`

plot<-data %>%
ggplot(aes(Groups,variable)) +
  geom_point(aes(color = `Time on Test`), size=3) +
  geom_line(aes(group = paired), color = "grey") +
  scale_y_continuous(name = "WBC count") + 
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20))
plot


setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Plot.tiff", units="in", width=12, height=6, res=150)
plot
dev.off()

