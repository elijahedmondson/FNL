library(survMisc)
library(survival)
library(jskm)
library(survey)
library(survminer)
library(survival)
library(ggplot2)
library(ggfortify)
library(readxl)
library(survival)
library(survminer)
library(flexsurv)
library(dplyr)
library(survtools)
library(gridExtra)
library(readxl)
library(ggpubr)
library(Rmisc)
library(tidyverse)
library(plyr)
library(GGally)
library(ggplot2)
library(tidyverse)
library(gapminder)
library(dplyr)
library(ggsignif)
library(ggplot2)
library(gridExtra)
library(readxl)
library(patchwork)

Study1 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", 
                     sheet = "19-331-137")
Study2 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", 
                     sheet = "19-331-121")
Study3 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", 
                     sheet = "22-331-2")

data <- read_excel("C:/Users/edmondsonef/Desktop/Humanized/MHL Humanized Tissue Counts.xlsx", 
                   sheet = "122 Tumor")


variable = data$`Adjusted Marrow Grade`
group = data$`Groups`


F01 <- ggplot(data, aes(group, variable))+
  geom_boxplot()+
  scale_y_continuous(name = "Survival Adjusted Leukemic Grade \n(BM Grade / Time on Test)") +
  geom_jitter(aes(x = group, y = variable, color = data$'Group'), width = 0.2, height = 0.01, size = 4) +
  geom_signif(
    comparisons = list(c("F01", "F02"),
                       c("F01", "F03"),
                       c("F01", "F04"),
                       c("F01", "F05"),
                       c("F01", "F06"),
                       c("F01", "F07")),
    map_signif_level = T, textsize = 4, vjust=0.4, 
    #y_position = c(13, 13.33,13.66, 14, 14.33, 14.66),
    #y_position = c(20, 20.33,20.66, 21, 21.33, 21.66),
    y_position = c(25, 25.33,25.66, 26, 26.33, 26.66),
    tip_length = 0.01)+
  theme_bw(base_size = 18) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())

F02 <- ggplot(data, aes(group, variable))+
  geom_boxplot()+
  scale_y_continuous(name = "Survival Adjusted Leukemic Grade \n(BM Grade / Time on Test)") +
  geom_jitter(aes(x = group, y = variable, color = data$'Group'), width = 0.2, height = 0.01, size = 4) +
  geom_signif(comparisons = list(#c("F02", "F03"), 
                                 c("F02", "F04"),
                                 c("F02", "F05"),
                                 c("F02", "F06"), 
                                 c("F02", "F07")),
    map_signif_level = T, textsize = 4, vjust=0.4, 
    #y_position = c(13, 13.33,13.66, 14, 14.33, 14.66),
    #y_position = c(20, 20.33,20.66, 21, 21.33, 21.66),
    y_position = c(25, 25.33,25.66, 26, 26.33, 26.66),
    tip_length = 0.01)+
  theme_bw(base_size = 18) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())


tiff("Study2_F01.tiff", units="in", width=14, height=8, res=300)
F01
dev.off()
tiff("Study2_F02.tiff", units="in", width=14, height=8, res=300)
F02
dev.off()


