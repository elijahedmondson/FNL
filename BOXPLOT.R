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



### Albulmin:Globulin Ratio
data<-dplyr:: mutate(data, AGR = (ALB/(TP-ALB)))
data<-dplyr:: mutate(data, BCR = (BUN/CRE))


colnames(data)

plot_list = list(colnames(data))
for(i in 17:length(colnames(data))){
  theme_set(theme_bw(12))
  plot_list[[i]] <- local ({
    i <- i
    variable = as.matrix(data[i])
    y_title = colnames(data[i])
    ggplot(data, aes(x = Genotype, y = variable, color = Treatment)) +  
      geom_boxplot(outlier.shape=NA, show.legend = F) +
      scale_y_continuous(name = y_title) + 
      theme(axis.title.x=element_blank()) +
      geom_point(aes(color = Treatment, shape = Sex),
                 position=position_jitterdodge(dodge.width = 0.75, jitter.width = 0.003),
                 size = 2)
  })
}



names(data)

library(patchwork)
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("CBC_WBC.tiff", units="in", width=8, height=8, res=200)
(plot_list[[51]] | plot_list[[52]]) / 
  (plot_list[[53]]  | plot_list[[54]]) /
  (plot_list[[55]]  | plot_list[[56]]) /
  (plot_list[[65]]  | plot_list[[66]]) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "CBC: White Blood Cell and Platelet Counts")
dev.off()

tiff("CBC_RBC.tiff", units="in", width=8, height=8, res=200)
(plot_list[[57]] | plot_list[[58]]) / 
  (plot_list[[59]]  | plot_list[[60]]) /
  (plot_list[[61]]  | plot_list[[62]]) /
  (plot_list[[63]]  | plot_list[[64]]) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "CBC: Erythroid Parameters")
dev.off()

tiff("Chemistry.tiff", units="in", width=14, height=12, res=200)
(plot_list[[42]] | plot_list[[30]] | plot_list[[43]] | plot_list[[67]]) / 
  (plot_list[[35]] | plot_list[[38]] | plot_list[[68]] | plot_list[[39]]) /
  (plot_list[[36]] | plot_list[[37]] | plot_list[[41]] | plot_list[[40]]) /
  (plot_list[[31]] | plot_list[[32]] | plot_list[[34]] | plot_list[[33]]) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Clinical Chemistry")
dev.off()

tiff("Absolute organ weights.tiff", units="in", width=8, height=8, res=200)
(plot_list[[17]] | plot_list[[18]]) / 
  (plot_list[[20]]  | plot_list[[22]]) /
  (plot_list[[24]]  | plot_list[[26]]) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Absolute Organ Weights (grams)")
dev.off()

tiff("Relative organ weights.tiff", units="in", width=8, height=8, res=200)
(plot_list[[17]] | plot_list[[19]]) / 
  (plot_list[[21]]  | plot_list[[23]]) /
  (plot_list[[25]]  | plot_list[[27]]) /
  plot_layout(guides = "collect") + 
  plot_annotation(title = "Relative Organ Weights (% of body weight)")
dev.off()





