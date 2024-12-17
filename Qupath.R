library(ggExtra)
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
corr <- cor(data$'OSA % Osteoid', data$'ROI % Osteoid', method = c("pearson"), use = "pairwise.complete.obs")

ggplot(data, aes(x = data$'OSA % Osteoid', y = data$'ROI % Osteoid')) +
  geom_point(aes(color = data$Group), size = 5)+
  scale_y_continuous(name = "ROI % Osteoid") +
  scale_x_continuous(name = "OSA % Osteoid") +
  theme_bw(base_size = 18)+
  stat_smooth(method = "lm", col = "#C42126",se = T, size = 1) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

# New2 <- data %>% group_by(`Image Tag`) %>% count(`Layer Name`)
# export <- left_join(New, New2)
# write.csv(export, "C:/Users/edmondsonef/Desktop/export.csv")
# new <- data %>% group_by(`Image Tag`, `Layer Name`) %>% summarise(sum = sum(`Area (microns squared)`))
# write.csv(new, "C:/Users/edmondsonef/Desktop/new.csv")

data <- dplyr::filter(data, Cohort!="1")


theme_set(theme_bw(12))
variable = data$`Avg Lipid Median Diameter (??m)`

### Bar Plot ###
### Bar Plot ###
### Bar Plot ###
### Bar Plot ###
### Bar Plot ###




setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("BodyWeight41024.tiff", units="in", width=12, height=6, res=200)
plot
dev.off()

LipidA<-data %>%
  ggplot(aes(`Group`,`Steatosis Score`)) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color = "grey", width=0.5,size = 1.5)+
  geom_jitter(aes(color = Groups), width = 0.2, height = 0.001, size = 5) +
  #stat_summary(fun.y=mean, geom="point", color = "grey", size = 7)+
  scale_y_continuous(name = "Steatosis Histology Score") +   
  theme(axis.text.x=element_text(angle=0,hjust=0.5)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20))

LipidB<-data %>%
  ggplot(aes(`Group`,`Vacuolated Hepatocytes (% area)`)) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color = "grey", width=0.5,size = 1.5)+
  geom_jitter(aes(color = Groups), width = 0.2, height = 0.001, size = 5) +
  stat_summary(fun.y=mean, geom="point", color = "grey", size = 7)+
  scale_y_continuous(name = "Vacuolated Hepatocytes (% area)") +   
  theme(axis.text.x=element_text(angle=0,hjust=0.5)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20)) 


LipidC<-data %>%
  ggplot(aes(`Group`,`Lipid Percent Area`)) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color = "grey", width=0.5,size = 1.5)+
  geom_jitter(aes(color = Groups), width = 0.2, height = 0.001, size = 5) +
  stat_summary(fun.y=mean, geom="point", color = "grey", size = 7)+
  scale_y_continuous(name = "Total Lipid % Area") +   
  theme(axis.text.x=element_text(angle=0,hjust=0.5)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20)) 
LipidD<-data %>%
  ggplot(aes(`Group`,`Avg Lipid Area (??m²)`)) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color = "grey", width=0.5,size = 1.5)+
  geom_jitter(aes(color = Groups), width = 0.2, height = 0.001, size = 5) +
  stat_summary(fun.y=mean, geom="point", color = "grey", size = 7)+
  scale_y_continuous(name = "Avg Lipid Object Size (??m²)") +   
  theme(axis.text.x=element_text(angle=0,hjust=0.5)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20)) 



tiff("Absolute Organ Weights.tiff", units="in", width=15, height=10, res=200)
(LipidC | LipidD) /
  (LipidB | LipidA) /
  plot_layout(guides = "collect") +
  plot_annotation(title = "")
dev.off()



fig <- cowplot::plot_grid(plotLPA, plotRPA, ncol=2, labels=LETTERS[1:2])
fig




plot<-data %>%
  mutate(Group = fct_relevel(Group,"Normal", "Dysplasia", "HPV- Cancer")) %>%
  ggplot(aes(`Group`,variable)) +
  geom_jitter(aes(x=`Group`,y=variable, fill=factor(Location), color = `Group`), width = 0.2, height = 0.001, size = 5) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color = "grey", width=0.5,size = 1.5)+
  stat_summary(fun.y=mean, geom="point", color = "grey", size = 7)+
  scale_y_continuous(name = "SUV420H1 H-Score") +   
  #geom_signif(comparisons = list(c("HPV- Cancer", "Normal","Dysplasia")), test = "t.test", map_signif_level=TRUE) +
  #geom_signif(comparisons = list(c("HPV- Cancer", "Normal","Dysplasia")), test = "wilcox.test", map_signif_level=F) +
  geom_signif(comparisons = list(c("Normal","Dysplasia"),
                                 c("Normal","HPV- Cancer")),
              #test = "t.test",
              step_increase = 0.1,
              map_signif_level=F,
              show.legend = TRUE) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20)) +
  theme(legend.position="none") +
facet_wrap(~ Location)
plot

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Plot1.tiff", units="in", width=12, height=8, res=200)
plot
dev.off()  





data <- read_excel("C:/Users/edmondsonef/Desktop/Pathology Reports/Saloura/MHL Saloura SUV420H1 TMA.xlsx", 
                   sheet = "plot5")

theme_set(theme_bw(12))
variable = data$`H-score`


plot<-data %>%
  #mutate(Group = fct_relevel(Group,"Normal", "Dysplasia", "HPV- Cancer")) %>%
  ggplot(aes(`Location`,variable)) +
  geom_jitter(aes(x=`Location`,y=variable, fill=factor(Location), color = `Location`), width = 0.2, height = 0.001, size = 5) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color = "grey", width=0.5,size = 1.5)+
  stat_summary(fun.y=mean, geom="point", color = "grey", size = 7)+
  scale_y_continuous(name = "SUV420H1 H-Score") +   
  geom_signif(comparisons = list(c("HPV- Cytoplasm","HPV- Nuclear")),
              #test = "t.test",
              #step_increase = 0.1,
              map_signif_level=T,
              show.legend = TRUE) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20)) +
  theme(legend.position="none") #+
  #facet_wrap(~ Location)
plot

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Plot1.tiff", units="in", width=8, height=6, res=200)
plot
dev.off()  



### Bar Plot ###
### Bar Plot ###
### Bar Plot ###
### Bar Plot ###
### Bar Plot ###



### HISTOGRAM ###
### HISTOGRAM ###
### HISTOGRAM ###
### HISTOGRAM ###
### HISTOGRAM ###
plot<-data %>%  mutate(Groups = fct_relevel(Group,"Normal", "Dysplasia", "HPV- Cancer", "HPV+ Cancer")) %>%
  mutate(text = fct_reorder(Group, variable)) %>%
  ggplot(aes(x=variable, color=Group, fill=Group)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  facet_wrap(~Groups)

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Plot1.tiff", units="in", width=10, height=8, res=200)
plot
dev.off()


plot<-data %>%
  mutate(Group = fct_relevel(Group,"Normal", "Dysplasia", "HPV- Cancer", "HPV+ Cancer")) %>%
  ggplot(aes(`Nuclear H-score`, `Cytoplasm H-score`, color = Group)) + 
  geom_point() + theme_classic()
ggExtra::ggMarginal(plot, groupColour = TRUE, groupFill = TRUE, type = "boxplot")


plot <- ggplot(data, aes(`Nuclear H-score`, `Cytoplasm H-score`, color = Group)) + 
  geom_point() + theme_classic()
ggExtra::ggMarginal(plot, groupColour = TRUE, groupFill = TRUE, type = "boxplot")
#("density", "histogram", "boxplot", "violin", "densigram")

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Plot1.tiff", units="in", width=8, height=6, res=200)
ggExtra::ggMarginal(plot, groupColour = TRUE, groupFill = TRUE, type = "boxplot")
dev.off()

### HISTOGRAM ###
### HISTOGRAM ###
### HISTOGRAM ###
### HISTOGRAM ###
### HISTOGRAM ###

piris <- ggplot(data, aes(`Nuclear H-score`, `Cytoplasm H-score`, color = Group)) +
  geom_point()
ggMarginal(piris, groupColour = TRUE, groupFill = TRUE)












data1 <- summarySE(data, measurevar="Tumor: Cell H-score", groupvars=c("Class"), na.rm = TRUE)


ggplot(data1, aes(x=data1$'Class', y=data1$'Tumor: Cell H-score')) + 
  geom_errorbar(aes(ymin=data1$'Tumor: Cell H-score'-se, ymax=data1$'Tumor: Cell H-score'+se))+
  geom_jitter(aes(color = `Class`), width = 0.2, height = 0.001, size = 5) +
  scale_y_continuous(name = "Tumor: Cell H-score") + 
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20))







Study1 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", 
                     sheet = "19-331-137")
Study2 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", 
                     sheet = "19-331-121")
Study3 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", 
                     sheet = "22-331-2")

setwd("C:/Users/edmondsonef/Desktop/R-plots/")

data <- Study1

variable = data$`Adjusted Marrow Grade`
group = data$`Groups`
my_mean = aggregate(variable, by=list(group), mean, na.rm=TRUE) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(variable, by=list(group), FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean, my_CI, by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)
my_info=merge(my_mean, my_sd, by.x=1 , by.y=1)
my_info$se <- my_info$sd / sqrt(cdata$N)

plot <- ggplot(data) + 
  geom_jitter(aes(x = group, y = variable, color = data$'Group'), width = 0.2, height = 0.01, size = 4) +
  geom_point(data = my_info, aes(x = my_info$'Group', y = my_info$mean), color = "grey", size = 4) +
  scale_y_continuous(name = "Survival Adjusted Leukemic Grade \n(BM Grade / Time on Test)") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.4 , size=1) +
  theme_bw(base_size = 18) +
  theme(axis.title.x=element_blank(), text = element_text(size = 18), legend.title=element_blank())+
  geom_signif(comparisons = list(c("F01", "F02")), 
            map_signif_level=TRUE)
plot

ggplot(data, aes(Groups, `Adjusted Marrow Grade`))+
  geom_boxplot()+
  geom_jitter(aes(x = group, y = variable, color = data$'Group'), width = 0.2, height = 0.01, size = 4) +
  
  theme_bw(base_size = 18) +
  geom_signif(
    comparisons = list(c("F01", "F02"), c("F01", "F03")),
    map_signif_level = TRUE, textsize = 6
  )


tiff("Study1.tiff", units="in", width=12, height=6, res=300)
#grid.arrange(Image1, Image2, Image3, ncol = 3, nrow = 1)
plot
dev.off()


Stroma
Epithelial
Type

#####SUBSET DATA
data <- read_excel("C:/Users/edmondsonef/Desktop/19-331-M13 Pathology Data.xlsx", na = ".")
cd34 <- data[ which(data$Cells=='CD34'), ]
PMBC <- data[ which(data$Cells=='hPBMC'), ]

############SD
############SD funs(mean, sem=sd(.)/sqrt(length(.)))
############SD
############SD
my_mean=aggregate(data$'Aspirate Grade', by=list(data$Group), mean, na.rm=TRUE) ; colnames(my_mean)=c("Group" , "mean")
my_sd=aggregate(data$'Aspirate Grade', by=list(data$Group), sd, na.rm=TRUE) ; colnames(my_sd)=c("Group" , "sd")
my_info=merge(my_mean, my_sd, by.x=1 , by.y=1)
my_info$se <- my_info$sd / sqrt(cdata$N)

ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = my_info$mean), color = "grey", size = 4) +
  scale_y_continuous(name = "Bone Marrow Aspirate Grade") + #, limits = c(15, 50)) +
  geom_errorbar(data = my_info, aes(x = Group, y = sd, ymin = mean - sd, ymax = mean + sd), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Aspirate Grade', color = data$Groups),height = 0.051, width = 0.1, size = 3) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20))#, legend.position="none")

############SEM
############SEM
############SEM 
############SEM
my_mean=aggregate(data$'H-score', by=list(data$Group), mean, na.rm=TRUE) ; colnames(my_mean)=c("Group" , "mean")
my_sd=aggregate(data$'H-score', by=list(data$Group), sd, na.rm=TRUE) ; colnames(my_sd)=c("Group" , "sd")
my_info=merge(my_mean, my_sd, by.x=1 , by.y=1)
my_info$se <- my_info$sd / sqrt(cdata$N)

ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = my_info$mean), color = "grey", size = 5) +
  scale_y_continuous(name = "Tumor: gH2AX H-score") + #, limits = c(15, 50)) +
  geom_errorbar(data = my_info, aes(x = Group, y = se, ymin = mean - se, ymax = mean + se), color = "grey", width = 0.2 , size=2) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'H-score', color = data$Group), width = 0.2, size = 6) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20), legend.position="none")



###########
###########
###########
###########
my_mean1 = aggregate(data$'Tumor: Casp8 Cytoplasmic Positive %', by=list(data$Group), mean) ; colnames(my_mean1)=c("Group" , "mean")
my_CI1 = aggregate(data$'Tumor: Casp8 Cytoplasmic Positive %', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI1)=c("Group" , "CI")
my_info1 = merge(my_mean1, my_CI1, by.x=1 , by.y=1)
my_info1$CIdiff = ((my_CI1$CI[,2] - my_CI1$CI[,1])/2)

ggplot(data) + 
  geom_point(data = my_info1, aes(x = Group, y = mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "xxx") +
  geom_errorbar(data = my_info1, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Tumor: Casp8 Cytoplasmic Positive %'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=45,hjust=0.5)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#

my_mean = aggregate(data$'Brain: Positive %', by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Brain: Positive %', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean, my_CI, by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

Brain <- ggplot(data) + 
  geom_point(data = my_info, aes(x = Group, y = mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "Normal Brain") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Brain: Positive %'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#
complete.data = na.omit(data)
my_mean3 = aggregate(complete.data$'Tumor: Positive %', by=list(complete.data$Group), mean, na.rm=TRUE) ; colnames(my_mean3)=c("Group" , "mean")
my_CI3 = aggregate(complete.data$'Tumor: Positive %', by=list(complete.data$Group), FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI3)=c("Group" , "CI")
my_info3 = merge(my_mean3, my_CI3, by.x=1 , by.y=1)
my_info3$CIdiff = ((my_CI3$CI[,2] - my_CI3$CI[,1])/2)

my_info3 <- my_info3 %>% add_row(Group = "F04")

#my_sd=aggregate(data$'First' , by=list(data$Group) , sd, na.rm=TRUE) ; colnames(my_sd)=c("Group" , "sd")
#my_info=merge(my_mean , my_sd , by.x=1 , by.y=1)


Tumor <- ggplot(complete.data) + 
  geom_point(data = my_info3, aes(x = Group, y = mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "DIPG Tumor") +
  #geom_errorbar(data = my_info3, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = complete.data$Group, y = complete.data$'Tumor: Positive %'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

ggarrange(Brain, Liver, Tumor,
          labels = c("A", "B", "C"),
          ncol = 1, nrow = 3, common.legend = TRUE)

ggarrange(Brain, Tumor,
          labels = c("A", "B"),
          ncol = 1, nrow = 2, common.legend = TRUE)

###########
###########
###########
###########
###########
###########
###########
###########
###########
###########
###########


############SD
############SD
############SD

my_mean=aggregate(data$'TUNEL: Positive % Cell' , by=list(data$Group) , mean, na.rm=TRUE) ; colnames(my_mean)=c("Group" , "mean")
my_sd=aggregate(data$'TUNEL: Positive % Cell' , by=list(data$Group) , sd, na.rm=TRUE) ; colnames(my_sd)=c("Group" , "sd")
my_info=merge(my_mean , my_sd , by.x=1 , by.y=1)


ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = my_info$mean), color = "grey", size = 3) +
  scale_y_continuous(name = "", limits = c(15, 50)) +
  geom_errorbar(data = my_info, aes(x = Group, y = sd, ymin = mean - sd, ymax = mean + sd), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'TUNEL: Positive % Cell'), width = 0.2, size = 3) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

### Generate Multiplots
ggarrange(First, Second,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)




###Scatterplot, tumor size over time, etc###
###Scatterplot, tumor size over time, etc###
###Scatterplot, tumor size over time, etc###
###Scatterplot, tumor size over time, etc###

corr <- cor(data$'OSA % Osteoid', data$'ROI % Osteoid', method = c("pearson"), use = "pairwise.complete.obs")

ggplot(data, aes(x = data$'OSA % Osteoid', y = data$'ROI % Osteoid')) +
  geom_point(aes(color = data$Group), size = 5)+
  scale_y_continuous(name = "ROI % Osteoid") +
  scale_x_continuous(name = "OSA % Osteoid") +
  theme_bw(base_size = 18)+
  stat_smooth(method = "lm", col = "#C42126",se = T, size = 1) +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)

  geom_text(x = 152, y = 250, label = paste0('r = ', corr),color = 'red')



plot(data$'OSA Osteoid Area (mm²)', data$'ROI Osteoid Area (mm²)')
abline(lm(data$'OSA Osteoid Area (mm²)'~data$'ROI Osteoid Area (mm²)'), col="red") # regression line (y~x) 

plot(data$'OSA Osteoid Area (mm²)', data$'ROI Osteoid Area (mm²)', main="Scatterplot", 
     xlab="Age (days) ", ylab="", pch=19)
abline(lm(data$'OSA Osteoid Area (mm²)'~data$'ROI Osteoid Area (mm²)'), col="red") # regression line (y~x) 
lines(lowess(data$'OSA Osteoid Area (mm²)',data$'ROI Osteoid Area (mm²)'), col="blue") # lowess line (x,y)


qplot(data$Days, data$'Lung Tumors', data = data, colour = data$Groups, geom = "histogram")


############
############

my.formula <- y ~ x  
ggplot(data = data, aes(y = data$'ROI Osteoid Area (mm²)', x = data$'OSA Osteoid Area (mm²)', color = data$Group), na.rm=TRUE) +
  geom_smooth(method = "lm", se=FALSE, formula = my.formula) +
  stat_poly_eq(formula = y ~ x, show.legend = T, parse = TRUE, na.rm=T) +  
  geom_point(na.rm=TRUE)+
  #scale_x_continuous(name = "Days") +
  #scale_y_continuous(name = "ss") +
  theme_bw(base_size = 18)   



linearMod <- lm(data$'OSA Osteoid Area (mm²)' ~ data$'ROI Osteoid Area (mm²)', data=data)  # build linear regression model on full data
print(linearMod)
modelSummary <- summary(linearMod)  # capture model summary as an object
summary(linearMod)



############
############ Plot paired samples 
############
library(hrbrthemes)
library(GGally)
library(viridis)

# Plot
ggparcoord(data, columns = 7:8, groupColumn = 3, scale="uniminmax", showPoints = TRUE, alphaLines = .8) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum()+
  theme(
    plot.title = element_text(size=18)
  )

## scale = std globalminmax uniminmax center
############
############
############
############
############
############
############
############
############
############

my.formula <- y ~ x
pswab1 <- ggplot(data = data1, aes(x = data1$'Urine', y = data1$'Dry Fecal Pellet'), na.rm=TRUE) +
  geom_smooth(method = "lm", se=FALSE, color="red", formula = my.formula, na.rm=TRUE) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, na.rm=TRUE) +  
  geom_point(na.rm=TRUE)+
  scale_y_continuous(name = "Swab PCR (CT Values)") +
  scale_x_continuous(name = "Urine PCR (CT Values)", xlim=c(20,32.5)) +
  theme_bw(base_size = 18)       

pfecal1

ggarrange(pfecal, pswab1, pother1, pblood, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


plot(data$'PD-L1 (mouse): Positive %', data$'CD45 Positive %', main="", 
     xlab="PD-L1 (mouse): Positive % ", ylab="CD45 Positive % ", pch=19)

abline(lm(data$'CD45 Positive %'~data$'PD-L1 (mouse): Positive %'), col="red") # regression line (y~x) 
lines(lowess(data$'PD-L1 (mouse): Positive %',data$'CD45 Positive %'), col="blue") # lowess line (x,y)


data <- merge(x = cecal$`Sample ID`, y = kidney$`Sample ID`, by = NULL)

# Basic syntax
data1 <- merge(feces, kidney, # dataset names
      by = "Identifier", # by common variable
      #by.x = by, # if the names are different
      #by.y = by, # if the names are different
      all = F, 
      #all.x = all, 
      #all.y = all,
      sort = TRUE, # Should the result be sorted on the by columns
      suffixes = c(".x",".y")
)
