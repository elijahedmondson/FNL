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



New <- data %>% group_by(`Image Tag`, `Layer Name`) %>% summarise(avg = mean(`Area (microns squared)`), 
                                                                   sum = sum(`Area (microns squared)`))

# New2 <- data %>% group_by(`Image Tag`) %>% count(`Layer Name`)
# export <- left_join(New, New2)
# write.csv(export, "C:/Users/edmondsonef/Desktop/export.csv")

new <- data %>% group_by(`Image Tag`, `Layer Name`) %>% summarise(sum = sum(`Area (microns squared)`))
write.csv(new, "C:/Users/edmondsonef/Desktop/new.csv")


theme_set(theme_bw(12))
variable = data$`pERK`

plot<-data %>%
  ggplot(aes(`Groups`,variable)) +
  geom_jitter(aes(color = `Groups`), width = 0.2, height = 0.001, size = 5) +
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", color = "grey", width=0.5,size = 1.5)+
  stat_summary(fun.y=mean, geom="point", color = "grey", size = 7)+
  scale_y_continuous(name = "pERK: % Positive Pixel") + 
  theme(axis.text.x=element_text(angle=0,hjust=0.5)) +
  #theme(axis.text.x=element_blank()) +
  theme(axis.title.x=element_blank(), text = element_text(size = 20))# +
  #facet_wrap(~ Tissue, ncol=1)
plot


setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Plot1.tiff", units="in", width=6, height=4, res=200)
plot
dev.off()


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


my_mean4 = aggregate(data$'Murine PD-L1: H-score', by=list(data$Group), mean) ; colnames(my_mean4)=c("Group" , "mean")
my_CI4 = aggregate(data$'Murine PD-L1: H-score', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI4)=c("Group" , "CI")
my_info4 = merge(my_mean4, my_CI4, by.x=1 , by.y=1)
my_info4$CIdiff = ((my_CI4$CI[,2] - my_CI4$CI[,1])/2)

muPDL1 <- ggplot(data) + 
  geom_point(data = my_info4, aes(x = Group, y = my_info4$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "Murine PD-L1: H-score") +
  geom_errorbar(data = my_info4, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Murine PD-L1: H-score'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#

my_mean5 = aggregate(data$'Human PD-L1: H-score', by=list(data$Group), mean) ; colnames(my_mean5)=c("Group" , "mean")
my_CI5 = aggregate(data$'Human PD-L1: H-score', by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI5)=c("Group" , "CI")
my_info5 = merge(my_mean5, my_CI5, by.x=1 , by.y=1)
my_info5$CIdiff = ((my_CI5$CI[,2] - my_CI5$CI[,1])/2)

huPDL1 <- ggplot(data) + 
  geom_point(data = my_info5, aes(x = Group, y = my_info5$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "Human PD-L1: H-score") +
  geom_errorbar(data = my_info5, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Human PD-L1: H-score'), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=25,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))

#

ggarrange(CD45, CD11b, huPDL1, muPDL1, MVD, 
          labels = c("A", "B", "C", "D", "E"),
          ncol = 2, nrow = 3)

########
###########

my_mean = aggregate(data$'Sum on Marrow Grade', by=list(data$'Group'), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Sum on Marrow Grade', by=list(data$'Group') , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean, my_CI, by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

ggplot(data) + 
  geom_point(data = my_info, aes(x = my_info$'Group', y = my_info$mean), color = "Grey", size = 5) +
  scale_y_continuous(name = "Bone Marrow Score: huCD45+ Cells") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$'Group', y = data$'Sum on Marrow Grade', color = data$Group), width = 0.2, size = 4) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.15), legend.title = element_blank(), legend.justification=c("right"), legend.box.margin = margin(6,6,6,6))

###########
###########
###########


###########
###########
###########
###########
###ONLY1###
 


###########
###########



### Generate Multiplots
ggarrange(first, Second,
          labels = c("A", "B"),
          ncol = 1, nrow = 2)



data1 <- summarySE(data, measurevar="Xenograft, Abdominal involvement", groupvars=c("Group"), na.rm = TRUE)
tgc2 <- tgc
tgc2$dose <- factor(tgc2$dose)

ggplot(data1, aes(x=data1$'Group', y=data1$'Xenograft, Abdominal involvement')) + 
  geom_errorbar(aes(ymin=data1$'Xenograft, Abdominal involvement'-se, ymax=data1$'Xenograft, Abdominal involvement'+se))


ggplot(data) 
  geom_point(data, aes(x=data$'Group', y=data$'Total Tumor Area / Total Tissue Area (%)'), color = "grey", size = 3) +
  scale_y_continuous(name = "Xeno") +
  geom_errorbar(aes(ymin=data1$'Xenograft, Abdominal involvement'-se, ymax=data1$'Xenograft, Abdominal involvement'+se), color = "grey", width = 0.2 , size=1) +
  theme_bw() +
  geom_jitter(aes(x = Group, y = data$ALB), width = 0.1)+
  theme(axis.title.x=element_blank())
  


ggplot(data, aes(x="Age Group", y="Tubular Changes (average)", color="Organ", shape="Sex")) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  theme(legend.position="top")

ggplot(data, aes(x=data$"Group", y=data$"Organ Weight")) 


########
########
########
my_mean = aggregate(data$'Disease', by=list(data$Group), mean) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Disease' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean, my_CI, by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)



ggplot(data) + 
  geom_point(data = my_info, aes(x = Group , y = my_info$mean), color = "grey", size = 5) +
  scale_y_continuous(name = "Disease") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = data$Group, y = data$'Disease'), width = 0.2, size = 4) +
  theme(axis.title.x=element_blank(), legend.position = c(.95,.95), legend.title = element_blank(), legend.justification=c("right", "top"), legend.box.margin = margin(6,6,6,6))


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




########
my_mean = aggregate(data$'Aspirate Grade', by=list(data$Group), mean, na.rm=TRUE) ; colnames(my_mean)=c("Group" , "mean")
my_CI = aggregate(data$'Aspirate Grade' , by=list(data$Group) , FUN = function(x) t.test(x)$conf.int) ; colnames(my_CI)=c("Group" , "CI")
my_info = merge(my_mean , my_CI , by.x=1 , by.y=1)
my_info$CIdiff = ((my_CI$CI[,2] - my_CI$CI[,1])/2)

ggplot(data) + 
  geom_point(data = my_info, aes(x = Group, y = mean), color = "grey", size = 3) +
  scale_y_continuous(name = "-") +
  geom_errorbar(data = my_info, aes(x = Group, y = CIdiff, ymin = mean - CIdiff, ymax = mean + CIdiff), color = "grey", width = 0.2 , size=1) +
  theme_bw(base_size = 18) +
  geom_jitter(aes(x = Group, y = data$'Aspirate Grade'), width = 0.2, size = 3) +
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) 



###Scatterplots, tumor size over time, etc###


ggplot(data, aes(x = data$'SUM1', y = data$'SUM2')) +
  geom_point(aes(color = data$Group), size = 5)+
  scale_y_continuous(name = "SUM2") +
  scale_x_continuous(name = "SUM1") +
  theme_bw(base_size = 18)+
  stat_smooth(method = "lm",
              col = "#C42126",
              se = F,
              size = 1)

plot(data$'Vessel Count Per mm�', data$'SMA H-Score')
abline(lm(data$'SMA H-Score'~data$'Vessel Count Per mm�'), col="red") # regression line (y~x) 

plot(data$'Vessel Count Per mm�', data$'SMA H-Score', main="Scatterplot", 
     xlab="Age (days) ", ylab="Pulmonary Metastatic Density", pch=19)
abline(lm(data$'Vessel Count Per mm�'~data$'SMA H-Score'), col="red") # regression line (y~x) 
lines(lowess(data$'Vessel Count Per mm�',data$'SMA H-Score'), col="blue") # lowess line (x,y)


qplot(data$Days, data$'Lung Tumors', data = data, colour = data$Groups, geom = "histogram")



plot(data$Adenoma, data$Days, main="Urine vs Fecal Pellet", 
     xlab="Urine PCR ", ylab="Fecal PCR ", pch=19)

abline(lm(data$`Dry Fecal Pellet`~data$Urine), col="red") # regression line (y~x) 
lines(lowess(data$Urine,data$`Dry Fecal Pellet`), col="blue") # lowess line (x,y)

###3D scatterplot
library(scatterplot3d)
colors <- c("#999999", "#E69F00", "#56B4E9")
x <- data$Urine
y <- data$`Dry Fecal Pellet`
z <- data$swab
grps <- as.factor(data$Species)
scatterplot3d(data$Urine, data$`Dry Fecal Pellet`, data$swab, pch = 16, color = colors[grps],
              grid = TRUE, box = FALSE, xlab = "Urine PCR", 
              ylab = "Fecal Pellet PCR", zlab = "Cage Swab PCR")


#############Histogram
#############Histogram
#############Histogram
#############Histogram
#############Histogram
#############Histogram
data$log_days = log(data$Age)

par(mfrow=c(2,2))
hist(data$Age, breaks=40)
hist(data$log_days, breaks=40)
hist(data$rankZ, breaks=40)

hist(data$`PreT LL Transform`, breaks=100)
hist(data$`CNS Polyglucosan Body Transform`, breaks=100)



res.cox <- coxph(Surv(data$days, data$event), data = data)

#############Histogram
#############Histogram
#############Histogram
#############Histogram
#############Histogram
#############Histogram
#############Histogram
#############Histogram



############
############

my.formula <- y ~ x  
ggplot(data = data, aes(y = data$'3/17/21 BM Grade', x = data$'3/4/21 BM Grade', color = data$Group), na.rm=TRUE) +
  geom_smooth(method = "lm", se=FALSE, formula = my.formula) +
  stat_poly_eq(formula = y ~ x, show.legend = T, parse = TRUE, na.rm=T) +  
  geom_point(na.rm=TRUE)+
  #scale_x_continuous(name = "Days") +
  #scale_y_continuous(name = "ss") +
  theme_bw(base_size = 18)   



linearMod <- lm(data$'CD206 Num Positive per mm^2' ~ data$'CD86 Num Positive per mm^2', data=data)  # build linear regression model on full data
print(linearMod)
modelSummary <- summary(linearMod)  # capture model summary as an object
summary(linearMod)


############
############
############
############
############ Plot with connected datapoints from different columns
############
############
############
############
############

library(hrbrthemes)
library(GGally)
library(viridis)

# Plot
ggparcoord(data, columns = 6:9, groupColumn = 2, scale="uniminmax", showPoints = TRUE, alphaLines = .8) +
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






###########################
###########################
###########################
###########################


path = "Q:/archive/PHL/Edmondson/Liver Moonshot/"


list.files(path = "Q:/archive/PHL/Edmondson/Liver Moonshot/", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE,
           ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

list = dir(path = "Q:/archive/PHL/Edmondson/Liver Moonshot/", pattern = NULL, all.files = FALSE,
    full.names = T, recursive = FALSE,
    ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

write.csv(list, file = "Q:/archive/PHL/Edmondson/Liver Moonshot/list.csv")



library(plyr)
library(dplyr)
library(readr)


path = "C:/Users/edmondsonef/Desktop/QuPath/Sterneck/MHL 200335/area/"
#path = "P:/archive/PHL/Edmondson/QuPath//"

setwd(path)
outFile <-"Result1.csv"
#### Replace .txt with whatever identifier will pick up all of the files you want to analyze. Detections or Annotations are common choices
Annotationfiles <- dir(path,pattern = ".txt")
Measurements <- data.frame()
for(i in 1:length(Annotationfiles)){
  data.raw <- read_delim(Annotationfiles[i],"\t", escape_double = FALSE, trim_ws = TRUE)
  Sample = tools::file_path_sans_ext(Annotationfiles[i])
  data.raw[1,2]<-Sample
  Measurements<-bind_rows(Measurements, data.raw)
}
write.csv(Measurements, outFile, row.names=T)

ggplot(data, aes(x = `family`, y = `Cataract`))+
  geom_col(aes(fill = `Cataract`), width = 0.7)




ggplot(data, aes(fill=`cat_score`, x=`family`, y="")) +
  geom_bar(position="stack", stat="identity")

  geom_histogram(fill="white", binwidth = 1)+
  theme_classic() +
  geom_text(aes(label=..count..),stat="count",position=position_stack())

ggplot(data, aes(x=`family`, color=data$`cat_score`)) +
  geom_histogram(fill="white", binwidth = 1)+
  theme_classic() +
  geom_text(aes(label=..count..),stat="count",position=position_stack())

ggplot(data, aes(x="", y=groups, fill=family)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() 




