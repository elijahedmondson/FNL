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
library(flexsurv)
library(dplyr)
library(survtools)
library(finalfit)
library(gtsummary)

#data <- read_excel("C:/Users/edmondsonef/Desktop/Pathology Reports/ThomasC/19-331-137 Efficacy/MHL 19-331-137.xlsx", sheet = "Animal data")
# Study1 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", sheet = "19-331-137")
# Study2 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", sheet = "19-331-121")
# Study3 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", sheet = "22-331-2")
#data <- read_excel("C:/Users/edmondsonef/Desktop/MHL 22-331-18 Efficacy.xlsx", sheet = "Full Path Data")

data <- read_excel("C:/Users/edmondsonef/Desktop/MHL 22-331-65 efficacy.xlsx", 
                   sheet = "Path")

data <- filter(data, Groups %in% c("F01", "F02", "F04")) 
F01 <- dplyr::filter(all, Group!="F01")
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 

coxfit <- coxph(Surv(`Day`) ~ Group, data = data, ties = 'exact')
summary(coxfit)

fit <- survfit(Surv(`Day`)~Group, data=data)
ggsurvplot2(fit, data=data, pval = T, risk.table = F)

plot <- ggsurvplot2(fit, data=data, pval = T, risk.table = F,
                    title = "", 
                    legend="right",
                    legend.title="Groups",legend.labs=c("F01 Vehicle",
                                                        "F02 Revumenib",
                                                        "F03 Tamibarotene",
                                                        "F04 Iadademstat",
                                                        "F05 Revumenib & Tamibarotene",
                                                        "F06 Iadademstat & Tamibarotene",
                                                        "F07 Revumenib & Iadademstat",
                                                        "F08 Revumenib, Tamibarotene, & Iadaemstat"))

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("KM.tiff", units="in", width=6, height=4, res=200)
plot
dev.off()



forest_plot <- ggforest(coxfit, data = data,
                        cpositions = c(0.01, 0.07, 0.3), 
                        fontsize = 1, noDigits = 3,
                        refLabel = "reference")
forest_plot

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("forest_plot.tiff", units="in", width=10, height=4, res=200)
forest_plot
dev.off()

###########




### CoxPH risk estimates
fit <- survfit(Surv(Days, Censor)~Group, data=data)
coxfit <- coxph(Surv(Days, Censor) ~ Group, data = data, ties = 'exact')
fit <- survfit(Surv(Days)~Group, data=data)
coxfit <- coxph(Surv(Days) ~ Group, data = data, ties = 'exact')
summary(coxfit)

forest_plot <- ggforest(coxfit, data = data,
                        #cpositions = c(0.01, 0.07, 0.3), 
                        fontsize = 1, noDigits = 3,
                        refLabel = "reference")
forest_plot



# List of ggsurvplots
require("survminer")
splots <- list()
splots[[1]] <- F03
splots[[2]] <- F04
splots[[3]] <- F05
splots[[4]] <- F06

# Arrange multiple ggsurvplots and print the output
arrange_ggsurvplots(splots, print = TRUE,
                    ncol = 2, nrow = 2, risk.table.height = 0.4)

tiff("KM.tiff", units="in", width=10, height=10, res=300)
(allplot) /
  (F03 | F04) / 
  (F05 | F06) 
  #plot_layout(guides = "collect") + 
  plot_annotation(title = "")
dev.off()

gg_all = plot_grid(allplot, F03, F04, F05, F06, labels=c('', ''), ncol=2)
gg_all <- cowplot::plot_grid(allplot, F03, F04, F05, F06, labels=LETTERS[1:5], rel_widths=c(2,1,1,1,1))

multiplot <- paste0(datadir, "_", outname, "_volcano_enrighGO_BP.png")
ggsave(gg_all, file=multiplot, width = 15, height = 10, units = "in")


ggsurvplot2(fit, data=data, pval = F, risk.table = T, linetype = 1, 
           ggtheme = theme_bw())




setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("19-331-137 Survival Curves.tiff", units="in", width=15, height=9, res=200)
allplot
dev.off()



####### OPTION 2 ####### 
####### OPTION 2 ####### 
####### OPTION 2 ####### 
####### OPTION 2 ####### 
####### OPTION 2 ####### 
####### OPTION 2 #######
####### OPTION 2 ####### 
####### OPTION 2 ####### 
####### OPTION 2 #######


model_fit <- survfit(Surv(Age, Status) ~ Groups, data = data)

autoplot(model_fit) + 
  labs(x = "\n Survival Time (Days) ", y = "Survival Probabilities \n", 
       title = "Survival Times \n JBM 336 \n") + 
  theme_bw()

autoplot(survfit(Surv(Age, Status)~ data$'CNS Polyglucosan Body', data = data), conf.int = FALSE, censor = T)
autoplot(survfit(Surv(Age, Status) ~ Groups, data = data))
autoplot(survfit(Surv(Age, Status) ~ Groups, data = data), conf.int = FALSE, censor = T)


####### BOX COX Transform ####### 
####### BOX COX Transform ####### 
####### BOX COX Transform ####### 
####### BOX COX Transform ####### 
####### BOX COX Transform ####### 
####### BOX COX Transform ####### 

library(MASS)

#fit linear regression model
model <- lm(data$Age~data$Status)
plot(model)

bc <- boxcox(data$Age~data$Status)
(lambda <- bc$x[which.max(bc$y)])

new_model <- lm(((data$Age^lambda-1)/lambda) ~ data$Status)
