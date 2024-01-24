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

data <- read_excel("C:/Users/edmondsonef/Desktop/Pathology Reports/ThomasC/19-331-137 Efficacy/MHL 19-331-137.xlsx", sheet = "Animal data")
# Study1 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", sheet = "19-331-137")
# Study2 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", sheet = "19-331-121")
# Study3 <- read_excel("C:/Users/edmondsonef/Desktop/NCGC00841450 Efficacy Study Summary.xlsx", sheet = "22-331-2")
#data <- read_excel("C:/Users/edmondsonef/Desktop/MHL 22-331-18 Efficacy.xlsx", sheet = "Full Path Data")

####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 


fit <- survfit(Surv(`Age`, Censor)~Groups, data=data)
plot <- ggsurvplot2(fit, data=data, pval = T, risk.table = F,
                    title = "", 
                    legend="right",
                    legend.title="Groups",legend.labs=c("01 Vehicle BRAF",
                                                                       "02 Dabrafenib BRAF",
                                                                       "03 Vehicle BRAF-SdhB",
                                                                       "04 Dabrafenib BRAF-SdhB"))

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Censor1.tiff", units="in", width=8, height=5, res=200)
plot
dev.off()

coxfit <- coxph(Surv(`Age`, Censor) ~ Groups, data = data, ties = 'exact')
fit <- survfit(Surv(Age)~Group, data=data)
coxfit <- coxph(Surv(Age) ~ Groups, data = data, ties = 'exact')
summary(coxfit)

forest_plot <- ggforest(coxfit, data = data,
                        cpositions = c(0.01, 0.07, 0.3), 
                        fontsize = 1, noDigits = 3,
                        refLabel = "reference")
forest_plot

####
#### AML Studies
####




fit <- survfit(Surv(`Day`, Censor)~Group, data=data)
surv_median(fit)
allplot <- ggsurvplot2(fit, data=data, xlab = "Days (post-dosing)", pval = T, risk.table = F, #surv.median.line = c("hv")),
                       title = "", 
                       legend="right",legend.title="Groups",legend.labs=c("Control",
                                                                          "Gilteritinib",
                                                                          "NCGC00841450",
                                                                          "NCGC00690381",
                                                                          "NCGC00841754",
                                                                          "NCGC00843798",
                                                                          "CA-4948"))
                       # legend="right", legend.title="Group", legend.labs=c("F01 Vehicle",
                       #                                                    "F02 5mg/kg NCGC-1450",
                       #                                                    "F03 5mg/kg NCGC-1450 & Venetoclax low",
                       #                                                    "F04 5mg/kg NCGC-1450 & Venetoclax high",
                       #                                                    "F05 Venetoclax low",
                       #                                                    "F06 Venetoclax high"))
allplot
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Censor1.tiff", units="in", width=12, height=8, res=200)
allplot
dev.off()

### CoxPH risk estimates
fit <- survfit(Surv(Day, Censor)~Group, data=data)
coxfit <- coxph(Surv(Day, Censor) ~ Group, data = data, ties = 'exact')
fit <- survfit(Surv(Day)~Group, data=data)
coxfit <- coxph(Surv(Day) ~ Group, data = data, ties = 'exact')
summary(coxfit)

forest_plot <- ggforest(coxfit, data = data,
                        cpositions = c(0.01, 0.07, 0.3), 
                        fontsize = 1, noDigits = 3,
                        refLabel = "reference")
forest_plot



data_F01 <- dplyr::filter(data, Group %in% c("F02 Gilteritinib", "F03 NCGC00841450","F04 NCGC00690381",
                                             "F05 NCGC00841754", "F06 NCGC00843798","F07 CA-4948"))
fit <- survfit(Surv(`Day`, Censor)~Group, data=data_F01)
surv_median(fit)
allplot <- ggsurvplot2(fit, data=data_F01, xlab = "Days (post-dosing)", pval = T, risk.table = T, #surv.median.line = c("hv")),
                       title = "", 
                       legend="right", legend.title="Group", legend.labs=c("Gilteritinib",
                                                                           "NCGC00841450",
                                                                           "NCGC00690381",
                                                                           "NCGC00841754",
                                                                           "NCGC00843798",
                                                                           "CA-4948"))
allplot
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("Censor1.tiff", units="in", width=16, height=10, res=200)
allplot
dev.off()

fit <- survfit(Surv(Day)~Group, data=data_F01)
coxfit <- coxph(Surv(Day) ~ Group, data = data_F01, ties = 'exact')
summary(coxfit)

forest_plot <- ggforest(coxfit, data = data_F01,
                        cpositions = c(0.01, 0.07, 0.3), 
                        fontsize = 1, noDigits = 3,
                        refLabel = "reference")
forest_plot








F01$Tx <- factor(F01$Tx, levels = c("30 mg/kg Gilteritinib",
                                          "1 mg/kg NCGC00841450",
                                          "3 mg/kg NCGC00841450",
                                          "10 mg/kg NCGC00841450",
                                          "10 mg/kg NCGC00841450 MWF",
                                          "30 mg/kg NCGC00841450"))
coxfit <- coxph(Surv(Days) ~ Tx, data = F01, ties = 'exact')
summary(coxfit)


data <- filter(all, Groups %in% c("F01", "F02", "F04")) 
fit <- survfit(Surv(`Day`, censor)~Group, data=data)
data$`Groups long`
F04 <- ggsurvplot2(fit, data=data, pval = F, risk.table = F, conf.int = F, 
            title = "F04 NCGC00690381", 
            legend="right",legend.title="Groups",legend.labs=c("Vehicle",
                                                               "Gilteritinib",
                                                               "F04 NCGC00690381")) 
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


F01 <- dplyr::filter(all, Groups!="F01")
fit <- survfit(Surv(TimeOnTest, censor)~Groups, data=F01)
SOC <- ggsurvplot(fit, data=F01, pval = TRUE, risk.table = F, #surv.median.line = c("hv"), 
                  legend="right",legend.title="Groups",legend.labs=c("Gilteritinib",
                                                                      "NCGC00841450",
                                                                      "NCGC00690381",
                                                                      "NCGC00841754",
                                                                      "NCGC00843798",
                                                                      "CA-4948"))
SOC
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("19-331-121 Survival Curves SOC.tiff", units="in", width=10, height=5, res=200)
SOC
dev.off()


coxfit <- coxph(Surv(TimeOnTest, censor) ~ Groups, data = F01, ties = 'exact')
summary(coxfit)


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
