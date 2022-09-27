data <- read_excel("C:/Users/edmondsonef/Desktop/JBM 336 Data.xlsx", 
                   sheet = "Full Data")



library(survMisc)
library(survival)
library(jskm)
library(survey)
library(survminer)
library(survival)
library(ggplot2)
library(ggfortify)

library(survival)
library(survminer)
library(flexsurv)
library(dplyr)
library(survtools)

####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
####
#### JBM Studies
####

#format data only with columsn for sum
DF <- data %>% group_by(Group) %>%
  replace(is.na(.), 0) %>%
  summarise_all(funs(sum))

fit <- survfit(Surv(data$'Age', Censor)~Group, data=data)
surv_median(fit)
x <- ggsurvplot2(fit, data=data, pval = F, risk.table = F,conf.int = F, surv.median.line = c("hv"), 
                       legend="right",legend.title="Groups",legend.labs=c("Control",
                                                                          "Rapamycin",
                                                                          "3 Gy",
                                                                          "3 Gy + 14 mg/kg",
                                                                          "3 Gy + 50 mg/kg"))
allplot

tiff("KM_all.tiff", units="in", width=7, height=4, res=300)
allplot
dev.off()

new <- filter(data, Group %in% c("F03", "F04", "F05")) 
fit <- survfit(Surv(new$'Age', Censor)~Group, data=new)
surv_median(fit)
newplot <- ggsurvplot2(fit, data=new, pval = T, risk.table = F,conf.int = F, surv.median.line = c("hv"), 
                       legend="right",legend.title="Groups",legend.labs=c("3 Gy",
                                                                          "3 Gy + 14 mg/kg",
                                                                          "3 Gy + 50 mg/kg"))
newplot
tiff("KM_censor_sub.tiff", units="in", width=7, height=4, res=300)
newplot
dev.off()


####
#### AML Studies
####


all <- read_excel("C:/Users/edmondsonef/Desktop/MHL 19-331-137.xlsx", 
                   sheet = "Animal data")

data <- all
fit <- survfit(Surv(`Day`, censor)~Group, data=data)
surv_median(fit)
allplot <- ggsurvplot2(fit, data=data, pval = F, risk.table = F,# surv.median.line = c("hv"), 
                       legend="right",legend.title="Groups",legend.labs=c("Vehicle",
                                                                          "Gilteritinib",
                                                                          "NCGC00841450",
                                                                          "NCGC00690381",
                                                                          "NCGC00841754",
                                                                          "NCGC00843798",
                                                                          "CA-4948"))
allplot



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
