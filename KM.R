#data <- read_excel("JBM 336.xlsx")

library(survMisc)
library(survival)
library(jskm)
library(survey)
library(survminer)
library(survival)
library(ggplot2)
library(ggfortify)

####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 
####### OPTION 1 ####### 

fit <- survfit(Surv(TimeOnTest, Censor)~Groups, data=data)
all <- ggsurvplot(fit, data=data, pval = TRUE, risk.table = F, surv.median.line = c("hv"), 
           legend="right",legend.title="Groups",legend.labs=c("F01 Control",
                                                              "F02 Gilteritinib",
                                                              "F03 NCGC00689526",
                                                              "F04 NCGC00690380",
                                                              "F05 NCGC00841450",
                                                              "F06 NCGC00689529"))

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("19-331-121 Survival Curves.tiff", units="in", width=10, height=5, res=200)
all
dev.off()


F01 <- dplyr::filter(data, Groups!="F01 - Control")
fit <- survfit(Surv(TimeOnTest, Censor)~Groups, data=F01)
SOC <- ggsurvplot(fit, data=F01, pval = TRUE, risk.table = F, surv.median.line = c("hv"), 
                  legend="right",legend.title="Groups",legend.labs=c("F02 Gilteritinib",
                                                                     "F03 NCGC00689526",
                                                                     "F04 NCGC00690380",
                                                                     "F05 NCGC00841450",
                                                                     "F06 NCGC00689529"))
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("19-331-121 Survival Curves SOC.tiff", units="in", width=10, height=5, res=200)
SOC
dev.off()


coxfit <- coxph(Surv(TimeOnTest, Censor) ~ Groups, data = F01, ties = 'exact')
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
