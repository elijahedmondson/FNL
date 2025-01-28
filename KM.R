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

data <- read_excel("C:/Users/edmondsonef/Desktop/MHL Doroshow Ls513 Parental SC.xlsx", 
                   sheet = "Ls513 EP Pool SC-1")

# data <- filter(data, Groups %in% c("F01", "F02", "F04")) 
NSG <- dplyr::filter(data, Strain!="ANU")
ANU <- dplyr::filter(data, Strain!="NSG")


####### KM ####### 
####### KM ####### 
####### KM ####### 

fit <- survfit(Surv(`Day`, Censor)~Group, data=NSG)
ggsurvplot2(fit, data=NSG, pval = F, risk.table = F, title = "", legend="right", xlim = c(0, 65), break.x.by = 10, xlab = "Day")
plot <- ggsurvplot2(fit, data=NSG, pval = F, risk.table = F, title = "", legend="right", xlim = c(0, 65), break.x.by = 10, xlab = "Day")
setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("NSG-KM.tiff", units="in", width=6, height=3, res=200)
plot
dev.off()

fit <- survfit(Surv(`Day`, Censor)~Group, data=ANU)
ggsurvplot2(fit, data=ANU, pval = F, risk.table = F, title = "", legend="right", xlim = c(0, 65), break.x.by = 10, xlab = "Day")
plot <- ggsurvplot2(fit, data=ANU, pval = F, risk.table = F, title = "", legend="right", xlim = c(0, 65), break.x.by = 10, xlab = "Day")#,
# legend.title="Groups",legend.labs=c("F01 Vehicle", "F08 Revumenib, Tamibarotene, & Iadaemstat"))

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("ANU-KM.tiff", units="in", width=6, height=3, res=200)
plot
dev.off()


####### Forest Plot ####### 
####### Forest Plot ####### 
####### Forest Plot ####### 

coxfit <- coxph(Surv(`Day`, Censor) ~ Group, data = data, ties = 'exact')
summary(coxfit)

forest_plot <- ggforest(coxfit, data = data,
                        cpositions = c(0.01, 0.07, 0.3), 
                        fontsize = 0.9, noDigits = 2,
                        refLabel = "reference")
forest_plot

setwd("C:/Users/edmondsonef/Desktop/R-plots/")
tiff("forest_plot.tiff", units="in", width=11, height=4, res=200)
forest_plot
dev.off()








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

