install.packages("gridExtra")
library(ggplot2) 
library(grid)
library(datasets)
library(gridExtra) 
data(ToothGrowth) 
attach(ToothGrowth)
ToothGrowth$dose <- factor(ToothGrowth$dose)
str(ToothGrowth)
set.seed(123)
scatter <- ggplot(ToothGrowth,aes(dose,len)) + geom_boxplot(aes(fill=supp)) + geom_jitter(alpha=I(3/4),aes(color=supp)) + scale_color_manual(values=c("black","blue")) + theme(legend.position=c(1,0.3),legend.justification=c(1,1)) +labs(title="Tooth Length and Dose Levels",x="Dose Levels",y="Tooth Length")
plot_right <- ggplot(ToothGrowth,aes(len,fill=supp)) + geom_density(alpha=.5) +coord_flip() +scale_fill_manual(values=c("blue","red")) + theme(legend.position="none") +labs(title="Density Plot",y="Density",x="Tooth Length")
grid.arrange(scatter, plot_right, ncol=2, nrow=1, widths=c(4, 2))
summary(ToothGrowth)
table(ToothGrowth$supp,ToothGrowth$dose)
t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth)
Dose1_05_10 <- subset(ToothGrowth, dose %in% c(0.5, 1.0))

Dose2_05_20 <- subset(ToothGrowth, dose %in% c(0.5, 2.0))

Dose3_10_20 <- subset(ToothGrowth, dose %in% c(1.0, 2.0))
t.test(len ~ dose, paired = F, var.equal = F, data = Dose1_05_10)
t.test(len ~ dose, paired = F, var.equal = F, data = Dose_05_20)














