#LAB 4 PENGANTAR SAINS DATA
library(openxlsx)

#CHIS SQUARE

fauzi= read.xlsx("D:/DATA RSTUDIO/LAB PSD/DATA PRAK 4.xlsx", sheet = "chi square" )
fauzi
attach(fauzi)
tabel = table(Pengetahuan, Sikap)
tabel #opsional
chisq.test(tabel)

#ANOVA
library(easyanova)

fauzii= read.xlsx("D:/DATA RSTUDIO/LAB PSD/DATA PRAK 4.xlsx", sheet = "anova" )
fauzii
attach(fauzii)
anova = aov(Observed.tensile.strength~Percentage.of.Catton) #urutan numerik ke kategorik
summary(anova)

#diluar konteks
anova #opsional

