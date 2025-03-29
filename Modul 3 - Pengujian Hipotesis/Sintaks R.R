library(openxlsx)
data= read.xlsx("D:/DATA RSTUDIO/LAB PSD/DATA PRAK 3.xlsx")
data

attach(data)
?read

fauzi = read.xlsx("D:/DATA RSTUDIO/LAB PSD/DATA PRAK 3.xlsx", sheet=1)
fauzi
attach(fauzi)
fauzi1 = t.test(Pasien, mu = 25, alternative = "greater")
fauzi1

?t.test

library(BSDA)
library(MASS)
library(datasets)

fauzi = read.xlsx("D:/DATA RSTUDIO/LAB PSD/DATA PRAK 3.xlsx", sheet=2)
fauzi
library(BSDA)
fauzi2 = zsum.test(mean.x=mean(Kelahiran), 
                   sigma.x=sd(Kelahiran), 
                   n.x=length(Kelahiran),
                   alternative = "less", 
                   mu=33.5, 
                   conf.level = 0.95)
fauzi2

z.test(
  Kelahiran,
  alternative = "less",
  mu = 33.5,
  sigma.x = sd(Kelahiran),
  conf.level = 0.95
)

?zsum.test()


#saling bebas
data.fauzi3 = read.xlsx("D:/DATA RSTUDIO/PRAKTIKUM PSD/DATA PRAK 3.xlsx", sheet=4)
data.fauzi3
fauzi3 = t.test(Rasa.Durian ~ Pendidikan.Sales, var.equal =TRUE)
fauzi3

 t.test(Rasa.Durian ~ Pendidikan.Sales, data.fauzi3, var.equal =TRUE)



#berpasangan
fauzi4= read.xlsx("D:/DATA RSTUDIO/PRAKTIKUM PSD/DATA PRAK 3.xlsx", sheet=4)
fauzi4
attach(fauzi4)
fauzii4= t.test(Sebelum , Sesudah, paired =TRUE)
fauzii4

