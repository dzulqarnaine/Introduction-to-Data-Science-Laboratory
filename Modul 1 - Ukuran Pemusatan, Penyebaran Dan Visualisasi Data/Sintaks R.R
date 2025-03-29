#Lab 1

library(openxlsx)
df = read.xlsx("C:/Users/User/Downloads/IPM.xlsx")
df

attach(df)

#Ukuran Pemusatan data
mean(IPM2018)
median(IPM2018)
max(IPM2018)
min(IPM2018)

#Ukuran Penyebaran data
sd(IPM2018)
var(IPM2018)
IQR(IPM2018)

summary(df)

#Visualisasi
#Histogram
hist(IPM2018
     , col = "skyblue"
     , main = "Histogram IPM 2018"
     , xlab = "IPM 2018"
     , ylab = "Frekuensi"
     , labels = TRUE)

#Boxplot
boxplot(IPM2018
        ,main = "Boxplot IPM 2018"
        ,col= "green")

#Stem and leaf
stem(IPM2018)
