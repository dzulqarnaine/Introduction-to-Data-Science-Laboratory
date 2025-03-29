library(tidyverse)
library(cluster) #algoritma klastering
library(factoextra) #algoritma klastering dan visual

library(openxlsx)


datafauzi2=read.xlsx("D:/DATA RSTUDIO/LAB PSD/Nilai machine learning.xlsx")
datafauzi2


attach(datafauzi2)
datafauzi2= datafauzi2[, c("UAS", "UTS", "Tugas.1")]
datafauzi2
attach(datafauzi2)

str(datafauzi2)
head(datafauzi2)

datafauzi3 = na.omit(datafauzi2) #menghilangkan missing data
summary(datafauzi3)


#standarisasi data
datafixfauzi = scale(datafauzi3) #standarisasi data
datafixfauzi

head(datafixfauzi)

#patahan siku untuk menunjukkan K terbaik
fviz_nbclust(datafixfauzi, kmeans, method = "wss") #metode elbow

#Nilai tertinggi rata-ratanya untuk menentukan K terbaik
#metode silhouette
fviz_nbclust(datafixfauzi, kmeans, method = "silhouette") 
set.seed(123)

#motode gap statistic
gap_stat <- clusGap(datafixfauzi, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 34) 
fviz_gap_stat(gap_stat)


final <- kmeans(datafixfauzi, 3, nstart = 25)
print(final)

fviz_cluster(final, data = datafixfauzi)

datafauzi2 %>%
  mutate(cluster = final$cluster) %>%
  group_by(cluster) %>%
  summarise_all("mean")

