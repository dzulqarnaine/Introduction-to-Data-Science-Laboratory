#Distribusi Diskrit dan Kontinu
"Distribusi Diskrit"


#Distribusi Binomial

#diketahui  x = 3, size atau n = 38 dan peluang suksesnya 0.25
dbinom(3, 38, 0.25 ) #menggunakan dbinom utk x = 3
pbinom(6, 38, 0.25) #menggunakan pbinom utk X<=6
pbinom(6, 38, 0.25, lower.tail = FALSE)  #menggunakan Dbinom utk rumus  X>6
1-pbinom(3, 38, 0.25) #sama dengan baris 10, kalau tidak menggunakan lower tail maka hrus diikuti 1- p


#Distribusi Poisson

#diketahui lambda = 6
dpois(0, 6) #untuk X = 0
ppois(6, 6) #untuk x<=6
ppois(4, 6, lower.tail = FALSE) #untuk x>4
1-ppois(4,6)

#distribusi Kontinu

#Distribusi Normal

#misal dik miu = 7, std = 0.1
dnorm(7.25, 7, 0.1) #utk x= 7.25
pnorm(7.25, 7, 0.1) #utk x <=7.25
pnorm(7.25, 7, 0.1, lower.tail = FALSE) # utk x>=7.25
