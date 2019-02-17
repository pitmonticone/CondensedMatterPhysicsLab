library(readr)
library(readxl)
library(gdata)

################### ZNT [RICCARDO] ######################

###### ZNT1

zn1_data <- read_excel("~/github/Lab-Struttura/images/trasversale_normale [RICCARDO]/data_TN/data_merges_xR/zn1_data.xls", 
                       col_types = c("blank", "blank", "numeric", 
                                     "blank", "numeric", "blank", "blank", 
                                     "blank", "blank", "blank", "blank", 
                                     "blank", "blank"))
View(zn1_data)

zn2_data <- read_excel("~/github/Lab-Struttura/images/trasversale_normale [RICCARDO]/data_TN/data_merges_xR/zn2_data.xls", 
                       col_types = c("blank", "blank", "numeric", 
                                     "blank", "numeric", "blank", "blank", 
                                     "blank", "blank", "blank", "blank", 
                                     "blank", "blank"))
#View(zn2_data)

zn3_data <- read_excel("~/github/Lab-Struttura/images/trasversale_normale [RICCARDO]/data_TN/data_merges_xR/zn3_data.xls", 
                       col_types = c("blank", "blank", "numeric", 
                                     "blank", "numeric", "blank", "blank", 
                                     "blank", "blank", "blank", "blank", 
                                     "blank", "blank"))
#View(zn3_data)

zn4_data <- read_excel("~/github/Lab-Struttura/images/trasversale_normale [RICCARDO]/data_TN/data_merges_xR/zn4_data.xls", 
                       col_types = c("blank", "blank", "numeric", 
                                     "blank", "numeric", "blank", "blank", 
                                     "blank", "blank", "blank", "blank", 
                                     "blank", "blank"))
#View(zn4_data)

zn5_data <- read_excel("~/github/Lab-Struttura/images/trasversale_normale [RICCARDO]/data_TN/data_merges_xR/zn5_data.xls", 
                       col_types = c("blank", "blank", "numeric", 
                                     "blank", "numeric", "blank", "blank", 
                                     "blank", "blank", "blank", "blank", 
                                     "blank", "blank"))
#View(zn5_data)

zn6_data <- read_excel("~/github/Lab-Struttura/images/trasversale_normale [RICCARDO]/data_TN/data_merges_xR/zn6_data.xls", 
                       col_types = c("blank", "blank", "numeric", 
                                     "blank", "numeric", "blank", "blank", 
                                     "blank", "blank", "blank", "blank", 
                                     "blank", "blank"))
#View(zn6_data)

zn7_data <- read_excel("~/github/Lab-Struttura/images/trasversale_normale [RICCARDO]/data_TN/data_merges_xR/zn7_data.xls", 
                       col_types = c("blank", "blank", "numeric", 
                                     "blank", "numeric", "blank", "blank", 
                                     "blank", "blank", "blank", "blank", 
                                     "blank", "blank"))
#View(zn7_data)

zna8_data <- read_excel("~/github/Lab-Struttura/images/trasversale_normale [RICCARDO]/data_TN/data_merges_xR/zna8_data.xls", 
                       col_types = c("blank", "blank", "numeric", 
                                     "blank", "numeric", "blank", "blank", 
                                     "blank", "blank", "blank", "blank", 
                                     "blank", "blank"))
#View(zna8_data)

A1 <- zn1_data$Area[1:12]
A2 <- zn1_data$Area[14:23]
A3 <- zn1_data$Area[25:29]
d12 <- c(A1[2]-A1[1],A1[3]-A1[2],A1[5]-A1[4],A1[6]-A1[5],A1[8]-A1[7],A1[9]-A1[8],A1[11]-A1[10],A1[12]-A1[11],(A2[2]-A2[1])/2,(A2[4]-A2[3])/2,(A2[6]-A2[5])/2,(A2[8]-A2[7])/2,(A2[10]-A2[9])/2)
sd12 <- sd(d12)
D1 <- c(A1[4]-A1[1],A1[5]-A1[2],A1[6]-A1[3],A1[7]-A1[4],A1[8]-A1[5],A1[9]-A1[6],A1[10]-A1[7],A1[11]-A1[8],A1[12]-A1[9],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A2[7]-A2[5],A2[8]-A2[6],A2[9]-A2[7],A2[10]-A2[8],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3],A3[5]-A3[4])
sD1 <- sd(D1)
d12m <- mean(d12)
D1m <- mean(D1)
dD1 <- d12m/D1m
sdD1 <- sqrt((sd12/D1m)^2+((sD1*d12m)/(D1m^2))^2) # 0.158 pm 0.010


###### ZNT2

A1 <- zn2_data$Area[1:15]
A2 <- zn2_data$Area[17:26]
A3 <- zn2_data$Area[28:32]
d15 <- c(A1[2]-A1[1],A1[3]-A1[2],A1[5]-A1[4],A1[6]-A1[5],A1[8]-A1[7],A1[9]-A1[8],A1[11]-A1[10],A1[12]-A1[11],A1[14]-A1[13],A1[15]-A1[14],(A2[2]-A2[1])/2,(A2[4]-A2[3])/2,(A2[6]-A2[5])/2,(A2[8]-A2[7])/2,(A2[10]-A2[9])/2)
D2 <- c(A1[4]-A1[1],A1[5]-A1[2],A1[6]-A1[3],A1[7]-A1[4],A1[8]-A1[5],A1[9]-A1[6],A1[10]-A1[7],A1[11]-A1[8],A1[12]-A1[9],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A2[7]-A2[5],A2[8]-A2[6],A2[9]-A2[7],A2[10]-A2[8],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3],A3[5]-A3[4])
sD2 <- sd(D2)
sd15 <- sd(d15)
d15m <- mean(d15)
D2m <- mean(D2)
dD2 <- d15m/D2m
sdD2 <- sqrt((sd15/D2m)^2+((sD2*d15m)/(D2m^2))^2) # 0.188 pm 0.025

###### ZNT3

A1 <- zn3_data$Area[1:15]
A2 <- zn3_data$Area[17:26]
A3 <- zn3_data$Area[28:32]
d15 <- c(A1[2]-A1[1],A1[3]-A1[2],A1[5]-A1[4],A1[6]-A1[5],A1[8]-A1[7],A1[9]-A1[8],A1[11]-A1[10],A1[12]-A1[11],A1[14]-A1[13],A1[15]-A1[14],(A2[2]-A2[1])/2,(A2[4]-A2[3])/2,(A2[6]-A2[5])/2,(A2[8]-A2[7])/2,(A2[10]-A2[9])/2)
D3 <- c(A1[4]-A1[1],A1[5]-A1[2],A1[6]-A1[3],A1[7]-A1[4],A1[8]-A1[5],A1[9]-A1[6],A1[10]-A1[7],A1[11]-A1[8],A1[12]-A1[9],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A2[7]-A2[5],A2[8]-A2[6],A2[9]-A2[7],A2[10]-A2[8],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3],A3[5]-A3[4])
sD3 <- sd(D3)
sd15 <- sd(d15)
d15m <- mean(d15)
D3m <- mean(D3)
dD3 <- d15m/D3m
sdD3 <- sqrt((sd15/D3m)^2+((sD3*d15m)/(D3m^2))^2) # 0.221 pm 0.031


###### ZNT4

A1 <- zn4_data$Area[1:15]
A2 <- zn4_data$Area[17:26]
A3 <- zn4_data$Area[28:32]
d15 <- c(A1[2]-A1[1],A1[3]-A1[2],A1[5]-A1[4],A1[6]-A1[5],A1[8]-A1[7],A1[9]-A1[8],A1[11]-A1[10],A1[12]-A1[11],A1[14]-A1[13],A1[15]-A1[14],(A2[2]-A2[1])/2,(A2[4]-A2[3])/2,(A2[6]-A2[5])/2,(A2[8]-A2[7])/2,(A2[10]-A2[9])/2)
D4 <- c(A1[4]-A1[1],A1[5]-A1[2],A1[6]-A1[3],A1[7]-A1[4],A1[8]-A1[5],A1[9]-A1[6],A1[10]-A1[7],A1[11]-A1[8],A1[12]-A1[9],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A2[7]-A2[5],A2[8]-A2[6],A2[9]-A2[7],A2[10]-A2[8],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3],A3[5]-A3[4])
sD4 <- sd(D4)
sd15 <- sd(d15)
d15m <- mean(d15)
D4m <- mean(D4)
dD4 <- d15m/D4m
sdD4 <- sqrt((sd15/D4m)^2+((sD4*d15m)/(D4m^2))^2) # 0.233 pm 0.025


###### ZNT5

A1 <- zn5_data$Area[1:15]
A2 <- zn5_data$Area[17:26]
A3 <- zn5_data$Area[28:32]
d15 <- c(A1[2]-A1[1],A1[3]-A1[2],A1[5]-A1[4],A1[6]-A1[5],A1[8]-A1[7],A1[9]-A1[8],A1[11]-A1[10],A1[12]-A1[11],A1[14]-A1[13],A1[15]-A1[14],(A2[2]-A2[1])/2,(A2[4]-A2[3])/2,(A2[6]-A2[5])/2,(A2[8]-A2[7])/2,(A2[10]-A2[9])/2)
D5 <- c(A1[4]-A1[1],A1[5]-A1[2],A1[6]-A1[3],A1[7]-A1[4],A1[8]-A1[5],A1[9]-A1[6],A1[10]-A1[7],A1[11]-A1[8],A1[12]-A1[9],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A2[7]-A2[5],A2[8]-A2[6],A2[9]-A2[7],A2[10]-A2[8],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3],A3[5]-A3[4])
sD5 <- sd(D5)
sd15 <- sd(d15)
d15m <- mean(d15)
D5m <- mean(D5)
dD5 <- d15m/D5m
sdD5 <- sqrt((sd15/D5m)^2+((sD5*d15m)/(D5m^2))^2) # 0.263 pm 0.034

###### ZNT6

A1 <- zn6_data$Area[1:12]
A2 <- zn6_data$Area[14:23]
A3 <- zn6_data$Area[25:29]
d12 <- c(A1[2]-A1[1],A1[3]-A1[2],A1[5]-A1[4],A1[6]-A1[5],A1[8]-A1[7],A1[9]-A1[8],A1[11]-A1[10],A1[12]-A1[11],(A2[2]-A2[1])/2,(A2[4]-A2[3])/2,(A2[6]-A2[5])/2,(A2[8]-A2[7])/2,(A2[10]-A2[9])/2)
sd12 <- sd(d12)
D6 <- c(A1[4]-A1[1],A1[5]-A1[2],A1[6]-A1[3],A1[7]-A1[4],A1[8]-A1[5],A1[9]-A1[6],A1[10]-A1[7],A1[11]-A1[8],A1[12]-A1[9],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A2[7]-A2[5],A2[8]-A2[6],A2[9]-A2[7],A2[10]-A2[8],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3],A3[5]-A3[4])
sD6 <- sd(D6)
d12m <- mean(d12)
D6m <- mean(D6)
dD6 <- d12m/D6m
sdD6 <- sqrt((sd12/D6m)^2+((sD6*d12m)/(D6m^2))^2) # 0.198 pm 0.022

###### ZNT7

A1 <- zn7_data$Area[1:11]
A2 <- zn7_data$Area[13:21]
A3 <- zn7_data$Area[23:27]
d11 <- c(A1[2]-A1[1],A1[3]-A1[2],A1[5]-A1[4],A1[6]-A1[5],A1[8]-A1[7],A1[9]-A1[8],A1[11]-A1[10],(A2[2]-A2[1])/2,(A2[4]-A2[3])/2,(A2[6]-A2[5])/2,(A2[8]-A2[7])/2)
D7 <- c(A1[4]-A1[1],A1[5]-A1[2],A1[6]-A1[3],A1[7]-A1[4],A1[8]-A1[5],A1[9]-A1[6],A1[10]-A1[7],A1[11]-A1[8],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A2[7]-A2[5],A2[8]-A2[6],A2[9]-A2[7],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3],A3[5]-A3[4])
sd11 <- sd(d11)
sD7 <- sd(D7)
d11m <- mean(d11)
D7m <- mean(D7)
dD7 <- d11m/D7m
sdD7 <- sqrt((sd11/D7m)^2+((sD7*d11m)/(D7m^2))^2) # 0.125 pm 0.023

###### ZNT8

A2 <- zna8_data$Area[1:8]
A3 <- zna8_data$Area[10:14]
d4 <- c((A2[2]-A2[1])/2,(A2[4]-A2[3])/2,(A2[6]-A2[5])/2,(A2[8]-A2[7])/2)
D8 <- c(A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A2[7]-A2[5],A2[8]-A2[6],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3],A3[5]-A3[4])
sd4 <- sd(d4)
sD8 <- sd(D8)
d4m <- mean(d4)
D8m <- mean(D8)
dD8 <- d4m/D8m
sdD8 <- sqrt((sd4/D8m)^2+((sD8*d4m)/(D8m^2))^2) # 0.061 pm 0.009