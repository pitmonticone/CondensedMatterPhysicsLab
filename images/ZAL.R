library(readr)
library(readxl)
library(gdata)

################### ZAL [ALBERTO] ######################


zal0_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_anomalo [ALBI]/data_LA/zal0_data.xls", 
                        col_types = c("numeric", "numeric", "blank", 
                                      "numeric"))
#View(zal0_data)

zal1_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_anomalo [ALBI]/data_LA/zal1_data.xls", 
                        col_types = c("numeric", "numeric", "blank", 
                                      "blank"))
#View(zal1_data)

zal2_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_anomalo [ALBI]/data_LA/zal2_data.xls", 
                        col_types = c("numeric", "numeric", "blank", 
                                      "blank"))
#View(zal2_data)

zal3_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_anomalo [ALBI]/data_LA/zal3_data.xls", 
                        col_types = c("numeric", "numeric", "blank", 
                                      "blank"))
#View(zal3_data)

zal4_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_anomalo [ALBI]/data_LA/zal4_data.xls", 
                        col_types = c("numeric", "numeric", "blank", 
                                      "blank"))
#View(zal4_data)

zal5_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_anomalo [ALBI]/data_LA/zal5_data.xls", 
                        col_types = c("numeric", "numeric", "blank", 
                                      "blank"))
#View(zal5_data)

zal6_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_anomalo [ALBI]/data_LA/zal6_data.xls", 
                        col_types = c("numeric", "numeric", "blank", 
                                      "blank"))
#View(zal6_data)

## ZAL0
r <- zal0_data$Raggio[1:6]
d <-pi*c( ((r[2]-r[1])^2)/4 + (r[2]-r[1])*r[1], ((r[4]-r[3])^2)/4 + (r[4]-r[3])*r[3]  , ((r[6]-r[5])^2)/4 + (r[6]-r[5])*r[5] )
a <- pi*c( r[2]^2-r[1]^2-d[1]/pi , r[4]^2-r[3]^2-d[2]/pi  , r[6]^2-r[5]^2-d[3]/pi )


## ZAL1

r <- zal0_data$Raggio[1:6]
d <-pi*c( ((r[2]-r[1])^2)/4 + (r[2]-r[1])*r[1], ((r[4]-r[3])^2)/4 + (r[4]-r[3])*r[3]  , ((r[6]-r[5])^2)/4 + (r[6]-r[5])*r[5] )
a <- pi*c( r[2]^2-r[1]^2-d[1]/pi , r[4]^2-r[3]^2-d[2]/pi  , r[6]^2-r[5]^2-d[3]/pi )

A1 <- zal1_data$Area[1:8]
A1[1] <- A1[1]+d[1]
A1[2] <- A1[2]-a[1]
A1[3] <- A1[3]+d[1]
A1[4] <- A1[4]-a[1]
A1[5] <- A1[5]+d[2]
A1[6] <- A1[6]-a[2]
A1[7] <- A1[7]+d[2]
A1[8] <- A1[8]-a[2]
A2 <- zal1_data$Area[10:15]
A2[1] <- A2[1]+d[1]
A2[2] <- A2[2]-a[1]
A2[3] <- A2[3]+d[2]
A2[4] <- A2[4]-a[2]
A2[5] <- A2[5]+d[3]
A2[6] <- A2[6]-a[3]
A3 <- zal1_data$Area[17:22]
A3[1] <- A3[1]+d[1]
A3[2] <- A3[2]-a[1]
A3[3] <- A3[3]+d[2]
A3[4] <- A3[4]-a[2]
A3[5] <- A3[5]+d[3]
A3[6] <- A3[6]-a[3]

d1 <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5],A1[8]-A1[7],(A2[2]-A2[1]),(A2[4]-A2[3]),(A2[6]-A2[5]),(A3[2]-A3[1]),(A3[4]-A3[3]),(A3[6]-A3[5]))/2
D1 <- c(A1[5]-A1[1],A1[6]-A1[2],A1[7]-A1[3],A1[8]-A1[4],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A3[3]-A3[1],A3[4]-A3[2],A3[5]-A3[3],A3[6]-A3[4])
sd1 <- sd(d1)
sD1 <- sd(D1)
d1m <- mean(d1)
D1m <- mean(D1)
dD1 <- d1m/D1m
sdD1 <- sqrt((sd1/D1m)^2+((sD1*d1m)/(D1m^2))^2) # 0.158 pm 0.010


###### ZAL2

r <- zal0_data$Raggio[1:6]
d <-pi*c( ((r[2]-r[1])^2)/4 + (r[2]-r[1])*r[1], ((r[4]-r[3])^2)/4 + (r[4]-r[3])*r[3]  , ((r[6]-r[5])^2)/4 + (r[6]-r[5])*r[5] )
a <- pi*c( r[2]^2-r[1]^2-d[1]/pi , r[4]^2-r[3]^2-d[2]/pi  , r[6]^2-r[5]^2-d[3]/pi )

A1 <- zal2_data$Area[1:8]
A1[1] <- A1[1]+d[1]
A1[2] <- A1[2]-a[1]
A1[3] <- A1[3]+d[1]
A1[4] <- A1[4]-a[1]
A1[5] <- A1[5]+d[2]
A1[6] <- A1[6]-a[2]
A1[7] <- A1[7]+d[2]
A1[8] <- A1[8]-a[2]
A2 <- zal2_data$Area[10:15]
A2[1] <- A2[1]+d[1]
A2[2] <- A2[2]-a[1]
A2[3] <- A2[3]+d[2]
A2[4] <- A2[4]-a[2]
A2[5] <- A2[5]+d[3]
A2[6] <- A2[6]-a[3]
A3 <- zal2_data$Area[17:20]
A3[1] <- A3[1]+d[1]
A3[2] <- A3[2]-a[1]
A3[3] <- A3[3]+d[2]
A3[4] <- A3[4]-a[2]

d2 <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5],A1[8]-A1[7],(A2[2]-A2[1]),(A2[4]-A2[3]),(A2[6]-A2[5]),(A3[2]-A3[1]),(A3[4]-A3[3]))/2
D2 <- c(A1[5]-A1[1],A1[6]-A1[2],A1[7]-A1[3],A1[8]-A1[4],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A3[3]-A3[1],A3[4]-A3[2])
sd2 <- sd(d2)
sD2 <- sd(D2)
d2m <- mean(d2)
D2m <- mean(D2)
dD2 <- d2m/D2m
sdD2 <- sqrt((sd2/D2m)^2+((sD2*d2m)/(D2m^2))^2) # 0.158 pm 0.010

###### ZAL3

r <- zal0_data$Raggio[1:6]
d <-pi*c( ((r[2]-r[1])^2)/4 + (r[2]-r[1])*r[1], ((r[4]-r[3])^2)/4 + (r[4]-r[3])*r[3]  , ((r[6]-r[5])^2)/4 + (r[6]-r[5])*r[5] )
a <- pi*c( r[2]^2-r[1]^2-d[1]/pi , r[4]^2-r[3]^2-d[2]/pi  , r[6]^2-r[5]^2-d[3]/pi )

A1 <- zal3_data$Area[1:8]
A1[1] <- A1[1]+d[1]
A1[2] <- A1[2]-a[1]
A1[3] <- A1[3]+d[1]
A1[4] <- A1[4]-a[1]
A1[5] <- A1[5]+d[2]
A1[6] <- A1[6]-a[2]
A1[7] <- A1[7]+d[2]
A1[8] <- A1[8]-a[2]
A2 <- zal3_data$Area[10:15]
A2[1] <- A2[1]+d[1]
A2[2] <- A2[2]-a[1]
A2[3] <- A2[3]+d[2]
A2[4] <- A2[4]-a[2]
A2[5] <- A2[5]+d[3]
A2[6] <- A2[6]-a[3]
A3 <- zal3_data$Area[17:22]
A3[1] <- A3[1]+d[1]
A3[2] <- A3[2]-a[1]
A3[3] <- A3[3]+d[2]
A3[4] <- A3[4]-a[2]
A3[5] <- A3[5]+d[3]
A3[6] <- A3[6]-a[3]

d3 <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5],A1[8]-A1[7],(A2[2]-A2[1]),(A2[4]-A2[3]),(A2[6]-A2[5]),(A3[2]-A3[1]),(A3[4]-A3[3]),(A3[6]-A3[5]))/2
D3 <- c(A1[5]-A1[1],A1[6]-A1[2],A1[7]-A1[3],A1[8]-A1[4],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A3[3]-A3[1],A3[4]-A3[2],A3[5]-A3[3],A3[6]-A3[4])
sd3 <- sd(d3)
sD3 <- sd(D3)
d3m <- mean(d3)
D3m <- mean(D3)
dD3 <- d3m/D3m
sdD3 <- sqrt((sd3/D3m)^2+((sD3*d3m)/(D3m^2))^2) # 0.158 pm 0.010

###### ZAL4

r <- zal0_data$Raggio[1:6]
d <-pi*c( ((r[2]-r[1])^2)/4 + (r[2]-r[1])*r[1], ((r[4]-r[3])^2)/4 + (r[4]-r[3])*r[3]  , ((r[6]-r[5])^2)/4 + (r[6]-r[5])*r[5] )
a <- pi*c( r[2]^2-r[1]^2-d[1]/pi , r[4]^2-r[3]^2-d[2]/pi  , r[6]^2-r[5]^2-d[3]/pi )

A1 <- zal4_data$Area[1:4]
A1[1] <- A1[1]+d[1]
A1[2] <- A1[2]-a[1]
A1[3] <- A1[3]+d[2]
A1[4] <- A1[4]-a[2]

A2 <- zal4_data$Area[6:9]
A2[1] <- A2[1]+d[1]
A2[2] <- A2[2]-a[1]
A2[3] <- A2[3]+d[2]
A2[4] <- A2[4]-a[2]

A3 <- zal4_data$Area[12:17]
A3[1] <- A3[1]+d[1]
A3[2] <- A3[2]-a[1]
A3[3] <- A3[3]+d[2]
A3[4] <- A3[4]-a[2]
A3[5] <- A3[5]+d[3]
A3[6] <- A3[6]-a[3]

d4 <- c(2*(A1[2]-A1[1])/5,2*(A1[4]-A1[3])/5,(A2[2]-A2[1]),(A2[4]-A2[3]),(A3[2]-A3[1]),(A3[4]-A3[3]),(A3[6]-A3[5]))/2
D4 <- c(A1[3]-A1[1],A1[4]-A1[2],A2[3]-A2[1],A2[4]-A2[2],A3[3]-A3[1],A3[4]-A3[2],A3[5]-A3[3],A3[6]-A3[4])
sd4 <- sd(d4)
sD4 <- sd(D4)
d4m <- mean(d4)
D4m <- mean(D4)
dD4 <- d4m/D4m
sdD4 <- sqrt((sd4/D4m)^2+((sD4*d4m)/(D4m^2))^2) # 0.158 pm 0.010


###### ZAL5
r <- zal0_data$Raggio[1:6]
d <-pi*c( ((r[2]-r[1])^2)/4 + (r[2]-r[1])*r[1], ((r[4]-r[3])^2)/4 + (r[4]-r[3])*r[3]  , ((r[6]-r[5])^2)/4 + (r[6]-r[5])*r[5] )
a <- pi*c( r[2]^2-r[1]^2-d[1]/pi , r[4]^2-r[3]^2-d[2]/pi  , r[6]^2-r[5]^2-d[3]/pi )

A1 <- zal5_data$Area[1:4]
A1[1] <- A1[1]+d[1]
A1[2] <- A1[2]-a[1]
A1[3] <- A1[3]+d[2]
A1[4] <- A1[4]-a[2]

A2 <- zal5_data$Area[6:11]
A2[1] <- A2[1]+d[1]
A2[2] <- A2[2]-a[1]
A2[3] <- A2[3]+d[2]
A2[4] <- A2[4]-a[2]
A2[5] <- A2[5]+d[3]
A2[6] <- A2[6]-a[3]

A3 <- zal5_data$Area[13:18]
A3[1] <- A3[1]+d[1]
A3[2] <- A3[2]-a[1]
A3[3] <- A3[3]+d[2]
A3[4] <- A3[4]-a[2]
A3[5] <- A3[5]+d[3]
A3[6] <- A3[6]-a[3]

d5 <- c(2*(A1[2]-A1[1])/5,2*(A1[4]-A1[3])/5,(A2[2]-A2[1]),(A2[4]-A2[3]),(A2[6]-A2[5]),(A3[2]-A3[1]),(A3[4]-A3[3]),(A3[6]-A3[5]))/2
D5 <- c(A1[3]-A1[1],A1[4]-A1[2],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A3[3]-A3[1],A3[4]-A3[2],A3[5]-A3[3],A3[6]-A3[4])
sd5 <- sd(d5)
sD5 <- sd(D5)
d5m <- mean(d5)
D5m <- mean(D5)
dD5 <- d5m/D5m
sdD5 <- sqrt((sd5/D5m)^2+((sD5*d5m)/(D5m^2))^2) # 0.158 pm 0.010

###### ZAL6
r <- zal0_data$Raggio[1:6]
d <-pi*c( ((r[2]-r[1])^2)/4 + (r[2]-r[1])*r[1], ((r[4]-r[3])^2)/4 + (r[4]-r[3])*r[3]  , ((r[6]-r[5])^2)/4 + (r[6]-r[5])*r[5] )
a <- pi*c( r[2]^2-r[1]^2-d[1]/pi , r[4]^2-r[3]^2-d[2]/pi  , r[6]^2-r[5]^2-d[3]/pi )

A1 <- zal6_data$Area[1:4]
A1[1] <- A1[1]+d[1]
A1[2] <- A1[2]-a[1]
A1[3] <- A1[3]+d[2]
A1[4] <- A1[4]-a[2]

A2 <- zal6_data$Area[6:11]
A2[1] <- A2[1]+d[1]
A2[2] <- A2[2]-a[1]
A2[3] <- A2[3]+d[2]
A2[4] <- A2[4]-a[2]
A2[5] <- A2[5]+d[3]
A2[6] <- A2[6]-a[3]

A3 <- zal6_data$Area[13:18]
A3[1] <- A3[1]+d[1]
A3[2] <- A3[2]-a[1]
A3[3] <- A3[3]+d[2]
A3[4] <- A3[4]-a[2]
A3[5] <- A3[5]+d[3]
A3[6] <- A3[6]-a[3]

d6 <- c(2*(A1[2]-A1[1])/5,2*(A1[4]-A1[3])/5,(A2[2]-A2[1]),(A2[4]-A2[3]),(A2[6]-A2[5]),(A3[2]-A3[1]),(A3[4]-A3[3]),(A3[6]-A3[5]))/2
D6 <- c(A1[3]-A1[1],A1[4]-A1[2],A2[3]-A2[1],A2[4]-A2[2],A2[5]-A2[3],A2[6]-A2[4],A3[3]-A3[1],A3[4]-A3[2],A3[5]-A3[3],A3[6]-A3[4])
sd6 <- sd(d6)
sD6 <- sd(D6)
d6m <- mean(d6)
D6m <- mean(D6)
dD6 <- d6m/D6m
sdD6 <- sqrt((sd6/D6m)^2+((sD6*d6m)/(D6m^2))^2) # 0.158 pm 0.010
