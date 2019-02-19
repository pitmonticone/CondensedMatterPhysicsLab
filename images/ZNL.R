library(readr)
library(readxl)
library(gdata)

################### ZNL [PIETRO] ######################

###### ZNL1

znl1_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_normale [PIT]/data_LN/znl1_data.xls", 
                          col_types = c("text", "blank", "blank", 
                                        "blank", "numeric", "blank", "blank",
                                        "blank", "blank", "blank", "blank",
                                        "blank", "blank"))
#View(znl1_data)

znl2_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_normale [PIT]/data_LN/znl2_data.xls", 
                        col_types = c("text", "blank", "blank", 
                                      "blank", "numeric", "blank", "blank",
                                      "blank", "blank", "blank", "blank",
                                      "blank", "blank"))
#View(znl2_data)

znl3_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_normale [PIT]/data_LN/znl3_data.xls", 
                        col_types = c("text", "blank", "blank", 
                                      "blank", "numeric", "blank", "blank",
                                      "blank", "blank", "blank", "blank",
                                      "blank", "blank"))
#View(znl3_data)

znl4_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_normale [PIT]/data_LN/znl4_data.xls", 
                        col_types = c("text", "blank", "blank", 
                                      "blank", "numeric", "blank", "blank",
                                      "blank", "blank", "blank", "blank",
                                      "blank", "blank"))
#View(znl4_data)

znl5_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_normale [PIT]/data_LN/znl5_data.xls", 
                        col_types = c("text", "blank", "blank", 
                                      "blank", "numeric", "blank", "blank",
                                      "blank", "blank", "blank", "blank",
                                      "blank", "blank"))
#View(znl5_data)

znl6_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_normale [PIT]/data_LN/znl6_data.xls", 
                        col_types = c("text", "blank", "blank", 
                                      "blank", "numeric", "blank", "blank",
                                      "blank", "blank", "blank", "blank",
                                      "blank", "blank"))
#View(znl6_data)

znl7_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_normale [PIT]/data_LN/znl7_data.xls", 
                        col_types = c("text", "blank", "blank", 
                                      "blank", "numeric", "blank", "blank",
                                      "blank", "blank", "blank", "blank",
                                      "blank", "blank"))
#View(znl7_data)

znl8_data <- read_excel("~/github/Lab-Struttura/images/longitudinale_normale [PIT]/data_LN/znl8_data.xls", 
                        col_types = c("text", "blank", "blank", 
                                      "blank", "numeric", "blank", "blank",
                                      "blank", "blank", "blank", "blank",
                                      "blank", "blank"))
#View(znl8_data)


A1 <- znl1_data$Area[1:6] #come A2 ric
A2 <- znl1_data$Area[7:9] # A3 ric
A3 <- znl1_data$Area[10:13] # A3 ric
d <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5])/2
sd <- sd(d)/(sqrt(length(d)))
D <- c(A1[3]-A1[1],A1[4]-A1[2],A1[5]-A1[3],A1[6]-A1[4],A2[2]-A2[1],A2[3]-A2[2],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3])
sD <- sd(D)/(sqrt(length(D)))
dm <- mean(d)
Dm <- mean(D)
dD <- dm/Dm
sdD <- sqrt((sd/Dm)^2+((sD*dm)/(Dm^2))^2) # 0.070 pm 0.002


###### ZNL2

A1 <- znl2_data$Area[1:6] #come A2 ric
A2 <- znl2_data$Area[7:9] # A3 ric
A3 <- znl2_data$Area[10:13] # A3 ric
d <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5])/2
sd <- sd(d)/(sqrt(length(d)))
D <- c(A1[3]-A1[1],A1[4]-A1[2],A1[5]-A1[3],A1[6]-A1[4],A2[2]-A2[1],A2[3]-A2[2],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3])
sD <- sd(D)/(sqrt(length(D)))
dm <- mean(d)
Dm <- mean(D)
dD <- dm/Dm
sdD <- sqrt((sd/Dm)^2+((sD*dm)/(Dm^2))^2) # 0.140 pm 0.003

###### ZNL3

A1 <- znl3_data$Area[1:6] #come A2 ric
A2 <- znl3_data$Area[7:9] # A3 ric
A3 <- znl3_data$Area[10:13] # A3 ric
d <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5])/2
sd <- sd(d)/(sqrt(length(d)))
D <- c(A1[3]-A1[1],A1[4]-A1[2],A1[5]-A1[3],A1[6]-A1[4],A2[2]-A2[1],A2[3]-A2[2],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3])
sD <- sd(D)/(sqrt(length(D)))
dm <- mean(d)
Dm <- mean(D)
dD <- dm/Dm
sdD <- sqrt((sd/Dm)^2+((sD*dm)/(Dm^2))^2) # 0.165 pm 0.002

###### ZNL4

A1 <- znl4_data$Area[1:6] #come A2 ric
A2 <- znl4_data$Area[7:9] # A3 ric
A3 <- znl4_data$Area[10:13] # A3 ric
d <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5])/2
sd <- sd(d)(sqrt(length(d)))
D <- c(A1[3]-A1[1],A1[4]-A1[2],A1[5]-A1[3],A1[6]-A1[4],A2[2]-A2[1],A2[3]-A2[2],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3])
sD <- sd(D)(sqrt(length(D)))
dm <- mean(d)
Dm <- mean(D)
dD <- dm/Dm
sdD <- sqrt((sd/Dm)^2+((sD*dm)/(Dm^2))^2) # 0.192 pm 0.002

###### ZNL5

A1 <- znl5_data$Area[1:6] #come A2 ric
A2 <- znl5_data$Area[7:9] # A3 ric
A3 <- znl5_data$Area[10:13] # A3 ric
d <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5])/2
sd <- sd(d)/(sqrt(length(d)))
D <- c(A1[3]-A1[1],A1[4]-A1[2],A1[5]-A1[3],A1[6]-A1[4],A2[2]-A2[1],A2[3]-A2[2],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3])
sD <- sd(D)/(sqrt(length(D)))
dm <- mean(d)
Dm <- mean(D)
dD <- dm/Dm
sdD <- sqrt((sd/Dm)^2+((sD*dm)/(Dm^2))^2) # 0.218 pm 0.002


###### ZNL6

A1 <- znl6_data$Area[1:6] #come A2 ric
A2 <- znl6_data$Area[7:9] # A3 ric
A3 <- znl6_data$Area[10:13] # A3 ric
d <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5])/2
sd <- sd(d)/(sqrt(length(d)))
D <- c(A1[3]-A1[1],A1[4]-A1[2],A1[5]-A1[3],A1[6]-A1[4],A2[2]-A2[1],A2[3]-A2[2],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3])
sD <- sd(D)/(sqrt(length(D)))
dm <- mean(d)
Dm <- mean(D)
dD <- dm/Dm
sdD <- sqrt((sd/Dm)^2+((sD*dm)/(Dm^2))^2) # 0.202 pm 0.010

###### ZNL7

A1 <- znl7_data$Area[1:6] #come A2 ric
A2 <- znl7_data$Area[7:9] # A3 ric
A3 <- znl7_data$Area[10:13] # A3 ric
d <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5])/2
sd <- sd(d)/(sqrt(length(d)))
D <- c(A1[3]-A1[1],A1[4]-A1[2],A1[5]-A1[3],A1[6]-A1[4],A2[2]-A2[1],A2[3]-A2[2],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3])
sD <- sd(D)/(sqrt(length(D)))
dm <- mean(d)
Dm <- mean(D)
dD <- dm/Dm
sdD <- sqrt((sd/Dm)^2+((sD*dm)/(Dm^2))^2) # 0.237 pm 0.006

###### ZNL8

A1 <- znl8_data$Area[1:6] #come A2 ric
A2 <- znl8_data$Area[7:9] # A3 ric
A3 <- znl8_data$Area[10:13] # A3 ric
d <- c(A1[2]-A1[1],A1[4]-A1[3],A1[6]-A1[5])/2
sd <- sd(d)/(sqrt(length(d)))
D <- c(A1[3]-A1[1],A1[4]-A1[2],A1[5]-A1[3],A1[6]-A1[4],A2[2]-A2[1],A2[3]-A2[2],A3[2]-A3[1],A3[3]-A3[2],A3[4]-A3[3])
sD <- sd(D)/(sqrt(length(D)))
dm <- mean(d)
Dm <- mean(D)
dD <- dm/Dm
sdD <- sqrt((sd/Dm)^2+((sD*dm)/(Dm^2))^2) # 0.263 pm 0.009



# BOHR MAGNETON
h <- 6.626070040*(10^(-34))
c <- 299792458
mn <- 1.4560
t <- 0.003
q <- 0.424
errq <- 0.009
mB <- (q*h*c)/(2*mn*t)
errmB <- errq*(h*c)/(2*mn*t) #  9.641175e-24 pm 0.2046476e-24



