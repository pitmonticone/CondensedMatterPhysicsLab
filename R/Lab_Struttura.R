##############  LAB STRUTTURA 1 #######################

# CORRENTE (PEAKTECH 3335 DMM )
I1 <- c(0,1.02,2.03,3.00,4.04,4.99,6.02,7.00,8.02,8.48,9.01,9.51,10.04)   # A 
I2 <- c(9.50,8.97,8.49,7.00,6.05,4.98,4.02,3.00,2.00,1.00,0.00)
I3 <- c(0.99,1.97,3.03,4.02,5.04,6.00,7.02,8.06,8.46,9.02,9.57,10.06)
I4 <- c(9.57,9.00,8.52, 8.03, 6.94, 6.02, 5.03, 3.96, 3.06,2.06,1.09,0.00 )
errI <- c(0.02) #sensitivity > fluctuation


# CAMPO MAGNETICO (5180 FW SELL)
B1 <- c(0,0.071,0.140,0.205,0.277,0.342,0.408,0.471,0.535,0.563,0.588,0.612,0.636)  #T
B2 <- c(0.613,0.590,0.566,0.478,0.416,0.348,0.282,0.215,0.145,0.078,0.009)
B3 <- c(0.072,0.138,0.208,0.276,0.345,0.409,0.475, 0.540,0.561,0.590,0.616,0.637)
B4 <- c(0.618,0.592,0.567, 0.542, 0.474, 0.414, 0.350, 0.277, 0.216,0.150,0.081,0.008)
errB <- c(0.001)

dIB1 <- data.frame(I1,errI,B1,errB)
kable(dIB1, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

dIB2 <- data.frame(I2,errI, B2, errB)
kable(dIB2, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

dIB3 <- data.frame(I3,errI, B3, errB)
kable(dIB3, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

dIB4 <- data.frame(I4,errI, B4, errB)
kable(dIB2, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

plot1 <- plot(I1,B1)
plot2 <- plot(I2,B2)
plot3 <- plot(I3,B3)
plot2 <- plot(I4,B4)

linearMod <- lm(B2 ~ alpha, data=data.frame(I2,B2))  # build linear regression model on full data
print(linearMod)


I5 <- c(4.20)
I6 <- c(4.13) # scesa a 4.07
Bu6 <- c(0.283,0.285,0.233)
Bd6 <- c(0.282,0.279,0.262)
Bc6 <- c(0.282)
dx <- c(2.95, 3.125)   #mm
errdx <- c(0.035,0.035) #mm

BX <- c(0.248,0.278,0.285,0.285,0.285, 0.278,0.247)
BY <- c(0.262,0.279,0.282,0.282,0.283,0.285,0.233)
X <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
Y <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
plotBX <- plot(X,BX)
plotBY <- plot(Y,BY)

ggplot(data.frame(X,BX),aes(x,B))+geom_point()
ggplot(data.frame(Y,BY))

v_lr <- 0.007
v_ud <- max(BY)-min(BY)
v <- sum(v_lr,v_ud)/2


library(knitr)
library(kableExtra)
library(ggplot2)

I <- c(0) 
X <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
BX <- c(0.248,0.278,0.285,0.285,0.285, 0.278,0.247)
ggplot(data.frame(X,BX),aes(X,BX))+geom_point()
Y <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
BY <- c(0.262,0.279,0.282,0.282,0.283,0.285,0.233)
ggplot(data.frame(Y,BY),aes(Y,BY))+geom_point()


Ddis <- data.frame(I,X,BX,Y,BY)
kable(Ddis, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))




######################################## ZEEMAN NORMALE #################################
########## TRANSVERSE GEOMETRY
# POLARIZATION: Exposure= 5000 ; Gain= 7   [zna=verticale=pi, znb=orizzontale==sigma]
# NO POLARIZATION:  Exposure= 4000 ; Gain= 6 [zn]
# SOFTWARE : Motic Image Plus 3.0

It0 <- c(0.00)
It1 <- mean(c(5.82,5.85))
It2 <- mean(c(6.83,6.83))
It3 <- mean(c(7.90,7.93))
It4 <- mean(c(8.90,8.92))
It5 <- mean(c(9.91,9.93))
It6 <- mean(c(7.20,7.25))
It7 <- mean(c(4.91,4.93))
It8 <- mean(c(2.60,2.62))

#GEOMETRIC PARAMETERS : radius (r,micron), diameter (d=2r), area (A,micron^2)
C01 <- c(69.31,15091.12)
C02 <- c(96.43,29210.04)
C03 <- c(117.98,43725.06)
C04 <- c(136.68,58692.26)
C05 <- c(153.62,74139.89)

########## LONGITUDINAL GEOMETRY
# POLARIZATION: Exposure= 9000 ; Gain= 55   [znla=verticale=sigma+, znlb=orizzontale=sigma-]
# NO POLARIZATION:  Exposure= 8000 ; Gain= 40 [znl]

Il0 <- c(0.00)
Il1 <- mean(c(2.67,2.70))
Il2 <- mean(c(4.82,4.86))
Il3 <- mean(c(5.89,5.92))
Il4 <- mean(c(6.90,6.96))
Il5 <- mean(c(7.85,7.86))
Il6 <- mean(c(7.22,7.23))
Il7 <- mean(c(8.82,8.83))
Il8 <- mean(c(9.82,9.78))


# PLOT delta/Delta \propto vs B 
# stima errore con media e deviazione std della distribuzione

#media pesata tra trasversale e longitudinale 


######################################## ZEEMAN NORMALE #################################
########## TRANSVERSE GEOMETRY
# POLARIZATION: Exposure= 5000 ; Gain= 7   [zna=verticale=pi, znb=orizzontale==sigma]
# NO POLARIZATION:  Exposure= 4000 ; Gain= 6 [zn]
# SOFTWARE : Motic Image Plus 3.0

It0 <- c(0.00)
It1 <- mean(c(5.82,5.85))
It2 <- mean(c(6.83,6.83))
It3 <- mean(c(7.90,7.93))
It4 <- mean(c(8.90,8.92))
It5 <- mean(c(9.91,9.93))
It6 <- mean(c(7.20,7.25))
It7 <- mean(c(4.91,4.93))
It8 <- mean(c(2.60,2.62))

#GEOMETRIC PARAMETERS : radius (r,micron), diameter (d=2r), area (A,micron^2)
C01 <- c(69.31,15091.12)
C02 <- c(96.43,29210.04)
C03 <- c(117.98,43725.06)
C04 <- c(136.68,58692.26)
C05 <- c(153.62,74139.89)

########## LONGITUDINAL GEOMETRY
# POLARIZATION: Exposure= 9000 ; Gain= 55   [znla=verticale=sigma+, znlb=orizzontale=sigma-]
# NO POLARIZATION:  Exposure= 8000 ; Gain= 40 [znl]

Il0 <- c(0.00)
Il1 <- mean(c(2.67,2.70))
Il2 <- mean(c(4.82,4.86))
Il3 <- mean(c(5.89,5.92))
Il4 <- mean(c(6.90,6.96))
Il5 <- mean(c(7.85,7.86))
Il6 <- mean(c(7.22,7.23))
Il7 <- mean(c(8.82,8.83))
Il8 <- mean(c(9.82,9.78))


# PLOT delta/Delta \propto vs B 
# stima errore con media e deviazione std della distribuzione

#media pesata tra trasversale e longitudinale 



######################################## ZEEMAN NORMALE #################################
########## TRANSVERSE GEOMETRY
# POLARIZATION: Exposure= 5000 ; Gain= 7   [zna=verticale=pi, znb=orizzontale==sigma]
# NO POLARIZATION:  Exposure= 4000 ; Gain= 6 [zn]
# SOFTWARE : Motic Image Plus 3.0

It0 <- c(0.00)
It1 <- mean(c(5.82,5.85))
It2 <- mean(c(6.83,6.83))
It3 <- mean(c(7.90,7.93))
It4 <- mean(c(8.90,8.92))
It5 <- mean(c(9.91,9.93))
It6 <- mean(c(7.20,7.25))
It7 <- mean(c(4.91,4.93))
It8 <- mean(c(2.60,2.62))

#GEOMETRIC PARAMETERS : radius (r,micron), diameter (d=2r), area (A,micron^2)
C01 <- c(69.31,15091.12)
C02 <- c(96.43,29210.04)
C03 <- c(117.98,43725.06)
C04 <- c(136.68,58692.26)
C05 <- c(153.62,74139.89)

########## LONGITUDINAL GEOMETRY
# POLARIZATION: Exposure= 9000 ; Gain= 55   [znla=verticale=sigma+, znlb=orizzontale=sigma-]
# NO POLARIZATION:  Exposure= 8000 ; Gain= 40 [znl]

Il0 <- c(0.00)
Il1 <- mean(c(2.67,2.70))
Il2 <- mean(c(4.82,4.86))
Il3 <- mean(c(5.89,5.92))
Il4 <- mean(c(6.90,6.96))
Il5 <- mean(c(7.85,7.86))
Il6 <- mean(c(7.22,7.23))
Il7 <- mean(c(8.82,8.83))
Il8 <- mean(c(9.82,9.78))


# PLOT delta/Delta \propto vs B 
# stima errore con media e deviazione std della distribuzione

#media pesata tra trasversale e longitudinale 


######################################## ZEEMAN ANOMALO #################################
########## TRANSVERSE GEOMETRY
# POLARIZATION: Exposure= 7000 ; Gain= -57; Enhance =62/255;    [zaa=verticale=pi, zab=orizzontale==sigma]
# NO POLARIZATION:  Exposure= 5000 ; Gain= -2; Enhance=48/255   [za]
# SOFTWARE : Motic Image Plus 3.0

It0 <- c(0.00)
It1 <- mean(c(5.71,5.79))
It2 <- mean(c(6.73,6.75))
It3 <- mean(c(7.81,7.83))
It4 <- mean(c(8.70,8.89))
It5 <- mean(c(9.91,9.99))
It6 <- mean(c(7.25,7.30))


#GEOMETRIC PARAMETERS : radius (r,micron), diameter (d=2r), area (A,micron^2)

########## LONGITUDINAL GEOMETRY
# POLARIZATION: Exposure= 9000 ; Gain= 55   [znla=verticale=sigma+, znlb=orizzontale=sigma-]
# NO POLARIZATION:  Exposure= 8000 ; Gain= 40 [znl]

Il0 <- c(0.00)
Il1 <- mean(c(7.30,7.31))
Il2 <- mean(c(7.84,7.85))
Il3 <- mean(c(8.30,8.33))
Il4 <- mean(c(8.83,8.85))
Il5 <- mean(c(9.39,9.44))
Il6 <- mean(c(9.98, 9.99))



# PLOT delta/Delta \propto vs B 
# stima errore con media e deviazione std della distribuzione

#media pesata tra trasversale e longitudinale 



################################## DATA ANALYSIS ###################################
h <- 6.626070040*(10^(-34))
c <- 299792458
mn <- 1.4560
ma <- 1.4519
t <- 0.003

# NORMAL COMPATIBILITY TEST 
p <- 0.436
errp <- 0.040
Mnt <- (p*h*c)/(2*mn*t)
dMnt <- errp*(h*c)/(2*mn*t)   # 9.914038e-24 pm  0.9095448e-24
q <- 0.425
errq <- 0.039
Mnl <- (q*h*c)/(2*mn*t)
dMnl <- errq*(h*c)/(2*mn*t) #  9.663913e-24 pm 0.8868062e-24

Z_n <- (Mnt-Mnl)/sqrt(dMnt^2+dMnl^2) # 0.1969

# NORMAL WEIGHTED AVG 

Mn <- c( ((Mnl)/((dMnl)^2) + (Mnt)/((dMnt)^2)) /((1/dMnl^2)+(1/dMnt^2))  ,   sqrt((1)/((1/dMnt^2) + (1/dMnl^2)))) # 9.785810e-24 0.6349533e-24
# NORMAL-THEORETICAL COMPATIBILITY

ZnT <- (9.27*10^(-24)-Mn[1])/(Mn[2]) # -0.4060495

# ANOMALOUS COMPATIBILITY TEST 
h <- 6.626070040*(10^(-34))
c <- 299792458
ma <- 1.4519
t <- 0.003

# ZAT
b <- 0.070
errb <- 0.171
Matb <- (b*h*c)/(ma*t)
dMatb <- errb*(h*c)/(ma*t)  ## 3.192396e-24 pm 7.798568e-24

# ZAL
k <- 0.737
errk <- 0.433
Malk <- (k*h*c)/(ma*t)
dMalk <- errk*(h*c)/(ma*t) # 33.61137e-24 pm 19.74725e-24

Z_a <- (Matb-Malk)/sqrt(dMatb^2+dMalk^2) # -1.432736

Ma <- c( ((Malk)/((dMalk)^2) + (Matb)/((dMatb)^2)) /((1/dMalk^2)+(1/dMatb^2))  ,    sqrt((1)/((1/dMatb^2) + (1/dMalk^2)))) # 7.296495e-24 pm 7.253427e-24

M <- c( ((Ma[1])/((Ma[2])^2) + (Mn[1])/((Mn[2])^2)) /((1/Ma[2]^2)+(1/Mn[2]^2))  ,   sqrt((1)/((1/Ma[2]^2) + (1/Mn[2]^2)))) # 9.766880e-24 0.6325344e-24
Z <- (9.27*10^(-24)-M[1])/M[2] # -0.7855378







# ZATBS
i <- 0.202
erri <- 0.057
Mat <- (i*h*c)/(ma*t)
dMat <- erri*(h*c)/(ma*t)  ## 9.212344e-24 pm 2.599523e-24

# ZAL
k <- 0.737
errk <- 0.433
Malk <- (k*h*c)/(ma*t)
dMalk <- errk*(h*c)/(ma*t) # 33.61137e-24 pm 19.74725e-24

#ZAL_APPROX
j <- 0.130/0.644
errnum <- 0.022
errden <- 0.067
errj <- sqrt((errnum/(0.644))^2+(0.130*errden/(0.644^2))^2)
Mal <- (j*h*c)/(ma*t)
dMal <- errj*(h*c)/(ma*t) # 9.206112e-24 pm 1.828817e-24


Z_a <- (Mat-Mal)/sqrt(dMat^2+dMal^2) # -0.002

# ANOMALOUS AVG 
Ma <- c( ((Mal)/((dMal)^2) + (Mat)/((dMat)^2)) /((1/dMal^2)+(1/dMat^2))  ,   sqrt((1)/((1/dMat^2) + (1/dMal^2)))) # 9.208175e-24 pm 1.495747e-24

# ANOMALOUS AVG - THEORETICAL COMPATIBILITY

ZaT <- (9.27*10^(-24)-Ma[1])/(Ma[2]) # 0.0194517

#ANOMALOUS - NORMAL COMPATIBILITY TEST

Z_an <- (Ma[1]-Mn[1])/sqrt(Ma[2]^2+Mn[2]^2) # -0.1687593

#GLOBAL AVG 

M <- c( ((Ma[1])/((Ma[2])^2) + (Mn[1])/((Mn[2])^2)) /((1/Ma[2]^2)+(1/Mn[2]^2))  ,   sqrt(Ma[2]^2 + Mn[2]^2)) # 9.706248e-24 pm 3.422833e-24

# GLOBAL COMPATIBILITY TEST

Z <- (9.27*10^(-24)-M[1])/M[2] # -0.1274525




# DATA TABLE ZNT

I <- c(2.61,4.92,5.835,6.83,7.225,7.915,8.91,9.92) 
sI <- c(0.02,0.02,0.02,0.02,0.025,0.020,0.02,0.02)
B <-  c(0.182,0.338,0.400,0.466,0.500,0.543,0.596,0.641)  
sB <- c(0.002,0.002,0.002,0.002,0.009,0.009,0.010,0.011)
dDm <- c(0.061,0.125,0.158,0.188,0.198,0.221,0.233,0.263)
sdDm <- c(0.004,0.007,0.003,0.006,0.006,0.004,0.006,0.009)
dZNT <- data.frame(I,sI,B,sB,dDm,sdDm)
kable(dZNT, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

# DATA TABLE ZNL

I <- c(2.685,4.84,5.905,6.93,7.225,7.855,8.825,9.8) 
sI <- c(0.02,0.02,0.02,0.03,0.02,0.020,0.02,0.02)
B <-  c(0.188,0.333,0.404,0.473,0.500,0.539,0.592,0.636)  
sB <- c(0.002,0.002,0.002,0.002,0.009,0.009,0.010,0.011)
dDm <- c(0.070,0.14,0.165,0.192,0.202,0.218,0.237,0.263)
sdDm <- c(0.002,0.003,0.002,0.002,0.010,0.002,0.006,0.009)
dZNL <- data.frame(I,sI,B,sB,dDm,sdDm)
kable(dZNL, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

# DATA TABLE ZAL

I <- c(7.305,7.845,8.315,8.84,9.415,9.985) 
sI <- c(0.02,0.02,0.02,0.02,0.025,0.02)
B <-  c(0.188,0.333,0.404,0.473,0.500,0.539)  
sB <- c(0.009,0.009,0.010,0.010,0.011,0.011)
dDm <- c(0.057,0.069,0.088,0.128,0.147,0.130)
sdDm <- c(0.011,0.012,0.017,0.031,0.022,0.022)
dZAL <- data.frame(I,sI,B,sB,dDm,sdDm)
kable(dZAL, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

# DATA TABLE ZAT

I <- c(5.75,6.74,7.275,7.82,8.795,9.95) 
sI <- c(0.04,0.02,0.025,0.02,0.095,0.04)
B <-  c(0.394,0.460,0.504,0.537,0.590,0.642)  
sB <- c(0.003,0.002,0.009,0.009,0.011,0.011)
dDm <- c(0.061,0.081,0.082,0.085,0.105,0.108)
sdDm <- c(0.002,0.005,0.008,0.003,0.005,0.007)
dZAT <- data.frame(I,sI,B,sB,dDm,sdDm)
kable(dZAT, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

