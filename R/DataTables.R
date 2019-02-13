######################### TABLES #############################################
# e(mV)_FAL

tA <- c(150)  # sec
DiscA <- c(100, 200, 300, 400,500) # mV
NtA <- c(352, 212, 96, 31,9)
NdA <- c(360, 326, 362, 311, 336)
NsA <- c(5685, 1719, 653, 241, 104)

dFAL <- data.frame(tA, DiscA,NtA,NdA, NsA)
kable(dFAL, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

# e(mV)_FGL

tG <- c(150)  # sec
DiscG <- c(100.3, 200, 300, 400,500) # mV
NtG <- c(348, 272, 157, 71,39)
NdG <- c(358, 361, 370, 378, 376)
NsG <- c(7321, 2182, 983, 527, 336)

dFGL <- data.frame(tG, DiscG,NtG,NdG, NsG)
kable(dFGL, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))


# e(V)_FAL
t <- c(200)  # sec
HVA <- c(1900,1925,1940,1950,2000,2050,2100) # V
errHVA <- c(4)
NtAA <- c(51,63,151,232,332,308,344)
NdAA <- c(446,333,329,299,341,316,353)
NsAA <- c(478,599,1134,1796,4825,9567,15768)
errNsAA <- sqrt(NsAA)
RsAA <- c(NsAA[1]/t[1], NsAA[2]/t[1],NsAA[3]/t[1], NsAA[4]/t[1],NsAA[5]/t[1],NsAA[6]/t[1],NsAA[7]/t[1])
errRsAA <- c(errNsAA[1]/t[1],errNsAA[2]/t[1],errNsAA[3]/t[1],errNsAA[4]/t[1],errNsAA[5]/t[1],errNsAA[6]/t[1],errNsAA[7]/t[1])

dtFAL <- data.frame(t, HVA,errHVA,NtAA,NdAA, NsAA)
kable(dtFAL, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))



# e(V)_FGL

t <- c(200)  # sec
HVG <- c(1400,1450,1500,1550,1600,1650,1700,1750,1800) # V
errHVG <- c(4,3,4,3,4,4,3,3,3)
NtGG <- c(19,65,148,248,373,445,442,436,464)
NdGG <- c(421,456,448,432,456,482,461,447,468)
NsGG <- c(219,546,1077,1863,3348,5349,8127,12173,17924)
errNsGG <- sqrt(NsGG)
RsGG <- c(NsGG[1]/t[1], NsGG[2]/t[1],NsGG[3]/t[1], NsGG[4]/t[1],NsGG[5]/t[1],NsGG[6]/t[1],NsGG[7]/t[1],NsGG[8]/t[1],NsGG[9]/t[1])
errRsGG <- c(errNsGG[1]/t[1],errNsGG[2]/t[1],errNsGG[3]/t[1],errNsGG[4]/t[1],errNsGG[5]/t[1],errNsGG[6]/t[1],errNsGG[7]/t[1],errNsGG[8]/t[1],errNsGG[9]/t[1])


dtFGL <- data.frame(t, HVG,errHVG,NtGG,NdGG, NsGG)
kable(dtFGL, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

# R(u) @ rooftop

theta_deg <- c(0,15,30,45,60)
Dtheta_deg <- c(5)
theta_rad <- c(0,pi/12,pi/6,pi/4,pi/3)
Dtheta_rad <- c(pi/36)
N_roof <- c(250,100,150,100,50)
DN_roof <- sqrt(N_roof)
t_roof <- c(1093,450.6,870.2,793.7,826.7)
Dt_roof <-c(0.1,0.1,0.1,0.1,0.1)
Ru <- c(N_roof[1]/t_roof[1],N_roof[2]/t_roof[2],N_roof[3]/t_roof[3],N_roof[4]/t_roof[4],N_roof[5]/t_roof[5])
DRu <- c(sqrt((DN_roof[1]/t_roof[1])^2+((Dt_roof[1]*N_roof[1])/(t_roof[1]^2))^2), sqrt((DN_roof[2]/t_roof[2])^2+((Dt_roof[2]*N_roof[2])/(t_roof[2]^2))^2), sqrt((DN_roof[3]/t_roof[3])^2+((Dt_roof[3]*N_roof[3])/(t_roof[3]^2))^2), sqrt((DN_roof[4]/t_roof[4])^2+((Dt_roof[4]*N_roof[4])/(t_roof[4]^2))^2), sqrt((DN_roof[5]/t_roof[5])^2+((Dt_roof[5]*N_roof[5])/(t_roof[5]^2))^2))
dangle <- data.frame(theta_rad,Dtheta_rad,N_roof, DN_roof,t_roof,Dt_roof)
kable(dangle, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))


# R(d) @ rooftop , @theta=0 
# Il grafico corrispondente rappresenta G(d)/G(d_0), con test Z tra G_sp/Gsp vs. G_sim/G_sim
# TENERE CONTO DI ERRORI SISTEMATICI che spiegano l'errore trascurabile!!!

d <- c(10.2,12.3,29.3,31.6,40)  # cm 
Dd <- c(0.1)   # cm
Nd <- c(201,150,250,100,100)
DNd <- sqrt(Nd)
td <- c(180.4, 185.1,1093.0,446.7,608.9)  # sec
Dtd <-c(0.1,0.1,0.1,0.1,0.1)
Rd <- c(Nd[1]/td[1],Nd[2]/td[2],Nd[3]/td[3],Nd[4]/td[4],Nd[5]/td[5])
DRd <- c(sqrt((DNd[1]/td[1])^2+((Dtd[1]*Nd[1])/(td[1]^2))^2), sqrt((DNd[2]/td[2])^2+((Dtd[2]*Nd[2])/(td[2]^2))^2), sqrt((DNd[3]/td[3])^2+((Dtd[3]*Nd[3])/(td[3]^2))^2), sqrt((DNd[4]/td[4])^2+((Dtd[4]*Nd[4])/(td[4]^2))^2), sqrt((DNd[5]/td[5])^2+((Dtd[5]*Nd[5])/(td[5]^2))^2))
Rd0 <- Rd[3]
DRd0 <- DRd[3]
Rdd0 <- c(Rd[1]/Rd0,Rd[2]/Rd0,Rd[3]/Rd0,Rd[4]/Rd0,Rd[5]/Rd0)
DRdd0 <- c(sqrt((DRd[1]/Rd0)^2+((DRd0*Rd[1])/(Rd0^2))^2), sqrt((DRd[2]/Rd0)^2+((DRd0*Rd[2])/(Rd0^2))^2), sqrt((DRd[3]/Rd0)^2+((DRd0*Rd[3])/(Rd0^2))^2), sqrt((DRd[4]/Rd0)^2+((DRd0*Rd[4])/(Rd0^2))^2), sqrt((DRd[5]/Rd0)^2+((DRd0*Rd[5])/(Rd0^2))^2))

dd <- data.frame(d,Dd,Nd,DNd,td,Dtd,Rd,DRd,Rdd0,DRdd0)
kable(dd, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

# R(x) d=d_0 ,  @theta=0


x <- c(0,1.39,2.32,3.02,3.7)  # m from 6 to -2
Dx <- c(0.01,0.01,0.01,0.01,0.01)
Nx <- c(100, 101, 50, 50, 100 )
DNx <- sqrt(Nx)
tx <- c(1093.0, 521.7, 325.7, 353.1, 771.0)  # sec
Dtx <-c(0.1,0.1,0.1,0.1,0.1)
Rx <- c(Nx[1]/tx[1],Nx[2]/tx[2],Nx[3]/tx[3],Nx[4]/tx[4],Nx[5]/tx[5])
DRx <- c(sqrt((DNx[1]/tx[1])^2+((Dtx[1]*Nx[1])/(tx[1]^2))^2), sqrt((DNx[2]/tx[2])^2+((Dtx[2]*Nx[2])/(tx[2]^2))^2), sqrt((DNx[3]/tx[3])^2+((Dtx[3]*Nx[3])/(tx[3]^2))^2), sqrt((DNx[4]/tx[4])^2+((Dtx[4]*Nx[4])/(tx[4]^2))^2), sqrt((DNx[5]/tx[5])^2+((Dtx[5]*Nx[5])/(tx[5]^2))^2) )

dx <- data.frame(x,Dx,Nx,DNx,tx,Dtx,Rx,DRx)
kable(dx, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

# Coincidence Curve 

t <- c(250,250,250,250,250,250,100,100,100,100,100,100,100,100,100,100,100, 100,100,100,100,100,100,100,100,100,250,250,250,250,250,250)
OR_AL <- c(21171,19481,19159,21056,21084,20788,8321,8485,8338,8316,8335,8374,8337,8191,8369,8228, 8802,8390,8170, 8429, 8635, 8508, 8231, 8282, 8295,8233,8004+12256,8270+12310,8364+12191,8378+12567,8412+8256+4229,20490)
OR_GF <- c(18973,18538,18841,18740,18868,18737,7450,7600,7216,7401,7517,7468,7476,7422,7468,7665, 7393,7414,7433, 7417, 7360, 7367, 7618, 7569, 7591,7500,7415+11179,7347+11127,7563+11519,7474+11265,7390+7450+3760,18877)
AND_ALGF <- c(1078,2221,2727,3027,3144,3166,1294,1307,1228,1259,1281,1320,1236,1244,1298,1351,1241,1332,1257,1264,1275,1335,1293,1325,1324,1268,1178+1721,934+1408,590+893,327+472,160+177+86,152)
DAND_ALGF <- sqrt(AND_ALGF)
delay <- c(-31.5,-30,-29,-28,-27,-26,-25,-24,-23,-22,-21,-20,-15,-10,-5,0,5,10,15,19,20,21,22,23,24,25,26,27,28,29,30,31.5)
Ddelay <- c(0.5)   #ns
R_ALGF <- AND_ALGF / t
DR_ALGF <- sqrt((DAND_ALGF / t)^2)

dataAND <- data.frame(OR_AL,OR_GF,AND_ALGF,DAND_ALGF,delay,Ddelay,t,R_ALGF,DR_ALGF)
kable(dataAND, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))%>% kable_styling(latex_options = c("repeat_header"))

################################### SIPM #################################

# G35i2
I_35 <- c(2,2,2,2)
G_35 <- c(35,35,35,35)
C_35 <- c(0.69,530.566,1060.02,1588.27)
N_35 <- c(34384.2,27772.0,12699.7,4167.2) # massimi
DN_35 <- sqrt(N_35)
Dpp_35 <- c(529.876,529.454,528.250) # Distanze picco-picco in ascissa
FWHM_35 <- c(59.044,69.91,83.04,81.27)
rFWHM_35 <- c(59.044,69.91,83.04,81.27)/2.355
mDpp_35 <- mean(Dpp_35)

G35i2 <- data.frame(I_35,G_35,C_35,N_35,FWHM_35,rFWHM_35)
kable(G35i2, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))


# poissonG35i2
int_35 <- c(309188,319420,178846,72816.3)
peaks <- c(0,1,2,3)

poissonG35i2 <- data.frame(int_35,peaks)
kable(poissonG35i2, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

# sigmaG35i2
varr <- rFWHM_35^2-rFWHM_35[1]^2
errsigman <- c(0.02,0.02,0.04,0.9)
err_varn <- 2*rFWHM_35*errsigman
err_varr <- sqrt(err_varn^2+err_varn[1]^2)

sigmaG35i2 <- data.frame(varr,errsigman,err_varn,err_varr)
kable(sigmaG35i2, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))


# G20i8

I_20 <- c(2.8,2.8,2.8,2.8,2.8,2.8,2.8)
G_20 <- c(20,20,20,20)
C_20 <- c(0.69,530.566,1060.02,1588.27)
N_20 <- c(2360,8546,12699.7,4167.2)
DN_20 <- sqrt(N_20)


#DCROCT30

G_30 <- c(30,30,30) #dB
V_thr30 <- c(-12,-30,-46) #mV
DCR30 <- c(54.90,1.60,0.05) #kHz
errDCR30 <- c(0.300,0.040,0.007)
OCT30 <- 0.0291
errOCT30 <- 0.0007

dcroct30 <- data.frame(G_30,V_thr30,DCR30, errDCR30,OCT30,errOCT30)
kable(dcroct30, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))


#DCROCT32

G_32 <- c(32,32,32) #dB
V_thr32 <- c(-12,-35,-60) #mV
DCR32 <- c(61.2,1.65,0.045) #kHz
errDCR32 <- c(0.3,0.04,0.007)
OCT32 <- 0.0270
errOCT32 <- 0.0008

dcroct32 <- data.frame(G_32,V_thr32,DCR32, errDCR32,OCT32,errOCT32)
kable(dcroct32, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

#DCROCT35

G_35 <- c(35,35,35) #dB
V_thr35 <- c(-12,-50,-80) #mV
DCR35 <- c(61.5,1.72,0.055) #kHz
errDCR35 <- c(0.3,0.4,0.008)
OCT35 <- 0.0280
errOCT35 <- 0.0007

dcroct35 <- data.frame(G_35,V_thr35,DCR35, errDCR35,OCT35,errOCT35)
kable(dcroct35, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

#g20i28

I28 <- 2.8
G28 <- 20
ADC28m<- c(0,99,197,293,390,487,583)
erADC28m<- 2
ADC28l <- c(-26,65,170,166,362,459,545)
erADC28l<- 2
ADC28r <- c(24,126,226,323,420,515,618)
erADC28r<- 2

g20i28 <- data.frame(I28,G28,ADC28m,erADC28m,ADC28l,erADC28l,ADC28r,erADC28r)
kable(g20i28, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))

#g35i20
I20 <- 2.0
G20 <- 35
ADC20m<- c(0,530,1060,1588)
erADC20m<- 2
ADC20l <- c(-28,498,1018,1549)
erADC20l<- 2
ADC20r <- c(31,567,1101,1631)
erADC20r<- 2

g35i20 <- data.frame(I20,G20,ADC20m,erADC20m,ADC20l,erADC20l,ADC20r,erADC20r)
kable(g35i20, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))
kable(g35i20, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down")) %>% kable_styling(latex_options = c("repeat_header"))

####################

long_dt <- rbind(mtcars, mtcars)
kable(long_dt, "latex", longtable = T, booktabs = T, caption = "Longtable") %>% add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6)) %>% kable_styling(latex_options = c("repeat_header"))
