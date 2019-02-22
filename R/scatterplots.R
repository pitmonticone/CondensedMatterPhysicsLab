library(knitr)
library(kableExtra)
library(ggplot2)
library(tikzDevice)
tikz("./MagneticDishomogeneity2.tex", width = 6, height = 4)
I <- c(0) 
x <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
dx <- 0.05
Bx <- c(0.248,0.278,0.285,0.285,0.285, 0.278,0.247)
dB <- 0.001
ggplot(data.frame(x,Bx),aes(x,Bx))+theme_classic()+geom_point()+labs(title="$B(x)$", x="$x$ [mm]", y="$B\\;[T]$")+geom_errorbar(aes(ymin = Bx-dB, ymax = Bx+dB), width = 0.2)+geom_errorbarh(aes(xmin = x-dx, xmax = x+dx), width = 0.2)
y <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
dy <- 0.05
By <- c(0.262,0.279,0.282,0.282,0.283,0.285,0.233)
dB <- 0.001
ggplot(data.frame(y,By),aes(y,By))+theme_classic()+geom_point()+labs(title="$B(y)$", x="$y$ [mm]", y="$B\\;[T]$")+geom_errorbar(aes(ymin = By-dB, ymax = By+dB), width = 0.2)+geom_errorbarh(aes(xmin = y-dy, xmax = y+dy), width = 0.2)
y <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
dev.off()

Ddis <- data.frame(I,X,BX,Y,BY)
kable(Ddis, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))
