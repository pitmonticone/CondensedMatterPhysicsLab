library(knitr)
library(kableExtra)
library(ggplot2)
library(tikzDevice)

I <- c(0) 
x <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
B <- c(0.248,0.278,0.285,0.285,0.285, 0.278,0.247)
dbx <- data.frame(x,B)
ggplot(dbx,aes(x,B))+geom_point()+labs(title="B(x)", x="x [mm]", y="B [T]")
y <-  c(-10.225,-7.1,-4.15, 0, 4.15,7.1,10.225)
B <- c(0.262,0.279,0.282,0.282,0.283,0.285,0.233)
ggplot(data.frame(y,B),aes(y,B))+geom_point()+labs(x="x [mm]", y="B [T]")


Ddis <- data.frame(I,X,BX,Y,BY)
kable(Ddis, "latex", booktabs = T) %>% kable_styling(latex_options = c("striped", "hold_position","scale_down"))
