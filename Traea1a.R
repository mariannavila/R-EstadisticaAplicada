library(ggplot2)
library(plyr)

library(dplyr)

library(tidyr)
library(scales)
library(DT)



data("HairEyeColor")
#View(HairEyeColor)
dim(HairEyeColor)
class(HairEyeColor)


#HAIREYECOLOR
data(HairEyeColor)
help(HairEyeColor)
summary(HairEyeColor)
HairEyeColor
HEC<-HairEyeColor
HEC
#INCISO A
class(HEC)
dim(HEC)

#INCISO B
apply(HEC,1,sum)
HEC<-apply(HEC,1,sum)

#INCISO C
pie(HEC,col = rainbow(12))
title("Color de cabello")
legend(x=1.15,y=1.15,legend=c("108","286","71","127"),fill=c("red","orange","yellow","green"),cex=0.8,text.font=4)











