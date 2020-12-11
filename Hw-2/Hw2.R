rm(list = ls())
library(plotly)
library(data.table)
library(chron)
library(ellipsis)
library(forecast)
library(xts)
library(zoo)
library(stats)
library(dplyr)
library(corrr)
library(corrplot)
library(tidyverse)
library(miscTools)
library("factoextra")
library("ggplot2")
setwd("C:/Users/kadir/OneDrive/Masaüstü")

trainx<-fread("uWaveGestureLibrary_X_TRAIN")
trainy<-fread("uWaveGestureLibrary_Y_TRAIN")
trainz<-fread("uWaveGestureLibrary_Z_TRAIN")


trainx<-data.frame(trainx)
trainy<-data.frame(trainy)
trainz<-data.frame(trainz)



tx<-vector(mode = "numeric", length = 282240)

for(i in 1:896){
  for(j in 1:315){
    tx[(i-1)*315+j]<-trainx[i,j+1]
    
    
  }
}

ty<-vector(mode = "numeric", length = 282240)

for(i in 1:896){
  for(j in 1:315){
    ty[(i-1)*315+j]<-trainy[i,j+1]
    
    
  }
}


tz<-vector(mode = "numeric", length = 282240)

for(i in 1:896){
  for(j in 1:315){
    tz[(i-1)*315+j]<-trainz[i,j+1]
    
    
  }
}


class<-vector(mode = "numeric", length = 282240)

for(i in 1:896){
  for(j in 1:315){
    class[(i-1)*315+j]<-trainz[i,1]
    
    
  }
}

timeindex<-vector(mode = "numeric", length = 282240)

for(i in 1:896){
  for(j in 1:315){
    timeindex[(i-1)*315+j]<-j
    
    
  }
}

timeseriesid<-vector(mode = "numeric", length = 282240)

for(i in 1:896){
  for(j in 1:315){
    timeseriesid[(i-1)*315+j]<-i
    
    
  }
}

traintable<-data.table(timeseriesid,timeindex,tx,ty,tz,class)
traintable



ttx<-t(tx)
ttx<-cumsum(ttx)


tty<-t(ty)
tty<-cumsum(tty)

ttz<-t(tz)
ttz<-cumsum(ttz)


positionx<-t(ttx)
positionx<-cumsum(positionx)


positiony<-t(tty)
positiony<-cumsum(positiony)

positionz<-t(ttz)
positionz<-cumsum(positionz)

figclass1 <- plot_ly(x = positionx[3151:3465], y =positiony[3151:3465], z =positionz[3151:3465])
figclass1 <- figclass1  %>% layout(title = 'Class1')
figclass1


figclass2 <- plot_ly(x = positionx[4411:4725], y =positiony[4411:4725], z =positionz[4411:4725])
figclass2 <- figclass2  %>% layout(title = 'Class2')
figclass2


figclass3 <- plot_ly(x = positionx[946:1260], y =positiony[946:1260], z =positionz[946:1260])
figclass3 <- figclass3  %>% layout(title = 'Class3')
figclass3


figclass4 <- plot_ly(x = positionx[1261:1575], y =positiony[1261:1575], z =positionz[1261:1575])
figclass4 <- figclass4  %>% layout(title = 'Class4')
figclass4


figclass5 <- plot_ly(x = positionx[316:630], y =positiony[316:630], z =positionz[316:630])
figclass5 <- figclass5  %>% layout(title = 'Class5')
figclass5


figclass6 <- plot_ly(x = positionx[1:315], y =positiony[1:315], z =positionz[1:315])
figclass6 <- figclass6  %>% layout(title = 'Class6')
figclass6


figclass7 <- plot_ly(x = positionx[1:315], y =positiony[1:315], z =positionz[1:315])
figclass7 <- figclass7  %>% layout(title = 'Class7')
figclass7


figclass8 <- plot_ly(x = positionx[1576:1890], y =positiony[1576:1890], z =positionz[1576:1890])
figclass8  <- figclass8  %>% layout(title = 'Class8')
figclass8


#Time series for each class is visualized above.

traintable2<-copy(traintable[,3:5])



pca1 <- princomp(traintable2, cor=T)
summary(pca1, loadings=T)


traintable3<-data.table(timeseriesid,traintable2)
pca2 <- princomp(traintable3, cor=T)
summary(pca2, loadings=T)

gesture1<-copy(filter(traintable,class==1))

gesture2<-copy(filter(traintable,class==2))

gesture3<-copy(filter(traintable,class==3))

gesture4<-copy(filter(traintable,class==4))

gesture5<-copy(filter(traintable,class==5))

gesture6<-copy(filter(traintable,class==6))

gesture7<-copy(filter(traintable,class==7))

gesture8<-copy(filter(traintable,class==8))


sample1<-copy(filter(traintable,class==1 & timeseriesid==182 ))

sample2<-copy(filter(traintable,class==1 & timeseriesid==495 ))

sample1[,pca1:=0.427*tx+0.721*ty+0.546*tz]
sample2[,pca1:=0.427*tx+0.721*ty+0.546*tz]

class1deneme<-data.table(sample1$pca1,sample2$pca1)
class1deneme[,time:=1:315]
setnames(class1deneme, old = "V1", new = "first")
setnames(class1deneme, old = "V2", new = "second")

ggplot(class1deneme, aes(time)) + 
  geom_line(aes(y = first, colour = "var0")) + 
  geom_line(aes(y =  second, colour = "var1"))+ggtitle("Two Sample from Class1")+ylab("Pca Value")


sample3<-copy(filter(traintable,class==2 & timeseriesid==170 ))

sample4<-copy(filter(traintable,class==2 & timeseriesid==323 ))

sample3[,pca1:=0.427*tx+0.721*ty+0.546*tz]
sample4[,pca1:=0.427*tx+0.721*ty+0.546*tz]

class2deneme<-data.table(sample3$pca1,sample4$pca1)
class2deneme[,time:=1:315]
setnames(class2deneme, old = "V1", new = "first")
setnames(class2deneme, old = "V2", new = "second")


ggplot(class2deneme, aes(time)) + 
  geom_line(aes(y = first, colour = "var0")) + 
  geom_line(aes(y =  second, colour = "var1"))+ggtitle("Two Sample from Class2")+ylab("Pca Value")


sample5<-copy(filter(traintable,class==3 & timeseriesid==350))

sample6<-copy(filter(traintable,class==3 & timeseriesid==892))

sample5[,pca1:=0.427*tx+0.721*ty+0.546*tz]
sample6[,pca1:=0.427*tx+0.721*ty+0.546*tz]

class3deneme<-data.table(sample5$pca1,sample6$pca1)
class3deneme[,time:=1:315]
setnames(class3deneme, old = "V1", new = "first")
setnames(class3deneme, old = "V2", new = "second")


ggplot(class3deneme, aes(time)) + 
  geom_line(aes(y = first, colour = "var0")) + 
  geom_line(aes(y =  second, colour = "var1"))+ggtitle("Two Sample from Class3")+ylab("Pca Value")

sample7<-copy(filter(traintable,class==4 & timeseriesid==317))

sample8<-copy(filter(traintable,class==4 & timeseriesid==580))

sample7[,pca1:=0.427*tx+0.721*ty+0.546*tz]
sample8[,pca1:=0.427*tx+0.721*ty+0.546*tz]

class4deneme<-data.table(sample7$pca1,sample8$pca1)
class4deneme[,time:=1:315]
setnames(class4deneme, old = "V1", new = "first")
setnames(class4deneme, old = "V2", new = "second")

ggplot(class4deneme, aes(time)) + 
  geom_line(aes(y = first, colour = "var0")) + 
  geom_line(aes(y =  second, colour = "var1"))+ggtitle("Two Sample from Class4")+ylab("Pca Value")

sample9<-copy(filter(traintable,class==5 & timeseriesid==69))

sample10<-copy(filter(traintable,class==5 & timeseriesid==332))

sample9[,pca1:=0.427*tx+0.721*ty+0.546*tz]
sample10[,pca1:=0.427*tx+0.721*ty+0.546*tz]

class5deneme<-data.table(sample9$pca1,sample10$pca1)
class5deneme[,time:=1:315]
setnames(class5deneme, old = "V1", new = "first")
setnames(class5deneme, old = "V2", new = "second")

ggplot(class5deneme, aes(time)) + 
  geom_line(aes(y = first, colour = "var0")) + 
  geom_line(aes(y =  second, colour = "var1"))+ggtitle("Two Sample from Class5")+ylab("Pca Value")

sample11<-copy(filter(traintable,class==6 & timeseriesid==207))

sample12<-copy(filter(traintable,class==6 & timeseriesid==631))

sample11[,pca1:=0.427*tx+0.721*ty+0.546*tz]
sample12[,pca1:=0.427*tx+0.721*ty+0.546*tz]

class6deneme<-data.table(sample11$pca1,sample12$pca1)
class6deneme[,time:=1:315]
setnames(class6deneme, old = "V1", new = "first")
setnames(class6deneme, old = "V2", new = "second")

ggplot(class6deneme, aes(time)) + 
  geom_line(aes(y = first, colour = "var0")) + 
  geom_line(aes(y =  second, colour = "var1"))+ggtitle("Two Sample from Class6")+ylab("Pca Value")

sample13<-copy(filter(traintable,class==7 & timeseriesid==417))

sample14<-copy(filter(traintable,class==7 & timeseriesid==613))

sample13[,pca1:=0.427*tx+0.721*ty+0.546*tz]
sample14[,pca1:=0.427*tx+0.721*ty+0.546*tz]

class7deneme<-data.table(sample13$pca1,sample14$pca1)
class7deneme[,time:=1:315]
setnames(class7deneme, old = "V1", new = "first")
setnames(class7deneme, old = "V2", new = "second")

ggplot(class7deneme, aes(time)) + 
  geom_line(aes(y = first, colour = "var0")) + 
  geom_line(aes(y =  second, colour = "var1"))+ggtitle("Two Sample from Class7")+ylab("Pca Value")

sample15<-copy(filter(traintable,class==8 & timeseriesid==6))

sample16<-copy(filter(traintable,class==8 & timeseriesid==440))

sample15[,pca1:=0.427*tx+0.721*ty+0.546*tz]
sample16[,pca1:=0.427*tx+0.721*ty+0.546*tz]

class8deneme<-data.table(sample15$pca1,sample16$pca1)
class8deneme[,time:=1:315]
setnames(class8deneme, old = "V1", new = "first")
setnames(class8deneme, old = "V2", new = "second")

ggplot(class8deneme, aes(time)) + 
  geom_line(aes(y = first, colour = "var0")) + 
  geom_line(aes(y =  second, colour = "var1"))+ggtitle("Two Sample from Class8")+ylab("Pca Value")


#The two time series I chose for each class follow each other in harmony as can be seen in the graphs above.











pcaclass1 <- princomp(gesture1[,3:5], cor=T)
summary(pcaclass1, loadings=T)

pcaclass2 <- princomp(gesture2[,3:5], cor=T)
summary(pcaclass2, loadings=T)

pcaclass3 <- princomp(gesture3[,3:5], cor=T)
summary(pcaclass3, loadings=T)

pcaclass4 <- princomp(gesture4[,3:5], cor=T)
summary(pcaclass4, loadings=T)

pcaclass5 <- princomp(gesture5[,3:5], cor=T)
summary(pcaclass5, loadings=T)

pcaclass6 <- princomp(gesture6[,3:5], cor=T)
summary(pcaclass6, loadings=T)

pcaclass7 <- princomp(gesture7[,3:5], cor=T)
summary(pcaclass7, loadings=T)

pcaclass8 <- princomp(gesture8[,3:5], cor=T)
summary(pcaclass8, loadings=T)

pcatable<-data.table(matrix(0,9,3))



pcatable[1,1]=summary(pcaclass1, loadings=T)$loadings[1,1]
pcatable[1,2]=summary(pcaclass1, loadings=T)$loadings[2,1]
pcatable[1,3]=summary(pcaclass1, loadings=T)$loadings[3,1]


pcatable[2,1]=summary(pcaclass2, loadings=T)$loadings[1,1]
pcatable[2,2]=summary(pcaclass2, loadings=T)$loadings[2,1]
pcatable[2,3]=summary(pcaclass2, loadings=T)$loadings[3,1]

pcatable[3,1]=summary(pcaclass3, loadings=T)$loadings[1,1]
pcatable[3,2]=summary(pcaclass3, loadings=T)$loadings[2,1]
pcatable[3,3]=summary(pcaclass3, loadings=T)$loadings[3,1]

pcatable[4,1]=summary(pcaclass4, loadings=T)$loadings[1,1]
pcatable[4,2]=summary(pcaclass4, loadings=T)$loadings[2,1]
pcatable[4,3]=summary(pcaclass4, loadings=T)$loadings[3,1]

pcatable[5,1]=summary(pcaclass5, loadings=T)$loadings[1,1]
pcatable[5,2]=summary(pcaclass5, loadings=T)$loadings[2,1]
pcatable[5,3]=summary(pcaclass5, loadings=T)$loadings[3,1]

pcatable[6,1]=summary(pcaclass6, loadings=T)$loadings[1,1]
pcatable[6,2]=summary(pcaclass6, loadings=T)$loadings[2,1]
pcatable[6,3]=summary(pcaclass6, loadings=T)$loadings[3,1]

pcatable[7,1]=summary(pcaclass7, loadings=T)$loadings[1,1]
pcatable[7,2]=summary(pcaclass7, loadings=T)$loadings[2,1]
pcatable[7,3]=summary(pcaclass7, loadings=T)$loadings[3,1]

pcatable[8,1]=summary(pcaclass8, loadings=T)$loadings[1,1]
pcatable[8,2]=summary(pcaclass8, loadings=T)$loadings[2,1]
pcatable[8,3]=summary(pcaclass8, loadings=T)$loadings[3,1]

pcatable[9,1]=summary(pca1, loadings=T)$loadings[1,1]
pcatable[9,2]=summary(pca1, loadings=T)$loadings[2,1]
pcatable[9,3]=summary(pca1, loadings=T)$loadings[3,1]

setnames(pcatable, old = "V1", new = "coefficientx")
setnames(pcatable, old = "V2", new = "coefficienty")
setnames(pcatable, old = "V3", new = "coefficientz")

pcatable


#PC1 of 1st, 2nd and 7th classes are similar to each other.Y component has the largest coefficient while x component has the smallest coefficient.The same situation is valid for PC1 of all data.There are differences in PC1 of other classes.  


mdstrain<-cbind(trainx,trainy[,2:316],trainz[,2:316])

d <- dist(mdstrain)
fit <- cmdscale(d,eig=TRUE, k=2)

x <- fit$points[,1]
y <- fit$points[,2]


mds_appliedts <- cbind(x,y,trainx[,1])
mds_appliedts<-data.table(mds_appliedts)
setnames(mds_appliedts, old = "V3", new = "class")
mds_appliedts[,timeseriesid:=1:896]


mds_appliedts$class = as.character(mds_appliedts$class)


ggplot(mds_appliedts,aes(x=x,y=y,color=class))+geom_point() +labs(title = " MDS ", x="Coordinate 1", y="Coordinate 2")


#Observations of each class seem to be clustered in a specific area.






