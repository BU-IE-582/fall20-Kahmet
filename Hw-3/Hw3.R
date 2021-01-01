


#HW 3
library(quantreg) 
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
library(cvAUC)
library(pROC)
library(Metrics)
library(glmnet)
library(ggplot2)
library(CVXR)

setwd("C:/Users/kadir/OneDrive/Masaüstü")

tüketim<-read.csv("GercekZamanliTuketim-01012016-30112020.csv", sep = ",")

tüketim$Tüketim.Miktarý..MWh.<-gsub(pattern="\\.", replacement = "", x=tüketim$Tüketim.Miktarý..MWh.)
tüketim$Tüketim.Miktarý..MWh.<- gsub(",",".",tüketim$Tüketim.Miktarý..MWh.)
tüketim$Tüketim.Miktarý..MWh.<- as.numeric(as.character( tüketim$Tüketim.Miktarý..MWh. ))

#TASK A




gzt<-tüketim$Tüketim.Miktarý..MWh.

gzt<-data.table(gzt)
gzt[,index:=1:43104]
gzt[gzt==0]

gzt$gzt[2067]<-(gzt$gzt[2066]+gzt$gzt[2068])/2  #interpolation

gzt<-gzt$gzt

laggzt48<-lag(gzt,48)
laggzt168<-lag(gzt,168)


tasks_mape<-data.table(matrix(0,1,5))
a__lag48_mape<-data.table(matrix(0,24,1))
a__lag48_mape<-data.table(matrix(0,24,1))
a__lag168_mape<-data.table(matrix(0,24,1))
b_mape<-data.table(matrix(0,24,1))
c_mape<-data.table(matrix(0,24,1))
d_mape<-data.table(matrix(0,24,1))

setnames(tasks_mape,old="V1",new="A_48_MAPE")
setnames(tasks_mape,old="V2",new="A_168_MAPE")
setnames(tasks_mape,old="V3",new="B_MAPE")
setnames(tasks_mape,old="V4",new="C_MAPE")
setnames(tasks_mape,old="V5",new="D_MAPE")

setnames(a__lag48_mape,old="V1",new="MAPE")
setnames(a__lag168_mape,old="V1",new="MAPE")
setnames(b_mape,old="V1",new="MAPE")
setnames(c_mape,old="V1",new="MAPE")
setnames(d_mape,old="V1",new="MAPE")


mape(gzt[42385:43104],laggzt48[42385:43104]) #start 1 november 2020
mape(gzt[42385:43104],laggzt168[42385:43104])

tasks_mape[1,1]<-mape(gzt[42385:43104],laggzt48[42385:43104]) #start 1 november 2020
tasks_mape[1,2]<-mape(gzt[42385:43104],laggzt168[42385:43104])


#Hourly filtered the data

Hour<-0:43103
datagzt<-cbind(tüketim$Tarih,Hour,laggzt48,laggzt168,gzt)
datagzt<-data.table(datagzt)
setnames(datagzt,old="V1",new="Date")
datagzt[,Hour:=Hour%%24]
setnames(datagzt,old="gzt",new="Consumption")

datagzt$Date<-tüketim$Tarih

traindata<-datagzt[169:42384,]
testdata<-datagzt[42385:43104,]



traindata0<-copy(filter(traindata,Hour==0))
testdata0<-copy(filter(testdata,Hour==0))
traindata1<-copy(filter(traindata,Hour==1))
testdata1<-copy(filter(testdata,Hour==1))
traindata2<-copy(filter(traindata,Hour==2))
testdata2<-copy(filter(testdata,Hour==2))
traindata3<-copy(filter(traindata,Hour==3))
testdata3<-copy(filter(testdata,Hour==3))
traindata4<-copy(filter(traindata,Hour==4))
testdata4<-copy(filter(testdata,Hour==4))
traindata5<-copy(filter(traindata,Hour==5))
testdata5<-copy(filter(testdata,Hour==5))
traindata6<-copy(filter(traindata,Hour==6))
testdata6<-copy(filter(testdata,Hour==6))
traindata7<-copy(filter(traindata,Hour==7))
testdata7<-copy(filter(testdata,Hour==7))
traindata8<-copy(filter(traindata,Hour==8))
testdata8<-copy(filter(testdata,Hour==8))
traindata9<-copy(filter(traindata,Hour==9))
testdata9<-copy(filter(testdata,Hour==9))
traindata10<-copy(filter(traindata,Hour==10))
testdata10<-copy(filter(testdata,Hour==10))
traindata11<-copy(filter(traindata,Hour==11))
testdata11<-copy(filter(testdata,Hour==11))
traindata12<-copy(filter(traindata,Hour==12))
testdata12<-copy(filter(testdata,Hour==12))
traindata13<-copy(filter(traindata,Hour==13))
testdata13<-copy(filter(testdata,Hour==13))
traindata14<-copy(filter(traindata,Hour==14))
testdata14<-copy(filter(testdata,Hour==14))
traindata15<-copy(filter(traindata,Hour==15))
testdata15<-copy(filter(testdata,Hour==15))
traindata16<-copy(filter(traindata,Hour==16))
testdata16<-copy(filter(testdata,Hour==16))
traindata17<-copy(filter(traindata,Hour==17))
testdata17<-copy(filter(testdata,Hour==17))
traindata18<-copy(filter(traindata,Hour==18))
testdata18<-copy(filter(testdata,Hour==18))
traindata19<-copy(filter(traindata,Hour==19))
testdata19<-copy(filter(testdata,Hour==19))
traindata20<-copy(filter(traindata,Hour==20))
testdata20<-copy(filter(testdata,Hour==20))
traindata21<-copy(filter(traindata,Hour==21))
testdata21<-copy(filter(testdata,Hour==21))
traindata22<-copy(filter(traindata,Hour==22))
testdata22<-copy(filter(testdata,Hour==22))
traindata23<-copy(filter(traindata,Hour==23))
testdata23<-copy(filter(testdata,Hour==23))

#Hour 0


a__lag48_mape[1,1]<-mape(testdata0$Consumption,testdata0$laggzt48)
a__lag168_mape[1,1]<-mape(testdata0$Consumption,testdata0$laggzt168)
mape(testdata0$Consumption,testdata0$laggzt48)
mape(testdata0$Consumption,testdata0$laggzt168)
#Hour 1

a__lag48_mape[2,1]<-mape(testdata1$Consumption,testdata1$laggzt48)
a__lag168_mape[2,1]<-mape(testdata1$Consumption,testdata1$laggzt168)
mape(testdata1$Consumption,testdata1$laggzt48)
mape(testdata1$Consumption,testdata1$laggzt168)
#Hour 2

a__lag48_mape[3,1]<-mape(testdata2$Consumption,testdata2$laggzt48)
a__lag168_mape[3,1]<-mape(testdata2$Consumption,testdata2$laggzt168)
mape(testdata2$Consumption,testdata2$laggzt48)
mape(testdata2$Consumption,testdata2$laggzt168)
#Hour 3

a__lag48_mape[4,1]<-mape(testdata3$Consumption,testdata3$laggzt48)
a__lag168_mape[4,1]<-mape(testdata3$Consumption,testdata3$laggzt168)
mape(testdata3$Consumption,testdata3$laggzt48)
mape(testdata3$Consumption,testdata3$laggzt168)
#Hour 4

a__lag48_mape[5,1]<-mape(testdata4$Consumption,testdata4$laggzt48)
a__lag168_mape[5,1]<-mape(testdata4$Consumption,testdata4$laggzt168)
mape(testdata4$Consumption,testdata4$laggzt48)
mape(testdata4$Consumption,testdata4$laggzt168)
#Hour 5

a__lag48_mape[6,1]<-mape(testdata5$Consumption,testdata5$laggzt48)
a__lag168_mape[6,1]<-mape(testdata5$Consumption,testdata5$laggzt168)
mape(testdata5$Consumption,testdata5$laggzt48)
mape(testdata5$Consumption,testdata5$laggzt168)
#Hour 6

a__lag48_mape[7,1]<-mape(testdata6$Consumption,testdata6$laggzt48)
a__lag168_mape[7,1]<-mape(testdata6$Consumption,testdata6$laggzt168)
mape(testdata6$Consumption,testdata6$laggzt48)
mape(testdata6$Consumption,testdata6$laggzt168)
#Hour 7

a__lag48_mape[8,1]<-mape(testdata7$Consumption,testdata7$laggzt48)
a__lag168_mape[8,1]<-mape(testdata7$Consumption,testdata7$laggzt168)
mape(testdata7$Consumption,testdata7$laggzt48)
mape(testdata7$Consumption,testdata7$laggzt168)
#Hour 8

a__lag48_mape[9,1]<-mape(testdata8$Consumption,testdata8$laggzt48)
a__lag168_mape[9,1]<-mape(testdata8$Consumption,testdata8$laggzt168)
mape(testdata8$Consumption,testdata8$laggzt48)
mape(testdata8$Consumption,testdata8$laggzt168)
#Hour 9

a__lag48_mape[10,1]<-mape(testdata9$Consumption,testdata9$laggzt48)
a__lag168_mape[10,1]<-mape(testdata9$Consumption,testdata9$laggzt168)
mape(testdata9$Consumption,testdata9$laggzt48)
mape(testdata9$Consumption,testdata9$laggzt168)
#Hour 10

a__lag48_mape[11,1]<-mape(testdata10$Consumption,testdata10$laggzt48)
a__lag168_mape[11,1]<-mape(testdata10$Consumption,testdata10$laggzt168)
mape(testdata10$Consumption,testdata10$laggzt48)
mape(testdata10$Consumption,testdata10$laggzt168)
#Hour 11

a__lag48_mape[12,1]<-mape(testdata11$Consumption,testdata11$laggzt48)
a__lag168_mape[12,1]<-mape(testdata11$Consumption,testdata11$laggzt168)
mape(testdata11$Consumption,testdata11$laggzt48)
mape(testdata11$Consumption,testdata11$laggzt168)
#Hour 12

a__lag48_mape[13,1]<-mape(testdata12$Consumption,testdata12$laggzt48)
a__lag168_mape[13,1]<-mape(testdata12$Consumption,testdata12$laggzt168)
mape(testdata12$Consumption,testdata12$laggzt48)
mape(testdata12$Consumption,testdata12$laggzt168)
#Hour 13

a__lag48_mape[14,1]<-mape(testdata13$Consumption,testdata13$laggzt48)
a__lag168_mape[14,1]<-mape(testdata13$Consumption,testdata13$laggzt168)
mape(testdata13$Consumption,testdata13$laggzt48)
mape(testdata13$Consumption,testdata13$laggzt168)
#Hour 14

a__lag48_mape[15,1]<-mape(testdata14$Consumption,testdata14$laggzt48)
a__lag168_mape[15,1]<-mape(testdata14$Consumption,testdata14$laggzt168)
mape(testdata14$Consumption,testdata14$laggzt48)
mape(testdata14$Consumption,testdata14$laggzt168)
#Hour 15

a__lag48_mape[16,1]<-mape(testdata15$Consumption,testdata15$laggzt48)
a__lag168_mape[16,1]<-mape(testdata15$Consumption,testdata15$laggzt168)
mape(testdata15$Consumption,testdata15$laggzt48)
mape(testdata15$Consumption,testdata15$laggzt168)
#Hour 16

a__lag48_mape[17,1]<-mape(testdata16$Consumption,testdata16$laggzt48)
a__lag168_mape[17,1]<-mape(testdata16$Consumption,testdata16$laggzt168)
mape(testdata16$Consumption,testdata16$laggzt48)
mape(testdata16$Consumption,testdata16$laggzt168)
#Hour 17

a__lag48_mape[18,1]<-mape(testdata17$Consumption,testdata17$laggzt48)
a__lag168_mape[18,1]<-mape(testdata17$Consumption,testdata17$laggzt168)
mape(testdata17$Consumption,testdata17$laggzt48)
mape(testdata17$Consumption,testdata17$laggzt168)
#Hour 18

a__lag48_mape[19,1]<-mape(testdata18$Consumption,testdata18$laggzt48)
a__lag168_mape[19,1]<-mape(testdata18$Consumption,testdata18$laggzt168)
mape(testdata18$Consumption,testdata18$laggzt48)
mape(testdata18$Consumption,testdata18$laggzt168)
#Hour 19

a__lag48_mape[20,1]<-mape(testdata19$Consumption,testdata19$laggzt48)
a__lag168_mape[20,1]<-mape(testdata19$Consumption,testdata19$laggzt168)
mape(testdata19$Consumption,testdata19$laggzt48)
mape(testdata19$Consumption,testdata19$laggzt168)
#Hour 20

a__lag48_mape[21,1]<-mape(testdata20$Consumption,testdata20$laggzt48)
a__lag168_mape[21,1]<-mape(testdata20$Consumption,testdata20$laggzt168)
mape(testdata20$Consumption,testdata20$laggzt48)
mape(testdata20$Consumption,testdata20$laggzt168)
#Hour 21

a__lag48_mape[22,1]<-mape(testdata21$Consumption,testdata21$laggzt48)
a__lag168_mape[22,1]<-mape(testdata21$Consumption,testdata21$laggzt168)
mape(testdata21$Consumption,testdata21$laggzt48)
mape(testdata21$Consumption,testdata21$laggzt168)
#Hour 22

a__lag48_mape[23,1]<-mape(testdata22$Consumption,testdata22$laggzt48)
a__lag168_mape[23,1]<-mape(testdata22$Consumption,testdata22$laggzt168)
mape(testdata22$Consumption,testdata22$laggzt48)
mape(testdata22$Consumption,testdata22$laggzt168)
#Hour 23

a__lag48_mape[24,1]<-mape(testdata23$Consumption,testdata23$laggzt48)
a__lag168_mape[24,1]<-mape(testdata23$Consumption,testdata23$laggzt168)
mape(testdata23$Consumption,testdata23$laggzt48)
mape(testdata23$Consumption,testdata23$laggzt168)



#TASK B



mymodel<-lm(Consumption~laggzt48+laggzt168,data=traindata)
testdata[,preds:=predict(mymodel,testdata)]
tasks_mape[1,3]<-mape(testdata$Consumption,testdata$preds)
mape(testdata$Consumption,testdata$preds)

#Hour 0


testdata0[,preds_b:=predict(mymodel,testdata0)]
b_mape[1,1]<-mape(testdata0$Consumption,testdata0$preds)
mape(testdata0$Consumption,testdata0$preds)
#Hour 1


testdata1[,preds_b:=predict(mymodel,testdata1)]
b_mape[2,1]<-mape(testdata1$Consumption,testdata1$preds)
mape(testdata1$Consumption,testdata1$preds)
#Hour 2


testdata2[,preds_b:=predict(mymodel,testdata2)]
b_mape[3,1]<-mape(testdata2$Consumption,testdata2$preds)
mape(testdata2$Consumption,testdata2$preds)
#Hour 3


testdata3[,preds_b:=predict(mymodel,testdata3)]
b_mape[4,1]<-mape(testdata3$Consumption,testdata3$preds)
mape(testdata3$Consumption,testdata3$preds)
#Hour 4


testdata4[,preds_b:=predict(mymodel,testdata4)]
b_mape[5,1]<-mape(testdata4$Consumption,testdata4$preds)
mape(testdata4$Consumption,testdata4$preds)
#Hour 5


testdata5[,preds_b:=predict(mymodel,testdata5)]
b_mape[6,1]<-mape(testdata5$Consumption,testdata5$preds)
mape(testdata5$Consumption,testdata5$preds)
#Hour 6


testdata6[,preds_b:=predict(mymodel,testdata6)]
b_mape[7,1]<-mape(testdata6$Consumption,testdata6$preds)
mape(testdata6$Consumption,testdata6$preds)
#Hour 7


testdata7[,preds_b:=predict(mymodel,testdata7)]
b_mape[8,1]<-mape(testdata7$Consumption,testdata7$preds)
mape(testdata7$Consumption,testdata7$preds)
#Hour 8


testdata8[,preds_b:=predict(mymodel,testdata8)]
b_mape[9,1]<-mape(testdata8$Consumption,testdata8$preds)
mape(testdata8$Consumption,testdata8$preds)
#Hour 9


testdata9[,preds_b:=predict(mymodel,testdata9)]
b_mape[10,1]<-mape(testdata9$Consumption,testdata9$preds)
mape(testdata9$Consumption,testdata9$preds)
#Hour 10


testdata10[,preds_b:=predict(mymodel,testdata10)]
b_mape[11,1]<-mape(testdata10$Consumption,testdata10$preds)
mape(testdata10$Consumption,testdata10$preds)
#Hour 11


testdata11[,preds_b:=predict(mymodel,testdata11)]
b_mape[12,1]<-mape(testdata11$Consumption,testdata11$preds)
mape(testdata11$Consumption,testdata11$preds)
#Hour 12


testdata12[,preds_b:=predict(mymodel,testdata12)]
b_mape[13,1]<-mape(testdata12$Consumption,testdata12$preds)
mape(testdata12$Consumption,testdata12$preds)
#Hour 13


testdata13[,preds_b:=predict(mymodel,testdata13)]
b_mape[14,1]<-mape(testdata13$Consumption,testdata13$preds)
mape(testdata13$Consumption,testdata13$preds)
#Hour 14


testdata14[,preds_b:=predict(mymodel,testdata14)]
b_mape[15,1]<-mape(testdata14$Consumption,testdata14$preds)
mape(testdata14$Consumption,testdata14$preds)
#Hour 15


testdata15[,preds_b:=predict(mymodel,testdata15)]
b_mape[16,1]<-mape(testdata15$Consumption,testdata15$preds)
mape(testdata15$Consumption,testdata15$preds)
#Hour 16


testdata16[,preds_b:=predict(mymodel,testdata16)]
b_mape[17,1]<-mape(testdata16$Consumption,testdata16$preds)
mape(testdata16$Consumption,testdata16$preds)
#Hour 17


testdata17[,preds_b:=predict(mymodel,testdata17)]
b_mape[18,1]<-mape(testdata17$Consumption,testdata17$preds)
mape(testdata17$Consumption,testdata17$preds)
#Hour 18


testdata18[,preds_b:=predict(mymodel,testdata18)]
b_mape[19,1]<-mape(testdata18$Consumption,testdata18$preds)
mape(testdata18$Consumption,testdata18$preds)
#Hour 19


testdata19[,preds_b:=predict(mymodel,testdata19)]
b_mape[20,1]<-mape(testdata19$Consumption,testdata19$preds)
mape(testdata19$Consumption,testdata19$preds)
#Hour 20


testdata20[,preds_b:=predict(mymodel,testdata20)]
b_mape[21,1]<-mape(testdata20$Consumption,testdata20$preds)
mape(testdata20$Consumption,testdata20$preds)
#Hour 21


testdata21[,preds_b:=predict(mymodel,testdata21)]
b_mape[22,1]<-mape(testdata21$Consumption,testdata21$preds)
mape(testdata21$Consumption,testdata21$preds)
#Hour 22


testdata22[,preds_b:=predict(mymodel,testdata22)]
b_mape[23,1]<-mape(testdata22$Consumption,testdata22$preds)
mape(testdata22$Consumption,testdata22$preds)
#Hour 23


testdata23[,preds_b:=predict(mymodel,testdata23)]
b_mape[24,1]<-mape(testdata23$Consumption,testdata23$preds)
mape(testdata23$Consumption,testdata23$preds)




#TASK C




#Hour 0

mymodel0<-lm(Consumption~laggzt48+laggzt168,data=traindata0)
testdata0[,preds_c:=predict(mymodel0,testdata0)]
c_mape[1,1]<-mape(testdata0$Consumption,testdata0$preds_c)
mape(testdata0$Consumption,testdata0$preds_c)
#Hour 1

mymodel1<-lm(Consumption~laggzt48+laggzt168,data=traindata1)
testdata1[,preds_c:=predict(mymodel1,testdata1)]
c_mape[2,1]<-mape(testdata1$Consumption,testdata1$preds_c)
mape(testdata1$Consumption,testdata1$preds_c)
#Hour 2

mymodel2<-lm(Consumption~laggzt48+laggzt168,data=traindata2)
testdata2[,preds_c:=predict(mymodel2,testdata2)]
c_mape[3,1]<-mape(testdata2$Consumption,testdata2$preds_c)
mape(testdata2$Consumption,testdata2$preds_c)
#Hour 3

mymodel3<-lm(Consumption~laggzt48+laggzt168,data=traindata3)
testdata3[,preds_c:=predict(mymodel3,testdata3)]
c_mape[4,1]<-mape(testdata3$Consumption,testdata3$preds_c)
mape(testdata3$Consumption,testdata3$preds_c)
#Hour 4

mymodel4<-lm(Consumption~laggzt48+laggzt168,data=traindata4)
testdata4[,preds_c:=predict(mymodel4,testdata4)]
c_mape[5,1]<-mape(testdata4$Consumption,testdata4$preds_c)
mape(testdata4$Consumption,testdata4$preds_c)
#Hour 5

mymodel5<-lm(Consumption~laggzt48+laggzt168,data=traindata5)
testdata5[,preds_c:=predict(mymodel5,testdata5)]
c_mape[6,1]<-mape(testdata5$Consumption,testdata5$preds_c)
mape(testdata5$Consumption,testdata5$preds_c)
#Hour 6

mymodel6<-lm(Consumption~laggzt48+laggzt168,data=traindata6)
testdata6[,preds_c:=predict(mymodel6,testdata6)]
c_mape[7,1]<-mape(testdata6$Consumption,testdata6$preds_c)
mape(testdata6$Consumption,testdata6$preds_c)
#Hour 7

mymodel7<-lm(Consumption~laggzt48+laggzt168,data=traindata7)
testdata7[,preds_c:=predict(mymodel7,testdata7)]
c_mape[8,1]<-mape(testdata7$Consumption,testdata7$preds_c)
mape(testdata7$Consumption,testdata7$preds_c)
#Hour 8

mymodel8<-lm(Consumption~laggzt48+laggzt168,data=traindata8)
testdata8[,preds_c:=predict(mymodel8,testdata8)]
c_mape[9,1]<-mape(testdata8$Consumption,testdata8$preds_c)
mape(testdata8$Consumption,testdata8$preds_c)
#Hour 9

mymodel9<-lm(Consumption~laggzt48+laggzt168,data=traindata9)
testdata9[,preds_c:=predict(mymodel9,testdata9)]
c_mape[10,1]<-mape(testdata9$Consumption,testdata9$preds_c)
mape(testdata9$Consumption,testdata9$preds_c)
#Hour 10

mymodel10<-lm(Consumption~laggzt48+laggzt168,data=traindata10)
testdata10[,preds_c:=predict(mymodel10,testdata10)]
c_mape[11,1]<-mape(testdata10$Consumption,testdata10$preds_c)
mape(testdata10$Consumption,testdata10$preds_c)
#Hour 11

mymodel11<-lm(Consumption~laggzt48+laggzt168,data=traindata11)
testdata11[,preds_c:=predict(mymodel11,testdata11)]
c_mape[12,1]<-mape(testdata11$Consumption,testdata11$preds_c)
mape(testdata11$Consumption,testdata11$preds_c)
#Hour 12

mymodel12<-lm(Consumption~laggzt48+laggzt168,data=traindata12)
testdata12[,preds_c:=predict(mymodel12,testdata12)]
c_mape[13,1]<-mape(testdata12$Consumption,testdata12$preds_c)
mape(testdata12$Consumption,testdata12$preds_c)
#Hour 13

mymodel13<-lm(Consumption~laggzt48+laggzt168,data=traindata13)
testdata13[,preds_c:=predict(mymodel13,testdata13)]
c_mape[14,1]<-mape(testdata13$Consumption,testdata13$preds_c)
mape(testdata13$Consumption,testdata13$preds_c)
#Hour 14

mymodel14<-lm(Consumption~laggzt48+laggzt168,data=traindata14)
testdata14[,preds_c:=predict(mymodel14,testdata14)]
c_mape[15,1]<-mape(testdata14$Consumption,testdata14$preds_c)
mape(testdata14$Consumption,testdata14$preds_c)
#Hour 15

mymodel15<-lm(Consumption~laggzt48+laggzt168,data=traindata15)
testdata15[,preds_c:=predict(mymodel15,testdata15)]
c_mape[16,1]<-mape(testdata15$Consumption,testdata15$preds_c)
mape(testdata15$Consumption,testdata15$preds_c)
#Hour 16

mymodel16<-lm(Consumption~laggzt48+laggzt168,data=traindata16)
testdata16[,preds_c:=predict(mymodel16,testdata16)]
c_mape[17,1]<-mape(testdata16$Consumption,testdata16$preds_c)
mape(testdata16$Consumption,testdata16$preds_c)
#Hour 17

mymodel17<-lm(Consumption~laggzt48+laggzt168,data=traindata17)
testdata17[,preds_c:=predict(mymodel17,testdata17)]
c_mape[18,1]<-mape(testdata17$Consumption,testdata17$preds_c)
mape(testdata17$Consumption,testdata17$preds_c)
#Hour 18

mymodel18<-lm(Consumption~laggzt48+laggzt168,data=traindata18)
testdata18[,preds_c:=predict(mymodel18,testdata18)]
c_mape[19,1]<-mape(testdata18$Consumption,testdata18$preds_c)
mape(testdata18$Consumption,testdata18$preds_c)
#Hour 19

mymodel19<-lm(Consumption~laggzt48+laggzt168,data=traindata19)
testdata19[,preds_c:=predict(mymodel19,testdata19)]
c_mape[20,1]<-mape(testdata19$Consumption,testdata19$preds_c)
mape(testdata19$Consumption,testdata19$preds_c)
#Hour 20

mymodel20<-lm(Consumption~laggzt48+laggzt168,data=traindata20)
testdata20[,preds_c:=predict(mymodel20,testdata20)]
c_mape[21,1]<-mape(testdata20$Consumption,testdata20$preds_c)
mape(testdata20$Consumption,testdata20$preds_c)
#Hour 21

mymodel21<-lm(Consumption~laggzt48+laggzt168,data=traindata21)
testdata21[,preds_c:=predict(mymodel21,testdata21)]
c_mape[22,1]<-mape(testdata21$Consumption,testdata21$preds_c)
mape(testdata21$Consumption,testdata21$preds_c)
#Hour 22

mymodel22<-lm(Consumption~laggzt48+laggzt168,data=traindata22)
testdata22[,preds_c:=predict(mymodel22,testdata22)]
c_mape[23,1]<-mape(testdata22$Consumption,testdata22$preds_c)
mape(testdata22$Consumption,testdata22$preds_c)
#Hour 23

mymodel23<-lm(Consumption~laggzt48+laggzt168,data=traindata23)
testdata23[,preds_c:=predict(mymodel23,testdata23)]
c_mape[24,1]<-mape(testdata23$Consumption,testdata23$preds_c)
mape(testdata23$Consumption,testdata23$preds_c)


newtest_data<-rbind(testdata0,testdata1,testdata2,testdata3,testdata4,testdata5,testdata6,testdata7,testdata8,testdata9,testdata10,testdata11,testdata12,testdata13,testdata14,testdata15,testdata16,testdata17,testdata18,testdata19,testdata20,testdata21,testdata22,testdata23)
tasks_mape[1,4]<-mape(newtest_data$Consumption,newtest_data$preds_c)
mape(newtest_data$Consumption,newtest_data$preds_c)

#TASK D





newdatagzt<-data.table(matrix(0,42936,50))
newdatagzt<-data.frame(newdatagzt)
datagzt<-data.frame(datagzt)
for(i in 1:1789){
  for(j in 1:24){
    for(d in 1:24){
newdatagzt[(i-1)*24+d,j+1]<-datagzt$Consumption[(i-1)*24+j]
  }
  }
}

for(i in 1:1789){
  for(j in 1:24){
    for(d in 1:24){
      newdatagzt[(i-1)*24+d,j+25]<-datagzt$Consumption[(i-1)*24+j+120]
    }
  }
}
newdatagzt<-data.table(newdatagzt)
datagzt<-data.table(datagzt)

newdatagzt$V1<-tüketim$Tarih[169:43104]
newdatagzt$V50<-datagzt$Consumption[169:43104]
names(newdatagzt)<-c("Date","Lag_day7_hour_0","Lag_day7_hour_1","Lag_day7_hour_2","Lag_day7_hour_3","Lag_day7_hour_4","Lag_day7_hour_5","Lag_day7_hour_6","Lag_day7_hour_7","Lag_day7_hour_8","Lag_day7_hour_9","Lag_day7_hour_10","Lag_day7_hour_11","Lag_day7_hour_12","Lag_day7_hour_13","Lag_day7_hour_14","Lag_day7_hour_15","Lag_day7_hour_16","Lag_day7_hour_17","Lag_day7_hour_18","Lag_day7_hour_19","Lag_day7_hour_20","Lag_day7_hour_21","Lag_day7_hour_22","Lag_day7_hour_23","Lag_day2_hour_0","Lag_day2_hour_1","Lag_day2_hour_2","Lag_day2_hour_3","Lag_day2_hour_4","Lag_day2_hour_5","Lag_day2_hour_6","Lag_day2_hour_7","Lag_day2_hour_8","Lag_day2_hour_9","Lag_day2_hour_10","Lag_day2_hour_11","Lag_day2_hour_12","Lag_day2_hour_13","Lag_day2_hour_14","Lag_day2_hour_15","Lag_day2_hour_16","Lag_day2_hour_17","Lag_day2_hour_18","Lag_day2_hour_19","Lag_day2_hour_20","Lag_day2_hour_21","Lag_day2_hour_22","Lag_day2_hour_23","Consumption")
newdatagzt[,Hour:=0:42935]
newdatagzt[,Hour:=Hour%%24]

newtraindata<-newdatagzt[1:42216,]
newtestdata<-newdatagzt[42217:42936,]




#Hour 0
newtrain0<-copy(filter(newtraindata,Hour==0))
newtest0<-copy(filter(newtestdata,Hour==0))
matrixy0<-as.matrix(newtrain0[,50])
matrixx0<-as.matrix(newtrain0[,2:49])
cv.lasso0 <- cv.glmnet(matrixx0, matrixy0, alpha=1,nfolds=10)
lambda0<-cv.lasso0$lambda.1se
fit0<-glmnet(matrixx0, matrixy0,family="gaussian",lambda=lambda0)
testmatrixx0<-as.matrix(newtest0[,2:49])
newtest0[,pred_d:=predict(fit0, s=lambda0, testmatrixx0, type="response")]
newnewtest0<-newtest0[,c("Consumption","pred_d")]
d_mape[1,1]<-mape(newnewtest0$Consumption,newnewtest0$pred_d)
mape(newnewtest0$Consumption,newnewtest0$pred_d)
#Hour 1
newtrain1<-copy(filter(newtraindata,Hour==1))
newtest1<-copy(filter(newtestdata,Hour==1))
matrixy1<-as.matrix(newtrain1[,50])
matrixx1<-as.matrix(newtrain1[,2:49])
cv.lasso1 <- cv.glmnet(matrixx1, matrixy1, alpha=1,nfolds=10)
lambda1<-cv.lasso1$lambda.1se
fit1<-glmnet(matrixx1, matrixy1,family="gaussian",lambda=lambda1)
testmatrixx1<-as.matrix(newtest1[,2:49])
newtest1[,pred_d:=predict(fit1, s=lambda1, testmatrixx1, type="response")]
newnewtest1<-newtest1[,c("Consumption","pred_d")]
d_mape[2,1]<-mape(newnewtest1$Consumption,newnewtest1$pred_d)
mape(newnewtest1$Consumption,newnewtest1$pred_d)
#Hour 2
newtrain2<-copy(filter(newtraindata,Hour==2))
newtest2<-copy(filter(newtestdata,Hour==2))
matrixy2<-as.matrix(newtrain2[,50])
matrixx2<-as.matrix(newtrain2[,2:49])
cv.lasso2 <- cv.glmnet(matrixx2, matrixy2, alpha=1,nfolds=10)
lambda2<-cv.lasso2$lambda.1se
fit2<-glmnet(matrixx2, matrixy2,family="gaussian",lambda=lambda2)
testmatrixx2<-as.matrix(newtest2[,2:49])
newtest2[,pred_d:=predict(fit2, s=lambda2, testmatrixx2, type="response")]
newnewtest2<-newtest2[,c("Consumption","pred_d")]
d_mape[3,1]<-mape(newnewtest2$Consumption,newnewtest2$pred_d)
mape(newnewtest2$Consumption,newnewtest2$pred_d)
#Hour 3
newtrain3<-copy(filter(newtraindata,Hour==3))
newtest3<-copy(filter(newtestdata,Hour==3))
matrixy3<-as.matrix(newtrain3[,50])
matrixx3<-as.matrix(newtrain3[,2:49])
cv.lasso3 <- cv.glmnet(matrixx3, matrixy3, alpha=1,nfolds=10)
lambda3<-cv.lasso3$lambda.1se
fit3<-glmnet(matrixx3, matrixy3,family="gaussian",lambda=lambda3)
testmatrixx3<-as.matrix(newtest3[,2:49])
newtest3[,pred_d:=predict(fit3, s=lambda3, testmatrixx3, type="response")]
newnewtest3<-newtest3[,c("Consumption","pred_d")]
d_mape[4,1]<-mape(newnewtest3$Consumption,newnewtest3$pred_d)
mape(newnewtest3$Consumption,newnewtest3$pred_d)
#Hour 4
newtrain4<-copy(filter(newtraindata,Hour==4))
newtest4<-copy(filter(newtestdata,Hour==4))
matrixy4<-as.matrix(newtrain4[,50])
matrixx4<-as.matrix(newtrain4[,2:49])
cv.lasso4 <- cv.glmnet(matrixx4, matrixy4, alpha=1,nfolds=10)
lambda4<-cv.lasso4$lambda.1se
fit4<-glmnet(matrixx4, matrixy4,family="gaussian",lambda=lambda4)
testmatrixx4<-as.matrix(newtest4[,2:49])
newtest4[,pred_d:=predict(fit4, s=lambda4, testmatrixx4, type="response")]
newnewtest4<-newtest4[,c("Consumption","pred_d")]
d_mape[5,1]<-mape(newnewtest4$Consumption,newnewtest4$pred_d)
mape(newnewtest4$Consumption,newnewtest4$pred_d)
#Hour 5
newtrain5<-copy(filter(newtraindata,Hour==5))
newtest5<-copy(filter(newtestdata,Hour==5))
matrixy5<-as.matrix(newtrain5[,50])
matrixx5<-as.matrix(newtrain5[,2:49])
cv.lasso5 <- cv.glmnet(matrixx5, matrixy5, alpha=1,nfolds=10)
lambda5<-cv.lasso5$lambda.1se
fit5<-glmnet(matrixx5, matrixy5,family="gaussian",lambda=lambda5)
testmatrixx5<-as.matrix(newtest5[,2:49])
newtest5[,pred_d:=predict(fit5, s=lambda5, testmatrixx5, type="response")]
newnewtest5<-newtest5[,c("Consumption","pred_d")]
d_mape[6,1]<-mape(newnewtest5$Consumption,newnewtest5$pred_d)
mape(newnewtest5$Consumption,newnewtest5$pred_d)
#Hour 6
newtrain6<-copy(filter(newtraindata,Hour==6))
newtest6<-copy(filter(newtestdata,Hour==6))
matrixy6<-as.matrix(newtrain6[,50])
matrixx6<-as.matrix(newtrain6[,2:49])
cv.lasso6 <- cv.glmnet(matrixx6, matrixy6, alpha=1,nfolds=10)
lambda6<-cv.lasso6$lambda.1se
fit6<-glmnet(matrixx6, matrixy6,family="gaussian",lambda=lambda6)
testmatrixx6<-as.matrix(newtest6[,2:49])
newtest6[,pred_d:=predict(fit6, s=lambda6, testmatrixx6, type="response")]
newnewtest6<-newtest6[,c("Consumption","pred_d")]
d_mape[7,1]<-mape(newnewtest6$Consumption,newnewtest6$pred_d)
mape(newnewtest6$Consumption,newnewtest6$pred_d)
#Hour 7
newtrain7<-copy(filter(newtraindata,Hour==7))
newtest7<-copy(filter(newtestdata,Hour==7))
matrixy7<-as.matrix(newtrain7[,50])
matrixx7<-as.matrix(newtrain7[,2:49])
cv.lasso7 <- cv.glmnet(matrixx7, matrixy7, alpha=1,nfolds=10)
lambda7<-cv.lasso7$lambda.1se
fit7<-glmnet(matrixx7, matrixy7,family="gaussian",lambda=lambda7)
testmatrixx7<-as.matrix(newtest7[,2:49])
newtest7[,pred_d:=predict(fit7, s=lambda7, testmatrixx7, type="response")]
newnewtest7<-newtest7[,c("Consumption","pred_d")]
d_mape[8,1]<-mape(newnewtest7$Consumption,newnewtest7$pred_d)
mape(newnewtest7$Consumption,newnewtest7$pred_d)
#Hour 8
newtrain8<-copy(filter(newtraindata,Hour==8))
newtest8<-copy(filter(newtestdata,Hour==8))
matrixy8<-as.matrix(newtrain8[,50])
matrixx8<-as.matrix(newtrain8[,2:49])
cv.lasso8 <- cv.glmnet(matrixx8, matrixy8, alpha=1,nfolds=10)
lambda8<-cv.lasso8$lambda.1se
fit8<-glmnet(matrixx8, matrixy8,family="gaussian",lambda=lambda8)
testmatrixx8<-as.matrix(newtest8[,2:49])
newtest8[,pred_d:=predict(fit8, s=lambda8, testmatrixx8, type="response")]
newnewtest8<-newtest8[,c("Consumption","pred_d")]
d_mape[9,1]<-mape(newnewtest8$Consumption,newnewtest8$pred_d)
mape(newnewtest8$Consumption,newnewtest8$pred_d)
#Hour 9
newtrain9<-copy(filter(newtraindata,Hour==9))
newtest9<-copy(filter(newtestdata,Hour==9))
matrixy9<-as.matrix(newtrain9[,50])
matrixx9<-as.matrix(newtrain9[,2:49])
cv.lasso9 <- cv.glmnet(matrixx9, matrixy9, alpha=1,nfolds=10)
lambda9<-cv.lasso9$lambda.1se
fit9<-glmnet(matrixx9, matrixy9,family="gaussian",lambda=lambda9)
testmatrixx9<-as.matrix(newtest9[,2:49])
newtest9[,pred_d:=predict(fit9, s=lambda9, testmatrixx9, type="response")]
newnewtest9<-newtest9[,c("Consumption","pred_d")]
d_mape[10,1]<-mape(newnewtest9$Consumption,newnewtest9$pred_d)
mape(newnewtest9$Consumption,newnewtest9$pred_d)
#Hour 10
newtrain10<-copy(filter(newtraindata,Hour==10))
newtest10<-copy(filter(newtestdata,Hour==10))
matrixy10<-as.matrix(newtrain10[,50])
matrixx10<-as.matrix(newtrain10[,2:49])
cv.lasso10 <- cv.glmnet(matrixx10, matrixy10, alpha=1,nfolds=10)
lambda10<-cv.lasso10$lambda.1se
fit10<-glmnet(matrixx10, matrixy10,family="gaussian",lambda=lambda10)
testmatrixx10<-as.matrix(newtest10[,2:49])
newtest10[,pred_d:=predict(fit10, s=lambda10, testmatrixx10, type="response")]
newnewtest10<-newtest10[,c("Consumption","pred_d")]
d_mape[11,1]<-mape(newnewtest10$Consumption,newnewtest10$pred_d)
mape(newnewtest10$Consumption,newnewtest10$pred_d)
#Hour 11
newtrain11<-copy(filter(newtraindata,Hour==11))
newtest11<-copy(filter(newtestdata,Hour==11))
matrixy11<-as.matrix(newtrain11[,50])
matrixx11<-as.matrix(newtrain11[,2:49])
cv.lasso11 <- cv.glmnet(matrixx11, matrixy11, alpha=1,nfolds=10)
lambda11<-cv.lasso11$lambda.1se
fit11<-glmnet(matrixx11, matrixy11,family="gaussian",lambda=lambda11)
testmatrixx11<-as.matrix(newtest11[,2:49])
newtest11[,pred_d:=predict(fit11, s=lambda11, testmatrixx11, type="response")]
newnewtest11<-newtest11[,c("Consumption","pred_d")]
d_mape[12,1]<-mape(newnewtest11$Consumption,newnewtest11$pred_d)
mape(newnewtest11$Consumption,newnewtest11$pred_d)
#Hour 12
newtrain12<-copy(filter(newtraindata,Hour==12))
newtest12<-copy(filter(newtestdata,Hour==12))
matrixy12<-as.matrix(newtrain12[,50])
matrixx12<-as.matrix(newtrain12[,2:49])
cv.lasso12 <- cv.glmnet(matrixx12, matrixy12, alpha=1,nfolds=10)
lambda12<-cv.lasso12$lambda.1se
fit12<-glmnet(matrixx12, matrixy12,family="gaussian",lambda=lambda12)
testmatrixx12<-as.matrix(newtest12[,2:49])
newtest12[,pred_d:=predict(fit12, s=lambda12, testmatrixx12, type="response")]
newnewtest12<-newtest12[,c("Consumption","pred_d")]
d_mape[13,1]<-mape(newnewtest12$Consumption,newnewtest12$pred_d)
mape(newnewtest12$Consumption,newnewtest12$pred_d)
#Hour 13
newtrain13<-copy(filter(newtraindata,Hour==13))
newtest13<-copy(filter(newtestdata,Hour==13))
matrixy13<-as.matrix(newtrain13[,50])
matrixx13<-as.matrix(newtrain13[,2:49])
cv.lasso13 <- cv.glmnet(matrixx13, matrixy13, alpha=1,nfolds=10)
lambda13<-cv.lasso13$lambda.1se
fit13<-glmnet(matrixx13, matrixy13,family="gaussian",lambda=lambda13)
testmatrixx13<-as.matrix(newtest13[,2:49])
newtest13[,pred_d:=predict(fit13, s=lambda13, testmatrixx13, type="response")]
newnewtest13<-newtest13[,c("Consumption","pred_d")]
d_mape[14,1]<-mape(newnewtest13$Consumption,newnewtest13$pred_d)
mape(newnewtest13$Consumption,newnewtest13$pred_d)
#Hour 14
newtrain14<-copy(filter(newtraindata,Hour==14))
newtest14<-copy(filter(newtestdata,Hour==14))
matrixy14<-as.matrix(newtrain14[,50])
matrixx14<-as.matrix(newtrain14[,2:49])
cv.lasso14 <- cv.glmnet(matrixx14, matrixy14, alpha=1,nfolds=10)
lambda14<-cv.lasso14$lambda.1se
fit14<-glmnet(matrixx14, matrixy14,family="gaussian",lambda=lambda14)
testmatrixx14<-as.matrix(newtest14[,2:49])
newtest14[,pred_d:=predict(fit14, s=lambda14, testmatrixx14, type="response")]
newnewtest14<-newtest14[,c("Consumption","pred_d")]
d_mape[15,1]<-mape(newnewtest14$Consumption,newnewtest14$pred_d)
mape(newnewtest14$Consumption,newnewtest14$pred_d)
#Hour 15
newtrain15<-copy(filter(newtraindata,Hour==15))
newtest15<-copy(filter(newtestdata,Hour==15))
matrixy15<-as.matrix(newtrain15[,50])
matrixx15<-as.matrix(newtrain15[,2:49])
cv.lasso15 <- cv.glmnet(matrixx15, matrixy15, alpha=1,nfolds=10)
lambda15<-cv.lasso15$lambda.1se
fit15<-glmnet(matrixx15, matrixy15,family="gaussian",lambda=lambda15)
testmatrixx15<-as.matrix(newtest15[,2:49])
newtest15[,pred_d:=predict(fit15, s=lambda15, testmatrixx15, type="response")]
newnewtest15<-newtest15[,c("Consumption","pred_d")]
d_mape[16,1]<-mape(newnewtest15$Consumption,newnewtest15$pred_d)
mape(newnewtest15$Consumption,newnewtest15$pred_d)
#Hour 16
newtrain16<-copy(filter(newtraindata,Hour==16))
newtest16<-copy(filter(newtestdata,Hour==16))
matrixy16<-as.matrix(newtrain16[,50])
matrixx16<-as.matrix(newtrain16[,2:49])
cv.lasso16 <- cv.glmnet(matrixx16, matrixy16, alpha=1,nfolds=10)
lambda16<-cv.lasso16$lambda.1se
fit16<-glmnet(matrixx16, matrixy16,family="gaussian",lambda=lambda16)
testmatrixx16<-as.matrix(newtest16[,2:49])
newtest16[,pred_d:=predict(fit16, s=lambda16, testmatrixx16, type="response")]
newnewtest16<-newtest16[,c("Consumption","pred_d")]
d_mape[17,1]<-mape(newnewtest16$Consumption,newnewtest16$pred_d)
mape(newnewtest16$Consumption,newnewtest16$pred_d)
#Hour 17
newtrain17<-copy(filter(newtraindata,Hour==17))
newtest17<-copy(filter(newtestdata,Hour==17))
matrixy17<-as.matrix(newtrain17[,50])
matrixx17<-as.matrix(newtrain17[,2:49])
cv.lasso17 <- cv.glmnet(matrixx17, matrixy17, alpha=1,nfolds=10)
lambda17<-cv.lasso17$lambda.1se
fit17<-glmnet(matrixx17, matrixy17,family="gaussian",lambda=lambda17)
testmatrixx17<-as.matrix(newtest17[,2:49])
newtest17[,pred_d:=predict(fit17, s=lambda17, testmatrixx17, type="response")]
newnewtest17<-newtest17[,c("Consumption","pred_d")]
d_mape[18,1]<-mape(newnewtest17$Consumption,newnewtest17$pred_d)
mape(newnewtest17$Consumption,newnewtest17$pred_d)
#Hour 18
newtrain18<-copy(filter(newtraindata,Hour==18))
newtest18<-copy(filter(newtestdata,Hour==18))
matrixy18<-as.matrix(newtrain18[,50])
matrixx18<-as.matrix(newtrain18[,2:49])
cv.lasso18 <- cv.glmnet(matrixx18, matrixy18, alpha=1,nfolds=10)
lambda18<-cv.lasso18$lambda.1se
fit18<-glmnet(matrixx18, matrixy18,family="gaussian",lambda=lambda18)
testmatrixx18<-as.matrix(newtest18[,2:49])
newtest18[,pred_d:=predict(fit18, s=lambda18, testmatrixx18, type="response")]
newnewtest18<-newtest18[,c("Consumption","pred_d")]
d_mape[19,1]<-mape(newnewtest18$Consumption,newnewtest18$pred_d)
mape(newnewtest18$Consumption,newnewtest18$pred_d)
#Hour 19
newtrain19<-copy(filter(newtraindata,Hour==19))
newtest19<-copy(filter(newtestdata,Hour==19))
matrixy19<-as.matrix(newtrain19[,50])
matrixx19<-as.matrix(newtrain19[,2:49])
cv.lasso19 <- cv.glmnet(matrixx19, matrixy19, alpha=1,nfolds=10)
lambda19<-cv.lasso19$lambda.1se
fit19<-glmnet(matrixx19, matrixy19,family="gaussian",lambda=lambda19)
testmatrixx19<-as.matrix(newtest19[,2:49])
newtest19[,pred_d:=predict(fit19, s=lambda19, testmatrixx19, type="response")]
newnewtest19<-newtest19[,c("Consumption","pred_d")]
d_mape[20,1]<-mape(newnewtest19$Consumption,newnewtest19$pred_d)
mape(newnewtest19$Consumption,newnewtest19$pred_d)
#Hour 20
newtrain20<-copy(filter(newtraindata,Hour==20))
newtest20<-copy(filter(newtestdata,Hour==20))
matrixy20<-as.matrix(newtrain20[,50])
matrixx20<-as.matrix(newtrain20[,2:49])
cv.lasso20 <- cv.glmnet(matrixx20, matrixy20, alpha=1,nfolds=10)
lambda20<-cv.lasso20$lambda.1se
fit20<-glmnet(matrixx20, matrixy20,family="gaussian",lambda=lambda20)
testmatrixx20<-as.matrix(newtest20[,2:49])
newtest20[,pred_d:=predict(fit20, s=lambda20, testmatrixx20, type="response")]
newnewtest20<-newtest20[,c("Consumption","pred_d")]
d_mape[21,1]<-mape(newnewtest20$Consumption,newnewtest20$pred_d)
mape(newnewtest20$Consumption,newnewtest20$pred_d)
#Hour 21
newtrain21<-copy(filter(newtraindata,Hour==21))
newtest21<-copy(filter(newtestdata,Hour==21))
matrixy21<-as.matrix(newtrain21[,50])
matrixx21<-as.matrix(newtrain21[,2:49])
cv.lasso21 <- cv.glmnet(matrixx21, matrixy21, alpha=1,nfolds=10)
lambda21<-cv.lasso21$lambda.1se
fit21<-glmnet(matrixx21, matrixy21,family="gaussian",lambda=lambda21)
testmatrixx21<-as.matrix(newtest21[,2:49])
newtest21[,pred_d:=predict(fit21, s=lambda21, testmatrixx21, type="response")]
newnewtest21<-newtest21[,c("Consumption","pred_d")]
d_mape[22,1]<-mape(newnewtest21$Consumption,newnewtest21$pred_d)
mape(newnewtest21$Consumption,newnewtest21$pred_d)
#Hour 22
newtrain22<-copy(filter(newtraindata,Hour==22))
newtest22<-copy(filter(newtestdata,Hour==22))
matrixy22<-as.matrix(newtrain22[,50])
matrixx22<-as.matrix(newtrain22[,2:49])
cv.lasso22 <- cv.glmnet(matrixx22, matrixy22, alpha=1,nfolds=10)
lambda22<-cv.lasso22$lambda.1se
fit22<-glmnet(matrixx22, matrixy22,family="gaussian",lambda=lambda22)
testmatrixx22<-as.matrix(newtest22[,2:49])
newtest22[,pred_d:=predict(fit22, s=lambda22, testmatrixx22, type="response")]
newnewtest22<-newtest22[,c("Consumption","pred_d")]
d_mape[23,1]<-mape(newnewtest22$Consumption,newnewtest22$pred_d)
mape(newnewtest22$Consumption,newnewtest22$pred_d)
#Hour 23
newtrain23<-copy(filter(newtraindata,Hour==23))
newtest23<-copy(filter(newtestdata,Hour==23))
matrixy23<-as.matrix(newtrain23[,50])
matrixx23<-as.matrix(newtrain23[,2:49])
cv.lasso23 <- cv.glmnet(matrixx23, matrixy23, alpha=1,nfolds=10)
lambda23<-cv.lasso23$lambda.1se
fit23<-glmnet(matrixx23, matrixy23,family="gaussian",lambda=lambda23)
testmatrixx23<-as.matrix(newtest23[,2:49])
newtest23[,pred_d:=predict(fit23, s=lambda23, testmatrixx23, type="response")]
newnewtest23<-newtest23[,c("Consumption","pred_d")]
d_mape[24,1]<-mape(newnewtest23$Consumption,newnewtest23$pred_d)
mape(newnewtest23$Consumption,newnewtest23$pred_d)

totaltestdata<-rbind(newnewtest0,newnewtest1,newnewtest2,newnewtest3,newnewtest4,newnewtest5,newnewtest6,newnewtest7,newnewtest8,newnewtest9,newnewtest10,newnewtest11,newnewtest12,newnewtest13,newnewtest14,newnewtest15,newnewtest16,newnewtest17,newnewtest18,newnewtest19,newnewtest20,newnewtest21,newnewtest22,newnewtest23)
tasks_mape[1,5]<-mape(totaltestdata$Consumption,totaltestdata$pred_d)
mape(totaltestdata$Consumption,totaltestdata$pred_d)



#TASK F

combinemapedata<-rbind(a__lag48_mape,a__lag168_mape,b_mape,c_mape,d_mape)
combinemapedata[,variable:="mape_a_48"]
combinemapedata[25:48,2]<-"mape_a_168"
combinemapedata[49:72,2]<-"mape_b"
combinemapedata[73:96,2]<-"mape_c"
combinemapedata[97:120,2]<-"mape_d"


ggplot(combinemapedata, aes(x=variable, y=MAPE,fill=variable)) + 
  geom_boxplot() + ggtitle("Mape Comparison")

tasks_mape













