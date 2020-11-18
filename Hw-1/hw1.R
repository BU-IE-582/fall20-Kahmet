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

setwd("C:/Users/kadir/OneDrive/Masaüstü")

pl2018<-read.csv("E0 (3).csv", sep = ",")
pl2019<-read.csv("E0 (2).csv", sep = ",")
pl2020<-read.csv("E0 (4).csv", sep = ",")
newpl2018<-pl2018[,1:56]
newpl2019<-pl2019[,1:56]
newpl2020<-pl2020[,1:56]

newpl2018<-data.table(newpl2018)
newpl2019<-data.table(newpl2019)
newpl2020<-data.table(newpl2020)



Homescore<-1:828
Homescore[1:380]<-newpl2018$FTHG
Homescore[381:760]<-newpl2019$FTHG
Homescore[761:828]<-newpl2020$FTHG

Awayscore<-1:828
Awayscore[1:380]<-newpl2018$FTAG
Awayscore[381:760]<-newpl2019$FTAG
Awayscore[761:828]<-newpl2020$FTAG

Diff<-1:828
Diff<-Homescore-Awayscore

sort(unique(Homescore)) 
homebreaks<-0:9-0.5 

sort(unique(Awayscore)) 
awaybreaks<-0:10-0.5 

sort(unique(Diff)) 
diffbreaks<--9:9-0.5 



hist(Homescore, breaks=homebreaks,xlab="Home Goals",
     ylab="Number of Games",
     col="darkmagenta",
)


hist(Awayscore,breaks=awaybreaks, xlab="Away Goals",
     ylab="Number of Games",
     col="darkmagenta",
)


hist(Diff,breaks=diffbreaks, xlab="Home goals – Away Goals",
     ylab="Number of Games",
     col="darkmagenta",
)


homerate<-mean(Homescore) 
awayrate<-mean(Awayscore) 
homex<-sort(unique(Homescore)) 
awayx<-sort(unique(Awayscore)) 


hist(Homescore,breaks=homebreaks, xlab="Home Goals",
     ylab="Number of Games",
     col="gray",
)
points(homex,dpois(homex,homerate)*828,type="b",col="blue") 

hist(Awayscore,breaks=awaybreaks, xlab="Away Goals",
     ylab="Number of Games",
     col="gray",
)

points(awayx,dpois(awayx,awayrate)*828,type="b",col="blue") 


newpl2018[,MatchResult:=ifelse(FTHG>FTAG,'home',ifelse(FTHG==FTAG,'draw','away'))]
newpl2019[,MatchResult:=ifelse(FTHG>FTAG,'home',ifelse(FTHG==FTAG,'draw','away'))]
newpl2020[,MatchResult:=ifelse(FTHG>FTAG,'home',ifelse(FTHG==FTAG,'draw','away'))]

Result<-1:828
Result[1:380]<-newpl2018$MatchResult
Result[381:760]<-newpl2019$MatchResult
Result[761:828]<-newpl2020$MatchResult

RedCardH<-1:828
RedCardH[1:380]<-newpl2018$HR
RedCardH[381:760]<-newpl2019$HR
RedCardH[761:828]<-newpl2020$HR


RedCardA<-1:828
RedCardA[1:380]<-newpl2018$AR
RedCardA[381:760]<-newpl2019$AR
RedCardA[761:828]<-newpl2020$AR

Bet365H.odd1<-1:828
Bet365H.odd1[1:380]<-newpl2018$B365H
Bet365H.odd1[381:760]<-newpl2019$B365H
Bet365H.odd1[761:828]<-newpl2020$B365H

Bet365D.oddx<-1:828
Bet365D.oddx[1:380]<-newpl2018$B365D
Bet365D.oddx[381:760]<-newpl2019$B365D
Bet365D.oddx[761:828]<-newpl2020$B365D

Bet365A.odd2<-1:828
Bet365A.odd2[1:380]<-newpl2018$B365A
Bet365A.odd2[381:760]<-newpl2019$B365A
Bet365A.odd2[761:828]<-newpl2020$B365A

Bet365H.ph<-1:828
Bet365H.ph[1:380]<-1/newpl2018$B365H
Bet365H.ph[381:760]<-1/newpl2019$B365H
Bet365H.ph[761:828]<-1/newpl2020$B365H

Bet365D.px<-1:828
Bet365D.px[1:380]<-1/newpl2018$B365D
Bet365D.px[381:760]<-1/newpl2019$B365D
Bet365D.px[761:828]<-1/newpl2020$B365D

Bet365A.pa<-1:828
Bet365A.pa[1:380]<-1/newpl2018$B365A
Bet365A.pa[381:760]<-1/newpl2019$B365A
Bet365A.pa[761:828]<-1/newpl2020$B365A

NormBet365H<-Bet365H.ph/(Bet365H.ph+Bet365D.px+Bet365A.pa)
NormBet365D<-Bet365D.px/(Bet365H.ph+Bet365D.px+Bet365A.pa)
NormBet365A<-Bet365A.pa/(Bet365H.ph+Bet365D.px+Bet365A.pa)


Bet365<-data.table(Homescore,Awayscore,Result,Bet365H.odd1,Bet365A.odd2,Bet365D.oddx,Bet365H.ph,Bet365A.pa,Bet365D.px,NormBet365H,NormBet365A,NormBet365D,RedCardH,RedCardA)

Bet365[,diff:=NormBet365H-NormBet365A]

br<-(-5:5)/5
bucketBet365<-cut(Bet365$diff,breaks=br,labels=1:10)
Bet365[,bucket:=bucketBet365]

plot(Bet365$NormBet365H-Bet365$NormBet365A,Bet365$NormBet365D,col="green",main="Bet365",xlab="P(home win) - P(away win)",ylab="P(tie)")

plot(Bet365$NormBet365H-Bet365$NormBet365A,Bet365$NormBet365D,col="green",main="Bet365",xlab="P(home win) - P(away win)",ylab="P(tie)")

bet365draws<-1:10
 for(i in 1:10)
     bet365draws[i]<-sum(Bet365[bucket==i]$Result=="draw",na.rm=TRUE)


bet365sum<-1:10
 for(i in 1:10)
     bet365sum[i]<-sum(!is.na(Bet365[bucket==i]$Result))


bet365p<-bet365draws/bet365sum
y<-c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
points(y,bet365p,pch=3,cex = 5)


    
    
    
BWH.odd1<-1:828
BWH.odd1[1:380]<-newpl2018$BWH
BWH.odd1[381:760]<-newpl2019$BWH
BWH.odd1[761:828]<-newpl2020$BWH

BWD.oddx<-1:828
BWD.oddx[1:380]<-newpl2018$BWD
BWD.oddx[381:760]<-newpl2019$BWD
BWD.oddx[761:828]<-newpl2020$BWD

BWA.odd2<-1:828
BWA.odd2[1:380]<-newpl2018$BWA
BWA.odd2[381:760]<-newpl2019$BWA
BWA.odd2[761:828]<-newpl2020$BWA

BWH.ph<-1:828
BWH.ph[1:380]<-1/newpl2018$BWH
BWH.ph[381:760]<-1/newpl2019$BWH
BWH.ph[761:828]<-1/newpl2020$BWH

BWD.px<-1:828
BWD.px[1:380]<-1/newpl2018$BWD
BWD.px[381:760]<-1/newpl2019$BWD
BWD.px[761:828]<-1/newpl2020$BWD

BWA.pa<-1:828
BWA.pa[1:380]<-1/newpl2018$BWA
BWA.pa[381:760]<-1/newpl2019$BWA
BWA.pa[761:828]<-1/newpl2020$BWA

NormBWH<-BWH.ph/(BWH.ph+BWD.px+BWA.pa)
NormBWD<-BWD.px/(BWH.ph+BWD.px+BWA.pa)
NormBWA<-BWA.pa/(BWH.ph+BWD.px+BWA.pa)


BW<-data.table(Homescore,Awayscore,Result,BWH.odd1,BWA.odd2,BWD.oddx,BWH.ph,BWA.pa,BWD.px,NormBWH,NormBWA,NormBWD,RedCardH,RedCardA)

BW[,diff:=NormBWH-NormBWA]

br<-(-5:5)/5
bucketBW<-cut(BW$diff,breaks=br,labels=1:10)
BW[,bucket:=bucketBW]

plot(BW$NormBWH-BW$NormBWA,BW$NormBWD,col="red",main="BetAndWin",xlab="P(home win) - P(away win)",ylab="P(tie)")
plot(BW$NormBWH-BW$NormBWA,BW$NormBWD,col="red",main="BetAndWin",xlab="P(home win) - P(away win)",ylab="P(tie)")


bwdraws<-1:10
for(i in 1:10)
    bwdraws[i]<-sum(BW[bucket==i]$Result=="draw",na.rm=TRUE)


bwsum<-1:10
for(i in 1:10)
    bwsum[i]<-sum(!is.na(BW[bucket==i]$Result))


bwp<-bwdraws/bwsum
y<-c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
points(y,bwp,pch=3,cex = 5)




PSH.odd1<-1:828
PSH.odd1[1:380]<-newpl2018$PSH
PSH.odd1[381:760]<-newpl2019$PSH
PSH.odd1[761:828]<-newpl2020$PSH

PSD.oddx<-1:828
PSD.oddx[1:380]<-newpl2018$PSD
PSD.oddx[381:760]<-newpl2019$PSD
PSD.oddx[761:828]<-newpl2020$PSD

PSA.odd2<-1:828
PSA.odd2[1:380]<-newpl2018$PSA
PSA.odd2[381:760]<-newpl2019$PSA
PSA.odd2[761:828]<-newpl2020$PSA

PSH.ph<-1:828
PSH.ph[1:380]<-1/newpl2018$PSH
PSH.ph[381:760]<-1/newpl2019$PSH
PSH.ph[761:828]<-1/newpl2020$PSH

PSD.px<-1:828
PSD.px[1:380]<-1/newpl2018$PSD
PSD.px[381:760]<-1/newpl2019$PSD
PSD.px[761:828]<-1/newpl2020$PSD

PSA.pa<-1:828
PSA.pa[1:380]<-1/newpl2018$PSA
PSA.pa[381:760]<-1/newpl2019$PSA
PSA.pa[761:828]<-1/newpl2020$PSA

NormPSH<-PSH.ph/(PSH.ph+PSD.px+PSA.pa)
NormPSD<-PSD.px/(PSH.ph+PSD.px+PSA.pa)
NormPSA<-PSA.pa/(PSH.ph+PSD.px+PSA.pa)


PS<-data.table(Homescore,Awayscore,Result,PSH.odd1,PSA.odd2,PSD.oddx,PSH.ph,PSA.pa,PSD.px,NormPSH,NormPSA,NormPSD,RedCardH,RedCardA)

PS[,diff:=NormPSH-NormPSA]

br<-(-5:5)/5
bucketPS<-cut(PS$diff,breaks=br,labels=1:10)
PS[,bucket:=bucketPS]

plot(PS$NormPSH-PS$NormPSA,PS$NormPSD,col="blue",main="Pinnacle",xlab="P(home win) - P(away win)",ylab="P(tie)")
plot(PS$NormPSH-PS$NormPSA,PS$NormPSD,col="blue",main="Pinnacle",xlab="P(home win) - P(away win)",ylab="P(tie)")

psdraws<-1:10
for(i in 1:10)
    psdraws[i]<-sum(PS[bucket==i]$Result=="draw",na.rm=TRUE)


pssum<-1:10
for(i in 1:10)
    pssum[i]<-sum(!is.na(PS[bucket==i]$Result))


psp<-psdraws/pssum
y<-c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
points(y,psp,pch=3,cex = 5)



WHH.odd1<-1:828
WHH.odd1[1:380]<-newpl2018$WHH
WHH.odd1[381:760]<-newpl2019$WHH
WHH.odd1[761:828]<-newpl2020$WHH

WHD.oddx<-1:828
WHD.oddx[1:380]<-newpl2018$WHD
WHD.oddx[381:760]<-newpl2019$WHD
WHD.oddx[761:828]<-newpl2020$WHD

WHA.odd2<-1:828
WHA.odd2[1:380]<-newpl2018$WHA
WHA.odd2[381:760]<-newpl2019$WHA
WHA.odd2[761:828]<-newpl2020$WHA

WHH.ph<-1:828
WHH.ph[1:380]<-1/newpl2018$WHH
WHH.ph[381:760]<-1/newpl2019$WHH
WHH.ph[761:828]<-1/newpl2020$WHH

WHD.px<-1:828
WHD.px[1:380]<-1/newpl2018$WHD
WHD.px[381:760]<-1/newpl2019$WHD
WHD.px[761:828]<-1/newpl2020$WHD

WHA.pa<-1:828
WHA.pa[1:380]<-1/newpl2018$WHA
WHA.pa[381:760]<-1/newpl2019$WHA
WHA.pa[761:828]<-1/newpl2020$WHA

NormWHH<-WHH.ph/(WHH.ph+WHD.px+WHA.pa)
NormWHD<-WHD.px/(WHH.ph+WHD.px+WHA.pa)
NormWHA<-WHA.pa/(WHH.ph+WHD.px+WHA.pa)


WH<-data.table(Homescore,Awayscore,Result,WHH.odd1,WHA.odd2,WHD.oddx,WHH.ph,WHA.pa,WHD.px,NormWHH,NormWHA,NormWHD,RedCardH,RedCardA)

WH[,diff:=NormPSH-NormWHA]

br<-(-5:5)/5
bucketWH<-cut(WH$diff,breaks=br,labels=1:10)
WH[,bucket:=bucketWH]

plot(WH$NormWHH-WH$NormWHA,WH$NormWHD,col="purple",main="WH",xlab="P(home win) - P(away win)",ylab="P(tie)")
plot(WH$NormWHH-WH$NormWHA,WH$NormWHD,col="purple",main="WH",xlab="P(home win) - P(away win)",ylab="P(tie)")


whdraws<-1:10
for(i in 1:10)
    whdraws[i]<-sum(WH[bucket==i]$Result=="draw",na.rm=TRUE)


whsum<-1:10
for(i in 1:10)
    whsum[i]<-sum(!is.na(WH[bucket==i]$Result))


whp<-whdraws/whsum
y<-c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
points(y,whp,pch=3,cex = 5)

updatedBet365<-filter(Bet365,RedCardH==0 & RedCardA==0)

br<-(-5:5)/5
bucketupdatedBet365<-cut(updatedBet365$diff,breaks=br,labels=1:10)
updatedBet365[,bucket:=bucketupdatedBet365]

plot(updatedBet365$NormBet365H-updatedBet365$NormBet365A,updatedBet365$NormBet365D,col="green",main="Bet365",xlab="P(home win) - P(away win)",ylab="P(tie)")
plot(updatedBet365$NormBet365H-updatedBet365$NormBet365A,updatedBet365$NormBet365D,col="green",main="Bet365",xlab="P(home win) - P(away win)",ylab="P(tie)")



ubet365draws<-1:10
for(i in 1:10)
    ubet365draws[i]<-sum(updatedBet365[bucket==i]$Result=="draw",na.rm=TRUE)


ubet365sum<-1:10
for(i in 1:10)
    ubet365sum[i]<-sum(!is.na(updatedBet365[bucket==i]$Result))


ubet365p<-ubet365draws/ubet365sum
y<-c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
points(y,ubet365p,pch=3,cex = 5)



updatedBW<-filter(BW,RedCardH==0 & RedCardA==0)

br<-(-5:5)/5
bucketupdatedBW<-cut(updatedBW$diff,breaks=br,labels=1:10)
updatedBW[,bucket:=bucketupdatedBW]

plot(updatedBW$NormBWH-updatedBW$NormBWA,updatedBW$NormBWD,col="red",main="BetAndWin",xlab="P(home win) - P(away win)",ylab="P(tie)")
plot(updatedBW$NormBWH-updatedBW$NormBWA,updatedBW$NormBWD,col="red",main="BetAndWin",xlab="P(home win) - P(away win)",ylab="P(tie)")

updatedbwdraws<-1:10
for(i in 1:10)
    updatedbwdraws[i]<-sum(updatedBW[bucket==i]$Result=="draw",na.rm=TRUE)


updatedbwsum<-1:10
for(i in 1:10)
    updatedbwsum[i]<-sum(!is.na(updatedBW[bucket==i]$Result))


updatedbwp<-updatedbwdraws/updatedbwsum
y<-c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
points(y,updatedbwp,pch=3,cex = 5)

updatedPS<-filter(PS,RedCardH==0 & RedCardA==0)

br<-(-5:5)/5
bucketupdatedPS<-cut(updatedPS$diff,breaks=br,labels=1:10)
updatedPS[,bucket:=bucketupdatedPS]

plot(updatedPS$NormPSH-updatedPS$NormPSA,updatedPS$NormPSD,col="blue",main="Pinnacle",xlab="P(home win) - P(away win)",ylab="P(tie)")
plot(updatedPS$NormPSH-updatedPS$NormPSA,updatedPS$NormPSD,col="blue",main="Pinnacle",xlab="P(home win) - P(away win)",ylab="P(tie)")

updatedpsdraws<-1:10
for(i in 1:10)
    updatedpsdraws[i]<-sum(updatedPS[bucket==i]$Result=="draw",na.rm=TRUE)


updatedpssum<-1:10
for(i in 1:10)
    updatedpssum[i]<-sum(!is.na(PS[bucket==i]$Result))


updatedpsp<-updatedpsdraws/updatedpssum
y<-c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
points(y,updatedpsp,pch=3,cex = 5)


updatedWH<-filter(WH,RedCardH==0 & RedCardA==0)

br<-(-5:5)/5
bucketupdatedWH<-cut(updatedWH$diff,breaks=br,labels=1:10)
updatedWH[,bucket:=bucketupdatedWH]

plot(updatedWH$NormWHH-updatedWH$NormWHA,updatedWH$NormWHD,col="purple",main="WH",xlab="P(home win) - P(away win)",ylab="P(tie)")
plot(updatedWH$NormWHH-updatedWH$NormWHA,updatedWH$NormWHD,col="purple",main="WH",xlab="P(home win) - P(away win)",ylab="P(tie)")



updatedwhdraws<-1:10
for(i in 1:10)
    updatedwhdraws[i]<-sum(updatedWH[bucket==i]$Result=="draw",na.rm=TRUE)


updatedwhsum<-1:10
for(i in 1:10)
    updatedwhsum[i]<-sum(!is.na(updatedWH[bucket==i]$Result))


updatedwhp<-updatedwhdraws/updatedwhsum
y<-c(-0.9,-0.7,-0.5,-0.3,-0.1,0.1,0.3,0.5,0.7,0.9)
points(y,updatedwhp,pch=3,cex = 5)



