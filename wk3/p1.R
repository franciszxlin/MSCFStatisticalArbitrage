install.packages("ada")
library(ada)
install.packages("rpart")
library(rpart)
#Problem 1
#Getting the data
dates<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\dates.txt"))
qr<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\quickRatio.csv", sep=","))
cr<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\currentRatio.csv", sep=","))
d2a<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\debtAssetRatio.csv", sep=","))
tm<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\totalMargin.csv", sep=","))
s2i<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\salesInventoryRatio.csv", sep=","))
r2e<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\return2equity.csv", sep=","))
r2a<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\return2asset.csv", sep=","))
bm<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\book2market.csv", sep=","))
ep<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\earnings2price.csv", sep=","))
oi2p<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\operating2price.csv", sep=","))
cp<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\cash2price.csv", sep=","))
h52<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\high52.csv", sep=","))
mom1<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\mom1.csv", sep=","))
mom3<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\mom3.csv", sep=","))
mom6<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\mom6.csv", sep=","))
mom9<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\mom9.csv", sep=","))
mom12<-as.matrix(read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p1data\\mom12.csv", sep=","))
#Construct labels
moms=rbind(mom6[-c(1:6),], matrix(NA, 6, ncol(mom6)))
meds=apply(moms, 1, median, na.rm=TRUE)
themeds=matrix(rep(meds, ncol(mom6)), nrow(mom6), ncol(mom6))
labels=moms>themeds
#Part(a)
touse=which(dates==201403)
temp=data.frame(cbind(labels[touse,],
                      qr[touse,],
                      cr[touse,], 
                      d2a[touse,], 
                      tm[touse,], 
                      s2i[touse,], 
                      r2e[touse,], 
                      r2a[touse,], 
                      bm[touse,], 
                      ep[touse,], 
                      oi2p[touse,], 
                      cp[touse,], 
                      h52[touse,], 
                      mom1[touse,],
                      mom3[touse,],
                      mom6[touse,],
                      mom9[touse,], 
                      mom12[touse,]))
cantuse=is.na(temp[,1])
out=ada(X1~X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18, data=temp[!cantuse,], bag.frac=0.5, rpart.control(maxdepth=1,cp=-1,minsplit=0))
#Get prediction data frame
topred=touse+6
temp=data.frame(cbind(labels[topred,],
                      qr[topred,],
                      cr[topred,], 
                      d2a[topred,], 
                      tm[topred,], 
                      s2i[topred,], 
                      r2e[topred,], 
                      r2a[topred,], 
                      bm[topred,], 
                      ep[topred,], 
                      oi2p[topred,], 
                      cp[topred,], 
                      h52[topred,], 
                      mom1[topred,],
                      mom3[topred,],
                      mom6[topred,],
                      mom9[topred,], 
                      mom12[topred,]))
pred=predict(out, newdata=temp, type="F")
num_select=sum(!cantuse)
num_select
num_pred=length(pred)
num_pred
sum(is.na(pred))
dates[topred]
dates[touse]
#Part (b)
nonmiss=!is.na(mom6[topred+6,])
plot(pred[nonmiss], mom6[topred+6, nonmiss], pch=".", xlab="Prediction", ylab="Return")
lp=min(mom6[topred+6, nonmiss])
up=max(mom6[topred+6, nonmiss])
quants=quantile(pred[nonmiss], probs=c(0.3, 0.7), na.rm=TRUE)
lines(rep(quants[1],2), c(lp, up))
lines(rep(quants[2],2), c(lp, up))
lines(c(min(pred), max(pred)), c(0,0))
abline(0,1)
#Part (c)
bottom30=(pred<quants[1])
top30=(pred>quants[2])
avg_return=mean(mom6[topred+6, nonmiss&bottom30])
avg_return
avg_return1=mean(mom6[topred+6, nonmiss&top30])
avg_return1
#Part (d), (e), (f) are just changing parameters
