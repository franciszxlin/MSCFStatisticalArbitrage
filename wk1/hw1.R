# Problem 1
hw1dt<-read.csv(file='C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk1\\hw1.csv', header=TRUE, sep=',')
# (a)
nrow(hw1dt)
ncol(hw1dt)
colnames(hw1dt)
# (b)
summary(hw1dt$TICKER)
summary(hw1dt$CUSIP)
# (c)
summary(hw1dt$COMNAM)
# (d)
unique(hw1dt$CFACPR)
sum(is.na(hw1dt$PRC))
sum(is.na(hw1dt$CFACPR))
hw1dt$PRCadj<-hw1dt$PRC/hw1dt$CFACPR
hw1dt[1,]
# (e)
sum(is.na(hw1dt$PRCadj))
# (f)
length(unique(hw1dt$date))
# (g)
sel_bool<-hw1dt$date>=20140101 & hw1dt$date<=20141231 & hw1dt$PRC<0
hw1dt[sel_bool,]
hw1dt$PRC<-abs(hw1dt$PRC)
hw1dt$PRCadj<-abs(hw1dt$PRCadj)
# (h) 
sel_bool2<- is.na(hw1dt$BID) | is.na(hw1dt$ASK) | (hw1dt$BID > hw1dt$ASK)
hw1dt[sel_bool2,]
# (i)
dflist<-split(hw1dt, hw1dt$CUSIP)
bby<-dflist$`8651610`
mdsy<-dflist$M7037810
b<-which(bby$date>=20010101 & bby$date<=20010131)
e<-which(bby$date>=20150601 & bby$date<=20150630)
bby$momentum<-numeric(length(bby$PRCadj))
mdsy$momentum<-numeric(length(mdsy$PRCadj))
for (i in b:e)
{
  bby$momentum[i]<-bby$PRCadj[i-1]/bby$PRCadj[i-4]-1
  mdsy$momentum[i]<-mdsy$PRCadj[i-1]/mdsy$PRCadj[i-4]-1
}
# (j)
dates<-as.Date(as.character(bby$date[b:e]),"%Y%m%d")
require(xts)
bbyts<-xts(bby$momentum[b:e],order.by = dates)
mdsyts<-xts(mdsy$momentum[b:e],order.by = dates)
plot(bbyts, main="BBY: Monthly Time Series of Momentum for January 2001 through June 2015")
plot(mdsyts, main="MDSY: Monthly Time Series of Momentum for January 2001 through June 2015")
# (k) 
bby<-bby[b:e,]
mdsy<-mdsy[b:e,]
write.csv(bby,file='C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk1\\bby.csv')
write.csv(mdsy,file='C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk1\\mdsy.csv')

# Problem 2 
hw1dt2<-read.csv(file='C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk1\\hw1comp.csv', header=TRUE, sep=',')
# (a)
summary(hw1dt$CUSIP)
summary(hw1dt2$cusip)
# (b)
sel_bool3<-hw1dt2$datadate>=20010101 & hw1dt2$datadate<=20030101
hw1dt2[sel_bool3,]
# (c)
dflist2<-split(hw1dt2, hw1dt2$cusip)
bbycomp<-dflist2$`86516101`
mdsycomp<-dflist2$M70378100
# BBY join:
nrow(bby)
bbycomp<-bbycomp[rep(seq_len(nrow(bbycomp)), each=3),]
bbycomp<-bbycomp[2:(nrow(bbycomp)-2),]
nrow(bbycomp)
head(bby)
head(bbycomp)
tail(bby)
tail(bbycomp)
bbyfinal<-cbind(bby,bbycomp)
write.csv(bbyfinal,file='C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk1\\bbyfinal.csv')
# MDSY join: 
nrow(mdsy)
mdsycomp<-mdsycomp[rep(seq_len(nrow(mdsycomp)), each=3),]
mdsycomp<-mdsycomp[1:(nrow(mdsycomp)-3),]
nrow(mdsycomp)
head(mdsy)
head(mdsycomp)
tail(mdsy)
tail(mdsycomp)
mdsyfinal<-cbind(mdsy,mdsycomp)
write.csv(mdsyfinal,file='C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk1\\mdsyfinal.csv')
# print record
print(bbyfinal[nrow(bbyfinal),])
print(mdsyfinal[nrow(bbyfinal),])
# (d)
bbyfinal$e2p<-(bbyfinal$epspxq*bbyfinal$cshprq)/(bbyfinal$PRC*bbyfinal$SHROUT)
mdsyfinal$e2p<-(mdsyfinal$epspxq*mdsyfinal$cshprq)/(mdsyfinal$PRC*mdsyfinal$SHROUT)
dates<-as.Date(as.character(bbyfinal$date),"%Y%m%d")
require(xts)
bbyepts<-xts(bbyfinal$e2p,order.by = dates)
mdsyepts<-xts(mdsyfinal$e2p,order.by = dates)
plot(bbyepts, main="BBY: Monthly Time Series of Earning-to-Price Ratio for January 2001 through June 2015")
plot(mdsyepts, main="MDSYF: Monthly Time Series of Earning-to-Price Ratio for January 2001 through June 2015")

