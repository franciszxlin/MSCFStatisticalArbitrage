#Problem 2
#Import data
dates<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p2data\\dates.txt")
ticker<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p2data\\ticker.txt")
cusip<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p2data\\cusip.txt")
price<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p2data\\price.csv",sep=",")
bid<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p2data\\bid.csv", sep=",")
ask<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p2data\\ask.csv", sep=",")
tbtO<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p2data\\TBToptions.csv", sep=",", header=TRUE)
tbfO<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk3\\p2data\\TBFoptions.csv", sep=",", header=TRUE)
dates=as.matrix(dates)
ticker=as.matrix(ticker)
cusip=as.matrix(cusip)
price=as.matrix(price)
bid=as.matrix(bid)
ask=as.matrix(ask)
#Part (a)
use=(dates>=20130101)&(dates<=20130331)
difflogprice=diff(log(price[use,]))
correlations=cor(difflogprice)
sorted_cor=unique(sort(correlations, decreasing = TRUE))[2:4]
max1=which(correlations==sorted_cor[1], arr.ind = TRUE)
max2=which(correlations==sorted_cor[2], arr.ind = TRUE)
max3=which(correlations==sorted_cor[3], arr.ind = TRUE)
ticker[c(max1[1,1], max1[1,2]),]
ticker[c(max2[1,1], max2[1,2]),]
ticker[c(max3[1,1], max3[1,2]),] 
sorted_cor
#Part (b) See write up
#Part (c)
#(i)
tbt_i=which(ticker=="TBT")
tbf_i=which(ticker=="TBF")
two=c(tbt_i, tbf_i)
logprice=log(price[use, c(tbt_i, tbf_i)])
tbt_tbf_cor=cor(logprice)
tbt_tbf_cor
#(ii)
#The time cutoffs
start=which(dates==20130401)
end=which(dates==20130830)
#(iii)
movestand=function(s1,s2,d1,d2,nday=20){
  # Computes the items needed for doing nday-moving standardization
  # The stocks are s1 and s2
  # The range of dates is d1 to d2 (d1 must be at least nday+1)
  long=log(bid[d1:d2,s1]/ask[d1:d2,s2])
  short=log(ask[d1:d2,s1]/bid[d1:d2,s2])
  root=log(price[(d1-nday):d2,s1]/price[(d1-nday):d2,s2])
  mave=NULL
  msd=NULL
  for(i in 1:(d2-d1+1)){
    mave[i]=mean(root[i:(i+nday-1)])
    msd[i]=sqrt(var(root[i:(i+nday-1)]))
  }
  # The standardized series are (long-mave)/msd and (short-mave)/msd
  list(long=long,short=short,mave=mave,msd=msd)
}

test=movestand(tbt_i, tbf_i, start, end)

#Allocations
logratios=matrix(0, nrow=(end-start+1), ncol=3)
for (i in start:end)
{
  logratios[i-start+1,1]=dates[i]
  logratios[i-start+1,2]=(test$long[i-start+1]-test$mave[i-start+1])/test$msd[i-start+1]
  logratios[i-start+1,3]=(test$short[i-start+1]-test$mave[i-start+1])/test$msd[i-start+1]
}
plot(as.Date(as.character(logratios[,1]), format="%Y%m%d"), logratios[,2], xlab="Date", ylab="Log Ratios", main="Time Series of Log Ratios (Long)", type="l")
abline(a=0, b=0)
abline(a=-2, b=0)
abline(a=-3.5, b=0)
abline(a=2, b=0)
abline(a=3.5, b=0)
plot(as.Date(as.character(logratios[,1]), format="%Y%m%d"), logratios[,3], xlab="Date", ylab="Log Ratios", main="Time Series of Log Ratios (Short)", type="l")
abline(a=0, b=0)
abline(a=2, b=0)
abline(a=3.5, b=0)
abline(a=-2, b=0)
abline(a=-3.5, b=0)
# (iv)
logratios[logratios[,3]>2,]
open1=which(dates==20130801)
freeze_avg1=test$mave[open1-start+1]
freeze_std1=test$msd[open1-start+1]
oppo_ratios1=matrix(0, nrow=(end-open1+1), ncol=2)
for (i in open1:end)
{
  oppo_ratios1[i-open1+1, 1]=dates[i]
  oppo_ratios1[i-open1+1, 2]=(log(ask[i,tbt_i]/bid[i,tbf_i])-freeze_avg1)/freeze_std1
}
oppo_ratios1
#(d)
nl=0.03271181
ns=0.0169837
values=c()
date=which(dates==20130513)
end=which(dates==20131220)
for (i in date:end) {
  temp = toupper(format(as.Date(as.character(dates[i]), format="%Y%m%d"), "%d%h%Y"))
  sel1=(as.character(tbfO$date)==temp)&(as.character(tbfO$cp_flag)=="P")&(tbfO$strike_price<=(29.42*1000))&(tbfO$strike_price>=(28.42*1000))&(tbfO$optionid==100856073)
  put=tbfO[sel1,]
  put
  sel2=(as.character(tbtO$date)==temp)&(as.character(tbtO$cp_flag)=="C")&(tbtO$strike_price>=(64.52*1000))&(tbtO$strike_price<=(65.42*1000))&(tbtO$optionid==100856122)
  call=tbtO[sel2,]
  call
  #partii
  value=nl*max((bid[i ,tbf_i]-put$best_bid), put$strike_price/1000)-ns*min((ask[i ,tbt_i]-call$best_bid), call$strike_price/1000)
  values=c(values, value)
}
values
plot(values[1:78], type="l", xlab="Date", ylab="Value", main="Value of the hedged position")
abline(a=0,b=0)
values[78]
