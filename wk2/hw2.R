# Import data
dates<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\dates.txt")
bid1<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\bid1.csv", sep=',')
bid2<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\bid2.csv", sep=',')
ask1<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\ask1.csv", sep=',')
ask2<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\ask2.csv", sep=',')
price<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\price.csv", sep=',')
marketcap<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\marketcap.csv", sep=',')
book2market<-read.table("C:\\Users\\zil20\\Desktop\\Statistical Arbitrage\\wk2\\data\\book2market.csv", sep=',')

length(dates$V1)
traincutoff<-which(dates$V1==200710)
endd<-nrow(price)

strategy<-function(x, y, z, w, v)
{
    # Step 1
    use=!is.na(book2market)
    rsprd=abs(ask1-bid1)/price
    use=use&(!is.na(rsprd))
    use=use&(rsprd<=x)
    use=use&(bid1>0)
    use=use&(ask1>0)
    use=use&(price>=v)
    marketcap[!use]=NA
    test.m=apply(marketcap, 1, quantile, probs=y, na.rm=TRUE)
    use=use&(marketcap>=test.m)&(!is.na(marketcap))
    print(paste("First formation period has",as.character(sum(use[1,])), "stocks"))
    
    # Step 2
    book2market[!use]=NA
    test.h=apply(book2market, 1, quantile, probs=c(z,1-z),na.rm=TRUE)
    short=(book2market<=test.h[1,])&use&(bid2>0)
    long=(book2market>=test.h[2,])&use&(ask2>0)
  
    # Step 3
    n=0
    m=0
    i=1
    while ((n<w)|(m<w))
    {
      n=length(which(long[i,]))
      m=length(which(short[i,]))
      i=i+1
    }
    # After the loop stop, i points to the month after the first holding period for which we open trades
    print(dates$V1[i-1])
    print(dates$V1[i-1+1])
    print(dates$V1[i-1+6])
    
    # Step 4
    ndays=traincutoff 
    returns=NULL 
    for(i in (ndays+1):(endd-6))
    { 
      port.short=which(short[i,]) 
      port.long=which(long[i,]) 
      if(min(c(length(port.short),length(port.long)))<w) next 
      returns[i]=mean(abs(as.numeric(bid1[i+6,port.long]))/as.numeric(ask2[i,port.long])-1)+ mean(1-abs(as.numeric(ask1[i+6,port.short]))/as.numeric(bid2[i,port.short])) 
    }
    print(min(returns, na.rm=TRUE))
    print(max(returns, na.rm=TRUE))
    print(mean(returns, na.rm=TRUE))
}
# set 1 
strategy(0.002, 0.7, 0.1, 10, 2)

# set 2
strategy(0.003, 0.7, 0.1, 10, 2)

# set 3
strategy(0.002, 0.8, 0.1, 10, 2)

# set 4
strategy(0.002, 0.7, 0.2, 10, 2)

# set 5
strategy(0.002, 0.7, 0.1, 20, 2)

# set 6
strategy(0.002, 0.7, 0.1, 10, 0)

