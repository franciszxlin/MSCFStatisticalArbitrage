movestand=function(thedata,s1,s2,d1,d2,nday=20){
# Computes the items needed for doing nday-moving standardization
# The stocks are s1 and s2
# The range of dates is d1 to d2 (d1 must be at least nday+1)
	long=log(thedata$bid[d1:d2,s1]/thedata$ask[d1:d2,s2])
	short=log(thedata$ask[d1:d2,s1]/thedata$bid[d1:d2,s2])
	root=log(thedata$price[(d1-nday):d2,s1]/thedata$price[(d1-nday):d2,s2])
	mave=NULL
	msd=NULL
	for(i in 1:(d2-d1+1)){
	      mave[i]=mean(root[i:(i+nday-1)])
	      msd[i]=sqrt(var(root[i:(i+nday-1)]))
	}
# The standardized series are (long-mave)/msd and (short-mave)/msd
	list(long=long,short=short,mave=mave,msd=msd)
}

