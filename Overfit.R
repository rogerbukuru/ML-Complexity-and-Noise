par(mfrow=c(1,1))
# Legendre polynomials
Legendre=function(x,n){
	val=0
	for(i in 0:n){
		val=val+((x^i)*choose(n,i)*choose((n+i-1)/2,n))
	}
	return((2^n)*val)
}

# creates random polynomial target functions
rLegfunc=function(x,n){
	val=0
	vec=runif(n,-1,1)
	for(i in 1:n){
		val=val+(Legendre(x,i)*vec[i])
	}
	return(val)
}

#simulates a dataset with target "func" of size n with noise sig
generator=function(n,x,func,sig){
	l<-length(func)
	dat<-matrix(rep(NA,2*n),ncol=n)
	xdat<-floor(runif(n)*l)+1
	ydat<-func[xdat]+rnorm(n,0,sig)
	xdat<-x[xdat]
	Data<-data.frame(xdat,ydat)
	return(Data)
}

#fitted model of the data
fit=function(x,model){
	v=0
	for(i in 1:length(model$coefficient)){
		v=v+(model$coefficient[i]*(x^(i-1)))
	}
return(v)
}

# gives the bias for a given fitted model (DOES NOT ACCOUNT FOR VARIABILITY IN THE DATA)
fdiff=function(x,target,model){
	f=fit(x,model)
	return((t(f-target)%*%(f-target))*(x[2]-x[1]))
}

# Quadratic target fn with an overfitted fourth-order polynomial
x<-seq(-1,1,0.01);n=5;sig=0.5;t=2;
plot(c(-1,1),c(-4,4),main="Random functions",type="n",xlab="x",ylab=expression(f(x)))
func=rLegfunc(x,t)
lines(x,func,type="l",lwd=2,col="blue")
d<-generator(n,x,func,sig)
points(d$xdat,d$ydat)
four<-lm(d$ydat~d$xdat+I(d$xdat^2)+I(d$xdat^3)+I(d$xdat^4))
err.in<-(t(four$residuals)%*%four$residuals)/n # In sample error for the quadratic model
err.out<-fdiff(x,func,four)+(sig^2) # Out of sample error
lines(x,fit(x,four),lty=2,col="red",lwd=0.5)
text1=bquote(italic(E)["in"](x^4)==.(format(err.in,digits=3)))
text2=bquote(italic(E)["out"](x^4)==.(format(err.out,digits=3)))
text(x=-0.5,y=3.7,labels=text1)
text(x=0.,y=3.7,labels=text2)

# 10-th order target with noise
x<-seq(-1,1,0.01);n=15;sig=0.5;t=10;
plot(c(min(x),max(x)),c(-4,4),main="10-th order target + noise",type="n",xlab="x",ylab=expression(f(x)))
func=rLegfunc(x,t)
lines(x,func,type="l",lwd=2)
d<-generator(n,x,func,sig)
points(d$xdat,d$ydat)

# have a 10-th order target. Fit 10 order polynomial or quadratic?
qfit<-lm(d$ydat~d$xdat+I(d$xdat^2))
err.q<-(t(qfit$residuals)%*%qfit$residuals)/n # In sample error for the quadratic model
tfit<-lm(d$ydat~d$xdat+I(d$xdat^2)+I(d$xdat^3)+I(d$xdat^4)+I(d$xdat^5)+I(d$xdat^6)+I(d$xdat^7)+I(d$xdat^8)+I(d$xdat^9)+I(d$xdat^10))
err.t<-(t(tfit$residuals)%*%tfit$residuals)/n # In sample error for the quadratic model
lines(x,fit(x,qfit),type="l",lty=2,col="blue")
lines(x,fit(x,tfit),type="l",lty=2,col="red")
text1=bquote(italic(E)["in"](x^2)==.(format(err.q,digits=3)))
text2=bquote(italic(E)["in"](x^10)==.(format(err.t,digits=3)))
text(x=-0.5,y=3.7,labels=text1)
text(x=-0.5,y=3.2,labels=text2)
err.out.q<-fdiff(x,func,qfit)+(sig^2)
err.out.t<-fdiff(x,func,tfit)+(sig^2)
text3=bquote(italic(E)["out"](x^2)==.(format(err.out.q,digits=2)))
text4=bquote(italic(E)["out"](x^{10})==.(format(err.out.t,digits=2)))
text(x=0.,y=3.7,labels=text3)
text(x=0.,y=3.2,labels=text4)

plot(c(-0.75,0.75),c(-4,4),main="50-th order target + noise",type="n",xlab="x",ylab=expression(f(x)))
x<-seq(-0.75,0.75,0.01)
func=rLegfunc(x,50)
lines(x,func,type="l",col="blue",lwd=2)
d<-generator(20,x,func,0.)
points(d)