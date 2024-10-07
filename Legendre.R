# Legendre polynomials
Legendre=function(x,n){
	val=0
	for(i in 0:n){
		val=val+((x^i)*choose(n,i)*choose((n+i-1)/2,n))
	}
	return((2^n)*val)
}

x<-seq(-1,1,0.01)
plot(x,Legendre(x,1),type="l",ylab=expression(L[n](x)),col="blue")
lines(x,Legendre(x,0),type="l",col="black")
lines(x,Legendre(x,2),type="l",col="green")
lines(x,Legendre(x,3),type="l",col="red")
lines(x,Legendre(x,4),type="l",col="orange")


# creates random polynomial target functions
rLegfunc=function(x,n){
	val=0
	vec=runif(n,-1,1)
	for(i in 1:n){
		val=val+(Legendre(x,i)*vec[i])
	}
	return(val)
}

x<-seq(-1,1,0.01)
plot(x,1.1*x,type="n",ylab=expression(f(x)))
lines(x,rLegfunc(x,20),type="l")
lines(x,rPolyfunc(x,20),type="l")
lines(x,rLegfunc(x,3),col="blue")
lines(x,rLegfunc(x,4),col="red")
lines(x,rLegfunc(x,5),col="green")

rPolyfunc=function(x,n){
  val=0
  vec=runif(n,-1,1)
  for(i in 1:n){
    val=val+(x^i*vec[i])
  }
  return(val)
}

x<-seq(-1,1,0.01)
plot(x,1.5*x,type="n",ylab=expression(f(x)))
lines(x,rPolyfunc(x,2),type="l")
lines(x,rPolyfunc(x,3),col="blue")
lines(x,rPolyfunc(x,4),col="red")
lines(x,rPolyfunc(x,5),col="green")
