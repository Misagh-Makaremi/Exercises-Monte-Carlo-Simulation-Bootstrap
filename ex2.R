###### Prog 1: Momte Carlo Method for xbar  ###########
##### Exponential Dis.
Prog1<-function(n,m,rate){
  xbar=1:m
  mu=1/rate
  sigma=1/rate
  for(i in 1:m){xbar[i]=mean(rexp(n,rate))}
  Montdist=(xbar-mu)/(sigma/sqrt(n))
  hist(Montdist,freq=FALSE,main="Monte Carlo Distribution For n=100 & rate=3",xlab="")
  z=seq(-4,4,0.001)
  f=(1/(sqrt(2*pi)))*exp(-(z^2)/2)
  points(z,f,col=2)
}
Prog1(100,10000,3)