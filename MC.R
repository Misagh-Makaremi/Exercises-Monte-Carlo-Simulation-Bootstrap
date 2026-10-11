########## Monte Carlo Integrate ########## 

## Ex1
f<-function(x){x*exp(x)}
integrate(f,0,5)

u<-runif(100000)
mean(u*exp(u))

## Ex2
f<-function(x){x^4}
integrate(f,0,5)

u<-runif(100000)
mean(u^4)

## Ex3
f<-function(x){x^2}
integrate(f,-3,5)

u<-runif(100000,-3,5)
mean(u^2)*8

## Ex4
integrate(sin,2,5)

u<-runif(100000,2,5)
mean(sin(u))*(5-2)



## Ex5
f<-function(x){sin(x)/(x^2)}

e<-rexp(100000)
mean(sin(e)/(e^2*exp(-e)))



u<-runif(100000,1,7)
v<-runif(100000,3,10)
mean(sin(u-v))*6*7



pnorm(1.96)

x<-rnorm(100000)
mean(x<1.96)
