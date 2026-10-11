##### Ex3-1: Distribution of Estimators based on MC Simulation
Exam2<-function(n,m,rate){
  sval=1:m
  for(i in 1:m){sval[i]=rate(rexp(n,1))}
  hist(sval,probability=TRUE,main="Monte Carlo Distribution of stat")
  lines(density(sval),col="red")
  return(c(summary(sval),var(sval),quantile(sval,c(0.025,0.975))))
}

Exam2(10,10000,mean)
Exam2(10,10000,var)
Exam2(10,10000,sd)
Exam2(10,10000,median)
Exam2(10,10000,quantile)
Exam2(10,10000,min)
Exam2(10,10000,max)
fun<-function(x){max(x)-min(x)}
Exam2(10,10000,fun)
fun<-function(x){sd(x)/mean(x)}
Exam2(10,10000,fun)
Exam2(10,10000,sum)

Exam2(30,10000,mean)
Exam2(30,10000,var)
Exam2(30,10000,sd)
Exam2(30,10000,median)
Exam2(30,10000,quantile)
Exam2(30,10000,min)
Exam2(30,10000,max)
fun<-function(x){max(x)-min(x)}
Exam2(30,10000,fun)
fun<-function(x){sd(x)/mean(x)}
Exam2(30,10000,fun)
Exam2(30,10000,sum)

Exam2(100,10000,mean)
Exam2(100,10000,var)
Exam2(100,10000,sd)
Exam2(100,10000,median)
Exam2(100,10000,quantile)
Exam2(100,10000,min)
Exam2(100,10000,max)
fun<-function(x){max(x)-min(x)}
Exam2(100,10000,fun)
fun<-function(x){sd(x)/mean(x)}
Exam2(100,10000,fun)
Exam2(100,10000,sum)

