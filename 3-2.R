###### Prog 3: Monte Carlo Method for xbar & s^2 ###########
Prog3<-function(n,m){
  xbar=s2=numeric(m)
  for(i in 1:m){
    x=rexp(n,1)
    xbar[i]=mean(x)
    s2[i]=var(x)
  }
  bxbar=mean(xbar)-0
  bs2=mean(s2)-1
  varxbar=var(xbar)
  vars2=var(s2)
  par(mfrow=c(1,2))
  hist(xbar);abline(v=0)
  hist(s2);abline(v=1)
  return(rbind(c("bias of xbar","bias of s^2","var of xbar","var of s^2"),
               round(c(bxbar,bs2,varxbar,vars2),5),
               round(c(0,0,1/n,2/(n-1)),5)))
}
#####################
Prog3(10,10000)
Prog3(30,10000)
Prog3(100,10000)
