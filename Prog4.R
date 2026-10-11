###### Prog 4: m=? Motne Carlo Method ###########
Prog4<-function(a,n,m){
xbar=s2=bxbar=bs2=varxbar=vars2=numeric(m)
for(i in 1:m){
              x=rexp(n,1)
              xbar[i]=mean(x)
              s2[i]=var(x)
bxbar[i]=mean(xbar[1:i])-0
bs2[i]=mean(s2[1:i])-1
varxbar[i]=var(xbar[1:i])
vars2[i]=var(s2[1:i])
              }
par(mfrow=c(2,2))
plot(a:m,bxbar[a:m],main="bias of xbar",cex=.2);abline(h=0)
plot(a:m,bs2[a:m],main="bias of s^2",cex=.2);abline(h=0)
plot(a:m,varxbar[a:m],main="var of xbar",cex=.2);abline(h=1/n)
plot(a:m,vars2[a:m],main="var of s^2",cex=.2);abline(h=2/(n-1))

}
#####################
Prog4(1,100,10000)
 

