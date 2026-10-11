###### Example 5: Comparison of between Classic and Bootstrap CI ###########
Prog5<-function(n,B,m,a,sigma){
n1=n2=n3=n4=n5=n6=n7=n8=n9=0
d1=d2=d3=numeric(m)
#xbar<-numeric(m)
for(i in 1:m){
              x=rexp(n,sigma)
              xbar<-mean(x)
############# Ordinary Classic ##########
              l1=mean(x)+(qexp(a/2)*sd(x)/sqrt(n))
              l2=mean(x)+(qexp(1-a/2)*sd(x)/sqrt(n))

#             l1=mean(x)+(qexp(a/2)*sigma/sqrt(n))
#             l2=mean(x)+(qexp(1-a/2)*sigma/sqrt(n))

#             l1=mean(x)+(qt(a/2,n-1)*sd(x)/sqrt(n))
#             l2=mean(x)+(qt(1-a/2,n-1)*sd(x)/sqrt(n))

              d1[i]=l2-l1
              if (0<l2 && 0>l1) n1=n1+1 
              if (0<l1)         n2=n2+1
              if (0>l2)         n3=n3+1 
############# Ordinary Bootstrap ############
              xs=matrix(sample(x,n*B,replace=T),nrow=B)
              xbars=apply(xs,1,mean) 
              l3=mean(x)+(qexp(a/2)*sd(xbars))
              l4=mean(x)+(qexp(1-a/2)*sd(xbars))

#              l3=mean(x)+(qt(a/2,n-1)*sd(xbars))
#              l4=mean(x)+(qt(1-a/2,n-1)*sd(xbars))

              d2[i]=l4-l3
              if (0<l4 && 0>l3) n4=n4+1 
              if (0<l3)         n5=n5+1
              if (0>l4)         n6=n6+1 
############# Percentile Bootstrap ###############
              l5=quantile(xbars,a/2)
              l6=quantile(xbars,1-a/2)
              d3[i]=l6-l5
              if (0<l6 && 0>l5) n7=n7+1 
              if (0<l5)         n8=n8+1
              if (0>l6)         n9=n9+1 
     
              }
#####################
ml=mean(d1)
m2=mean(d2)
m3=mean(d3)
#              l7=quantile(xbar,a/2)
#              l8=quantile(xbar,1-a/2)
#              ci=pexp(l8)-pexp(l7)
return(rbind(c("Method","Mean of Lenght CI","Prob. CI","Prob. CI L.","Prob. CI R."),
             c("Ordinary Classic",ml,n1/m,n2/m,n3/m),
             c("Ordinary Bootstrap",m2,n4/m,n5/m,n6/m),
             c("Percentile Bootstrap",m3,n7/m,n8/m,n9/m)))
}
###################################
Prog5(10,1000,1000,0.05,1)
