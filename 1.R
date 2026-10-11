#monte_carlo

X = rexp(10000,2)
mean(X)
var(X)
median(X)
quantile(X,0.75)
quantile(X,0.2)
quantile(X,0.67)

CDF = ecdf(X)
CDF(2)
CDF(5)
CDF(10)

hist(X,
     col="gray",
     border="black",
     prob = TRUE,
     xlab = "",
     main = "")

lines(density(X),
      lwd = 2,
      col = "red")


#real_distribution

'lambda' = 2
  mean<-1/lambda
  var<-1/lambda^2
  median<-qexp(0.5,lambda)

print(mean)
print(var)
print(median)
qexp(0.75,2)
qexp(0.2,2)
qexp(0.67,2)

pexp(2,2)
pexp(5,2)
pexp(10,2)
