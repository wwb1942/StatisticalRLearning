x<-c(75,64,47.4,66.9,62.2,62.2,58.7,63.5)
sort(x)

sort(x, decreasing = T)


x.na<-c(75,64,47.4,NA,66.9,62.2,62.2,58.7,63.5)
sort(x.na)

sort(x.na, na.last = T)
sort(x.na, na.last = F)

w<-c(75.0,64.0,47.4,66.9,62.2,62.2,58.7,63.5,66.6,64.0,57.0,69.0,56.9,50.0,72.0)
quantile(w)
var(w)
sd(w)
cv <- (sd(w)/mean(w))*100;cv

css <-sum((w-mean(w))^2);css
uss <-sum(w^2);uss
IQR(w)
diff(range(w))
sm <- sd(w)/sqrt(length(w));sm

data_outline <- function(x){
  n <- length(x)
  m <- mean(x)
  v <- var(x)
  s <- sd(x)
  me <- median(x)
  cv <- 100*s/m
  css <- sum((x-m)^2)
  uss <- sum(x^2)
  R <- diff(range(x))
  R1 <- IQR(x)
  sm <- s/sqrt(n)
  g1 <- (n*sum((x-m)^3))/((n-1)*(n-2)*s^3)
  g2 <- (n*(n+1))/((n-1)*(n-2)*(n-3)*s^4)
  data.frame(N=n, Mean=m, Var=v, std_dev=s,
             Median=me, std_mean=sm, CV=cv, CSS=css,
             USS=uss, R=R, R1=R1, Skewness=g1, Kurtosis=g2,
             row.names = 1)
}



