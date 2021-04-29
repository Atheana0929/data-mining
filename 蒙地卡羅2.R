### 蒙地卡羅


rm (list = ls())

f <- function(x){
  return(2*x)
}

n <- 10000  #注意設太多飛鏢可能會當機
x <- runif(n, min = 0, max = 1)
y <- runif(n, min = 0, max = 2)
I <- sum(y <= f(x))
prob <- I/n
s_mc <- 2*prob

message(s_mc)

f <- function(x){
  return(1/sqrt(2*pi)*exp(-0.5*x^2))
}

n <- 2000000  #注意設太多飛鏢可能會當機
x <- runif(n, min = 0, max = 1.96)
y <- runif(n, min = 0, max = f(0))
I <- sum( y <= f(x))
prob <- I/n
s_mc <- (1.96*f(0))*prob
message(s_mc)


f <- function(x){
  return((x^3+exp(x))*(3*x^2+exp(x)))
}

curve(f(x), from = 0, to=1, n=101 ,col= 'blue')

n <- 2000000  #注意設太多飛鏢可能會當機
x <- runif(n, min = 0, max = 1)
y <- runif(n, min = 0, max = f(1))
I <- sum( y <= f(x))
prob <- I/n
s_mc <- (1*f(1))*prob
message( s_mc )


f <- function(x){
  return(1/sqrt(2*pi)*exp(-0.5*x^2))
}

curve(f(x), from = -2.575, to = 2.575, n=101 ,col= 'blue')

n <- 2000000  #注意設太多飛鏢可能會當機
x <- runif(n, min = -2.575, max = 2.575)
y <- runif(n, min = 0, max = f(0))
I <- sum( y <= f(x))
prob <- I/n
s_mc <- (2.575*2*f(0))*prob
message( s_mc )

pnorm(2.575,0,1)-pnorm(-2.575,0,1)