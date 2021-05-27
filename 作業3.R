rm( list = ls())

##question 1

f <-  function(x){
  return(   1/ sqrt(2*pi)*exp(-0.5*x^2))
  
}

# lower sum
m  <- 10000 
lb <-  1
ub <-  2
DELTA <- (ub-lb)/m
x <- lb+DELTA*(0:(m-1))
y <- f(x)
int_lower <- sum(y*DELTA)

#upper sum
m  <- 100000 
lb <-  1
ub <-  2
DELTA <- (ub-lb)/m
x <- lb+DELTA*(1:m)
y <- f(x)
int_upper <- sum(y*DELTA)


## trapeaion 
m  <- 100000 
lb <-  1
ub <-  12
DELTA <- (ub-lb)/m
x_rigjt <- lb+DELTA*(0:(m-1))
x_left <-  lb+DELTA*(0:(m+1))
y_left <- f(x_left)
y_right <- f(x_right)
int_trap <- sum((y_left+y_right)*DELTA/2)




#Monte- carlo  
##curve  (f, from = 1, to = 2)


x <- 1
y <- f(1)
m <- 100000
x_arrow <- runif(m, min = 1, max = 2 )
y_arrow <- runif(m, min = f(2), max = 2 )
I <- sum(y_arrow<=f(x_arrow))
prob <- I/m
int_MC <- x*y*prob


# excat form
int_exact <-  pnorm(2)-pnorm(1)

##question 2
payoff <- function(S){
  K1 <- 80
  K2 <- 120
  V <- (S-K1)*(S>K1)*(S<=K2)+(K2-K1)*(S>K2)
  return(V)
}

curve( payoff, from =50 , to =300)

T0 <- 270
K1 <- 80
K2 <- 120
S <- 100
r <- 0.03/365                #年利率 -> 日利率
sigma <- 0.15/sqrt(365)    #年波動度 -> 日波動度 
  

u <- exp(sigma)          #上漲幅度
d <-1/u                  #下跌幅度  
p <- (exp(r)-d)/(u-d)    #上漲幅度(風險中立機率/人造機率)

x <- rbinom(m, T0, p)     #股票在0~T0天內上漲的天數
ST <- S*(u^x)*(d^(T0-x))  #股票在第T0天的價格
payoff_T <- payoff(ST)    #衍生商品的到期日和現金流量
Value <- mean(payoff_T)*exp(-1*r*T0)   #衍生商品期初價格