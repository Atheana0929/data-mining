rm( list = ls())

## question 1
## Please use the lower Riemann sum, the upper Riemann sum, the trapezoidal rule, 
## and the Monte-Carlo method, respectively, to compute the following definite intigral.

f <-  function(x){
  return(   1/ sqrt(2*pi)*exp(-0.5*x^2))
  
}

## lower sum
m  <- 1000000 
lb <-  1  #下限
ub <-  2  #上限
DELTA <- (ub-lb)/m       #將面積切成m等分，也就是寬
x <- lb+DELTA*(0:(m-1))  #假設切成10等分->就是算從0-9
y <- f(x)
int_lower <- sum(y*DELTA)

## upper sum
m  <- 100000 
lb <-  1
ub <-  2
DELTA <- (ub-lb)/m
x <- lb+DELTA*(1:m)
y <- f(x)
int_upper <- sum(y*DELTA)


## trapezoidal rule 梯形
m     <- 1000000 
lb    <-  1
ub    <-  2
DELTA <- (ub-lb)/m

x_left  <- lb+DELTA*(0:(m-1))
x_right <- lb+DELTA*(1:m)

y_left  <- f(x_left)
y_right <- f(x_right)

int_trap <- sum((y_left+y_right)*DELTA/2)




## Monte- carlo  蒙地卡羅 -> 誤差相對大
curve  (f, from = 1, to = 2)

x <- 1
y <- f(1)-f(2)
m <- 1000000   #飛鏢數目會影響精準度

x_arrow <- runif(m, min = 1, max = 2 )
y_arrow <- runif(m, min = 0, max = f(1))
I <- sum(y_arrow <= f(x_arrow))  #y_arrow <= f(x_arrow)當視為射中
                                 #成立Ture ；不成立 False
prob <- I/m   #射中紅心的機率
int_MC <- x*y*prob


## excat form 標準常態分配機率函數
int_exact <-  pnorm(2)-pnorm(1)

# >int_lower
# [1] 0.1359052

# > int_upper
# [1] 0.1359042

# > int_trap
# [1] 0.1359051

# > int_MC
# [1] 0.105595

# > int_exact
# [1] 0.1359051


## question 2
## Consider a capped option with the payoff at maturity as follow.
## V (T; K1, K2, T) = [S (T) − K1] · 1{K1<S(T)≤K2} + (K2 − K1) · 1{S(T)>K2}.
## Let S (0) = $100, K1 = $80, K2 = $120, T = 0.75 years, r = 0.03 (in year), and σ = 0.15
## (in year). Please use the Binomial tree pricing method to compute the premium of this
## option at initial.

payoff <- function(S){
  K1 <- 80
  K2 <- 120
  V <- (S-K1)*(S>K1)*(S<=K2)+(K2-K1)*(S>K2)
  return(V)
}

curve( payoff, from =50 , to =300)

T0  <- 270   #0.75年約為9個月
K1  <- 80
K2  <- 120
S   <- 100
r   <- 0.03/365                #年利率 -> 日利率
sigma <- 0.15/sqrt(365)       #年波動度 -> 日波動度 
## 持有最後一天才可以拿到一筆現金  

u <- exp(sigma)          #上漲幅度
d <-1/u                  #下跌幅度  
p <- (exp(r)-d)/(u-d)    #上漲機率(風險中立機率/人造機率)

##做binom分配
x         <- rbinom(m, T0, p)     #股票在0~T0天內上漲的天數，題目為270天內
ST        <- S*(u^x)*(d^(T0-x))  #股票在第T0天的價格
payoff_T  <- payoff(ST)    #衍生商品的到期日和現金流量
Value     <- mean(payoff_T)*exp(-1*r*T0)   #衍生商品期初價格
