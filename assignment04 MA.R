
rm( list = ls() )

## AR(1) model
'
自我回歸模型(AR)
R(0) 已知
R(1) = omega+alpha1*R(0)+ epsilon(1)
R(2) = omega+alpha1*R(1)+ epsilon(2)
...
R(100) = omega+alpha1*R(99)+ epsilon(100)

R(t) = ln[S(t)/S(t-1)] -> S(t)

epsilon = R(t)- E[R(t)] >0 好消息  
epsilon = R(t)- E[R(t)] >0 壞消息
'

omega  <- 0.001      # intercept term
alpha1 <- 0.002      #R(t-1) 對 R(t)的影響，自我相關
sigma  <- 0.25/ sqrt(365)  # 股票報酬的標準差(由年轉日)

## random numbers(Gaisson/noraml white noise)
PathN  <- 100
T0      <- 300
epsilon <- matrix(rnorm(T0*PathN, mean = 0, sd = sigma),
                  nrow = T0, ncol = PathN)

## simulation stock price
R       <- matrix(NA,nrow =T0+1, ncol = PathN)
R[1,]    <- 0        # 第0天的報酬率
S       <- matrix(NA,nrow =T0+1, ncol = PathN)
S[1,]    <- 100
for (t in 1:T0){
  R[t+1,] <- omega + alpha1*R[t,]+ epsilon[t,]   #第t+1天的報酬率
  S[t+1,] <- S[t,]*exp(R[t+1,])
}

plot( 0:T0, S[,1], type = "l", col = 1 , ylim = c(min(S),max(S)) )
for (j in 2:PathN){
  lines(0:T0, S[,j], col = j)
}


## Assigmeant 5 - homework 1:

'AR model 信心水準95% VaR(S300) = ???'

VaR_AR <- quantile( S[301,], 0.05 )
VaR_AR


## MA(1) model
omega  <- 0.001      # intercept term
beta1 <- 0.002      #epsilon(t-1) 對 R(t)的影響
sigma  <- 0.25/ sqrt(365)  # 股票報酬的標準差(由年轉日)

## random numbers(Gaisson/noraml white noise)
PathN  <- 1000
T0      <- 300
epsilon <- matrix(rnorm(T0*PathN, mean = 0, sd = sigma),
                  nrow = T0, ncol = PathN)

## simulation stock price
R       <- matrix(NA,nrow =T0+1, ncol = PathN)
R[1,]    <- 0        # 第0天的報酬率
S       <- matrix(NA,nrow =T0+1, ncol = PathN)
S[1,]    <- 100
for (t in 1:T0){
  if (t == 1){
    R[t+1,] <- omega + 0+ epsilon[t,]   #第t+1天的報酬率
    
    
  } else {
    R[t+1,] <- omega + beta1*R[t-1,]+ epsilon[t,]   #第t+1天的報酬率
  }  
    S[t+1,] <- S[t,]*exp(R[t+1,]) 
  
}

plot( 0:T0, S[,1], type = "l", col = 1 , ylim = c(min(S),max(S)) )
for (j in 2:PathN){
  lines(0:T0, S[,j], col = j)
}

## Assigmeant 5 - hmework 2:
'MA model 信心水準95% VaR(S300)=???'


VaR_MA <- quantile( S[T0+1,] , 0.05)

VaR_MA


## ARMA (1,1) model 
omega  <- 0.001      # intercept term
alpha1 <- 0.003      # R(t-1) 對 R(t)的影響，自我相關
beta1  <- 0.002      #epsilon(t-1) 對 R(t)的影響
sigma  <- 0.25/ sqrt(365)  # 股票報酬的標準差(由年轉日)

## random numbers(Gaisson/noraml white noise)
PathN  <- 20
T0      <- 300

epsilon <- matrix(rt(T0*PathN, 30),
                  nrow = T0, ncol = PathN)


# epsilon <- matrix(rnorm(T0*PathN, mean = 0, sd = sigma),
#                   nrow = T0, ncol = PathN)

## simulation stock price
R       <- matrix(NA,nrow =T0+1, ncol = PathN)
R[1,]    <- 0        # 第0天的報酬率
S       <- matrix(NA,nrow =T0+1, ncol = PathN)
S[1,]    <- 100
for (t in 1:T0){
  if (t == 1){
    R[t+1,] <- omega + alpha1*R[t,]+
                0 + epsilon[t,]   #第t+1天的報酬率
    
    
  } else {
    R[t+1,] <- omega + alpha1*R[t,]+
                beta1*R[t-1,]+ epsilon[t,]   #第t+1天的報酬率
  }  
  S[t+1,] <- S[t,]*exp(R[t+1,]) 
  
}

plot( 0:T0, S[,1], type = "l", col = 1 , ylim = c(min(S),max(S)) )
for (j in 2:PathN){
  lines(0:T0, S[,j], col = j)
}


'* 有點錯誤'
## Assigmeant 5 - hmework 3
'ARMA(1,1) ，殘差項~ t(30)，VaR(S300) = ??? '
VaR_ARMA <- quantile( S[T0+1,],0.05)
VaR_ARMA



## geometic Brownian motion
r  <- 0.02/365      # 無風險利率(油年轉日)

sigma  <- 0.65/ sqrt(365)  # 股票報酬的標準差(由年轉日)

## random numbers(Gaisson/noraml white noise)
PathN  <- 20
T0     <- 90
epsilon <- matrix(rnorm(T0*PathN, mean = 0, sd = sigma),
                  nrow = T0, ncol = PathN)



## simulation stock price
R       <- matrix(NA,nrow = T0+1, ncol = PathN)
R[1,]    <- 0        # 第0天的報酬率
S       <- matrix(NA,nrow =T0+1, ncol = PathN)
S[1,]    <- 100
for (t in 1:T0){
 
  R[t+1,] <- r-0.5*sigma^2+epsilon[t,]   #第t+1天的報酬率
  
  S[t+1,] <- S[t,]*exp(R[t+1,]) 
  
}
# 
# plot( 0:T0, S[,1], type = "l", col = 1 , ylim = c(min(S),max(S)) )
# for (j in 2:PathN){
#   lines(0:T0, S[,j], col = j)
# }


EC <- mean((S[T0+1,]-100)*(S[T0+1,]>100)*exp(-1*r*T0))

## Assigmeant 4 - homework 4 
'
payoff = S(T)-100 if 100 < S(T) <= 150
            50    if S(T) > 150
            0     if S(T) < 100   
PV(payoff) = ??

'

price <- mean( ((S[T0+1,]-100)*(S[T0+1,]>100)*(S[T0+1,]<=150)+
          50*(S[T0+1,]>150))*exp(-1*r*T0))