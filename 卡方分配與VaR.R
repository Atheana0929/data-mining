# 單元五 

rm( list = ls())

# 卡方分配
### 母體平均數、母體標準差皆知  <- Z 表
### 母體平均數未知、母體標準差已知 <- Z 表

## standard normal dist.
# m <-  1000000
# X <-  rnorm(m, mu = 0, sd = 1)

## chi-square dist.
m <- 100000

# method 1
X    <- rchisq(m, 4)
EX   <- mean(X)
VarX <- mean(X^2)-(mean(X))^2

# method 2
Z <-  matrix( rnorm(m*4, 0,1), nrow = m, ncol = 4)
View(Z) #查看表

X    <-   rowSums( Z^2 )
EX   <- mean(X)
VarX <-  mean(X^2)-(mean(X))^2

## non-central chi-square dist.  
# Y1, ..., Yn ~ Normal( mu, sigma^2 )
# X_ncchisq = Y1^2/sigma^2+ ...+ Yn^2/sigma^2 ~NcChi2( n, delta)
m <- 1000000

Y1 <- rnorm(m, mean = 2, sd = 3)
Y2 <- rnorm(m, mean = 1, sd = 10)
Y3 <- rnorm(m, mean = 5, sd = 3)   
Y4 <- rnorm(m, mean = 20, sd = 0.2)
X_ncchisq <- (Y1/3)^2+(Y2/10)^2+(Y3/3)^2+(Y4/0.2)^2
X_chisq   <- ((Y1-2)/3)^2+((Y2-1)/10)^2+((Y3-5)/3)^2+((Y4-20)/0.2)^2
#r~ CIR model ~ non-central chi-square dist.

## Student-t dist.
m    <- 100000
# method 1
X    <- rt(m, 3)
EX   <- mean(X)
VarX <- mean(X^2)-(mean(X))^2

# method 2 
Z    <- rnorm(m, mean = 0, sd = 1)
Y    <- rchisq(m, 3)
X    <- Z/sqrt(Y/3)
EX   <- mean(X)
VarX <- mean(X^2)-(mean(X))

# n >= 30 ，可以用Z表近似t表
# Z <- qnorm(0.95, mean = 0, sd = 1)
# Z <- qnorm(0.95, 300000)

## F dist.
m <- 1000000

# method 1
X <- rf(m, 20, 30)
View(X)

# method 2
# 用二兩個獨立的卡方分配，Y1~Chi2(20)、Y2~ Chi2(30)
# 合成 X = (Y1/20)/(Y2/30)~ F(20, 30)
df_Y1 <-  20
df_Y2 <- 30
Y1 <-  rchisq(m, df_Y1)
Y2 <-  rchisq(m, df_Y2)
X  <-  (Y1/df_Y1)/(Y2/df_Y2)


## compound Poisson dist.
#  單位時間內，平均發生lambda 次金融事件 N ~  Poisson( lambda )
#  每一次的事件史的股票瞬間變數 Y1, Y2, Y3, ..., Yn
#  Y1, .., Yn ~ Normal( mu, sigma)
#  summation (n = 1 to N)Y_n
m    <- 1000000
N    <- rpois(m, 3)
X    <- rnorm(m, mean = N*0.2, sd = sqrt(N*0.1^2))
EX   <- mean(X)
VarX <- mean(X^2)-(mean(X))^2 
3*0.2
3*(0.2^2+0.1^2)

## value-at-risk
# 避免違約交割，雙方必須交付保證金，一方違約將保證金給對履約者
# 計算違約保證金
m <- 100000
R <- rnorm(m, mean = 0.02, sd = 0.35)  #daily rate of return for portfolio
c <- 0.95  #信心水準
VaR <- quantile( R, 1-c )

qnorm( 0.05, mean = 0.02, sd = 0.35)

VaR
# R = X+Y , X~ Normal(0,1), Y~ ComPoisson
X <- rnorm(m, mean = 0, sd = 1)

N <- rpois(m, 10)
Y <- rnorm(m, mean = N*(-0.07), sd = sqrt(N*0.2^2))
R <- X+Y
VaR <- quantile(R, 1-c)
VaR  # 我有95%的信心水準,最大虧損只有27%