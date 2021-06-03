# Lecture 05

# 機率密度函數 probility density function(PDF)
# 連續型


# 機率質量函數 probility mass function (PMF)
# 離散型


# gamma dist. 具有相加性(given lambda 相同)
# X1~gamma(3, 2)
# X2~gamma(6, 2)
# Gamma 具有相加性 X1+X2 ~ Gamma(9,4)


# Normal dist. 具有相加性
# X1~ Normal(a1, b1)
# X2~ Normal(a2, b2)
# 常態具有相加性 X1+X2 ~ Normal(z1+a2, b1+b2)


# X~對數常態分配
# 把X取對數後服從常分配
#
# K = 股票報酬率(連續時間)~Normal
# S1(e股票價格) = S0*exp(k)
# ln(S1) = ln(S0) + k ~Normal
# X ~ Normal 
# exp(x)~LogNormal
# 
# ln(exp(X)) = X


# varience Gamma distribution(變異數加碼分配)
# X|G ~Normal(mu*G, sigma^2*G)
# G ~ Gamma( alpha, lambda)

# 樣本平均數
# 樣本平均數X_bar = (X1+X2+...Xn)
# E(X_bar)   = (mu+mu+....+mu)/n = mu
# Var(X_bar) = (1/n)^2(signa^2+signa^2+......+signa^2) = signa^2/n
# X_bar~ Normal(mu, sigma^2/n)


 
# uniform distribution
m <- 1000000
X <- runif(m, min = -1.2, max = 10  )
EX <-  mean(X)
VarX <-  mean(X^2)-(mean(X))^2 



# Exponential distribution

m <- 1000000
X <- rexp(m, 2  )
EX <-  mean(X)
VarX <- mean(X^2)-(mean(X))^2 




# X ~ 指數分配 直到第一次事件發生所需的時間
# X ~ Gamma distribution -> 直到第n次事件發生所要的時間
# n = 1 , gamma == exponential

# Gamma distribution
# lambda = 1/s --> scale = 1/lambda
m <- 1000000
X <- rgamma(m, shape =3 , scale = 1/2 )
EX <-  mean(X)
VarX <- mean(X^2)-(mean(X))^2 

# normal distribution
# lambda = 1/s --> scale = 1/lambda
m <- 1000000
X1 <- rnorm(m, mean = 0.05 , sd = 0.03 )
X2 <- rnorm(m, mean = -0.02 , sd = 0.10 )
Y <- X1+X2
EY <-  mean(Y)
VarY <- mean(Y^2)-(mean(Y))^2 


# 隨檢檢驗
Z <- 0.2*X1+0.8*X2
EZ <- mean(Z)
VarZ <- mean(Z^2)-(mean(Z))^2


# log-normal dist. 
m <- 1000000
X <- rlnorm(m, mean = 0.02 , sd = 0.10 ) # method 1
X <- exp(rnorm(m, mean = 0.2, sd = 0.1)) # method 2
EX <-  mean(X)
VarX <- mean(X^2)-(mean(X))^2 

# X ~ VG 
# mu     = 0.02
# sigma  = 0.1
# alpha  = 3
# lambda = 0.3
# 
# 抽出X的亂數，使用rnorm、rgamma合成X亂數

# varience gamma dist.
m     <- 1000000
mu    <- 0.02
sigma <- 0.1
G     <- rgamma(m, shape = 3, scale = 1/0.3)
X     <- exp(rnorm(m, mean = mu*G , sd = sigma*sqrt(G) ))

# varience gamma dist.
m     <- 1000000
X     <- rchisq(m, 30)  # method 01


# 卡方分配
Z2 <- 0
for(k in 1:30) {
  Z2 <- Z2 + rnorm(m ,mean = 0, sd = 1)^2
}

X <-  Z2

