### 第二次作業 ###

##question 01 : 歐式買權
# 常態分配N(u) = ∮(-∞ → u) 1/sqrt(2π)*exp(-x^2/2) dx  
# 
# code:
# 
# pnorm(u, mean = 0, sd = 1)

EP <- function(S, K, r, T0, sigma) {
  d1 <- (log(S/K)+(r+0.5*sigma^2*T0)/sigma*sqrt(T0))
  d2 <- d1- sigma*sqrt(T0)
  price <- K*exp(-1*r*T0)*pnorm(-1*d2, mean = 0, sd = 1)-
    S*pnorm(-1*d1, mean = 0, sd = 1)
  return(price)
  
}

S <- 100
K <- 100
r <- 0.02
T0 <- 0.5
sigma <- 0.15

h <- 10^(-6)

# backward differencing 向前差分
dEp_b <- ( EP(S+h, K, r, T0, sigma) -EP(S,K, r, T0, sigma))/h

# forward differencing 向後差分
dEp_f <- ( EP(S, K, r, T0, sigma) -EP(S-h,K, r, T0, sigma))/h

# center differencing 中央差分
dEp_c <- ( EP(S+h, K, r, T0, sigma) -EP(S-h,K, r, T0, sigma))/h


##question 02 :投資報酬服從常態分配
# 報酬率 Kp = w*Kx+(1-w)*Ky
# 變異數 Var(Kp) = Var(w*Kx+(1-w)*Ky)
#                =w^2*Var(x)+(1-w)^2*Var(y)+2*w*(1-w)*cov(x,y)


risk_p <- function(w, rho, sigma_x, sigma_y){
  
  sigma2_p <- w^2*sigma_x^2+(1-w)^2*sigma_y^2+
              2*w*(1-w)*rho*sigma_x*sigma_y
  return( sigma2_p)
  
}

result <-  optim(par     = 0.1,
                 fn      = risk_p,
                 method  = "L-BFGS-B",
                 lower   = -Inf,
                 upper   = Inf,
                 rho     = 0.8,
                 sigma_x = 0.6,
                 sigma_y = 0.2)


weight_x <- result$par
weight_y <- 1-weight_x


##question 03 :投資報酬服從常態分配
# 報酬率 Kp = w*Kx+(1-w)*Ky
# 變異數 Var(Kp) = Var(w*Kx+(1-w)*Ky)
#                =w^2*Var(x)+(1-w)^2*Var(y)+2*w*(1-w)*cov(x,y)
# 
# 變化:lower = 0 ,upper = 1  


risk_p <- function(w, rho, sigma_x, sigma_y){
  
  sigma2_p <- w^2*sigma_x^2+(1-w)^2*sigma_y^2+
    2*w*(1-w)*rho*sigma_x*sigma_y
  return( sigma2_p)
  
}

result <-  optim(par     = 0.1,
                 fn      = risk_p,
                 method  = "L-BFGS-B",
                 lower   = 0,
                 upper   = 1,
                 rho     = 0.8,
                 sigma_x = 0.6,
                 sigma_y = 0.2)


weight_x <- result$par
weight_y <- 1-weight_x

