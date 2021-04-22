myfun <- function(x){
  return(2*x)
}

ub <- 1
Ib <- 0
n <-  100
DELTA <- (ub-Ib)/n
w <- Ib+(0)*DELTA
A <-  rep(NA, n)
for (k in 1:n){
  A[k] <-  myfun(w[k]+myfun(w[k+1]))*DELTA/2
}

S_trap <- sum(A)
S_lower <- sum(myfun(w[0:n])*DELTA)
S_upper <- sum(myfun(w[2:(n+1)])*DELTA)

###

myfun <- function(x){
  return(6*x*exp(x ^2))
}

ub <- 2
Ib <- 1
n <-  1000
DELTA <- (ub-Ib)/n
w <- Ib+(0)*DELTA
A <-  rep(NA, n)
for (k in 1:n){
  A[k] <-  myfun(w[k]+myfun(w[k+1]))*DELTA/2
}

S_trap <- sum(A)
S_lower <- sum(myfun(w[0:n])*DELTA)
S_upper <- sum(myfun(w[2:(n+1)])*DELTA)


####需求函數 P = Q^(-0.5)
####供給函數 p = 8*Q^(2.5)

###需求函數
P_D <- function(Q){
  ( Q^(-0.5))
}

P_star <- sqrt(2)
Q_star <- 0.5

ub <- Q_star
Ib <- 0
n <- 1000
DELTA <- (ub-Ib)/n
w <- Ib+(0)*DELTA
A <-  rep(NA, n)

for (k in 1:n){
  A[k] <-  P_D(w[k]+P_D(w[k+1]))*DELTA/2
}

CS_trap <- sum(A)-P_star*Q_star
CS_lower <- sum(P_D(w[0:n])*DELTA)-P_star*Q_star
CS_upper <- sum(P_D(w[2:(n+1)])*DELTA)-P_star*Q_star


### 供給函數
P_S <- function(Q){
  ( 8*Q^(-2.5))
}

P_star <- sqrt(2)
Q_star <- 0.5

ub <- Q_star
Ib <- 0.0001
n <- 1000
DELTA <- (ub-Ib)/n
w <- Ib+(0)*DELTA
A <-  rep(NA, n)

for (k in 1:n){
  A[k] <-  P_S(w[k]+P_S(w[k+1]))*DELTA/2
}

CS_trap  <- P_star*Q_star-sum(A)
CS_lower <- P_star*Q_star-sum(P_S(w[0:n])*DELTA)
CS_upper <- P_star*Q_star-sum(P_S(w[2:(n+1)])*DELTA)

###蒙地卡羅

n <-  100
r <- 2
x_j <- runif(n, min = -1*r, max= 2)
y_j <- runif(n, min = -1*r, max= 2)


I <-  sum(x_j^2+y_j^2<=r^2)
Prob <-  I/n
S_mc <- (2*r)^2*Prob

