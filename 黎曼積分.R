#黎曼積分(Riemann intigral)為定積分
#f(x)=x
#反倒函數F(x)=1/2*x^2
#定積分

##練習1
myfun <- function(x){
  return(2*x)
}


ub <- 1
lb <- 0
s_lower <- rep(NA, 100)
for (n in 1:100){
  
  DELTA      <- (ub-lb)/n
  w          <- lb+(0:(n-1))*DELTA
  s_lower[n] <- sum(myfun(w)*DELTA)
} 

plot( 1:100, s_lower, type ="l", col = "blue")


##練習2
myfun2 <- function(x){
 
  return(6*x*exp(x**2))
}

ub <- 2
lb <- 1
s_lower <- rep(NA, 1000)
for (n in 1:1000){
  
  DELTA      <- (ub-lb)/n
  w          <- lb+(0:(n-1))*DELTA
  s_lower[n] <- sum(myfun2(w)*DELTA)
  
}
  
plot( 1:1000, s_lower, type ="l", col = "blue")


##練習3
myfun3 <- function(x){
  
  return(cos(x))
}

ub <- pi/2
lb <- 0
s_lower <- rep(NA, 1000)
for (n in 1:1000){
  
  DELTA      <- (ub-lb)/n
  w          <- lb+(0:(n-1))*DELTA
  s_lower[n] <- sum(myfun3(w)*DELTA)
  
}

plot( 1:1000, s_lower, type ="l", col = "blue")

##upper sum 
##範例1
s_upper <-  rep(NA, 100)
for (n in 1:1000){
  
  DELTA      <- (ub-lb)/n
  w          <- lb+(1:n)*DELTA
  s_upper[n] <- sum(myfun(w)*DELTA)
  
}

plot( 1:1000, s_upper, type ="l", col = "blue")

##範例2
s_upper <-  rep(NA, 100)
for (n in 1:1000){
  
  DELTA      <- (ub-lb)/n
  w          <- lb+(1:n)*DELTA
  s_upper[n] <- sum(myfun2(w)*DELTA)
  
}

plot( 1:1000, s_upper, type ="l", col = "blue")




##範例4

myfun4 <- function(x){
  
  return(1/sqrt(2*pi)*exp())
}

ub <- 1.96
lb <- 0
s_lower <- rep(NA, 1000)
for (n in 1:1000){
  
  DELTA      <- (ub-lb)/n
  w          <- lb+(0:(n-1))*DELTA
  s_lower[n] <- sum(myfun3(w)*DELTA)
  
}

s_upper <-  rep(NA, 100)
for (n in 1:1000){
  
  DELTA      <- (ub-lb)/n
  w          <- lb+(1:n)*DELTA
  s_upper[n] <- sum(myfun2(w)*DELTA)
  
}

plot( 1:1000, s_upper, type ="l", col = "blue")

