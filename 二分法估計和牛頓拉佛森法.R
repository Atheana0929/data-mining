rm(list= ls())

x <-seq(0,3,0.1)

f<- x**3/3+3*x^2-5*x+10
plot(x,f ,type = "l",col="red")

## 二分法估計& 牛頓法
#uniroot :找到函數的根和極值
#方法限制:
#
#*函數可以微分使用 ---> 牛頓法
#*函數無法微分使用 ---> 二分法求值

f<-function(x){
  return(-1*x**2+6*x-5)
}

#result <-uniroot(f,c(0,5),tol=10**(-8))

#x_star1 <-result$root

curve(f,from = -2, to=6, n=100)


result <-uniroot(f,c(0.5,1.4),tol = 10^(-8))


x_star1 <-result$root

#

f <-function(x){
  y<- 3*x^2-27
  return( y^2 )
}


result <- optim( par    = 4,
                 fn     = f,
                 method = "L-BFGS-B",
                 lower  = -Inf,
                 upper  = Inf)


x_star1 <- result$par


#

CF  <-28000
n   <- 5
INV <-100000

f <-function(x){
  y <-sum(CF/(1+x)^(1:n))-INV
  return(y**2)
}


result <-optim(par    = 0.3,
               fn     = f,
               method = "L-BFGS-B",
               lower  = 0,
               upper  = 1)

IRR <-result$par


