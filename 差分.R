## 3/25

rm(list=ls())

f<- function(x){
  output <- x^5
  return(output)
}



h<- 10^(-6)
x<- 2
##forward
d1f_F<-(f(x+h)-f(x))/h

##backward
d1f_B<-(f(x)-f(x-h))/h

##centering
d1f_C<-(f(x+h)-f(x-h))/(2*h)

##exact
d1f<- 2/sqrt(2*pi)*exp(-0.5*(2*x)^2)



##forward
d2f_F<-(f(x+2*h)-2*f(x+h)+f(x))/h^2

##backward
d2f_B<-(f(x)-2*f(x-h)+f(x-2*h))/h^2

##centering
d2f_C<-(f(x+2*h)-2*f(x)+f(x-2*h))/(4*h^2)

f <- function(x,y){
  output <- log(x**2+x*y+y**2)
  return(output)
}


h <- 10**-6
x <- 1
y <- 4

##forward
d1f_x_F <- (f(x+h,y)-f(x,y))/h

##backward
d1f_x_B <- (f(x,y)-f(x-h,y))/h

##center
d1f_x_B <- (f(x+h,y)-f(x-h,y))/2*h


