##最大概似估計法(maximum lokelihood estimation)
# 
# 考慮一組報酬率X1,....Xn 獨立服從相通分配(IID)
# Step1:概似函數 L(mean,sd;X1:n)=fx(X1;mean,sd(X1))*fx(X2;mean,sd(X2))*...=fx(Xn;mean,sd(Xn))
# Step2:將概似函數取ln 
# Step3:解釋 -> 發生這個x1這個值大概的機率為

### 第一堂課 ###

rm(list = ls())

logL <- function(pm, x) {
  
  #parameter
  
  mu    <-  pm[1]
  sigma <-  pm[2]
  
  # probability density function
  # normal disrtibution 
  
  
  PDF <- 1/sqrt(2*pi*sigma^2)*exp(-0.5*(x-mu)**2/sigma**2)
  
  PDF <- 2**(-52)*(PDF==0)+PDF*(PDF!=0)
  
  #output
  return(-1*sum(log(PDF)))
}    
  

##技巧:optim(...) <-  找一組參數使目標min 

##先將data隨意輸入
data <-  rnorm(10000, mean = 0.3, sd= 0.2)    # rnorm拆解 { [r]=random , [norm]=normal }

estimate <-  optim(par    = c(0,1),
                   fn     = logL,
                   method = "L-BFGS-B",
                   lower  = c(-1,2**(-52)),   #設定局部極小值
                   upper  = c(1,1),           #設定局部極大值 
                   x = data)          


pm_1 <-  estimation$par


### 第二堂課 ###
# 
# 效用函數: Max U(x,y) = x^(0.2)*Y^(0.6)
# 限制式st: 2X+5Y =200


u_fun  <- function(x) {
  return(x^0.2*(40-0.4*x)^0.6)
  
}


result <-  optim(par    = 5,
                 fn     = u_fun,
                 method = "L-BFGS-B",
                 lower  = 2^(-52),
                 upper  = 100)
                 

