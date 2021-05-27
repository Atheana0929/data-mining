rm( list = ls())

m <- 1000000
x <- runif(m, min = 1, max = 6)
EX <- mean(x)

VarX <- mean(x^2)-mean(x)^2