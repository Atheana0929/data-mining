
rm (list = ls())

# m <- 20
# cbind(sample(1:6),m,replace=TRUE,
#       sample(1:6),m,replace=TRUE)
# sample(1:6, m, replace = TRUE) 

# m <- 20
# I <- sum(sample(1:6, m, replace = TRUE)==2)
# message(I)
# prob <-  I/m
# message(prob)


m <- 100000
roll <- cbind(sample(1:6),m,replace=TRUE,
              sample(1:6),m,replace=TRUE)
E <- mean(roll) #期望值
message( E )

V <- mean(roll^2)-mean(roll)^2
message( V )