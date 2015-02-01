library(mvtnorm)
generate_multi_data <- function(d)
{
  sigma <- diag(d)
  m <- rep(1,d)
  x <- rmvnorm(n=300,mean=m,sigma=sigma)
  cls <- rep(1,300)
  x <- cbind(x,cls)
  
  m <- append(3,rep(1,d-2))
  m <- append(m,0)
  y <- rmvnorm(n=300,mean=m,sigma=sigma)
  cls <- rep(2,300)
  y <- cbind(y,cls)
  X <- rbind(x,y)
  return(X)
}

X <- generate_multi_data(100)