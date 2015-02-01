
Y <- list()
for(j in (1:2))
{
X <- list()
for(i in (1:100))
{
	X = cbind(X,round(rnorm(150,0,j*10)))
}
Y<- rbind(Y,X)
}
