library(mvtnorm)
#4.a. Generating a very high dimensional data sets

m <- rep(1,100)
a <- rmvnorm(n=300,mean=m,sigma=diag(100))
b <- rmvnorm(n=300,mean=m,sigma=diag(100))
X <- rbind(a,b)


actual <- list()
actual <- matrix(c(rep(1,150),rep(2,150)))

#4.b Show that the kmeans clustering method does not perform well on that data
confusion <- function(a,b)
{
	foo <- vector(mode="list", length=4)
	names(foo) <- c("TP", "FN", "FP","TN")
	for (i in (1:4))
		foo[[i]] <- 0
	for( i in (1:length(a)))
	{
		if(a[i]==1 && b[i]==1)
			foo[[1]] = foo[[1]]+1
		else if(a[i]==1 && b[i]==2)
			foo[[2]] = foo[[2]]+1
		else if(a[i]==2 && b[i]==1)
			foo[[3]] = foo[[3]]+1
		else if(a[i]==2 && b[i]==2)
			foo[[4]] = foo[[4]]+1
	}
	return(foo)
}

performance_metric <- function(mat)
{
	foo <- vector(mode="list", length=10)
	names(foo) <- c("Accuracy", "Error_Rate", "TPR","TNR","FPR","FNR","Precision-1","Precision-2","F-measure-1","F-measure-2")
	foo[[1]] <- (mat$TP + mat$TN) / 300
	foo[[2]] <- (mat$FN + mat$FP) / 300
	foo[[3]] <- (mat$TP) / (mat$TP + mat$FN)
	foo[[4]] <- (mat$TN) / (mat$FP + mat$TN)
 	foo[[5]] <- (mat$FP) / (mat$FP + mat$TN)
	foo[[6]] <- (mat$FN) / (mat$TP + mat$FN)
	foo[[7]] <- mat$TP / (mat$TP + mat$FP)
	foo[[8]] <- mat$TN / (mat$TN + mat$FN)
	foo[[9]] <- (2 * foo[[7]] * foo[[3]]) / (foo[[7]]+foo[[3]])
	foo[[10]]<- (2 * foo[[8]] * foo[[4]]) / (foo[[8]]+foo[[4]])
	return(foo)
}
print("1")

kc <- kmeans(X,2)

predicted <- kc$cluster

conf_kmean <- confusion(actual,predicted)
metric_kmean <- performance_metric(conf_kmean)
print("Metric for kmeans data")
print(metric_kmean)


# 4.c Applying the kernel PCA on original data set.
require(kernlab)
D <- as.matrix(X)
d <- ncol(X)
k_pca <- kpca(D,kernel = "rbfdot",kpar = list(sigma = 0.1))
P <- eig(feat_rbf)
x11()
plot(P)
pcv <- P[1:75]
print("Number of principal components")
print(length(pcv))
print("Variability of data") 
print(sum(pcv))

# 4.d Projecting the data on vectors
P <- pcv(feat_rbf)
p <- P[,1:75]

# 4.e Running K-means on new data projection
km <- kmeans(p,2)
pred <- km$cluster
conf_pca <- confusion(actual,pred)
metric_pca <- performance_metric(conf_pca)
print(metric_pca)

# 4.g Comparing kernel kmeans with kernal pca and kmeans.
kc <- kkmeans(X,centers=2,kernel="rbfdot")
conf_kmean <- confusion(actual,kc)
metric_kmean <- performance_metric(conf_kmean)
print(metric_kmean)





