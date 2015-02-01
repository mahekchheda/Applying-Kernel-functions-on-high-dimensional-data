source("ex-1.R")
require(kernlab)

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
		else
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
actual <- list()
#Actual class of points
actual <- matrix(c(rep(1,150),rep(2,150)))

#Applying Kernelization on Bad data sets
print("Applying Radial basis kernel function on kmeans data sets")
kc <- kkmeans(bad_kmeans,centers=2,kernel="rbfdot")
conf_kmean <- confusion(actual,kc)
metric_kmean <- performance_metric(conf_kmean)
print(metric_kmean)

print("Applying Laplace Kernel Function on kmeans data sets")
kc <- kkmeans(bad_kmeans,centers=2,kernel="laplacedot")
conf_kmean <- confusion(actual,kc)
metric_kmean <- performance_metric(conf_kmean)
print(metric_kmean)


#######Applying kernel tricks on SVM data sets#########
require(e1071)

metric_calc <- function(model)
{
	print(model)
	ypred = predict(svp,xtest)
	print("Predict labels on test")
	print(table(ytest,ypred))
	print("Accuracy of the prediction")
	print(sum(ypred==ytest)/length(ytest))
	ypredscore = predict(svp,xtest,type="decision")
	print("Prediction scores")
	print(table(ypredscore > 0,ypred))

}

ntrain <- round(300*0.6) # number of training examples
tindex <- sample(300,ntrain) # indices of training samples
xtrain <- bad_svm[tindex,]
xtest <- bad_svm[-tindex,]
ytrain <- actual[tindex]
ytest <- actual[-tindex]
istrain=rep(0,300)
istrain[tindex]=1

print("Applying Linear kernel function on bad data sets")
svp_lin <- ksvm(xtrain,ytrain,type="C-svc",kernel="vanilladot")
metric_ldot <- metric_calc(svp_lin)

 print("Applying Gaussian kernel function on bad data sets")
svp_rbf <- ksvm(xtrain,ytrain,type="C-svc",kernel="rbfdot")
metric_rbf <- metric_calc(svp_rbf)


#################Applying Kernel tricks on PCA data sets################
print("Applying kernel tricks on PCA bad data sets")

metric_pca <- function(feat,name)
{
	P <- pcv(feat)
	x11()
	plot (P[ ,1], P[ ,2],main=name);
	points (P [1:150, 1], P[1:150,2], col="red");
	points (P [151:300, 1], P[151:300,2], col="blue");
	km <- kmeans(P,2)
	pred <- km$cluster
	conf_pca <- confusion(actual,pred)
	metric_pca <- performance_metric(conf_pca)
	print(metric_pca)
}

print("Applying Linear Gaussian radial basis function")
feat_rbf <- kpca(bad_pca,kernel="rbfdot")
name <- "Gaussian Radial basis function"
metric_rbf <- metric_pca(feat_rbf,name)

print("Applying Laplacian kernel basis function")
feat_pol <- kpca(bad_pca,kernel="laplacedot")
name <- "Laplacian Kernel basis function"
metric_pol <- metric_pca(feat_pol,name)




