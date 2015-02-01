source("ex-1.R")

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
actual <- list()
#Actual class of points
actual <- matrix(c(rep(1,150),rep(2,150)))

#Metrics for K-means clustering.
kc <- kmeans(bad_kmeans,2)
predicted <- kc$cluster

conf_kmean <- confusion(actual,predicted)
metric_kmean <- performance_metric(conf_kmean)
print("Metric for bad kmeans data")
print(metric_kmean)


#Metrics for SVM
library(rpart)

## Prepare a training and a test set ##
T <- cbind(bad_svm,actual)
D <- data.frame(T)
index <- 1:nrow(D)
testindex <- sample(index,trunc(length(index)/4))
testset <- D[testindex,]
trainset <- D[-testindex,]
  
rpart.model <- rpart(actual~ .,data=D[,-3])
print(rpart.model)
rpart.pred <- predict(rpart.model,testset[,-3])

#Calculating metrics for SVM
rpart.pred[rpart.pred>=1.5] = 2
rpart.pred[rpart.pred<1.5] = 1
print("Confusion matrix for SVM Data prediction")
svm_conf <- table(rpart.pred,testset[,3])
print(svm_conf)
acc <- (svm_conf[1,1]+svm_conf[2,2])/nrow(testset)
print("Accuracy of SVM")
print(acc)

# Metrics for PCA
X <- bad_pca
pca = princomp (X, center=TRUE);
#print(loadings(pca));      # matrix of eigenvectors
#print(summary (pca)); # check proportion of variance
P=pca$scores;     # projection of X onto eigenvectors
x11()
plot (P[ ,1], P[ ,2],main="Plot PCA data");
points (P [1:150, 1], P[1:150,2], col="red");
points (P [151:300, 1], P[151:300,2], col="blue");

print("Metric for Bad PCA data")
km <- kmeans(P,2)
pred <- km$cluster
conf_pca <- confusion(actual,pred)
metric_pca <- performance_metric(conf_pca)
print(metric_pca)

