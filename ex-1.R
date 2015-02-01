require(ggplot2)

data_cluster <- function(i)
# Creating a sphere of data points with mean=0 and SD=i
{
	x <- round(rnorm(150,0,i))
	y <- round(rnorm(150,0,i))
	r <- cbind(x,y)
	return(r)
}

r <- data_cluster(10)
s <- data_cluster(20)

# 1-a Bad K-means
bad_kmeans <- rbind(r,s)

# 1-b Bad PCA
bad_svm <- rbind(r,s)

# 1-c Bad SVM
bad_pca <- rbind(r,s)

#Plotting each of these Bad data sets
plot_data <- function(mix)
{
	x11()
	plot(mix[,1],mix[,2])
	points (mix[1:150,1],mix[1:150,2],col="red")
	points (mix[151:300,1],mix[151:300,2],col="blue")
}

plot_data(bad_kmeans)
plot_data(bad_svm)
plot_data(bad_pca)
