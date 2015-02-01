data (iris);
 iris;
 X = iris [ , 1:4];
 
pca = princomp (X, center=TRUE);
pca;
x11();
plot (pca); # screeplot
 loadings(pca);      # matrix of eigenvectors
 summary (pca); # check proportion of variance
 P=pca$scores;     # projection of X onto eigenvectors
 plot (P[ ,1], P[ ,2]);
 points (P [1:50, 1], P[1:50,2], col="red");
 points (P [51:100, 1], P[51:100,2], col="blue");
 x11();