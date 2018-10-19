
# K-means
# ---------
# km.out <- kmeans(x, centers = 3, nstart = 1, iter.max = 20)

# iter.max = 10 (default value)


# returned values
# ----------------
# cluster <- a vector of integers (from 1:k) indicating cluster to which each point is allocated
# centers <- matrix of cluster centers
# totss <- the total sum of squares
# withinss <- vector of within-cluster sum of square, one component per cluster
# size <- number of points in each cluster

# simple example
# -------------
x1 <- rnorm(500, sd = 0.3)
x2 <- rnorm(500, mean = 4, sd = 0.3)
db <- as.data.frame(cbind(x1, x2))
plot(db[,c(1,2)])

km_cl <- kmeans(db, centers = 2, nstart = 5)
str(km_cl)
# clusters assigned to each point
km_cl$cluster

# total sum of squares
print(km_cl$totss)

# within-cluster sum of squares
print(km_cl$withinss)

# total within_cluster sum of squares
print(km_cl$tot.withinss)

# between-cluster sum of squares
print(km_cl$betweenss)

# or simply
print(km_cl$totss - km_cl$tot.withinss)

# size of cluster (number of points in each cluster)
print(km_cl$size)

plot(db, col = km_cl$cluster, pch = 20)
points(km_cl$centers,  pch = 10, cex = 4, lwd=3)


data(iris)
# number of centers (K) heruristic
# elbow method

# always standardize data before clustering
iris[,1:4] <- scale(iris[,1:4])

# How many clusters might we need? ss = sum of squares
within_ss <- numeric(6)
for(i in 1:6){
  km.cl <- kmeans(iris[,1:4], centers = i, nstart = 10)
  print(paste("number of clusters:",i," total within ss:",km.cl$tot.withinss))
  within_ss[i] <- km.cl$tot.withinss
}

# plot number of centers Vs. total within SS
plot(x = 1:6, y = within_ss, type='b', xlab = "number of clusters K", ylab = "total within SS" )


# comparing the clustering with 3 centers, with class labels
par(mfrow = c(1,2))
km.cl <- kmeans(iris[,1:4], centers = 3, nstart = 20)

# coloring points with cluster assigned
plot(iris[,1:2],  pch = as.character(km.cl$cluster), cex = 0.7, main="clustering", col = km.cl$cluster)

# coloring points with original class labels
plot(iris[,1:2], pch = as.numeric(iris$Species), main = "class labels", col = km.cl$cluster)

table(km.cl$cluster, iris$Species)


# silhouette() function from "cluster" package
# can be used to calculate average silhouette value
# it is a measure of how similar an object is to its own cluster
# compared to other clusters
distance_matrix <- dist(iris[,1:4], method = "euclidean")
a <- silhouette(km.cl$cluster, dist = distance_matrix)
average_silhouette_val <- mean(a[,3])
print(average_silhouette_val)


# 2. K-means++
# ----------------
# install.packages("LICORS")
library("LICORS")

kmpp.cl <- kmeanspp(iris[,1:4], k = 3)
plot(iris[,3:4], col = kmpp.cl$cluster, pch = as.character(kmpp.cl$cluster), cex = 0.7, main="k-means++")
points(kmpp.cl$centers[,c(3,4)], pch = c("1","2","3"), cex = 3, lwd=3)
table(kmpp.cl$cluster, iris$Species)


# 3. K-medoids
# ----------------
# install.packages("cluster")
library("cluster")
# pam(x, k, diss = FALSE, metric = "euclidean", stand = FALSE)
# kmed.cl <- pam(iris[,1:4], k = 3)
kmed.cl <- pam(iris[,1:4], k = 3, metric = "manhattan")
plot(iris[,1:2], col = kmed.cl$clustering, pch = as.character(kmed.cl$clustering), cex = 0.7, main="k-medoids")
points(kmed.cl$medoids, pch = c("1","2","3"), cex = 2, lwd=3)
table(kmed.cl$clustering, iris$Species)

# plot function in cluster package
plot(kmed.cl, main = "Clustering Information")

# average silhouette value
kmed.cl$silinfo$avg.width


# ruspini data
data(ruspini)
plot(pam(ruspini, k = 4), main="Ruspini Data")


# pam is slow when dataset is large
# so often clara() is used
kmed.clara <- clara(iris[,1:4], k = 3, metric = "manhattan")
plot(iris[,1:2], col = kmed.clara$clustering, pch = as.character(kmed.clara$clustering), cex = 0.7)
points(kmed.clara$medoids, pch = c("1","2","3"), cex = 2, lwd=3)

table(kmed.clara$clustering, iris$Species)


# 4. k-modes
# ----------------
# install.packages("klaR")
library("klaR")

set.seed(137)
df <- rbind(matrix(rbinom(250, 1, 0.25), ncol = 5), matrix(rbinom(250, 1, 0.75), ncol = 5))
colnames(df) <- c("A", "B", "C", "D", "E")
head(df)

kmodes.cl <- kmodes(df, modes = 3, iter.max = 50)
plot(jitter(df), col = kmodes.cl$cluster, main = "kmodes")
points(kmodes.cl$modes, col = 1:3, pch = c("1","2","3") , cex=3, lwd=3)


# 5. Jaccard distance
# distance between binary vectors (0 and 1)
# ------------------------------------------
# install.packages("proxy")
library("proxy")

# set.seed(101)
# samples <- sample(c(FALSE, TRUE), 8, rep = TRUE)
# x <- matrix(samples, ncol = 2)
# dist(x, method = "Jaccard")

j_dist <- dist(df, method = "Jaccard")
kmed.cl.j <- pam(j_dist, k = 3, diss = TRUE)
medoids <- df[kmed.cl.j$id.med,]
plot(jitter(df), col = kmed.cl.j$clustering, main="kmedoid with jaccard")
points(medoids, col = 1:3, pch = c("1","2","3") , cex=3, lwd=3)



# HIERARCHICAL CLUSTERING
# -------------------------------------------

# hierarchial clustering function
# hclust(d, method = "complete")
# try on synthetic data
set.seed(111)
x1 <- runif(10)
x2 <- runif(10)

df <- cbind(x1, x2)
dist.matrix <- dist(df, method = "euclidean")
syn.hclust <- hclust(dist.matrix, method = "complete")
# agglomeration linkage -> "complete", "single", "average", "centroid", "ward.D", "ward.D2"

# plot(x = iris.h.clust, hang = 0.1, labels = NULL)
# dendogram
plot(syn.hclust, main = "Hierarchical clust in Synthetic data")

str(syn.hclust)

# merging of examples stepwise
print(syn.hclust$merge)

# cophenetic distance between examples
print(syn.hclust$height)

# order of examples for clean plot without crossing paths
print(syn.hclust$order)


# iris dataset
data(iris)
head(iris)

# standardize data before clustering
iris[,1:4] <- scale(iris[,1:4])

# calculate distance matrix (distance between each observations) using dist() function
distance_matrix <- dist(iris[,1:4], method = "euclidean")

iris.h.clust <- hclust(d = distance_matrix, method = "complete")
plot(iris.h.clust, hang = 0.1, main = "Hierarchial Cluster", cex=0.5, label=FALSE)

# cut dendogram into various categories
# cutree(tree, k = NULL, h = NULL), 
# k <- desired number of clusters, h <- height to cut
clusters <- cutree(iris.h.clust, k = 3)
table(clusters, iris$Species)
table(clusters)

# cut the tree at height 3
clusters <- cutree(iris.h.clust, h = 3)
table(clusters, iris$Species)
table(clusters)


# to verify the hcluster tree
# correlation between cophenetic distance (height in dendogram) and original distance (from dist())
dist.coph <- cophenetic(iris.h.clust)
distance_matrix <- dist(iris[,1:4], method = "euclidean")

# correlation
cor(dist.coph, distance_matrix)


# try different agglomeration methods 
iris.h.clust <- hclust(d = distance_matrix, method = "ward.D2")
dist.coph <- cophenetic(iris.h.clust)
cor(dist.coph, distance_matrix)


# library "factoextra" for better visualization of dendograms
# install.packages("factoextra")
library("factoextra")
fviz_dend(iris.h.clust, k = 3, k_colors = c("red","blue","green"),
          color_labels_by_k = TRUE, rect = TRUE, main = "Dendogram Visualization")



# agglomerative method using "cluster" package
library("cluster")
iris.h.clust <- agnes(x = iris[,1:4], # data
                      stand = TRUE, # standardize
                      metric = "euclidean", # distance metric
                      method = "ward" # linkage method
)
fviz_dend(iris.h.clust, k = 3, k_colors = c("red","blue","green"),
          color_labels_by_k = TRUE, rect = TRUE, main = "Dendogram Visualization")


# divisive method
iris.h.clust.d <- diana(x = iris[,1:4],
                        stand = TRUE,
                        metric = "euclidean"
)

fviz_dend(iris.h.clust.d, k = 3, k_colors = c("red","blue","green"),
          color_labels_by_k = TRUE, rect = TRUE, main = "Dendogram Visualization")

clusters <- cutree(iris.h.clust.d, k = 3)
table(clusters, iris$Species)




# CLUSTERING EVALUATION
# ------------------------------------------------

#Silhouette Value
# silhouette() function in "cluster" package

km.cl.med <- pam(iris[,1:4], k = 3, metric = "manhattan")
si <- silhouette(km.cl.med)
plot(si, col = c("red", "green", "blue"),main = "Silhouette Plot")
table(iris$Species, km.cl.med$clustering)

# average silhouette width
km.cl.med$silinfo$avg.width

# Calculating silhouette width
si2 <- silhouette(km.cl.med$clustering, dist(iris[,1:4], "manhattan"))
plot(si2, col = c("red", "green", "blue"),main = "Silhouette Plot")

print(si2)
# calculate the mean of silhouette width for all observations
# average silhouette width
print(mean(si2[,3]))


# Silhouettefor K-means
km.cl <- kmeans(iris[,1:4], centers = 3, nstart = 10)
si3 <- silhouette(km.cl$cluster, dist(iris[,1:4], "euclidean"))
plot(si3)
# average silhouette widht
print(mean(si3[,3]))


# Silhouette for hierarchical
iris.h.clust <- agnes(x = iris[,1:4], # data
                      stand = TRUE, # standardize
                      metric = "euclidean", # distance metric
                      method = "complete" # linkage method
)


#iris.h.clust <- hclust(d = distance_matrix, method = "complete")

clusters <- cutree(iris.h.clust, k = 3)
si4 <- silhouette(clusters, dist(iris[,1:4], metric = "euclidean"))
plot(si4)
print(mean(si4[,3]))

# dunn index and many other statistics of clustering
# dunn index: (min sep)/(max diameter)
# can be used to decide number of clusters K
# choose K that gives maximum value of dunn index

library("fpc")
st <- cluster.stats(dist(iris[,1:4], "euclidean"), km.cl$cluster)
print(st)
print(st$dunn)
print(st$dunn2)



# purity
a = table(km.cl$cluster, iris[,5])
sum(apply(a, 1, max))/sum(a)

# Adjusted Rand Index (-1 to 1)
# install.packages("flexclust")
library("flexclust")
a = table(km.cl$cluster, iris[,5])
randIndex(a)



