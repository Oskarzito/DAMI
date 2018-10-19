################################# DAMI Programming Assignment 2: Clustering ##################################

## IMPORTANT! In this document, when you see [FIXME], that is where you should fill in your corresponding R code! 

## If would like to implement every part by yourself, in your submissions, you just comment out the provided codes, and write down your own codes in the corresponding place instead!

## IMPORTANT! Download the synthetic data "cluster.RData" from the course website. This data need to be used throughout the assignment!
## Save the data to your current working directory


# Load the data in your R environment
# This should give you dataframe "cluster.data" 
load("cluster.RData")


########################################
#   DONE BY: OSKAR EMILSSON - OSEM6498 #
########################################

###### Part 1: Distance Calculation ######
# TASK 1. Writing a function for distance calculation:

# parameters "a" and "b" are two vectors 
# Manhattan distance is calculated between "a" and "b" when metric = "manhattan"
# otherwise Euclidean distance is calculated and returned by the function
# NOTE: Implement the equation for distance calculation
my_dist_calculator <- function(a, b, metric = "euclidean"){
  #Save length of longest vector out of a and b
  len_of_longest_vec <- ifelse(length(a) > length(b), length(a), length(b))
  
  
  if(metric == "manhattan"){
    # write code for calculating manhattan distance, use the formula
    mydistance <- manhattanDist(a, b, len_of_longest_vec) #"FIXME"    See function below for calculation
    
  }else{
    # write code for calculating euclidean distance, use the forumula
    mydistance <- euclideanDist(a, b, len_of_longest_vec) #"FIXME"    See function below for calculation
    
  }
  
  return(mydistance) 
}

euclideanDist <- function(a, b, lengthOfLongest){
  idxA <- 0
  idxB <- 0
  sum <- 0
  for(i in 1:lengthOfLongest){
    if(i > length(a)){
      idxA <- 0 
    }else{
      idxA <- a[i]
    }
    if(i > length(b)){
      idxB <- 0 
    }else{
      idxB <- b[i]
    }
    sum <- sum + ((idxA - idxB)^2)
  }
  return(sqrt(sum))
}

manhattanDist <- function(a, b, lengthOfLongest){
  idxA <- 0
  idxB <- 0
  sum <- 0
  for(i in 1:lengthOfLongest){
    if(i > length(a)){
      idxA <- 0 
    }else{
      idxA <- a[i]
    }
    if(i > length(b)){
      idxB <- 0 
    }else{
      idxB <- b[i]
    }
    sum <- sum + (idxA - idxB)
  }
  return(sum)
}

# Test your function on these two vectors
a = c(1.7, 5)
b = c(4, 72)
# Call your function
my_dist_calculator(a, b, metric = "manhattan")
my_dist_calculator(a, b)


###### Part 2: K-Means Clustering #######
# TASK 2a. Write a function for performing K-Means clustering

# K-means steps
# 1. Assign random cluster ID (from 1 to K) to each observation
# 2. Compute mean (centroid) of observations belonging to each K clusters
# 3. Compute distance of each observations from all K centroids
# 4. Reassign each observation to a cluster with nearest centroid (reassign cluster ID)
# 5. Repeat from step 2 to 4 until cluster ID do not change or maximum iteration reached


# Function takes parameter "x" as dataframe to be clustered
# "k" as a number of clusters and "max.iter" as maximum allowed iteration before convergence
# Use the Euclidean distance calculator from part 1.

k_means <- function(x, k, max.iter = 20){
  
  # each observation is assigned a random cluster (id) from 1 to k
  random_index <- sample(1:k, nrow(x), replace = TRUE)
  
  # add a new column "cluster" to dataset (x) that carries cluster id for each observation
  data_with_cluster <- cbind(x, clusterID = random_index)
  
  # initialize number of iteration counts to 1
  # this is used to stop iterating when maximum limit is reached
  iterations = 1
  
  # plot the data points
  plot(data_with_cluster[,1:2])
  
  # this block of code tries to reassign new cluster ID to observations
  # based on which cluster centroid it is nearest to
  # repeat the process until the clusterID for observations do not change
  # or number of interation reach maximum limit
  while(TRUE){
    
    # create a new object "centroids" to hold the mean (centroid) of each cluster
    # It is a matrix whose number of rows equals to k (number of clusters)
    # and number of columns equals number of columns in original data (x)
    # initialize this matrix with 0 values
    centroids <- matrix(rep(0, times = k * ncol(x)), nrow = k, ncol = ncol(x))
    
    # compute the mean of observations in each cluster and save them in "centroids"
    for(i in 1:k){
      # find the rowids of observations in cluster i, use which() function
      obs_of_cluster_i <- which(data_with_cluster$clusterID == i) #"FIXME" DONE

      # find the column mean (use colMeans()) of observations with rowids = "obs_of_cluster_i"
      # and save in corresponding row in "centroids" matrix
      num_observations <- length(obs_of_cluster_i)
      col_count <- ncol(data_with_cluster)
      
      subset_with_cluster <- matrix(NA, ncol = col_count, nrow = num_observations)
      subset_with_cluster <- data_with_cluster[obs_of_cluster_i,]
      centroids[i,] <- colMeans(subset_with_cluster[,1:ncol(subset_with_cluster)-1]) #"FIXME" DONE
    }
    
    # ---------------------------------------------- #
    # plots the centroids discovered in each iteration
    points(centroids[,1:2], pch = 20, cex = 2)
    # waits until some charater is fed
    # this will help to track changing centroids in each iteration
    readline(prompt = "Press Enter to continue:")
    # ---------------------------------------------- #
    
    # Calculate an euclidean distance of each observation from all k centroids
    # Distance matrix "dist_from_centroids" has as many rows as x and k columns
    # to store distances
    # First column holds distances of each observations from first centroid
    # and similarly second and third column has distance from corresponding centroids.
    dist_from_centroids <- matrix(rep(0, nrow(x) * k), nrow = nrow(x), ncol = k)
    
    # Compute the distance between centroid and each observation 
    
    for(i in 1:nrow(x)){
      for(j in 1:nrow(centroids)){
        # Use the euclidean distance calculation function created in TASK 1.
        dist_from_centroids[i,j] <- my_dist_calculator(as.numeric(x[i,]), centroids[j,], metric = "euclidean") #"FIXME" DONE
      }
    }
    
    # from the distance matrix computed, find for each observation the closest cluster centroid
    obs_new_clusterID <- apply(dist_from_centroids, 1, which.min)
    
    # If the centroid is not changing any more for each observation, stop the iteration
    if(all(obs_new_clusterID == data_with_cluster$clusterID)){ 
      km.clusters <- obs_new_clusterID
      centroid.matrix <- centroids
      break
      # If number of iterations exceed the maximum iterations allowed, stop the iteration
    }else if(iterations > max.iter){
      break
      
      # Otherwise, assign the new centroids to the dataset, and continue the iteration
    }else{ 
      data_with_cluster$clusterID <- obs_new_clusterID
      iterations <- iterations + 1
    }
  }
  
  # Plot the final cluster after iteration stops
  plot(data_with_cluster[,1:2], col = data_with_cluster$clusterID)
  points(centroid.matrix[,1:2], pch = 20, cex = 2, col = 1:k)
  
  # when iterations are done, return clusters assigned and final centroid
  return(list("clusters" = km.clusters, "centroids" = centroid.matrix))
}

# call the K-Means function we just created
km_clusters <- k_means(cluster.data, k = 3, max.iter = 15)

# look into the object returned by our function
print(km_clusters)




# TASK 2b. Elbow Heuristics to determine best guess for value of K in kmeans().
# For this part, use built-in kmeans() function in R
# K-means is used on synthetic data for values of K ranging from 1 to 7
# total within sum of square for each one of them is saved

# initialize a vector to store total within ss for different values of K
within_ss <- numeric(7)

# Run k means seven times
for(k in 1:7){
  # call built-in kmeans() function in R
  km.cl <- kmeans(cluster.data, centers = k, nstart = 10) #"FIXME" DONE
  
  # save total within ss value for different values of k
  within_ss[k] <- km.cl$tot.withinss #"FIXME" DONE
}

# plot number of centers Vs. total within SS
plot(x = 1:7, y = within_ss, type='b', xlab = "number of centers K", ylab = "total within SS" )

# Based on the plot, write a comment describing what value of K would you choose.
# [YOUR ANSWER HERE]
# Answer: 
# The elbow heuristics shows us at what point we no longer significantly decrease a lot in our 
# sum of square distance when introducing more clusters. In the plot, we can see that going from 
# 1 to 2, and then 3, clusters minimizes the total sum of squares by a lot! But when going from 3 
# to 4, we don't really decrease the total within ss by that much. And every cluster we introduce after 4 
# doesn't result in a much better total within ss. Therefore, I would choose either 3 or 4 as my 
# value of K. Because I'd say there's not really enough significant gain in total within ss cost 
# reduction when choosing 4 instead of 3, I believe that K = 3 would be the optimal solution.



###### Part 3: K-Medoids Clustering ######

# TASK 3a. Use pam() in "cluster" library for k-medoids clustering. Try with value of k = 3
# use manhattan metric for distance calculation.
# install.packages("cluster")
library('cluster')
# perform clustering using pam()
kmed.cl <- pam(cluster.data, k = 3, metric = "manhattan") #"FIXME" DONE

# plot points
plot(cluster.data, col = kmed.cl$clustering, pch = as.character(kmed.cl$clustering), cex = 0.7, main="k-medoids")
# show medoids in the plot
points(kmed.cl$medoids, pch = c("1", "2", "3"), cex = 2, lwd = 3) #"FIXME" DONE


# TASK 3b. Calculate Silhouette value for clustering for K ranging from 2 to 5.
# Use manhattan distance

# dataframe to hold Silhouette value
sil_info <- data.frame("K" = 2:5, "Sil" = numeric(4))

# repeat pam() for value of k from 2 to 5
# average silhouette width can be accessed as "silinfo$avg.width"

for(k in 2:5){
  kmed.cl <- pam(cluster.data, k = k, metric = "manhattan") #"FIXME" DONE
  sil_info$Sil[k-1] <- kmed.cl$silinfo$avg.width  #"FIXME" DONE
}

print(sil_info)

# Based on the table above, what value of K gives the best result
# [WRITE YOUR ANSWER HERE]
# Answer:
# The Silhouette coefficient ranges from -1 to 1. In the table above, all the 
# values are between 0 and 1. The highest of those is the one I'd choose. This 
# is because a value closer to 1 indicates a better clustering according to the 
# Silhouette coefficient. When K = 3, we get a Silhouette value of 0.4198269. 
# This is the highest of all Silhouette values when K goes from 2 through 5,
# and therefore, I'd choose K = 3.



###### Part 4: Hierarchical Clustering ######

# TASK 4a. Perform hierarchical clustering using built-in R function
# Create smaller dataset of only 20 observations randomly picked

set.seed(101)
rand.sample <- sample(1:nrow(cluster.data), 20)
small.dataset <- cluster.data[rand.sample,]
rownames(small.dataset) <- 1:20

# Perform clustering on "small.dataset"
# Calculate distance matrix using dist() function in R

distance_matrix <- dist(small.dataset, method = "euclidean") #"FIXME" DONE

# Call a built-in R function to perform hierarchical clustering
hierarchial.clust <- hclust(distance_matrix, method = "average") #"FIXME" DONE
plot(hierarchial.clust, hang = 0.1, main = "Hierarchical Cluster", cex = 1)


# TASK 4b. Cut the dendrogram to get three clusters
#
clusters <- cutree(hierarchial.clust, k = 3) #"FIXME" DONE
plot(small.dataset, xlab = "x", ylab = "y", col = clusters, pch = as.character(clusters)) #"FIXME" DONE

######################################
# Done by: Oskar Emilsson - osem6498 #
######################################

########################################### END ##############################################
