
# CLUSTERING EVALUATION
# ------------------------------------------------

# Internal evaluation metric
# -------------------------

#Silhouette Value
# S(i) = {b(i) - a(i)}/max{a(i),b(i)}
# value between -1 and 1, higher is better
# silhouette() function in "cluster" package
library("cluster")

km.cl.med <- pam(iris[,1:4], k = 3, metric = "manhattan")
si <- silhouette(km.cl.med) #si info about silhouette values
dev.new(8,8)
plot(si, col = c("red", "green", "blue"),main = "Silhouette Plot")
table(iris$Species, km.cl.med$clustering) #distribution match with class label

# average silhouette width
km.cl.med$silinfo #average silhouette value for each cluster
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

#avg sil for hierach clustering
clusters <- cutree(iris.h.clust, k = 3)
si4 <- silhouette(clusters, daisy(iris[,1:4], metric = "euclidean"))
plot(si4)
print(mean(si4[,3]))


# dunn index: (min sep)/(max diameter)
# high intercluster distance, low intracluster distance

library("fpc")
km.cl <- kmeans(iris[,1:4], centers = 3, nstart = 10)
st <- cluster.stats(dist(iris[,1:4], "euclidean"), km.cl$cluster) #dist only for numerinc. daisy works for categories too
print(st)
print(st$dunn)




# External Evaluation
# --------------------
km.cl <- kmeans(iris[,1:4], centers = 3, nstart = 10)
# purity
# Each cluster is assigned to a class that is most frequent
class_label <- iris$Species
a = table(km.cl$cluster, class_label)
print(a)
apply(a, 1, max)
sum(apply(a, 1, max))/sum(a)
# simple and intuitive
# disadvantage: each object to its own cluster, purity 1



# Adjusted Rand Index (-1 to 1)
# measures percentage of correct decision taken, given a pair, is it assigned to same cluster
# contingency table of pairs, (TP+TN)/(TP+TN+FP+FN)
# robust version of rand index, ARI

# install.packages("flexclust")
library("flexclust")
a = table(km.cl$cluster, class_label)
randIndex(a)




###  CLASSIFICATION I ###
### ***************** ###

# get data from
# http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data

breast.cancer <- read.table(file="./breast-cancer-wisconsin.data.txt",
                            header = FALSE, sep=",", 
                            colClasses = NA, stringsAsFactor = FALSE, na.strings = "?")

# first column unique ID removed
breast.cancer.new <- breast.cancer[,2:11] 

names(breast.cancer.new) <- c("clump_thickness",
                              "uniform_cellsize",
                              "uniform_cellshape",
                              "marginal_adhesion",
                              "epithelial_cellsize",
                              "bare_nuclei",
                              "bland_chromatin",
                              "normal_nucleoli",
                              "mitoses",
                              "Cancer")

breast.cancer.db <- na.omit(breast.cancer.new)

# standardize the data
# breast.cancer.db[,1:9] <- scale(breast.cancer.db[,1:9])

# 2 => benign, 4 => malignant
table(breast.cancer.db$Cancer)


# edit class label for clarity

breast.cancer.db[which(breast.cancer.db$Cancer == 2), "Cancer"] <- "Benign"
breast.cancer.db[which(breast.cancer.db$Cancer == 4), "Cancer"] <- "Malignant"

breast.cancer.db$Cancer <- as.factor(breast.cancer.db$Cancer)

table(breast.cancer.db$Cancer)



# train and test set
# ------------------------
set.seed(113) #select training data
train.idx <- sample(1:nrow(breast.cancer.db), size = 0.8 * nrow(breast.cancer.db), replace = FALSE)

train.set <- breast.cancer.db[train.idx,]
test.set <- breast.cancer.db[-train.idx,]


table(breast.cancer.db$Cancer)
table(train.set$Cancer)
table(test.set$Cancer)

#1. Logistic Regression
# ------------------------
logistic.reg.model <- glm(Cancer ~ ., data = train.set, family = "binomial")
summary(logistic.reg.model)
logistic.reg.model$coefficients

# prefer less AIC, penalize for number of model coefficient
# Null and residual deviance, lower the better

logistic.reg.model2 <- glm(Cancer ~ clump_thickness + uniform_cellsize + clump_thickness * uniform_cellsize, 
                           data = train.set, family = "binomial")

# for linear regression, family = "gaussian"
# also lm() function fits linear regression model

coef(logistic.reg.model2)


# returns probabilities
class.probs <- predict(logistic.reg.model, newdata = test.set, type = "response")

# probability assigned by model for each observations to be of one class
# either "Benign" or "Malignant"
head(class.probs)

# check which class is 0 and which one is 1
# Class type with 1 is the target class type (class type we try to predict)
levels(train.set$Cancer)
contrasts(train.set$Cancer)

# predict class label, convert probability to class label
# assume probability 0.50 as cuttoff point 
pred <- ifelse(class.probs >= 0.50, "Malignant", "Benign")



table(pred)

table(Prediction = pred, Actual = test.set$Cancer)

# accuracy
mean(pred == test.set$Cancer)


#2. K- Nearest Neighbours
# -----------------------------
library("class")
# knn(train, test, cl, k = 1, prob = FALSE)
knn.pred <- knn(train = train.set[,1:9],
                test = test.set[,1:9],
                cl = train.set$Cancer,
                k = 3,
                prob = TRUE)

attr(knn.pred, "prob")

table(knn.pred, test.set$Cancer)
# accuracy
mean(knn.pred == test.set$Cancer)


# 3. Decision Treees
# -------------------------------
library("tree")
# tree(formula, data, weights)

tree.model <- tree(Cancer ~ ., data = train.set)
plot(tree.model)
text(tree.model, cex = 0.5)

tree.pred <- predict(tree.model, newdata = test.set, type = "class") 
table(Predictions = tree.pred, Actual = test.set$Cancer)
# accuracy
mean(tree.pred == test.set$Cancer)

# use type = "vector" for probability while using predict()
tree.prob <- predict(tree.model, newdata = test.set, type = "vector") 
head(tree.prob)



