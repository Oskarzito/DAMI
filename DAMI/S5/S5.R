
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
breast.cancer.db[,1:9] <- scale(breast.cancer.db[,1:9])

# 2 => benign, 4 => malignant
table(breast.cancer.db$Cancer)


# edit class label for clarity

index <- (breast.cancer.db$Cancer == 2)
breast.cancer.db[index, "Cancer"] <- "Benign"
breast.cancer.db[!index, "Cancer"] <- "Malignant"

breast.cancer.db$Cancer <- as.factor(breast.cancer.db$Cancer)

table(breast.cancer.db$Cancer)



# train and test set
# ------------------------
set.seed(113)
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

logistic.reg.model2 <- glm(Cancer ~ clump_thickness + uniform_cellsize + clump_thickness * uniform_cellsize, 
                           data = train.set, family = "binomial")

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

# library("caret")
# precision <- posPredValue(as.factor(pred), test.set$Cancer)
# recall <- sensitivity(as.factor(pred), test.set$Cancer)
# F1_score <- (2 * precision * recall) /(precision + recall)
# 
# library("ROCR")
# pred2 <- prediction(class.probs, test.set$Cancer )
# PR.curve <- performance(pred2, "prec", "rec")
# plot(PR.curve)
# 
# roc.curve <- performance(pred2, "tpr", "fpr")
# plot(roc.curve)
# lines(x=c(0,1), y= c(0,1))
# auc <- unlist(attr(performance(pred2, "auc"), "y.values"))
# print(auc)
# legend("bottomright", sprintf("%.3f",auc), title = "AUC")



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

tree.pred <- predict(tree.model, newdata = test.set, type = "class") # use type = "vector" for probability
table(Predictions = tree.pred, Actual = test.set$Cancer)

# accuracy
mean(tree.pred == test.set$Cancer)




# 4. Random Forest
# -------------------------------
library(randomForest)
# randomForest(formula, data, ntree, mtry, importance, nodesize)
rforest.model <- randomForest(Cancer ~ ., data = train.set, ntree = 100, importance = TRUE)
rforest.model

rforest.model$err.rate
plot(rforest.model)


rforest.pred <- predict(rforest.model, newdata = test.set, type = "response") # use type = "prob" for probability
table(Predictions = rforest.pred, Actual = test.set$Cancer)
mean(rforest.pred == test.set$Cancer)

# variable importance
rforest.model$importance

# Sort according to importance (Decrease in Accuracy or Gini)
rforest.model$importance[order(rforest.model$importance[,3], decreasing = TRUE),]

# plot variable importance according to decrease in accuracy 
imp.score <- sort(rforest.model$importance[,3], decreasing = FALSE)
par(las=2, oma = c(1,6,1,0), cex = 0.8)
barplot(imp.score, horiz = TRUE, col = "red", main = "Variable Importance")

# using varImpPlot
dev.new(10,10)
varImpPlot(rforest.model)


#5. Adaboost
library("fastAdaboost")
adbost.model <- adaboost(Cancer ~ ., data = train.set, nIter = 10)
pred <- predict(adbost.model, newdata = test.set)

library(tree)
tree1 <- adbost.model$trees[[1]]
dev.new(10,10)
plot(tree1)
text(tree1)



# 6. SVM
# ------------------------------
library("e1071")
# svm(formula, data, scale = TRUE, kernel = "linear", "polynomial" "radial", degree, gamma, cost)
svm.model <- svm(formula = Cancer ~ ., data = train.set, kernel = "linear", cost = 10, probability = TRUE)
svm.pred <- predict(svm.model, newdata = test.set, probability = TRUE)

plot(svm.model, train.set, bare_nuclei ~ uniform_cellsize)

table(Predictions = svm.pred, Actual = test.set$Cancer)
mean(svm.pred == test.set$Cancer)
a <- attr(svm.pred, 'prob')
print(a)



# 7. Naive Bayes
# -------------------------------
library("e1071")
# naiveBayes(formula, data)
# predict(object, newdata, type = c("class", "raw"))

nb.model <- naiveBayes(Cancer ~ ., data = train.set)
nb.pred <- predict(nb.model, newdata = test.set, type = "class")
table(Predictions = nb.pred, Actual = test.set$Cancer)
mean(nb.pred == test.set$Cancer)

nb.pred.prob <- predict(nb.model, newdata = test.set, type = "raw")
nb.pred.prob



#8. Neural Network
library(neuralnet)

breast.cancer.db$Cancer <- (as.numeric(breast.cancer.db$Cancer)-1)

set.seed(113)
train.idx <- sample(1:nrow(breast.cancer.db), size = 0.8 * nrow(breast.cancer.db), replace = FALSE)
train.set2 <- breast.cancer.db[train.idx,]
test.set2 <- breast.cancer.db[-train.idx,]


NN = neuralnet(Cancer ~ clump_thickness + uniform_cellsize + uniform_cellshape, 
               data = train.set2, hidden = c(3,2), act.fct = "logistic", 
               linear.output = FALSE)

plot(NN)

res <- compute(NN, test.set2[,c("clump_thickness","uniform_cellsize","uniform_cellshape")])

data.frame("actual" = test.set2$Cancer, "predicted" = round(res$net.result))




# 1. Model Evaluation Metrics
# ----------------------------------

library(randomForest)

# randomForest(formula, data, ntree, mtry, importance, nodesize)
# train model
rforest.model <- randomForest(Cancer ~ ., data = train.set, ntree = 100, importance = TRUE)

# predict on test set using trained model
rforest.pred <- predict(rforest.model, newdata = test.set, type = "response") # use type = "prob" for probability

table(Predictions = rforest.pred, Actual = test.set$Cancer)


# Accuracy
mean(rforest.pred == test.set$Cancer)


library("caret")

# Precision / Positive Predictive Value
rf.precision <- posPredValue(as.factor(rforest.pred), test.set$Cancer)
print(rf.precision)

# Recall / Sensitivity
rf.recall <- sensitivity(as.factor(rforest.pred), test.set$Cancer)
print(rf.recall)

# F1 Score
F1_score <- (2 * rf.precision * rf.recall) /(rf.precision + rf.recall)
print(F1_score)


# ROC curve and AUC value
library("ROCR")
class.probs <- predict(rforest.model, newdata = test.set, type = "prob")
pred2 <- prediction(class.probs[,2], test.set$Cancer )


# precision recall curve
PR.curve <- performance(pred2, "prec", "rec")
plot(PR.curve)

# roc curve
roc.curve <- performance(pred2, "tpr", "fpr")
plot(roc.curve)
lines(x = c(0,1), y = c(0,1))

# AUC (Area Under ROC)
auc <- unlist(attr(performance(pred2, "auc"), "y.values"))
print(auc)
legend("bottomright", sprintf("%.3f",auc), title = "AUC")


#2 Cross Validation
# -----------------------
library("caret")

head(breast.cancer.db)

set.seed(105)
num_folds <- 10
k_fold_data <- createFolds(breast.cancer.db$Cancer, k = num_folds)

# cv accuracy in each fold
cv.accuracy <- numeric(num_folds)

for(i in 1:num_folds){
  test.set.idx <- k_fold_data[[i]]
  
  test.set = breast.cancer.db[test.set.idx,]
  train.set = breast.cancer.db[-test.set.idx,]
  
  rforest.model <- randomForest(Cancer ~ ., data = train.set, ntree = 100, importance = FALSE)
  rforest.pred <- predict(rforest.model, newdata = test.set, type = "response")
  
  # fold accuracy
  cv.accuracy[i] <- mean(rforest.pred == test.set$Cancer)
}

print(cv.accuracy)

# average accuracy over 10 folds
print(mean(cv.accuracy))
