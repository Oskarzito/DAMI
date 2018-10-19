
# get data from
# http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data

breast.cancer <- read.table(file="./bcw.data.txt",
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

# 2 => benign, 4 => malignant
table(breast.cancer.db$Cancer)


# edit class label for clarity

index <- (breast.cancer.db$Cancer == 2)
breast.cancer.db[index, "Cancer"] <- "Benign"
breast.cancer.db[!index, "Cancer"] <- "Malignant"

breast.cancer.db$Cancer <- as.factor(breast.cancer.db$Cancer)

table(breast.cancer.db$Cancer)

# Split data into train and test set
# ----------------------------------------------------------- #
set.seed(113)
train.idx <- sample(1:nrow(breast.cancer.db), size = 0.8 * nrow(breast.cancer.db), replace = FALSE)

train.set <- breast.cancer.db[train.idx,]
test.set <- breast.cancer.db[-train.idx,]

# Train random forest model on the train set and test on testset
# -------------------------------------------------------------- #
library(randomForest)

# randomForest(formula, data, ntree, mtry, importance, nodesize)
# train model               # Class label is Cancer and I'm using all the attributes
rforest.model <- randomForest(Cancer ~ ., data = train.set, ntree = 100, importance = TRUE)

# predict on test set using trained model
rforest.pred <- predict(rforest.model, newdata = test.set, type = "response") # use type = "prob" for probability

table(Predictions = rforest.pred, Actual = test.set$Cancer)


# 1. Model Evaluation Metrics
# ----------------------------------
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

# roc curve (plot the curve)    (True positive rate, False positive rate)
roc.curve <- performance(pred2, "tpr", "fpr")
plot(roc.curve)
lines(x = c(0,1), y = c(0,1))

# AUC (Area Under ROC)
auc <- unlist(attr(performance(pred2, "auc"), "y.values"))
print(auc)
legend("bottomright", sprintf("%.3f",auc), title = "AUC")

# precision recall curve
plot(performance(pred2, "prec", "rec"))

# sensitivity specificity plot
plot(performance(pred2, "sens", "spec"))

# lift chart
plot(performance(pred2, "lift", "rpp"))

#2 Cross Validation
# -----------------------
library("caret")

head(breast.cancer.db)

set.seed(105)
num_folds <- 10                   #Class label
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



#3 Deploy
# -----------------------------
# install.packages("plumber")
library("plumber")
r <- plumb("function.files.R")
r$run(port = 8000)

# more in https://www.rplumber.io/ 



#4 PMML (Predictive Model Markup Language)
# install.packages("pmml")
library(pmml)
logistic.reg.model <- glm(Cancer ~ ., data = train.set, family = "binomial")
pmml.logreg <- pmml(logistic.reg.model)
saveXML(pmml.logreg, file = "logmodel.pmml")

#5 shiny package, interactive web apps using R
# install.packages("shiny")
# https://shiny.rstudio.com/

source("shiny.example.R")
shinyApp(u,s)
