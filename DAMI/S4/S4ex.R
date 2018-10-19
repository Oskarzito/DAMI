

db <- read.table("./cleveland.data.txt", 
                 header = FALSE,
                 sep = ",",
                 na.strings = "?",
                 strip.white = TRUE)

head(db)

names(db) <- c("age",
               "sex",
               "cp",
               "trestbps",
               "chol",
               "fbs",
               "restecg",
               "thalach",
               "exang",
               "oldspeak",
               "slope",
               "ca",
               "thal",
               "num")

db <- na.omit(db)


db[which(db$num != 0), "num"] <- "YES"
db[which(db$num == 0), "num"] <- "NO"

table(db$num)

# Man måste göra Strängar till en factor för att det ska gå igenom decision trees tree()-funktion
db$num <- as.factor(db$num)

head(db)

# Select 80% for training, 20% for testing
set.seed(113)
training_index <- sample(1:nrow(db), size = 0.8 * nrow(db), replace = FALSE)

trainingSet <- db[training_index,]
testingSet <- db[-training_index,]

library("tree")

dTree <- tree(num ~ ., data = trainingSet)
plot(dTree)
text(dTree, cex = 0.5)

prediction <- predict(dTree, newdata = testingSet, type = "class")
table(Predictions = prediction, Actual = testingSet$num)

mean(prediction == testingSet$num)

head(dTree)
