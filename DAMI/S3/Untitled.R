
db <- read.table("./bcw.data.txt",
                 header = FALSE,
                 sep = ",",
                 na.strings = "?",
                 strip.white = TRUE)

db <- subset(db, select = 2:10)

head(db)
tail(db)
summary(db)

db_std <- scale(db)

db_nomiss <- na.omit(db_std)

head(db_nomiss)
tail(db_nomiss)
summary(db_nomiss)
apply(db_nomiss, 2, sd)


within_ss <- numeric(9)
for(i in 1:9){
  km.cl <- kmeans(db_nomiss, centers = i, nstart = 10)
  print(paste("num clusters: ", i, " total within sum of squares: ", km.cl$withinss))
  within_ss[i] <- km.cl$tot.withinss
}

plot(x = 1:9, y = within_ss, type = "b", xlab = "num of clusters K", ylab = "total within sum of squares")
