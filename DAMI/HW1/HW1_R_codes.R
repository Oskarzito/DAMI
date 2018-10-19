
# Assignment done by: Oskar Emilsson (osem6498)

# *********************************************
# DAMI Preprocessing Exercise R file
# Complete the codes to complete the assignment
# *********************************************

# 1. Import data for analysis to R environment
# Downloaded "Adult" dataset from UCI Machine Learning Repository
# URL http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
# Import dataset in adult_db
# Missing values are represented as "?" in data file, make sure that R read them as missing values (NAs)
# ------------------------------------------------------------------------------------------------------ #
# use read.table(), type ?read.table for help
adult_db <- read.table(file = "./adult.data.txt",
                       header = FALSE,
                       sep = ",",
                       strip.white = TRUE,
                       na.strings = "?") # ****** YOUR CODE GOES HERE ******* DONE
  
  
  
  # Assign attribute names (column names) to the data we just imported
  # Attribute names are in separate file "adult.names", scroll down to the bottom of this file
  # Attribute names such as ("age", "workclass", "fnlwgt",...)
  # Last column of the dataset adult.db with values ">50K" and "<=50K" does not have name, 
  # this is the class attribute, so we just name it as "class"
  
  colnames(adult_db) = c("age",
                         "workclass",
                         "fnlwgt",
                         "education",
                         "education_num",
                         "marital_status",
                         "occupation",
                         "relationship",
                         "race",
                         "sex",
                         "capital_gain",
                         "capital_loss",
                         "hours_per_week",
                         "native_country",
                         "class")


# 2. Check for missing values
# Write code to plot missingness and count missing values each attribute has
# Inorder to plot missingness, you need to first install "Amelia" package
# Hint: use "apply" function along columns of "adult.db", for each column (attribute) find how many NAs are there
# --------------------------------------------------------------------------------------------------------------- #


library(Amelia)
# plot missing values in data
# ****** YOUR CODE HERE ******* # DONE
par(mfrow = c(1,1))
missmap(adult_db, x.cex = 0.8, rank.order = FALSE, legend = FALSE, margins = c(7,2))
# HINT: use missmap()

# count number of missing values in all attributes
# ****** YOUR CODE HERE ***** DONE
amount_missing_values <- apply(adult_db, MARGIN = 2, FUN = function(x){sum(is.na(x))})
############ OWN TEST: ############
amount_missing_values
###################################

# Delete records (rows) with any missing value
adult_db_nomiss <- na.omit(adult_db) # ****** YOUR CODE HERE ******* DONE
############ OWN TEST: ############
apply(adult_db_nomiss, MARGIN = 2, FUN = function(x){sum(is.na(x))})
###################################
  
  
  
# 3. We will take only small chunk of the data for our purpose.
# So, randomly select 1000 records from among 30 thousand records in the dataset.
# ------------------------------------------------------------------------------- #
set.seed(145)
idx = sample(1:nrow(adult_db_nomiss),1500)
adult_db_lim = adult_db_nomiss[idx,]
row.names(adult_db_lim) <- NULL



# 3a. Examine attributes of the dataset
# Plot histogram for numeric attribute "age", with 100 (for <=50K) and 50(for >50K) breaks, 
# show main title and attribute name on the plot.
# --------------------------------------------------------------------------------------------------------

# ******* YOUR CODE FOR HISTOGRAM PLOT GOES HERE ******** # DONE
par(mfrow = c(1,1))
hist(adult_db_lim$age[which(adult_db_lim$class == "<=50K")], breaks = 100, xlab = "Age", main = "Age of adults", col = "red")
hist(adult_db_lim$age[which(adult_db_lim$class == ">50K")], breaks = 50, xlab = "Age", col = "blue", add = T)
legend(x = 60, y = 30, legend = c(">50K", "<=50K"), col = c("blue", "red"), pch = 20, cex = 1.25)
# HINT: use hist()

# ******************************************************* #

# 3b. Plot barchart for categorical attribute "relationship", 
# show legend, attribute name and main title for the plot.
# ******* YOUR CODE FOR BAR CHART GOES HERE ******* # DONE
height_of_cols <- table(adult_db_lim$relationship)
par(mar=c(7,4,2,2))
barplot(height = height_of_cols,
        col = c("black", "red", "green", "blue", "cyan", "purple"),
        main = "Relationship of adults",
        las = 2,
        cex.names = 0.8)
legend(x = 5, y = 500,
       legend = c("Husband", "Not-in-family", "Other-relative", "Own-child", "Unmarried", "Wife"),
       col = c("black", "red", "green", "blue", "cyan", "purple"),
       pch = 20, cex = 0.7)
# HINT: use barplot()
# ************************************************* #


# 3c. Plot a boxplot for attribute "Age" for groups with earning "<=50K", ">50K"
# ------------------------------------------------------------------------------
# ****** YOUR CODE GOES HERE ***** # DONE
par(mfrow = c(1,1))
boxplot(age ~ class,
        data = adult_db_lim,
        pch = 20,
        col = "red",
        main = "Age of adults",
        las = 2)
# HINT: use boxplot()



# 4 Create new data set from our latest dataset with only numeric attributes
# ------------------------------------------------------------------------
adult_db_numeric <- adult_db_lim[,c("age", "fnlwgt", "education_num", "capital_gain", "capital_loss", "hours_per_week")]
class_cat <- adult_db_lim[,c("class")]



# Standardize numeric attributes in "adult_db_numeric" dataset.
# mean = 0 and sd = 1 for all numeric attributes
adult_db_num_std <- scale(adult_db_numeric) # *** YOUR CODE HERE *** # DONE


# we can check the mean and standard deviation of the standardized data
apply(adult_db_num_std, 2, mean)
apply(adult_db_num_std, 2, sd)

#######TEST MEAN (Own test)#######
summary(adult_db_num_std)
##################################


# 5a. Run Principal Component Analysis (PCA) on the numeric dataset from above "adult_db_num_std"
# plot the first 2 principal components
# ------------------------------------------------------------------------------------------

# ******** YOUR CODE FOR GETTING PRINCIPAL COMPONENTS GOES HERE ******** #
# HINT: use prcomp()
pr.out <- prcomp(adult_db_num_std, scale = TRUE, center = TRUE) # *** YOUR CODE HERE prcomp() *** # DONE

adult_db_pca <- pr.out$x

# Set correct colors (not part of assignment, but creates the correct output colorwise since macOS made
# default colors as red and black)
plotColors <- apply(as.array(class_cat), MARGIN = 1, FUN = function(x) {ifelse(toString(x) == "1", "red", "green")})

# ******** YOUR CODE TO PLOT FOR FIRST TWO PCs ****** # DONE
# plot(), legend()
par(mfrow = c(1,1))
plot(adult_db_pca[,1:2], col = plotColors, pch = 20, main = "First two Principal Components", las = 2)
legend(x = 0, y = 8,
       legend = c("<=50K", ">50K"),
       col = c("red", "green"),
       pch = 20, cex = 1.0)

# 5b. Plot percentage of the variance explained by principal components
# ----------------------------------------------------------------------------
# write a code to show proportion of variance explained by each Principal Component
# Standard deviation are stored as "sdev"

# *** YOUR CODE GOES HERE *** # DONE
# Get variance explained by PC:s
pr.var <- (pr.out$sdev)^2
pve <- pr.var/sum(pr.var)
# plot(), legend()
par(mfrow = c(1,2), oma = c(1,1,2,1))
plot(pve, xlab = "Principal Components", ylab = "Variance", type = "b", ylim = c(0,1), col = "red") 
plot(cumsum(pve), xlab = "Principal Components", ylab = "Cumulative variance", type = "b", ylim = c(0,1), col = "red")

# 5c. write answer for this as a comment using #
# ------------------------------------------------------------------------------
# How many principal components are needed to explain 50% and 90% of variance respectively
# Answer: 
# To see how much of the variance is explained by each Principal Component (PC) in percentage, we 
# should divide the variance with the sum of all the variance. That is actually something we've done 
# above with: "pr.var <- (pr.out$sdev)^2" and then "pve <- pr.var/sum(pr.var)". So the percentage 
# of each PC is stored in the variable 'pve'. Hence, to explain 50% of variance, we need AT LEAST 
# 3 PC:s, since 2 PC:s only explain about 40% (which is below 50%).
# 
# To explain 90%, we need AT LEAST all 6 PC:s (which explains 100%). 5 PC:s only explains about 87.5% 
# ish (which is below 90%).
#
# We could also, instead of counting ourselves, look at the plot of the cumulative sum of each PC. There 
# we have a pretty good diagram saying showing how many PC:s are explaining how much variance in percentage.
#
# Assignment done by: Oskar Emilsson (osem6498)
# ------------------------------------------------------------------------------
