# Team 5 (Terrance Taubes, Noura Azeem, Omar Abdeen)
# CSCI 6907_80 Big Data and Analytics
# Class Project 2
# Due: November 6, 2017


###  [ Initialize ]  ###

## Get Path, Set Path to Directory
getwd()

# Example Path
setwd("C:/Users/Terry/Desktop/fall2017/big_data/Group-5-Project-2")

## List Files
list.files()

## Imports
library(igraph)
library(cluster)
library(psych)
library(e1071)
library(rgl)
library(RColorBrewer)
library(scales)
library(kernlab)
require(graphics)


par("mar")
par(mar=c(1,1,1,1))

####  1) Data Set: Mushroom  ####

## Info

# - 8124 Mushroom Instances
# - 4208 Edible    (51.8%)
# - 3916 Poisonous (48.2%)

# -   22 Features/attributes
# - 2480 Missing values in attr #11 ("?")


###  [ Load/Handle Data ]  ###
mush_data <- read.csv("C:/Users/Terry/Desktop/fall2017/big_data/Group-5-Project-2/agaricus-lepiota.data", header=FALSE)

## Names
names(mush_data) <- c("class", "cshape", "csurface", "ccolor", "bruises", "odor", "gattach", "gspace", "gsize", "gcolor", "sshape", "sroot", "ssabove", "ssbelow", "scabove", "scbelow", "vtype", "vcolor", "rnumber", "rtype", "spcolor", "popnum", "habitat")

poisonous <- subset(mush_data, class == "p")
edible <- subset(mush_data, class == "e")

nrow(mush_data)
nrow(poisonous)
nrow(edible)

View(mush_data)
mush_data

dim(mush_data)
str(mush_data)
summary(mush_data)
class(mush_data)

head(mush_data)

## Convert Data to Numeric
mush_data.intClass <- mush_data
mush_data.intClass[] <- as.numeric(factor(as.matrix(mush_data)))

## Remove Class Column
mush_data.int <- mush_data.intClass
mush_data.int$class <- NULL
names(mush_data.int)

head(mush_data)
head(mush_data.int)
head(mush_data.intClass)

###  [ Plot Data ]  ###

## Attribute Pair Graphs
pairs(class ~ cshape + csurface + ccolor, data=mush_data)
pairs(class ~ gattach + gspace + gsize, data=mush_data)
# odor <- important (8/9)
pairs(class ~ bruises + odor + gcolor, data=mush_data)
pairs(class ~ sshape + sroot, data=mush_data)
pairs(class ~ ssabove + ssbelow, data=mush_data)
# scbelow <- important (6/9)
pairs(class ~ scabove + scbelow, data=mush_data)
# vcolor <- important (3/4)
pairs(class ~ vtype + vcolor, data=mush_data)
# rtype <- important (3/5)
pairs(class ~ rnumber + rtype, data=mush_data)
pairs(class ~ spcolor + popnum + habitat, data=mush_data)

# features that have been marked important
pairs(class ~ odor + scbelow + vcolor + rtype, data=mush_data)

## Plotting Attributes and Classifications
plot(class ~ cshape + csurface + ccolor, data=mush_data)
plot(class ~ gattach + gspace + gsize, data=mush_data )
plot(class ~ bruises + odor, data=mush_data)
plot(class ~ scabove + scbelow, data=mush_data)
plot(class ~ vtype + vcolor, data=mush_data)
plot(class ~ rnumber + rtype, data=mush_data)

#features that have been marked important
plot(class ~ odor + scbelow + vcolor + rtype, data=mush_data)

## 3D Plotting of Attributes
plot3d(mush_data$cshape, mush_data$csurface, mush_data$ccolor)
plot3d(mush_data$vcolor, mush_data$ccolor, mush_data$gcolor)

plot3d(mush_data$odor, mush_data$scbelow, mush_data$vcolor)
plot3d(mush_data$odor, mush_data$vcolor, mush_data$rtype)



### [ Principal Component Analysis ] ###

## Mushroom PCA
mush_pca <- princomp(mush_data.int)
mush_pca$sdev

## Scree Plot
plot(mush_pca, main="Mushroom Data Scree Plot", sub="Principal Component (x) vs Proportion of Variance (y)")
plot(mush_pca, type='l')

## PCA Vectors
mush_pca.vec = prcomp(mush_data.int)

## >> Component Vectors << - Used for Clustering
mush_model <- data.frame(mush_pca.vec$x[,1:3])

plot(mush_model)
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3)


###  [ Statistical Analysis ]  ###

summary(mush_data)
describe(mush_data)

summary(mush_pca)
loadings(mush_pca)


###  [ Split into Training and Test Sets ]  ###

## 50-50 Train/Test

# Split 50-50 Function
train.50 <- sample(nrow(mush_model), 0.5*nrow(mush_model))

# mush_train : training data structure
# mush_test  : testing data structure
mush_50 <- list()
mush_50$train <- mush_model[train.50,]
mush_50$test <- mush_model[-train.50,]
mush_50Class <- list()
mush_50Class$train <- mush_data.intClass[train.50,]
mush_50Class$test <- mush_data.intClass[-train.50,]

## 60-40 Train/Test
train.60 <- sample(nrow(mush_model), 0.6*nrow(mush_model))

mush_60 <- list()
mush_60$train <- mush_model[train.60,]
mush_60$test <- mush_model[-train.60,]
mush_60Class <- list()
mush_60Class$train <- mush_data.intClass[train.60,]
mush_60Class$test <- mush_data.intClass[-train.60,]


## 70-30 Train/Test
train.70 <- sample(nrow(mush_model), 0.7*nrow(mush_model))

mush_70 <- list()
mush_70$train <- mush_model[train.70,]
mush_70$test <- mush_model[-train.70,]
mush_70Class <- list()
mush_70Class$train <- mush_data.intClass[train.70,]
mush_70Class$test <- mush_data.intClass[-train.70,]


## Dimensions of Partitioned Data Sets
dim(mush_model)
dim(mush_50$train)
dim(mush_50$test)

dim(mush_60$train)
dim(mush_60$test)

dim(mush_70$train)
dim(mush_70$test)


####  2) Clustering  ####

###  [ K-Means ]  ###

## 50-50 Train/Test

# k = 2
km2 <- kmeans(mush_model, 2)
km2.50 <- kmeans(mush_50$train, 2)
km2.60 <- kmeans(mush_60$train, 2)
km2.70 <- kmeans(mush_70$train, 2)

# k = 3
km3 <- kmeans(mush_model, 3)
km3.50 <- kmeans(mush_50$train, 3)
km3.60 <- kmeans(mush_60$train, 3)
km3.70 <- kmeans(mush_70$train, 3)

# k = 5
km5 <- kmeans(mush_model, 5)
km5.50 <- kmeans(mush_50$train, 5)
km5.60 <- kmeans(mush_60$train, 5)
km5.70 <- kmeans(mush_70$train, 5)

# k = 7
km7 <- kmeans(mush_model, 7)
km7.50 <- kmeans(mush_50$train, 7)
km7.60 <- kmeans(mush_60$train, 7)
km7.70 <- kmeans(mush_70$train, 7)

km3.50$size
km5.50$size
km7.50$size
km3.60$size
km5.60$size
km7.60$size
km3.70$size
km5.70$size
km7.70$size


###  [ hclust Clustering ]  ###

mush_model_dist <- dist(mush_model, method = "euclidean")

hclust.3 <- cutree(hclust(mush_model_dist), k=3)
hclust.5 <- cutree(hclust(mush_model_dist), k=5)
hclust.7 <- cutree(hclust(mush_model_dist), k=7)

mush_50.df <- as.data.frame(mush_50$train)
mush_50_dist <- dist(mush_50.df, method = "euclidean")
mush_50.hc.3 <- cutree(hclust(mush_50_dist), k=3)
mush_50.hc.5 <- cutree(hclust(mush_50_dist), k=5)
mush_50.hc.7 <- cutree(hclust(mush_50_dist), k=7)

mush_60.df <- as.data.frame(mush_60$train)
mush_60_dist <- dist(mush_60.df, method = "euclidean")
mush_60.hc.3 <- cutree(hclust(mush_60_dist), k=3)
mush_60.hc.5 <- cutree(hclust(mush_60_dist), k=5)
mush_60.hc.7 <- cutree(hclust(mush_60_dist), k=7)

mush_70.df <- as.data.frame(mush_70$train)
mush_70_dist <- dist(mush_70.df, method = "euclidean")
mush_70.hc.3 <- cutree(hclust(mush_70_dist), k=3)
mush_70.hc.5 <- cutree(hclust(mush_70_dist), k=5)
mush_70.hc.7 <- cutree(hclust(mush_70_dist), k=7)



###  [ PAM Clustering ]  ###

pam.3 <- pam(mush_model, 3)
mush_50.pam.3 <- pam(mush_50.df, k=3)
mush_60.pam.3 <- pam(mush_60.df, k=3)
mush_70.pam.3 <- pam(mush_70.df, k=3)

pam.5 <- pam(mush_model, 5)
mush_50.pam.5 <- pam(mush_50.df, k=5)
mush_60.pam.5 <- pam(mush_60.df, k=5)
mush_70.pam.5 <- pam(mush_70.df, k=5)

pam.7 <- pam(mush_model, 7)
mush_50.pam.7 <- pam(mush_50.df, k=7)
mush_60.pam.7 <- pam(mush_60.df, k=7)
mush_70.pam.7 <- pam(mush_70.df, k=7)


####  3) Clustering Results  ####

palette(alpha(brewer.pal(9,'Set1'), 0.5))


### [ Plotting K-Means ] ###

## 2D Plotting
plot(mush_model, col=km2$clust, pch=16)
plot(mush_model, col=km3$clust, pch=16)
plot(mush_model, col=km5$clust, pch=16)
plot(mush_model, col=km7$clust, pch=16)

## 3D Plotting
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col=km2$clust)
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col=km3$clust)
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col=km5$clust)
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col=km7$clust)


###  [ hclust Plotting ]  ###

## 2D
plot(mush_model, col = hclust.3)
plot(mush_model, col = hclust.5)
plot(mush_model, col = hclust.7)

## 3D
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col = hclust.3)
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col = hclust.5)
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col = hclust.7)

## Hierarchy Plots
plot(hclust(mush_50_dist), h=1)
plot(hclust(mush_60_dist), h=1)
plot(hclust(mush_70_dist), h=1)



###  [ PAM Plotting ]  ###

## 2D Plot
plot(mush_model, col = pam.3$clustering)
plot(mush_model, col = pam.5$clustering)
plot(mush_model, col = pam.7$clustering)


## 3D Plot
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col = pam.3$clustering)
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col = pam.5$clustering)
plot3d(mush_model$PC1, mush_model$PC2, mush_model$PC3, col = pam.7$clustering)


####  4) LM + GLM Methods  ####
lm.fit <- lm(mush_data.int$cshape ~ ., data=mush_data.int)
summary(lm.fit)
step(lm.fit)
plot(lm.fit)

lm.fit <- lm(mush_data.int$gattach ~ ., data=mush_data.int)
summary(lm.fit)
step(lm.fit)

## GLM

glm(formula = mush_data.int$cshape ~ ccolor + bruises + odor + gattach + gsize + gcolor + sshape + sroot + scabove + scbelow + rtype + spcolor + popnum, family = "binomial", data = mush_data.int)


####  5) Support Vector Machines  ####

###  [ 50-50 Set ]  ###
View(mush_50Class$train)

x <- subset(mush_50Class$train, select=-class)
ytest <- subset(mush_50Class$test, select=class)


## SVM 50-50 Model
svm50 <- ksvm(class~., data=mush_50Class$train)
svm50

## SVM Predictions on Test set and Normalizing
svm.50pred <- predict(svm50, mush_50Class$test)
svm.50predN <- svm.50pred
svm.50predN <- apply(svm.50predN, 2, function(x) ifelse(x>12, 15, x))
svm.50predN <- apply(svm.50predN, 2, function(x) ifelse(x<12, 6, x))
svm.50pred
svm.50predN

## Test Predictions vs Gold Prediction List
compare50pred <- data.frame(svm.50predN, ytest)
compare50pred
View(compare50pred)

## Total Predictions
total_predictions50 <- nrow(compare50pred)
total_predictions50

## Find Accuracy of Predictions
correct_count50 <- 0
for (i in 1:total_predictions50) {
  if (compare50pred$svm.50predN[i] == compare50pred$class[i]) {
    correct_count50 <- correct_count50 + 1
  }
}
correct_count50

## Accuracy 50-50
accuracy50 <- (correct_count50 / total_predictions50) * 100
accuracy50

## Plot SVM Data
plot(svm.50pred, data=x)


###  [ 60-40 ]  ###

View(mush_60Class$train)

x <- subset(mush_60Class$train, select=-class)
ytest <- subset(mush_60Class$test, select=class)


## SVM 60-40 Model
svm60 <- ksvm(class~., data=mush_60Class$train)
svm60

## SVM Predictions on Test set and Normalizing
svm.60pred <- predict(svm60, mush_60Class$test)
svm.60predN <- svm.60pred
svm.60predN <- apply(svm.60predN, 2, function(x) ifelse(x>12, 15, x))
svm.60predN <- apply(svm.60predN, 2, function(x) ifelse(x<12, 6, x))
svm.60pred
svm.60predN

## Test Predictions vs Gold Prediction List
compare60pred <- data.frame(svm.60predN, ytest)
compare60pred
View(compare60pred)

## Total Predictions
total_predictions60 <- nrow(compare60pred)
total_predictions60

## Find Accuracy of Predictions
correct_count60 <- 0
for (i in 1:total_predictions60) {
  if (compare60pred$svm.60predN[i] == compare60pred$class[i]) {
    correct_count60 <- correct_count60 + 1
  }
}
correct_count60

## Accuracy 60-40
accuracy60 <- (correct_count60 / total_predictions60) * 100
accuracy60

## Plot SVM Data
plot(svm.60pred, data=x)


###  [ 70-30 ]  ###

x <- subset(mush_70Class$train, select=-class)
ytest <- subset(mush_70Class$test, select=class)

## SVM 70-30 Model
svm70 <- ksvm(class~., data=mush_70Class$train)
svm70

## SVM Predictions on Test set and Normalizing
svm.70pred <- predict(svm70, mush_70Class$test)
svm.70predN <- svm.70pred
svm.70predN <- apply(svm.70predN, 2, function(x) ifelse(x>12, 15, x))
svm.70predN <- apply(svm.70predN, 2, function(x) ifelse(x<12, 6, x))
svm.70pred
svm.70predN


## Test Predictions vs Gold Prediction List
compare70pred <- data.frame(svm.70predN, ytest)
compare70pred
View(compare70pred)

## Total Predictions
total_predictions70 <- nrow(compare70pred)
total_predictions70

## Find Accuracy of Predictions
correct_count70 <- 0
for (i in 1:total_predictions70) {
  if (compare70pred$svm.70predN[i] == compare70pred$class[i]) {
    correct_count70 <- correct_count70 + 1
  }
}
correct_count70

## Accuracy 70-30
accuracy70 <- (correct_count70 / total_predictions70) * 100
accuracy70

## Plot SVM Data
plot(svm.70pred, data=x)

svm50
svm60
svm70
accuracy50
accuracy60
accuracy70
