library(MASS)
library(plyr)
library(e1071)
library(class)
library(tree)

blue <- read.csv("Users/AR/Desktop/terra/data.csv", header = FALSE)
attach(blue)

## Classifying the training and testing data

blue$V1 <- as.factor(blue$V1)
train   <- sample(dim(blue)[1],  0.7*dim(blue)[1], replace = FALSE, prob = NULL)
train_data <- blue[train,]
test_data    <- blue[-train, ]
test_class <- V1[-train]
train_class <- V1[train]

## Linear Discriminant Analysis
lda.fit <- lda(V1 ~ ., data = train_data)

lda_class <- predict(lda.fit, test_data[,-1])$class

print('Accuracy using Linear Discriminant Analysis is') 

print(mean(lda.class==test_class)) ## 72.33% correctly classified

## Support Vector Machine
#svm.fit <- svm(V1~., data=train_data, gamma=1, cost =1)

## Tuning the model using cross validation to find the best hyper parameters. 
## In mutiple class case cross validation to find cost & gamma becomes computationally intensive process because of one-against-one classification strategy of svm and that too for 23 classes.
# (Optional) svm.tune = tune(svm, V1~., data=train_data, ranges=list(cost=c(0.1,1,10),gamma=c(0.5,1,2,3,4) ))

svm_class <- predict( svm.fit, test_data[,-1] )

print('Accuracy using Support Vector Machine is') 

print(mean(svm_class==test_class)) ## 94.54 % 

## K- Nearest Neighbor
knn_class = knn(train_data, test_data, train_class ,k=1)

print('Accuracy using KNN is') 

print(mean(knn_class==test_class))  ## 99.18% with k =1 & 99.02%, 98.9% and 98.63% for k= 2,3,5 respectively

## Decision Tree 
tree.fit = tree(V1~., data=train_data)

tree_class = predict(tree.fit,test_data,type="class" )

print('Accuracy using Decision Trees is') 

print(mean(tree_class==test_class))  ## 82.4 % 













