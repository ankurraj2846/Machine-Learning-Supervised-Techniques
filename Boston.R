library(MASS)
#install.packages('ISLR')
#install.packages("Hmisc")
library(ISLR)
library(Hmisc)


names(Boston)
dim(Boston)
View(Boston)

# medv : median house value
# rm : average number of rooms per house
# age : average age of houses
# lstat : % of households with low socioeconomic status

# First lets see the correlation among the various variables present
attach(Boston)
round(cor(Boston), 2)

# Check medv column to find the most co related variables with house value
# Now lets find their p - values using Hmisc package and rcorr function
d <- as.matrix(Boston)
rcorr(d)

# Linear Regression :Lets start regression with highest correlated variable

fit1 <- lm(medv~lstat)
fit1
summary(fit1)
confint(fit1, level =0.9)
predict(fit1, data.frame(lstat = c(1, 2 ,3 )), interval = 'confidence')
predict(fit1, data.frame(lstat = c(1, 2 ,3 )), interval = 'prediction')

plot(lstat, medv)
abline(fit1)

# Generic plots using plot(fit1)
par(mfrow = c(2,2)) # First split the screen then plot all the four graphs simultaneously
plot(fit1) # rstudent is used for studentized residuals means error/residual divided by its standard error
# If their is a pattern in Residual vs fitted values then there is an issue of non linearity
# Normal Q-Q plot signifies its normality if observations overlap with the line

plot(hatvalues(fit1))
which.max(hatvalues(fit1)) # Gives us the observation with highest leverage statistic

fit2 <- lm(medv~.-age-indus, data = Boston)
fit3 <- lm(medv ~ lstat + rm+ dis)
#install.packages('car')
library(car)
vif(fit2) # Variance Inflation Factor function a part of car library

anova(fit1, fit2, fit3) # For comparision of models: majorly used to compare two models

# Now lets perform classification to check whether a city has crime rate above or below the median value
# Lets first define the training and testing dataset

train = 1:(dim(Boston)[1]/2)
test = (dim(Boston)[1]/2+1):dim(Boston)[1]
Boston.train = Boston[train,]
Boston.test = Boston[test,]

# Logistic Regression 

med <- median(Boston$crim)
prob <- rep("Down", length(Boston$crim))
prob[crim > med] = "Up"
Boston <- data.frame(Boston, prob)
prob.train = prob[train] # To check the accuracy of training and testing data separately
prob.test = prob[test]

fit_logistic <-  glm(prob~. -crim-chas, data = Boston, family = binomial, subset = train)
fit_prob <- predict(fit_logistic , data= Boston, type = "response")
fit_pred <- rep("Down", length(fit_prob))
fit_pred[fit_prob > 0.2] <- "UP"

table(fit_pred,prob.test)
mean(fit_pred == prob.test)  
 
# KNN
library(class) # to use knn() function 

train.X = cbind(zn, indus, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[train,]
test.X = cbind(zn, indus, nox, rm, age, dis, rad, tax, ptratio, black, lstat, medv)[test,]
train.direction = prob[train]

knn.pred <- knn(train.X, test.X, train.direction , k= 10)
table(knn.pred, prob.test)

mean(knn.pred == prob.test)



