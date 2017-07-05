library(MASS)
install.packages('ISLR')
install.packages("Hmisc")
library(ISLR)
library(Hmisc)


names(Boston)
dim(Boston)



View(Boston)

ll<-m-2*sd
ul<-m+2*sd
newd<-as.numeric(ifelse(Boston$crim<ll,ll,ifelse(Boston$crim>ul,ul,Boston$crim)))



library(xlsx)
write.xlsx(mat, corelation, sheetName="Sheet1")



# medv : median house value
# rm : average number of rooms per house
# age : average age of houses
# lstat : % of households with low socioeconomic status

# First lets see the correlation among the various variables present
attach(Boston)
round(cor(Boston), 2)