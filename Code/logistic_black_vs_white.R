install.packages("caret")
library(caret)

# set working directory
setwd("~/Documents/Columbia/NYC Foster Care Data Challenge/")

# create afcars.foster data frame
afcars.foster <-read.csv("afcars_foster_clean.csv")
#vars.to.replace <- c("UNTODETM")
#temp <- afcars.foster[vars.to.replace]
#temp[is.na(temp)] <- FALSE
#df[vars.to.replace] <- temp
#afcars.foster[is.na("UNTODETM")] <- FALSE

# create subset data frames for black and white childen
black <- subset(afcars.foster,afcars.foster[[13]]==1)
white <- subset(afcars.foster,afcars.foster[[15]]==1)

# histograms of discharge reason for black and white children
hist(black$DISREASN)
hist(white$DISREASN)

########################################
# creation of logistic regression models
########################################

# BLACK

# randomly divide data into training, validation, and test sets
set.seed(1)
bound1 <- floor((nrow(black)/4)*3)
#bound2 <- floor((nrow(black)/4)*3)
randomized <- black[sample(nrow(black)), ]
train <- randomized[1:bound1, ]
#validation <- randomized[(bound1+1):bound2, ]
test <- randomized[(bound1+1):nrow(black), ]

x_train_family = subset(train, select=c("RF1BLKAA", "RF1WHITE")) # should only be selecting the family features
x_test_family = subset(test, select=c("RF1BLKAA", "RF1WHITE")) # should only be selecting the family features

#Ridge and Lasso regression have a tuneable parameter: lambda (See Session 7-19)
#We wish to choose the best model using CV among lambda=10^-2,10^-1,...,10^10

# outcome_model_list = c(1:length(outcome_column_name_list))

# t = 1 #to iterate through outcome_model_list because for loop below iterates through string names not numbers

#for (i in 1:length(outcome_column_name_list)){
#    outcome_name = outcome_column_name_list[i]

y_train = train$DISREASN >= 1 & train$DISREASN <= 5 & train$DISREASN != 4
for (i in 1:length(y_train)){
  if (is.na(y_train[i])){
    y_train[i] <- FALSE
  }
}
#y_train = subset(train, select=DISREASN)
#ytrain = train[,"DISREASN"]
y_train = as.numeric(unlist(y_train))
y_train = as.factor(y_train)
#y_train = as.vector(y_train)
    
grid=10^(-2:10) #set sequence of lambdas we want to test
    
#Use 5-fold CV to choose the best value of lambda for lasso regression
#For the command below, alpha=0: ridge regression, alpha=1: lasso regression
#Note: to perform normal logistic regression when the response is binary, change "multinomial" to "binomial"
cv.out=cv.glmnet(x_train_family,y_train,alpha=1,lambda=grid,family="multinomial",nfolds=5)
bestlam=cv.out$lambda.min
bestlam

#Train model with best value of lambda on the training set
lasso.mod=glmnet(only_child_data[train,], y[train], alpha=1, lambda=bestlam, family="multinomial")

#Evaluate this model on the test set
pred = predict(lasso.mod, only_child_data[test,],type="class");
table(pred,y[test])

outcome_model_list[t] = lasso.mod
t = t+1

#Error rate
#sum(pred != y[test])/length(pred)
#mean(pred!=y[test])
#  }
#}