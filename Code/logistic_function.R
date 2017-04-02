###########################################################
#### Multinomial Logistic Regression with Lasso Penalty
###########################################################
library(caret)


logit = function(child_feature_specific_csv, outcome_column_name_list)
{
  


#create our training set
set.seed(1)
train = sample(1:nrow(child_feature_specific_csv),0.75*nrow(child_feature_specific_csv))
test = -train

only_child_data = child_feature_specific_csv[,-outcome_column_name_list]

#Ridge and Lasso regression have a tuneable parameter: lambda (See Session 7-19)
#We wish to choose the best model using CV among lambda=10^-2,10^-1,...,10^10

outcome_model_list = c(1:length(outcome_column_name_list))

t = 1 #to iterate through outcome_model_list because for loop below iterates through string names not numbers

for (i in 1:length(outcome_column_name_list)){
outcome_name = outcome_column_name_list[i]
y = child_feature_specific_csv[,outcome_name]
y = as.factor(y)

grid=10^(-2:10) #set sequence of lambdas we want to test


#Use 5-fold CV to choose the best value of lambda for lasso regression
#For the command below, alpha=0: ridge regression, alpha=1: lasso regression
#Note: to perform normal logistic regression when the response is binary, change "multinomial" to "binomial"
cv.out=cv.glmnet(only_child_data[train,],y[train],alpha=1,lambda=grid,family="multinomial",nfolds=5)
bestlam=cv.out$lambda.min

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
}
}