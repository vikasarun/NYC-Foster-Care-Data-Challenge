# classification tree model creation function
install.packages("tree")
install.packages("MASS")
library(tree)
library(MASS)

tree = function(child_cluster_df)
{
  # create train and test sets
  set.seed(1)
  bound1 <- floor((nrow(child_cluster_df)/4)*3)
  randomized <- child_cluster_df[sample(nrow(child_cluster_df)), ]
  train <- randomized[1:bound1, ]
  test <- randomized[(bound1+1):nrow(child_cluster_df), ]
  
  # create train and test x variable sets from foster family attributes
  x_train_family = subset(train, select=c(13:23))
  x_test_family = subset(test, select=c(13:23))
  x_train_family <- as.matrix(x_train_family)
  x_test_family <- as.matrix(x_test_family)
  
  # create train and test y variable sets from outcomes in outcome_discharge_reason column
  y_train = train$outcome_discharge_reason >= 1 & train$outcome_discharge_reason <= 5 & train$outcome_discharge_reason != 4
  y_train = as.numeric(unlist(y_train))
  y_train = as.factor(y_train)
  y_test = test$outcome_discharge_reason >= 1 & test$outcome_discharge_reason <= 5 & test$outcome_discharge_reason != 4
  y_test = as.numeric(unlist(y_test))
  y_test = as.factor(y_test)
  
  
  
  temp_train = cbind(y_train,x_train_family)
  foster_tree = tree(outcome_discharge_reason~.,temp_train)
  cv.foster_tree = cv.tree(foster_tree, FUN=prune.misclass)
  optSize = cv.foster_tree$size[which.min(cv.foster_tree$dev)]
  prune.foster_tree = prune.misclass(foster_tree,best=optSize)
  #plot(prune.foster_tree)
  #text(prune.foster_tree,pretty=0)
  
  temp_test = cbind(test,x_test_family)
  tree.pred = predict(prune.foster_tree, temp_test, type = "class")
  model_error = mean(tree.pred!=temp_test$outcome_discharge_reason)
  
  # output some info
  return_list = list(prune.foster_tree, model_error)
  names(return_list) = c("tree", "model_error")
  
  # return return_list which contains the coefficients of the lasso model and the associated error rate
  return(return_list)
}
