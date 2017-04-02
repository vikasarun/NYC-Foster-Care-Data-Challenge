# install packages
install.packages("caret")
install.packages("glmnet")
install.packages("tree")
install.packages("MASS")
install.packages("rpart")
library(rpart)
library(caret)
library(glmnet)
library(tree)
library(MASS)

# set working directory
setwd("~/Documents/Columbia/NYC Foster Care Data Challenge/")

# create afcars.foster data frame
afcars.foster <- read.csv("afcars_foster_clean.csv", stringsAsFactors = FALSE)

# turn every NA into a zero
afcars.foster[is.na(afcars.foster)] <- 0

# only NYC (FIPS = 36061)
afcars.foster <- afcars.foster[afcars.foster$FIPSCode == 36061,]

#########################################################
## create new dataframe with child and family clusters ##
#########################################################

# function that clusters original data into intuitive child and family clusters
cluster_func = function(afcars.foster)
{
  # child clusters
  child_preteen <- ifelse(afcars.foster$AgeAtLatRem <= 12, 1, 0)
  child_female <- ifelse(afcars.foster$SEX == 2, 1, 0)
  child_disabled <- ifelse(afcars.foster$CLINDIS == 1, 1, 0)
  child_behavior_prob <- ifelse(afcars.foster$CHBEHPRB == 1, 1, 0)
  child_neglect <- ifelse(afcars.foster$NEGLECT == 1, 1, 0)
  child_moving_high <- ifelse(afcars.foster$NUMPLEP > 2, 1, 0)
  child_orig_fam_married <- ifelse(afcars.foster$CTKFAMST == 1, 1, 0)
  child_orig_fam_unmarried <- ifelse(afcars.foster$CTKFAMST == 2, 1, 0)
  child_orig_fam_sing_female <- ifelse(afcars.foster$CTKFAMST == 3, 1, 0)
  child_orig_fam_sing_male <- ifelse(afcars.foster$CTKFAMST == 4, 1, 0)
  child_orig_parent_old <- ifelse(2015 - afcars.foster$CTK1YR > 50, 1, 0)
  child_in_fc_long <- ifelse(afcars.foster$LifeLOS > 1092, 1, 0)
  
  # family clusters
  family_setting_pre_adopt <- ifelse(afcars.foster$CURPLSET == 1, 1, 0)
  family_setting_relative <- ifelse(afcars.foster$CURPLSET == 2, 1, 0)
  family_setting_non_relative <- ifelse(afcars.foster$CURPLSET == 3, 1, 0)
  family_setting_group <- ifelse(afcars.foster$CURPLSET == 4, 1, 0)
  family_setting_inst <- ifelse(afcars.foster$CURPLSET == 5, 1, 0)
  family_setting_indep <- ifelse(afcars.foster$CURPLSET == 6, 1, 0)
  family_setting_runaway <- ifelse(afcars.foster$CURPLSET == 7, 1, 0)
  family_setting_trial <- ifelse(afcars.foster$CURPLSET == 8, 1, 0)
  family_struct_married <- ifelse(afcars.foster$FOSFAMST == 1, 1, 0)
  family_struct_unmarried <- ifelse(afcars.foster$FOSFAMST == 2, 1, 0)
  family_struct_sing_female <- ifelse(afcars.foster$FOSFAMST == 3, 1, 0)
  family_struct_sing_male <- ifelse(afcars.foster$FOSFAMST == 4, 1, 0)
  family_metro_million <- ifelse(afcars.foster$RU13 == 1, 1, 0)
  family_parent_old <- ifelse(2015 - afcars.foster$FCCTK1YR > 50, 1, 0)
  family_payment_high <- ifelse(afcars.foster$FCMntPay > 3817, 1, 0)
  family_same_race <- ifelse(afcars.foster$AMIAKN == 1 & afcars.foster$RF1AMAKN == 1, 1,
                             ifelse(afcars.foster$ASIAN == 1 & afcars.foster$RF1ASIAN == 1, 1,
                                    ifelse(afcars.foster$BLKAFRAM == 1 & afcars.foster$RF1BLKAA == 1, 1,
                                           ifelse(afcars.foster$HAWAIIPI == 1 & afcars.foster$RF1NHOPI == 1, 1,
                                                  ifelse(afcars.foster$WHITE == 1 & afcars.foster$RF1WHITE == 1, 1,
                                                         ifelse(afcars.foster$UNTODETM == 1 & afcars.foster$RF1UTOD == 1, 1,
                                                                ifelse(afcars.foster$HISORGIN == 1 & afcars.foster$HOFCCTK1 == 1, 1, 0)))))))
  
  # discharge reason outcome column
  outcome_discharge_reason <- afcars.foster[, "DISREASN"]
  
  # combine child and family clusters into new dataframe
  cluster_df <- data.frame(child_preteen, 
                           child_female, child_disabled, child_behavior_prob, child_neglect,
                           child_moving_high, child_orig_fam_married, child_orig_fam_unmarried,
                           child_orig_fam_sing_female, child_orig_fam_sing_male, child_orig_parent_old,
                           child_in_fc_long,
                           family_setting_pre_adopt, family_setting_relative, family_setting_non_relative,
                           family_setting_group, family_setting_inst, family_setting_indep, family_setting_runaway,
                           family_setting_trial, 
                           family_struct_married, family_struct_unmarried, family_struct_sing_female,
                           family_struct_sing_male, family_metro_million, family_parent_old, family_payment_high, family_same_race,
                           outcome_discharge_reason)
  
  # return the new clustered data set
  return(cluster_df)
}

cluster_df <- cluster_func(afcars.foster)

#########################################
## creation of the logistic regression ##
#########################################

# logistic regression model creation funciton

# create train and test sets
set.seed(1)
bound1 <- floor((nrow(cluster_df)/4)*3)
randomized <- cluster_df[sample(nrow(cluster_df)), ]
train <- randomized[1:bound1, ]
test <- randomized[(bound1+1):nrow(cluster_df), ]

# create train and test x variable sets from foster family attributes
x_train_family = subset(train, select=c(1:28))
x_test_family = subset(test, select=c(1:28))
x_train_family <- as.matrix(x_train_family)
x_test_family <- as.matrix(x_test_family)

# create train and test y variable sets from outcomes in outcome_discharge_reason column
y_train = train$outcome_discharge_reason >= 1 & train$outcome_discharge_reason <= 5 & train$outcome_discharge_reason != 4
y_train = as.numeric(unlist(y_train))
y_train = as.factor(y_train)
y_test = test$outcome_discharge_reason >= 1 & test$outcome_discharge_reason <= 5 & test$outcome_discharge_reason != 4
y_test = as.numeric(unlist(y_test))
y_test = as.factor(y_test)

# set sequence of lambdas we want to test
grid=10^(-2:10)

# Use 5-fold CV to choose the best value of lambda for lasso regression
# For the command below, alpha=0: ridge regression, alpha=1: lasso regression
# Note: to perform normal logistic regression when the response is binary, change "multinomial" to "binomial"
cv.out=cv.glmnet(x_train_family,as.factor(y_train),alpha=1,lambda=grid,family="multinomial",nfolds=5)
bestlam=cv.out$lambda.min
bestlam

# Train model with best value of lambda on the training set
lasso.mod=glmnet(x_train_family, as.factor(y_train), alpha=1, lambda=bestlam, family="multinomial")

#########################################
## output from the logistic regression ##
#########################################

# outputs from logistic regression model
summary(lasso.mod)
coef(lasso.mod)
lasso.mod

# Evaluate this model on the test set
pred = predict(lasso.mod,x_test_family,type="class")
table(pred,y_test)

# Error rate
model_error = sum(pred != y_test)/length(pred)
#ean(pred != y_test)

###############################
## create probability matrix ##
###############################

# list of things for people to do
# christina: write code that accurately populates matrix with 1s and 0s
# andrew: work on the LP
# robbie: write code that correctly gets group_size
# vikas: help everyone

# create the list of 1s and 0s for populating the probability matrix so that every possible combination is reached
# there should be 160 different combinations of 1s and 0s that will be fed into logistic regression model
# for each row to output a final column of success probability
c_preteen <- rep(0, 160)
c_disabled <- rep(0, 160)
c_moving_high <- rep(0, 160)
f_setting_pre_adopt <- rep(1, 160)
f_setting_relative <- rep(0, 160)
f_setting_non_relative <- rep(0, 160)
f_setting_group <- rep(0, 160)
f_setting_inst <- rep(0, 160)
f_payment_high <- rep(0, 160)
f_same_race <- rep(0, 160)

other_cols <- rep(1, 160)

# combine the lists into a data frame
prob_matrix <- data.frame(c_preteen, c_disabled, c_moving_high, f_setting_pre_adopt,
                          f_setting_relative, f_setting_non_relative, f_setting_group,
                          f_setting_inst, f_payment_high, f_same_race)

# output a final column of success probability
prediction_data <- cluster_df[1:160,]
attach(prediction_data)
prediction_data <- data.frame(c_preteen, child_female, c_disabled, child_behavior_prob, child_neglect, c_moving_high,
                              child_orig_fam_married, child_orig_fam_unmarried, child_orig_fam_sing_female,
                              child_orig_fam_sing_male, child_orig_parent_old, child_in_fc_long, f_setting_pre_adopt,
                              f_setting_relative, f_setting_non_relative, f_setting_group,
                              f_setting_inst, family_setting_indep, family_setting_runaway, family_setting_trial,
                              family_struct_married, family_struct_unmarried,
                              family_struct_sing_female, family_struct_sing_male, family_metro_million, family_parent_old,
                              f_payment_high, f_same_race, outcome_discharge_reason)
# prediction_data <- data.frame(c_preteen, other_cols, c_disabled, other_cols, other_cols, c_moving_high,
#                               other_cols, other_cols, other_cols,
#                               other_cols, other_cols, other_cols, f_setting_pre_adopt,
#                               f_setting_relative, f_setting_non_relative, f_setting_group,
#                               f_setting_inst, other_cols, other_cols, other_cols,
#                               other_cols, other_cols, 
#                               other_cols, other_cols, other_cols, other_cols,
#                               f_payment_high, f_same_race, outcome_discharge_reason)
detach(prediction_data)
prediction_data$outcome_discharge_reason <- NULL
prediction_data <- as.matrix(prediction_data)
predict(object = lasso.mod, s = bestlam, newx = prediction_data, type = "response")
prob_matrix$success_probability <- 1 - predict(object = lasso.mod, s = bestlam, newx = prediction_data, type = "response")
  
# output a column of number of cases that fit that row's criteria (size)
prob_matrix$group_size <- 100

# show first few lines of probability matrix
head(prob_matrix)
