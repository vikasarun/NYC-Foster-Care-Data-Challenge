# install packages
install.packages("caret")
install.packages("glmnet")
library(caret)
library(glmnet)

# set working directory
setwd("~/Documents/Columbia/NYC Foster Care Data Challenge/")

# create afcars.foster data frame
afcars.foster <- read.csv("afcars_foster_clean.csv", stringsAsFactors = FALSE)

# turn every NA into a zero
afcars.foster[is.na(afcars.foster)] <- 0

#########################################################
## create new dataframe with child and family clusters ##
#########################################################

# child clusters -> still have lot to discuss on thresholds and splitting
child_age_old <- ifelse(afcars.foster$AgeAtLatRem > 12, 1, 0)
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
family_struct_married <- ifelse(afcars.foster$FOSFAMST == 1, 1, 0)
family_struct_unmarried <- ifelse(afcars.foster$FOSFAMST == 2, 1, 0)
family_struct_sing_female <- ifelse(afcars.foster$FOSFAMST == 3, 1, 0)
family_struct_sing_male <- ifelse(afcars.foster$FOSFAMST == 4, 1, 0)
family_metro_million <- ifelse(afcars.foster$RU13 == 1, 1, 0)
family_FIPS_36061 <- ifelse(afcars.foster$FIPSCode == 36061, 1, 0)
family_FIPS_36103 <- ifelse(afcars.foster$FIPSCode == 36103, 1, 0)
family_FIPS_36029 <- ifelse(afcars.foster$FIPSCode == 36029, 1, 0)
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
cluster_df <- data.frame(child_age_old, child_female, child_disabled, child_behavior_prob, child_neglect,
                         child_moving_high, child_orig_fam_married, child_orig_fam_unmarried,
                         child_orig_fam_sing_female, child_orig_fam_sing_male, child_orig_parent_old,
                         child_in_fc_long,
                         family_struct_married, family_struct_unmarried, family_struct_sing_female,
                         family_struct_sing_male, family_metro_million, family_FIPS_36061, family_FIPS_36103,
                         family_FIPS_36029, family_parent_old, family_payment_high, family_same_race,
                         outcome_discharge_reason)

###############################################
## create child attribute subset data frames ##
###############################################

# for example... (will replace this with collection of attributes)
# black <- subset(afcars.foster,afcars.foster[[13]] == 1)

# function that runs logit() on each child cluster
child_buckets = function(afcars.foster)
{
  # set up lists and counter
  column_num_list = c(10:43, 47:49, 88:94, 101) # all child variables col nums 
  model_list = list()
  count = 13
  
  # loop through child attributes
  for (j in column_num_list){ # k is a list of all the child variable column names 
    if (j == 10){ # SEX
      male <- subset(afcars.foster,afcars.foster[[j]] == 1) # males
      model_list[[1]] = logit(male)
      female <- subset(afcars.foster,afcars.foster[[j]] == 2) # females
      model_list[[2]] = logit(female)
    }
    
    else if (j == 17){ # HISORIGIN
      hispanic <- subset(afcars.foster,afcars.foster[[j]] == 1) # child is of hispanic origin
      model_list[[3]] = logit(hispanic)
    }
    
    else if (j == 18){ # CLINDIS 
      disabled <- subset(afcars.foster,afcars.foster[[j]] == 1) # has disability
      model_list[[4]] = logit(disabled)
    }
    
    else if (j==24){ # EVERADPT 
      adopted = subset(afcars.foster,afcars.foster[[j]]==1) # has been adopted
      model_list[[5]] = logit(adopted)
      never_adopted<-subset(afcars.foster,afcars.foster[[j]]==2) # never been adopted
      model_list[[6]] = logit(never_adopted)
    }
    
    else if (j==28){ # MANREM 
      voluntary_removal<-subset(afcars.foster,afcars.foster[[j]]==1) # voluntary
      model_list[[7]] = logit(voluntary_removal)
      ordered_removal<-subset(afcars.foster,afcars.foster[[j]]==2) # court ordered 
      model_list[[8]] = logit(ordered_removal)
    }
    
    else if (j==47){ # CTKFAMST 
      married_couple = subset(afcars.foster,afcars.foster[[j]]==1) # married couple
      model_list[[9]] = logit(married_couple)
      unmarried_couple = subset(afcars.foster,afcars.foster[[j]]==2) # unmarried couple
      model_list[[10]] = logit(unmarried_couple)
      single_female = subset(afcars.foster,afcars.foster[[j]]==3) # single female
      model_list[[11]] = logit(single_female)
      single_male = subset(afcars.foster,afcars.foster[[j]]==4) # single male
      model_list[[12]] = logit(single_male)
    }
    
    else if (sum(unique(as.numeric(na.omit(afcars.foster[[j]]))))==1){ # if binary 
      temp = subset(afcars.foster,afcars.foster[[j]]==1)
      cat(j)
      cat(" ")
      if (nrow(temp) > 140) {
        model_list[[count]] = logit(temp)
        cat(j)
        cat(" ")
        count = count + 1
      }
      #assign(colnames(afcars.foster)[j],temp)
    }
    
    else {
      #TBD
    }
  }
  
  return(model_list)
}

############################################
## creation of logistic regression models ##
############################################

# logistic regression model creation funciton
logit = function(black)
{
  # create train and test sets
  set.seed(1)
  bound1 <- floor((nrow(black)/4)*3)
  randomized <- black[sample(nrow(black)), ]
  train <- randomized[1:bound1, ]
  test <- randomized[(bound1+1):nrow(black), ]
  
  # create train and test x variable sets from foster family attributes
  x_train_family = subset(train, select=c(8, 44:46, 50:66, 68:75, 105:107))
  x_test_family = subset(test, select=c(8, 44:46, 50:66, 68:75, 105:107))
  x_train_family <- as.matrix(x_train_family)
  x_test_family <- as.matrix(x_test_family)
  
  # create train and test y variable sets from outcomes in DISREASN column
  y_train = train$DISREASN >= 1 & train$DISREASN <= 5 & train$DISREASN != 4
  y_train = as.numeric(unlist(y_train))
  y_train = as.factor(y_train)
  y_test = test$DISREASN >= 1 & test$DISREASN <= 5 & test$DISREASN != 4
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
  
  # Evaluate this model on the test set
  pred = predict(lasso.mod,x_test_family,type="class");
  table(pred,y_test)
  
  #Error rate
  sum(pred != y_test)/length(pred)
  mean(pred != y_test)
  
  # output some info
  lasso.mod
  coef(lasso.mod)
  
  return(coef(lasso.mod))
}
