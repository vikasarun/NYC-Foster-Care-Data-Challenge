# install packages
install.packages("caret")
library(caret)

# set working directory
setwd("~/Documents/Columbia/NYC Foster Care Data Challenge/")

# create afcars.foster data frame
afcars.foster <- read.csv("afcars_foster_clean.csv", stringsAsFactors=FALSE)

# turn every NA into a zero
afcars.foster[is.na(afcars.foster)] <- 0

###############################################
## create child attribute subset data frames ##
###############################################

# for example... (will replace this with collection of attributes)
black <- subset(afcars.foster,afcars.foster[[13]]==1)

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
  x_train_family = subset(train, select=c(8, 44:46, 50:66, 68:75, 105))
  x_test_family = subset(test, select=c(8, 44:46, 50:66, 68:75, 105))
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
  cv.out=cv.glmnet(x_train_family,y_train,alpha=1,lambda=grid,family="multinomial",nfolds=5)
  bestlam=cv.out$lambda.min
  bestlam
  
  # Train model with best value of lambda on the training set
  lasso.mod=glmnet(x_train_family, y_train, alpha=1, lambda=bestlam, family="multinomial")
  
  # Evaluate this model on the test set
  pred = predict(lasso.mod,x_test_family,type="class");
  table(pred,y_test)
  
  #Error rate
  sum(pred != y_test)/length(pred)
  mean(pred != y_test)
  
  # output some info
  lasso.mod
  coef(lasso.mod)
  
  return(lasso.mod)
}




child_buckets = function(afcars.foster)
{
  #read.csv("./Clean Data/afcars_ny.csv")
  
  column_num_list = c(10:43,47:49,88:94,100:102) # all child variables col nums 
  
  model_list = rep(NA, length(column_num_list) + 20)
  count = 13
  for(j in column_num_list){ # k is a list of all the child variable column names 
    if (j==10){ # SEX
      male<-subset(afcars.foster,afcars.foster[[j]]==1) # males
      model_list[1] = logit(male)
      
      female<-subset(afcars.foster,afcars.foster[[j]]==2) # females
    }
    
    else if (j==17){ # HISORIGIN
      hispanic<-subset(afcars.foster,afcars.foster[[j]]==1) # child is of hispanic origin
    }
    
    else if (j==18){ # CLINDIS 
      disabled<-subset(afcars.foster,afcars.foster[[j]]==1) # has disability

    }
    
    else if (j==24){ # EVERADPT 
      adopted = subset(afcars.foster,afcars.foster[[j]]==1) # has been adopted
  
      never_adopted<-subset(afcars.foster,afcars.foster[[j]]==2) # never been adopted
    }
    
    else if (j==28){ # MANREM 
      voluntary_removal<-subset(afcars.foster,afcars.foster[[j]]==1) # voluntary
  
      ordered_removal<-subset(afcars.foster,afcars.foster[[j]]==2) # court ordered 
     
    }
    
    else if (j==47){ # CTKFAMST 
      married_couple = subset(afcars.foster,afcars.foster[[j]]==1) # married couple
      
      unmarried_couple = subset(afcars.foster,afcars.foster[[j]]==2) # unmarried couple
  
      single_female = subset(afcars.foster,afcars.foster[[j]]==3) # single female
  
      single_male = subset(afcars.foster,afcars.foster[[j]]==4) # single male
  
    }
    
    else if (sum(unique(as.numeric(na.omit(afcars.foster[[j]]))))==1){ # if binary 
      temp = subset(afcars.foster,afcars.foster[[j]]==1)
      assign(colnames(afcars.foster)[j],temp) 
    }
    
    else { 
      #TBD
    } 
  }
  
  return(model_list)
}