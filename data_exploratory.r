library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('dplyr') # data manipulation
library('readr') # data input
library('tibble') # data wrangling
library('stringr') # string manipulation
library('tidyverse')
library('forcats') # factor manipulation
library('xgboost')
library('randomForest')
library('party')
set.seed(123)
#set work dirct
setwd("C:/Users/CZhao/Dropbox/DataScience/Kaggle Competitions/Mercedes-Benz Greener Manufacturing")
workpath = getwd()
###
train <- read.csv(paste0(workpath,"/data/train.csv"),head=T) # 4209 observation
test  <- read.csv(paste0(workpath,"/data/test.csv"),head=T)

# full <- bind_rows(train,test)
#
head(train)
glimpse(train)

train %>% select(y,X0:X12) %>% glimpse()
head(train)

# check missing value of the whole datasets
train  %>% map_df(function(col) sum(is.na(col)))  %>% gather()   %>% summarise(count_of_nas = sum(value,na.rm=T))

# check each feathures  value range
value_range<-apply(train, 2, unique)

length_value_range<-apply(train, 2, function(x) length(unique(x)))
#length_value_range<-apply(test, 2, function(x) length(unique(x)))
# extract all features which has at least one value 
uniqueValueVariable = which(length_value_range==1)

# train  %>% ggplot(aes(y)) + geom_histogram(aes(fill = ..count..),bins=40)  + xlim(NA,180) + 
#            ggtitle('Distribution of Response Variable Y')
# 
# ### the difference in average y between 0 and 1 values for the binary variables
# mean_difference <- train  %>% keep(is.numeric)  %>% gather(Variable,Value,-ID,-y)  %>% 
#   group_by(Variable,Value)  %>% summarise(mean_y = mean(y,na.rm=T))  %>% 
#   spread(Value,mean_y)  %>% rename(value_0=`0`,value_1=`1`)  %>% 
#   mutate(difference = value_1-value_0,
#          abs_difference = abs(difference))  %>% arrange(desc(abs_difference))  %>% ungroup()
# 
# mean_difference %>% 
#   ggplot(aes(reorder(Variable,-difference),difference)) + geom_point(size=0.2) + 
#   theme(axis.text.x =  element_blank()) + geom_hline(yintercept = 0,linetype=2,colour='orange')
#####################################################################################################
train = within(train,rm( X11,X93,X107,X233,X235,X268,X289,X290,X293,X297,X330,X347))
test  = within(test,rm( X11,X93,X107,X233,X235,X268,X289,X290,X293,X297,X330,X347))

#############################


test_df <- test 
train_df<-train
train_df_model <-train
y_train <- train_df_model$y
train_df_model$y <- NULL
#Row binding train & test set for feature engineering
train_test <- bind_rows(train_df_model, test_df)
ntrain <- nrow(train_df_model)

features <- names(train_df)



#convert character into integer
for (f in features) {
  if (is.character(train_test[[f]])) {
    levels = sort(unique(train_test[[f]]))
    train_test[[f]] = as.integer(factor(train_test[[f]],levels = levels))
  }
}


#splitting whole data back again
train_x <- train_test %>% .[1:ntrain,]
test_x <- train_test %>% .[(ntrain + 1):nrow(train_test),]

#convert into numeric for XGBoost implementation
train_x[] <- map(train_x, as.numeric)
test_x[] <- map(test_x, as.numeric)

dtrain <- xgb.DMatrix(as.matrix(train_x),label = y_train)
dtest <- xgb.DMatrix(as.matrix(test_x))

##xgboost parameters
xgb_params <- list(colsample_bytree = 0.7, #how many variables to consider for each tree
                   subsample = 0.7, #how much of the data to use for each tree
                   booster = "gbtree",
                   max_depth = 5, #how many levels in the tree
                   eta = 0.1, #shrinkage rate to control overfitting through conservative approach
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   gamma = 0)
#
xgb_params2 <-list(booster = "gbtree",
                   colsample_bytree = 0.7, #how many variables to consider for each tree
                   subsample = 0.8, 
                   max_depth = 4, #how many levels in the tree
                   eta = 0.2, #shrinkage rate to control overfitting through conservative approach
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   gamma = 0)

#cross validation
#xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 2, nrounds=500)
#train data
gb_dt <- xgb.train(xgb_params,dtrain,nrounds = 54)
gb_dt <- xgb.train(xgb_params2,dtrain,nrounds = 60)

gb_dt$params

test_preds <- predict(gb_dt,dtest)
(test_df[,1:2]  %>% mutate(y = test_preds))[,-2]  %>% write_csv('xgb_base_predictions3.csv')
(test_df[,1:2]  %>% mutate(y = test_preds))[,-2] %>% head()

# importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
# head(importance)

######--------------------------------------------------------------------#######
### create dummy variable of categorical varibles 
# names(full)
# head(full[,3:10])
# 
# 
# dyVar      = full %>% select(X0:X8) %>% dummyVars(~.,.)
# dyVarValue = full %>% select(X0:X8) %>% predict(dyVar,newdata=.) %>% data.frame
# dim(dyVarValue)
# 
# full_data= bind_cols(full,dyVarValue)
# ## remove character columns 
# full_data_keep <-full_data[,-which(names(full_data) %in% paste0('X',c(0:6,8)))]
######--------------------------------------------------------------------########

train_x <- full_data_keep %>% .[1:ntrain,]
test_x <- full_data_keep %>% .[(ntrain + 1):nrow(train_test),]


#convert into numeric for XGBoost implementation
train_x[] <- map(train_x, as.numeric)
test_x[] <- map(test_x, as.numeric)

dtrain <- xgb.DMatrix(as.matrix(train_x),label = y_train)
dtest <- xgb.DMatrix(as.matrix(test_x))

##xgboost parameters
xgb_params <- list(colsample_bytree = 0.7, #how many variables to consider for each tree
                   subsample = 0.7, #how much of the data to use for each tree
                   booster = "gbtree",
                   max_depth = 5, #how many levels in the tree
                   eta = 0.1, #shrinkage rate to control overfitting through conservative approach
                   eval_metric = "rmse", 
                   objective = "reg:linear",
                   gamma = 0)

#cross validation
#xgb_cv <- xgb.cv(xgb_params,dtrain,early_stopping_rounds = 10, nfold = 2, nrounds=500)
#train data
gb_dt <- xgb.train(xgb_params,dtrain,nrounds = 54)
gb_dt$params

test_preds <- predict(gb_dt,dtest)
(test_df[,1:2]  %>% mutate(y = test_preds))[,-2]  %>% write_csv('predictions3.csv')
(test_df[,1:2]  %>% mutate(y = test_preds))[,-2] %>% head()


####### random forest 
train_rf = within(train_x,rm(ID))
train_rf$y = y_train

test_rf = within(test_x,rm(ID))



rfmodel<- randomForest(y~.,data=train_rf,
                       importance=TRUE,
                       ntree = 1000,do.trace = T)

rf_preds<- predict(rfmodel,test_rf,type="response")

rf_gbm_preds= apply(cbind(test_preds,rf_preds),1,mean)


(test_df[,1:2]  %>% mutate(y = rf_preds))[,-2]  %>% write_csv('rf_predictions1.csv')

(test_df[,1:2]  %>% mutate(y = rf_gbm_preds))[,-2]  %>% write_csv('rf_predictions2.csv')

##### c random forest

crfmodel<- cforest(y~.,data=train_rf,controls=cforest_unbiased(ntree=3000, mtry=4))

crf_preds<- predict(crfmodel,test_rf, OOB=TRUE,type="response")
crf_preds<- as.vector(crf_preds)
(test_df[,1:2]  %>% mutate(y = crf_preds))[,-2]  %>% write_csv('crf_predictions3.csv')
