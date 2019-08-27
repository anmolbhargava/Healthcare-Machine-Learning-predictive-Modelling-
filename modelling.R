

## Reading the data and changing the data types

df = read.csv('training_data.csv')
test_courtney = read.csv('testing_data.csv')

sapply(df, class)
sapply(test_courtney,class)

library(magrittr)
library(dplyr)
library(plyr)
cols <- c("MONTH_ARR", "WEEKDAY_DEP", "HOUR_DEP", "MONTH_DEP", "SAME_DAY", 
          "CONSULT_ORDER", "CONSULT_CHARGE", "CONSULT_IN_ED","RETURN")
df %<>%
  mutate_each_(funs(factor(.)),cols)
str(df)
df$AGE= as.numeric(df$AGE)
df$DIAG_DETAILS=as.numeric(df$DIAG_DETAILS)


cols <- c("MONTH_ARR", "WEEKDAY_DEP", "HOUR_DEP", "MONTH_DEP", "SAME_DAY", 
          "CONSULT_ORDER", "CONSULT_CHARGE", "CONSULT_IN_ED")
test_courtney %<>%
  mutate_each_(funs(factor(.)),cols)
str(test_courtney)
test_courtney$AGE= as.numeric(test_courtney$AGE)
test_courtney$DIAG_DETAILS=as.numeric(test_courtney$DIAG_DETAILS)




## Data Partioning

set.seed(13)
num_obs <- nrow(df)
test_obs <- sample(num_obs, 0.25*num_obs)
df_test <- df[test_obs, ]
df_rest <- df[-test_obs, ]

num_obs <- nrow(df_rest)
valid_obs <- sample(num_obs, 0.25*num_obs)
df_valid <- df_rest[valid_obs, ]
df_train <- df_rest[-valid_obs, ]





## Modelling


## logistic models

## log model1 (all variables)
log_fit1 <- glm(RETURN~.,data=df_train,family="binomial")
summary(log_fit1)
log_preds1 <- predict(log_fit1,newdata=df_valid,type="response")

log_class1 <- ifelse(log_preds1>0.5,1,0)
table(df_valid$RETURN,log_class1,dnn=c("Actual","Predicted"))
sum(ifelse(df_valid$RETURN == log_class1,1,0))/nrow(df_valid)


## log model2 (LASSO -> all variables -DiagDetails-Diagnosis)
log_fit2 <- glm(RETURN~.-DIAG_DETAILS-DIAGNOSIS,
                data=df_train,family="binomial")
summary(log_fit2)
log_preds2 <- predict(log_fit2,newdata=df_valid,type="response")

log_class2 <- ifelse(log_preds2>0.5,1,0)
table(df_valid$RETURN,log_class2,dnn=c("Actual","Predicted"))
sum(ifelse(df_valid$RETURN == log_class2,1,0))/nrow(df_valid)


## log model3 (Interactions - Charges & Financial Class)
log_fit3 <- glm(RETURN~.-DIAG_DETAILS-DIAGNOSIS+CHARGES*FINANCIAL_CLASS,
                data=df_train,family="binomial")
summary(log_fit3)
log_preds3 <- predict(log_fit3,newdata=df_valid,type="response")

log_class3 <- ifelse(log_preds2>0.5,1,0)
table(df_valid$RETURN,log_class3,dnn=c("Actual","Predicted"))
sum(ifelse(df_valid$RETURN == log_class3,1,0))/nrow(df_valid)


## step wise log models

## backward step wise model
backward_log_model = step(log_fit1, direction="backward",trace=1)
summary(backward_log_model)

log_preds_back <- predict(backward_log_model,newdata=df_valid,type="response")

log_class_back <- ifelse(log_preds_back>0.5,1,0)
table(df_valid$RETURN,log_class_back,dnn=c("Actual","Predicted"))
sum(ifelse(df_valid$RETURN == log_class_back,1,0))/nrow(df_valid)


## forward step wise model
log_null <- glm(RETURN~1, data=df_train, family="binomial")

forward_log_model = step(log_null,scope=list(upper=log_fit1), direction="forward",trace=1)
summary(forward_log_model)

log_preds_frwd <- predict(forward_log_model,newdata=df_valid,type="response")

log_class_frwd <- ifelse(log_preds_frwd>0.5,1,0)
table(df_valid$RETURN,log_class_frwd,dnn=c("Actual","Predicted"))
sum(ifelse(df_valid$RETURN == log_class_frwd,1,0))/nrow(df_valid)


## finding best cut-off for log models
log_accuracy=rep(0,11)
#cutoffs=c(0.1,0.2,0.3,0.4,0.45,0.5,0.55,0.6,0.7,0.8,0.9,0.95,0.99)
cutoffs=c(0.41,0.42,0.43,0.44,0.45,0.46,0.47,0.48,0.49,0.495,0.5)

log_preds=predict(log_fit1,newdata=df_valid,type="response")

for(i in 1:11){
  
  log_class=ifelse(log_preds>cutoffs[i],1,0)
  confuse_test=table(df_valid$RETURN,log_class)
  log_accuracy[i]=(confuse_test[1,1]+confuse_test[2,2])/sum(confuse_test)
}

cbind(cutoffs, log_accuracy)


## our test data predictions
log_preds_test <- predict(log_fit1,newdata=df_test,type="response")
log_class_test <- ifelse(log_preds_test>0.50,1,0)
table(df_test$RETURN,log_class_test,dnn=c("Actual","Predicted"))
sum(ifelse(df_test$RETURN == log_class_test,1,0))/nrow(df_test)


## Prof Courtney test data predictions
log_preds_courtney <- predict(log_fit1,newdata=test_courtney,type="response")
log_class_courtney <- ifelse(log_preds_courtney>0.50,1,0)

write.csv(log_class_courtney, file='log_new.csv')





## LDA model

df_train$RETURN <- as.numeric(df_train$RETURN)
df_valid$RETURN <- as.numeric(df_valid$RETURN)
df_test$RETURN <- as.numeric(df_test$RETURN)



library(MASS)
lda_model_train <- lda(RETURN~., data=df_train)


## best cutoff in lda
cutoff_lda = 0.47

lda_predict_valid=predict(lda_model_train,newdata=df_valid)
lda_preds_valid <- lda_predict_valid$posterior[,2]
lda_class_valid <- ifelse(lda_preds_valid > cutoff_lda,1,0)
confuse_test_lda_valid <- table(df_valid$RETURN,lda_class_valid,dnn=c('Actual','Predicted'))
accuracy_lda_valid <- (confuse_test_lda_valid[1,1]+confuse_test_lda_valid[2,2])/sum(confuse_test_lda_valid)
accuracy_lda_valid


## accuracy on our testing data in lda
lda_predict_test=predict(lda_model_rest,newdata=df_test)
lda_preds_test <- lda_predict_test$posterior[,2]
lda_class_test <- ifelse(lda_preds_test > cutoff_lda,1,0)
confuse_test_lda_test <- table(df_test$RETURN,lda_class_test,dnn=c('Actual','Predicted'))
accuracy_lda_test <- (confuse_test_lda_test[1,1]+confuse_test_lda_test[2,2])/sum(confuse_test_lda_test)
accuracy_lda_test


df_train$RETURN <- as.factor(df_train$RETURN)
df_valid$RETURN <- as.factor(df_valid$RETURN)
df_test$RETURN <- as.factor(df_test$RETURN)






## KNN

colnames(df)
library(class)
library(glmnet)
Hospital_train_X <- model.matrix( ~ .-1, df_train[,-c(24)])
Hospital_valid_X <- model.matrix( ~ .-1, df_valid[,-c(24)])
Hospital_test_X <- model.matrix( ~ .-1, df_test[,-c(24)])
Hospital_rest_X <- model.matrix( ~ .-1, df_rest[,-c(24)])

train.return=df_train$RETURN
valid.return=df_valid$RETURN
test.return=df_test$RETURN
rest.return=df_rest$RETURN

## Use 1 nearest neighbor to predict validation values
## Use training X, validation X, and training Y with k value
knn.pred=knn(Hospital_train_X,Hospital_valid_X,train.return,k=31)


## How do our predictions do?
table(valid.return,knn.pred)

acc_knn_1_valid=sum(ifelse(knn.pred==df_valid$RETURN,1,0))/nrow(df_valid)
acc_knn_1_valid



for(i in c(1,3,5,10,25)) {
  knn.pred=knn(Hospital_train_X,Hospital_valid_X,train.return,k=i)
  cm = table(valid.return,knn.pred,dnn=c("Actual","Predicted"))
  accuracy = (cm[1,1] + cm[2,2])/sum(cm)
  print(c(i,accuracy))
}

for(i in c(27,29,31,33,35)) {
  knn.pred=knn(Hospital_train_X,Hospital_valid_X,train.return,k=i)
  cm = table(valid.return,knn.pred,dnn=c("Actual","Predicted"))
  accuracy = (cm[1,1] + cm[2,2])/sum(cm)
  print(c(i,accuracy))
}

for(i in c(41,51,61,71,81)) {
  knn.pred=knn(Hospital_train_X,Hospital_valid_X,train.return,k=i)
  cm = table(valid.return,knn.pred,dnn=c("Actual","Predicted"))
  accuracy = (cm[1,1] + cm[2,2])/sum(cm)
  print(c(i,accuracy))
}


knn.pred_rest_71=knn(Hospital_rest_X,Hospital_test_X,rest.return,k=71)
acc_knn_71_test=sum(ifelse(knn.pred_rest_71==df_test$RETURN,1,0))/nrow(df_test)
acc_knn_71_test

for(i in c(73,75,77,79)) {
  knn.pred=knn(Hospital_train_X,Hospital_valid_X,train.return,k=i)
  cm = table(valid.return,knn.pred,dnn=c("Actual","Predicted"))
  accuracy = (cm[1,1] + cm[2,2])/sum(cm)
  print(c(i,accuracy))
}
for(i in c(151,211,301)) {
  knn.pred=knn(Hospital_train_X,Hospital_valid_X,train.return,k=i)
  cm = table(valid.return,knn.pred,dnn=c("Actual","Predicted"))
  accuracy = (cm[1,1] + cm[2,2])/sum(cm)
  print(c(i,accuracy))
}

for(i in c(401,1001,1501)) {
  knn.pred=knn(Hospital_train_X,Hospital_valid_X,train.return,k=i)
  cm = table(valid.return,knn.pred,dnn=c("Actual","Predicted"))
  accuracy = (cm[1,1] + cm[2,2])/sum(cm)
  print(c(i,accuracy))
}





## Trees


library(tree)


Hospitals.tree=tree(RETURN~.,df_rest)
summary(Hospitals.tree)
plot(Hospitals.tree)
text(Hospitals.tree)
for (j in c(0.40,0.41,.42,.43,.44,.45,.46,.47,.48,.49,.50,.51,.52,.53,.54,.55,.56,.57,.58,.59,.60)){
  tree_preds_4_valid <- predict(Hospitals.tree.pruned_2,newdata=df_test)
  tree_preds_4_valid[1:10,]
  tree_probs_4_valid=tree_preds_4_valid[,2]
  tree_class_4_valid=ifelse(tree_probs_4_valid> j,1,0)
  table(df_test$RETURN,tree_class_4_valid,dnn=c("Actual","Predicted"))
  acc_tree_4_valid=sum(ifelse(tree_class_4_valid==df_test$RETURN,1,0))/nrow(df_test)
  print(c(j,acc_tree_4_valid) )}




Hospitals.tree.pruned_3=prune.tree(Hospitals.tree,best=3)
summary(Hospitals.tree.pruned_3)
plot(Hospitals.tree.pruned_3)
text(Hospitals.tree.pruned_3,pretty=1)

Hospitals.tree.pruned_2=prune.tree(Hospitals.tree,best=2)
summary(Hospitals.tree.pruned_2)
plot(Hospitals.tree.pruned_2)
text(Hospitals.tree.pruned_2,pretty=1)

## Let's use the tree to do probability predictions for the validation data
tree_preds_4_valid <- predict(Hospitals.tree,newdata=df_valid)

tree_preds_4_valid[1:10,]

tree_probs_4_valid=tree_preds_4_valid[,2]

tree_class_4_valid=ifelse(tree_probs_4_valid>0.,1,0)
table(df_valid$RETURN,tree_class_4_valid,dnn=c("Actual","Predicted"))
acc_tree_4_valid=sum(ifelse(tree_class_4_valid==df_valid$RETURN,1,0))/nrow(df_valid)
acc_tree_4_valid


tree_preds_3_valid <- predict(Hospitals.tree.pruned_3,newdata=df_valid)

tree_preds_3_valid[1:10,]

tree_probs_3_valid=tree_preds_3_valid[,2]

tree_class_3_valid=ifelse(tree_probs_3_valid>0.5,1,0)
table(df_valid$RETURN,tree_class_3_valid,dnn=c("Actual","Predicted"))
acc_tree_3_valid=sum(ifelse(tree_class_3_valid==df_valid$RETURN,1,0))/nrow(df_valid)
acc_tree_3_valid


tree_preds_2_valid <- predict(Hospitals.tree.pruned_2,newdata=df_valid)

tree_preds_2_valid[1:10,]

tree_probs_2_valid=tree_preds_2_valid[,2]

tree_class_2_valid=ifelse(tree_probs_2_valid>0.5,1,0)
table(df_valid$RETURN,tree_class_2_valid,dnn=c("Actual","Predicted"))
acc_tree_2_valid=sum(ifelse(tree_class_2_valid==df_valid$RETURN,1,0))/nrow(df_valid)
acc_tree_2_valid






## Random Forest

set.seed(13)
rf.trees=randomForest(RETURN~.,data=df_rest,ntree=500,importance=TRUE,parallel=TRUE)

rf.trees
importance(rf.trees)
varImpPlot(rf.trees)

rf_preds=predict(rf.trees,newdata=df_test,type="prob")
rf_probs=rf_preds[,2]
rf_class=ifelse(rf_probs>0.5,1,0)

table(df_test$RETURN,rf_class)
sum(ifelse(rf_class==df_test$RETURN,1,0))/nrow(df_test)
##
set.seed(13)
rf.trees_1000=randomForest(RETURN~.,data=df_rest,ntree=2000,importance=TRUE)


rf.trees_1000
importance(rf.trees_1000)
varImpPlot(rf.trees_1000)

rf_preds_1000=predict(rf.trees_1000,newdata=df_test,type="prob")
rf_probs_1000=rf_preds_1000[,2]
rf_class_1000=ifelse(rf_probs_1000>0.50,1,0)

table(df_test$RETURN,rf_class_1000)
sum(ifelse(rf_class_1000==df_test$RETURN,1,0))/nrow(df_test)

##writing 77.50916 accuracy of valid..checking on test

levels(test_courtney$ED_RESULT) <- levels(df_rest$ED_RESULT)
rf_preds_courtney=predict(rf.trees_1000,newdata=test_courtney,type="prob")
rf_probs_courtney=rf_preds_courtney[,2]
rf_class_courtney=ifelse(rf_probs_courtney>0.5,1,0)
write.csv(rf_class_courtney, file='RandomForest_1000.csv')

table(df_test$RETURN,rf_class_1000)
sum(ifelse(rf_class_1000==df_test$RETURN,1,0))/nrow(df_test)

## 1000 trees 10 variables
rf.trees_1000_10=randomForest(RETURN~.,data=df_rest,mtry=10,ntree=1000,importance=TRUE)

rf.trees_1000_10
importance(rf.trees_1000_10)
varImpPlot(rf.trees_1000_10)

rf_preds_1000_10=predict(rf.trees_1000_10,newdata=df_test,type="prob")
rf_probs_1000_10=rf_preds_1000_10[,2]
rf_class_1000_10=ifelse(rf_probs_1000_10>0.5,1,0)

table(df_test$RETURN,rf_class_1000_10)
sum(ifelse(rf_class_1000_10==df_test$RETURN,1,0))/nrow(df_test)

levels(df$MONTH_ARR)
levels(test_courtney$ADMIT_RESULT)

rf_preds_courtney=predict(rf.trees,newdata=df_train,type="prob")
rf_probs_courtney=rf_preds_courtney[,2]
rf_class_courtney=ifelse(rf_probs_courtney>0.5,1,0)

sapply(test_courtney,class)
ncol(test_courtney)
ncol(df_rest)
sapply(df_rest,class)



# loops for ensemble
set.seed(13)
for(i in c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)) {
  set.seed(13)
  rf.trees=randomForest(RETURN~.,data=df_rest,ntree=1000,mtry=i ,importance=TRUE,parallel=TRUE)
  rf_preds=predict(rf.trees,newdata=df_test,type="prob")
  rf_probs=rf_preds[,2]
  rf_class=ifelse(rf_probs>0.5,1,0)
  accuracy=sum(ifelse(rf_class==df_test$RETURN,1,0))/nrow(df_test)
  print(c(i,accuracy))
}
set.seed(13)
for (i in c(2500,3000,3500,4000)){
  set.seed(13)
  rf.trees=randomForest(RETURN~.,data=df_rest,ntree=i,mtry=4 ,importance=TRUE,parallel=TRUE)
  rf_preds=predict(rf.trees,newdata=df_test,type="prob")
  rf_probs=rf_preds[,2]
  for (j in c(0.40,0.41,.42,.43,.44,.45,.46,.47,.48,.49,.50,.51,.52,.53,.54,.55,.56,.57,.58,.59,.60)){
    rf_class=ifelse(rf_probs> j,1,0)
    accuracy=sum(ifelse(rf_class==df_test$RETURN,1,0))/nrow(df_test)
    print(c(i,j,accuracy))
  }
}





## Bagging

library(randomForest)
ncol(df_rest)

set.seed(13)
bag.trees=randomForest(RETURN~.,data=df_rest,ntree=2000,mtry=23,importance=TRUE)

bag.trees
importance(bag.trees)
varImpPlot(bag.trees)

bagging_preds=predict(bag.trees,newdata=df_test,type="prob")
bagging_probs=bagging_preds[,2]
bagging_class=ifelse(bagging_probs>0.54,1,0)

table(df_test$RETURN,bagging_class)
sum(ifelse(bagging_class==df_test$RETURN,1,0))/nrow(df_test)





## Boosting

colnames(df_rest)
library(gbm)
rest_boost=data.frame()

cols.num <- c(colnames(df_rest))
cols.num
df_rest[cols.num] <- sapply(df_rest[cols.num],as.numeric)
sapply(df_rest, class)

colnames(rest_boost)
## The one downside to gbm in R: it requires numeric variables
## So, let's turn our factor into a 0 and 1 numeric variable and tell it to use a binomial ("bernoulli") distribution
##    with numeric variables from our data set
set.seed(13)
boost.Hospital_rest=gbm(RETURN-1 ~ MONTH_ARR-1 + WEEKDAY_DEP-1 + HOUR_DEP-1 + MONTH_DEP-1 + SAME_DAY-1 + CONSULT_ORDER-1 + CONSULT_CHARGE-1 + CONSULT_IN_ED-1 + RETURN-1 + HOSPITAL + GENDER + AGE + RACE+ETHNICITY+FINANCIAL_CLASS+ED_RESULT+ACUITY_ARR+DC_RESULT+ADMIT_RESULT+DIAGNOSIS+DIAG_DETAILS+RISK+SEVERITY+CHARGES,data=df_rest,distribution="bernoulli",
                        n.trees=1000)
summary(boost.Hospital_rest)

predict(boost.Hospital_rest,newdata=df_test,n.trees=1000,type="response")
boosting_probs=predict(boost.Hospital_rest,newdata=df_test,n.trees=1000,type="response")

boosting_class=ifelse(boosting_probs>0.55,1,0)

table(df_test$RETURN,boosting_class)
sum(ifelse(boosting_class==df_test$RETURN,1,0))/nrow(df_test)





## XG Boost

set.seed(13)
library(xgboost)
labels_rest<-df_rest$RETURN
labels_test<-df_test$RETURN
nrow(df_rest)
nrow(total)
total<-rbind(df_rest[,-c(24)],test_courtney)
total_matrix<-model.matrix(~.-1,data=total)
new_training_rest<-total_matrix[1:28666,]
new_training_courtney<-total_matrix[28667:40698,]


new_training_test<-model.matrix(~.-1,data=df_test[,-c(24)])

labels_rest_numeric<-as.numeric(labels_rest)-1
labels_test_numeric<-as.numeric(labels_test)-1
dtrain<-xgb.DMatrix(data=new_training_rest,label=labels_rest_numeric)
dtest<-xgb.DMatrix(data=new_training_test,label=labels_test_numeric)
dcourtney<-xgb.DMatrix(data=new_training_courtney)
for (s in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)){
  
  for (col in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)){
    params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1, gamma=0, max_depth=6, min_child_weight=1, subsample=s, colsample_bytree=col)
    
    ##xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
    
    xgb.train <- xgb.train( params = params, data = dtrain, nrounds = 202, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 15, maximize = F)
    for (j in c(0.40,0.41,.42,.43,.44,.45,.46,.47,.48,.49,.50,.51,.52,.53,.54,.55,.56,.57,.58,.59,.60)){
      xgbpred <- predict (xgb.train,dtest)
      xgb_class <- ifelse (xgbpred > j,1,0)
      accuracy=sum(ifelse(xgb_class==df_test$RETURN,1,0))/nrow(df_test)
      print(c(s,col,j,accuracy))}}}



params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1, gamma=0, max_depth=7, min_child_weight=1, subsample=.8, colsample_bytree=.8)

##xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

xgb.train <- xgb.train( params = params, data = dtrain, nrounds = 202, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 15, maximize = F)

xgbpred <- predict (xgb.train,dtest)
xgb_class <- ifelse (xgbpred > .56,1,0)
accuracy=sum(ifelse(xgb_class==df_test$RETURN,1,0))/nrow(df_test)
print(accuracy)
set.seed(13)



courtney_predict<-predict(xgb.train,dcourtney)
courtney_class<-ifelse(courtney_predict>0.52,1,0)
results_xg_test <- ifelse(courtney_class == 1,'Yes','No')

write.csv(results_xg_test, file='Xg_courtney_202_.8_.8_.54.csv')
dim(dcourtney)
colnames(dtest)==colnames(dcourtney)
colnames(dtest)

count(df_rest$DC_RESULT)
count(df_rest$ADMIT_RESULT)




for (s in c(0.6,0.7,0.8,0.9,1)){
  
  for (col in c(0.6,0.7,0.8,0.9,1)){
    set.seed(13)
    params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1, gamma=0, max_depth=6, min_child_weight=1, subsample=s, colsample_bytree=col)
    
    ##xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
    
    xgb.train <- xgb.train( params = params, data = dtrain, nrounds = 202, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 15, maximize = F)
    for (j in c(0.40,0.41,.42,.43,.44,.45,.46,.47,.48,.49,.50,.51,.52,.53,.54,.55,.56,.57,.58,.59,.60)){
      xgbpred <- predict (xgb.train,dtest)
      xgb_class <- ifelse (xgbpred > j,1,0)
      accuracy=sum(ifelse(xgb_class==df_test$RETURN,1,0))/nrow(df_test)
      print(c(s,col,j,accuracy))}}}

for (dept in c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)){
  set.seed(13)
  params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.1, gamma=0, max_depth= 6, min_child_weight=1, subsample=.8, colsample_bytree=.8)
  
  ##xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
  
  xgb.train <- xgb.train( params = params, data = dtrain, nrounds = 202, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 15, maximize = F)
  
  xgbpred <- predict (xgb.train,dtest)
  for (i in c(0.40,0.41,.42,.43,.44,.45,.46,.47,.48,.49,.50,.51,.52,.53,.54,.55,.56,.57,.58,.59,.60)){
    xgb_class <- ifelse (xgbpred > i,1,0)
    accuracy=sum(ifelse(xgb_class==df_test$RETURN,1,0))/nrow(df_test)
    print(c(dept,i,accuracy))}}



set.seed(13)
params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.05, gamma=0, max_depth= 10, min_child_weight=1, subsample=.8, colsample_bytree=.8)

##xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 200, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)

xgb.train <- xgb.train( params = params, data = dtrain, nrounds = 250, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 15, maximize = F)
xgbpred <- predict (xgb.train,dtest)
for (i in c(0.40,0.41,.42,.43,.44,.45,.46,.47,.48,.49,.50,.51,.52,.53,.54,.55,.56,.57,.58,.59,.60)){
  xgb_class <- ifelse (xgbpred > i,1,0)
  accuracy=sum(ifelse(xgb_class==df_test$RETURN,1,0))/nrow(df_test)
  print(c(i,accuracy))}


courtney_predict<-predict(xgb.train,dcourtney)
courtney_class<-ifelse(courtney_predict>0.50,1,0)
results_xg_test <- ifelse(courtney_class == 1,'Yes','No')

write.csv(results_xg_test, file='Xg_courtney_250_.8_.8_.50_0.05eta_dep_10.csv')
dim(dcourtney)
colnames(dtest)==colnames(dcourtney)
colnames(dtest)

