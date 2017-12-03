# 3 Predicting Pokemon Type
# 3.2 Tree-based Methods

# 1) Setting up and Data inspection
setwd("/Users/LukerRong/Documents/Cornell 2017 Fall Course/STSCI 4740/Homework/Final")
Pokemon <- read.csv("Pokemon.csv")
attach(Pokemon)
summary(Pokemon)
112/800 # Prediction Baseline
unique(Pokemon$Type.1) # 18 unique value in Type 1
unique(Pokemon$Type.2) # 19 unique value in Type 1 (empty count as 1 unique value)
is.na(Pokemon$Type.2) # Empty count as 1 unique value, no need to encode it
#cor(Pokemon)


## Count how many [Type1,Type2] combination
Type.combine <- rep("type", nrow(Pokemon))
for (i in 1:nrow(Pokemon)) {
  if(Pokemon[i,]$Type.2==""){
    Type.combine[i] = c(Pokemon[i,]$Type.1)[1]*100
  }
  else{
    list = sort(c(Pokemon[i,]$Type.1,Pokemon[i,]$Type.2))
    Type.combine[i] = list[1]*100+list[2]
  }
}
ordered(Type.combine)


# 2) Trial run without feature engineering
# Decision Tree without feature engineering
library(tree)
tree.pokemon=tree(Type.1~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary,Pokemon)
summary(tree.pokemon)
plot(tree.pokemon)
text(tree.pokemon,pretty=0)
tree.pokemon

# Cross Validation, output: misclassification rate
test_error = rep(0,10)
for (i in 1:10) {
  train_size = floor(0.75 * nrow(Pokemon))
  train_ind = sample(seq_len(nrow(Pokemon)), size = train_size)
  Pokemon.train = Pokemon[train_ind, ]
  Pokemon.test = Pokemon[-train_ind, ]
  tree.pokemon=tree(Type.1~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary,data = Pokemon.train)
  tree.pred=predict(tree.pokemon,Pokemon.test,type="class")
  result <- table(tree.pred,Pokemon.test$Type.1)
  test_error[i] = 1-sum(diag(result))/sum(result)
}
mean(test_error)

# Random Forests without feature engineering
library(randomForest)
bag.pokemon=randomForest(Type.1~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary,data=Pokemon.train,ntree=500,importance=TRUE,proximities = TRUE)
bag.pokemon
yhat.bag = predict(bag.pokemon,newdata=Pokemon.test)
plot(yhat.bag, Pokemon.test$Type.1)
mean((yhat.bag!=Pokemon.test$Type.1)^2)

# Cross Validation, output: misclassification rate
test_error = rep(0,10)
for (i in 1:10) {
  train_size = floor(0.75 * nrow(Pokemon))
  train_ind = sample(seq_len(nrow(Pokemon)), size = train_size)
  Pokemon.train = Pokemon[train_ind, ]
  Pokemon.test = Pokemon[-train_ind, ]
  bag.pokemon=randomForest(Type.1~Total+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary,data=Pokemon.train,ntree=500,importance=TRUE,proximities = TRUE)
  yhat.bag = predict(bag.pokemon,newdata=Pokemon.test)
  test_error[i] = mean((yhat.bag!=Pokemon.test$Type.1)^2)
}
mean(test_error)


# 3.2.1 Feature Engineering
Pokemon$AtkP = Pokemon$Attack / Pokemon$Total
Pokemon$DefP = Pokemon$Defense / Pokemon$Total
Pokemon$SpAtkP = Pokemon$Sp..Atk / Pokemon$Total
Pokemon$SpDefP = Pokemon$Sp..Def / Pokemon$Total
Pokemon$SpeP = Pokemon$Speed / Pokemon$Total
Pokemon$HpP = Pokemon$HP / Pokemon$Total

Pokemon$AtkToSpatk = Pokemon$Attack / Pokemon$Sp..Atk
Pokemon$DefToSpdef = Pokemon$Defense / Pokemon$Sp..Def

set.seed(2)
train_size = floor(0.75 * nrow(Pokemon))
train_ind = sample(seq_len(nrow(Pokemon)), size = train_size)
Pokemon.train = Pokemon[train_ind, ]
Pokemon.test = Pokemon[-train_ind, ]

# 2) Run wit feature engineering
# Decision Tree with feature engineering
library(tree)
tree.pokemon=tree(Type.1~Total+AtkP+DefP+SpAtkP+SpDefP+SpeP+HpP+AtkToSpatk+DefToSpdef+Generation+Legendary,Pokemon)
summary(tree.pokemon)
plot(tree.pokemon)
text(tree.pokemon,pretty=0)
tree.pokemon

# Cross Validation, output: misclassification rate
test_error = rep(0,10)
for (i in 1:10) {
  train_size = floor(0.75 * nrow(Pokemon))
  train_ind = sample(seq_len(nrow(Pokemon)), size = train_size)
  Pokemon.train = Pokemon[train_ind, ]
  Pokemon.test = Pokemon[-train_ind, ]
  tree.pokemon=tree(Type.1~Total+AtkP+DefP+SpAtkP+SpDefP+SpeP+HpP+AtkToSpatk+DefToSpdef+Generation+Legendary,data = Pokemon.train)
  tree.pred=predict(tree.pokemon,Pokemon.test,type="class")
  result <- table(tree.pred,Pokemon.test$Type.1)
  test_error[i] = 1-sum(diag(result))/sum(result)
}
mean(test_error)

# Random Forests with feature engineering
library(randomForest)
bag.pokemon=randomForest(Type.1~Total+AtkP+DefP+SpAtkP+SpDefP+SpeP+HpP+AtkToSpatk+DefToSpdef+Generation+Legendary,data=Pokemon.train,ntree=500,importance=TRUE,proximities = TRUE)
bag.pokemon
yhat.bag = predict(bag.pokemon,newdata=Pokemon.test)
plot(yhat.bag, Pokemon.test$Type.1)
mean((yhat.bag!=Pokemon.test$Type.1)^2)

# Cross Validation, output: misclassification rate
test_error = rep(0,10)
for (i in 1:10) {
  train_size = floor(0.75 * nrow(Pokemon))
  train_ind = sample(seq_len(nrow(Pokemon)), size = train_size)
  Pokemon.train = Pokemon[train_ind, ]
  Pokemon.test = Pokemon[-train_ind, ]
  bag.pokemon=randomForest(Type.1~Total+AtkP+DefP+SpAtkP+SpDefP+SpeP+HpP+AtkToSpatk+DefToSpdef+Generation+Legendary,data=Pokemon.train,ntree=500,importance=TRUE,proximities = TRUE)
  yhat.bag = predict(bag.pokemon,newdata=Pokemon.test)
  test_error[i] = mean((yhat.bag!=Pokemon.test$Type.1)^2)
}
mean(test_error)



# Model Selection
# Final Models
tree.pokemon=tree(Type.1~Total+AtkP+DefP+SpAtkP+SpDefP+SpeP+HpP+AtkToSpatk+DefToSpdef+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary,data = Pokemon.train)
tree.pred=predict(tree.pokemon,Pokemon.test,type="class")
summary(tree.pokemon)
result <- table(tree.pred,Pokemon.test$Type.1)
1-sum(diag(result))/sum(result)
plot(tree.pokemon)
text(tree.pokemon,pretty=0)



## Other Tree-based Trials
library(randomForest)
library(ROCR)
type.rf=randomForest(Type.1~Total+AtkP+DefP+SpAtkP+SpDefP+SpeP+HpP+AtkToSpatk+DefToSpdef+HP+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary,data=Pokemon.train,test=Pokemon.test,ntree=500,importance=TRUE,proximities = TRUE)
type.rf.pr = predict(type.rf,type="class",newdata=Pokemon.test)
type.rf.pred = prediction(type.rf.pr, Pokemon.test$Type.1)
type.rf.perf = performance(type.rf.pred, "tpr", "fpr")
plot(type.rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)

library(rpart)
rpart.pokemon=rpart(Type.1~Total+Attack+Defense+Sp..Atk+Sp..Def+Speed+Generation+Legendary,data=Pokemon.train)
summary(rpart.pokemon)
rpart.pred=predict(rpart.pokemon,Pokemon.test,type="class")
plot(rpart.pokemon)

