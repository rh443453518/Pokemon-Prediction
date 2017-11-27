Pokemon = read.csv("Pokemon.csv")
summary(Pokemon)
library(MASS)
library(leaps)
library(bestglm)
library(dummies)
library(data.table)
library(glmnet)

# Split the Pokemon dataset into 75% training and 25% testing rondomly
# set.seed(1) to make the result reproductible
set.seed(1)
train_size = floor(0.75 * nrow(Pokemon))
train_ind = sample(seq_len(nrow(Pokemon)), size = train_size)
train = Pokemon[train_ind, ]
test = Pokemon[-train_ind, ]
selection_model1 = subset(Pokemon,select=c(6,7,8,9,10,11,13))

# Try Logistic Regression with only 6 stats
Lo_Reg1_train = subset(train,select=c(6,7,8,9,10,11,13))
Lo_Reg1_test = subset(test,select=c(6,7,8,9,10,11,13))
glm.fits1 = glm(Legendary~.,data=Lo_Reg1_train,family=binomial)
fitted.results1 = predict(glm.fits1,newdata=Lo_Reg1_test,type='response')
glm.pred1 =rep("FALSE",200)
glm.pred1 [fitted.results1>.5]="TRUE"
misClasificError1 = mean(glm.pred1 != test$Legendary)
table(glm.pred1 , test$Legendary)
print(paste('Misclassification Error is ', misClasificError1))

# Since we only have 6 features in this case, we can simply use best subset regression to see what variables to include
# We use cross-validation 
bestglm(selection_model1,IC="CV",family=binomial, method="exhaustive")
# Best Subset Selection chooses the model without sp. defense and attack

# Let's see how the model without sp. defense and attack looks like
Lo_Reg2_train = subset(train,select=c(6,8,9,11,13))
Lo_Reg2_test = subset(test,select=c(6,8,9,11,13))
glm.fits2 = glm(Legendary~.,data=Lo_Reg2_train,family=binomial)
fitted.results2 = predict(glm.fits2,newdata=Lo_Reg2_test,type='response')
glm.pred2 =rep("FALSE",200)
glm.pred2 [fitted.results2>.5]="TRUE"
misClasificError2 = mean(glm.pred2 != Lo_Reg2_test$Legendary)
table(glm.pred2 , Lo_Reg2_test$Legendary)
print(paste('Misclassification Error is ', misClasificError2))

# Right now, we can expand the model a little bit, by including not only 6 stats, but also generation and types
# To achieve this, we can use package dummies to transform these factors into dummy variables
rows = seq.int(800)
type1_dummy = dummy(Pokemon$Type.1)
type2_dummy = dummy(Pokemon$Type.2)
type_dummy = type1_dummy + type2_dummy[,c(2:19)]
generation_dummy = dummy(Pokemon$Generation)
type_dummy = as.data.frame.matrix(type_dummy)
generation_dummy = as.data.frame.matrix(generation_dummy)
type_dummy$ID = rows
generation_dummy$ID = rows
temp1 = merge(type_dummy, generation_dummy, by=c("ID"))
Pokemon_temp1 = Pokemon[,c(6:11)]
Pokemon_temp2 = Pokemon[,c(13,12)]
Pokemon_temp1$ID = rows
Pokemon_temp2$ID = rows
Pokemon_all_variable = merge(Pokemon_temp1, temp1, by=c("ID"))
Pokemon_all_variable = merge(Pokemon_all_variable, Pokemon_temp2, by=c("ID"))
Pokemon_all_variable = Pokemon_all_variable[,c(2:32)] 

# Try the full logistic model
train_full = Pokemon_all_variable[train_ind, ]
test_full = Pokemon_all_variable[-train_ind, ]
glm.fits_full = glm(Legendary~.,data=train_full,family=binomial)
fitted.results_full = predict(glm.fits_full,newdata=test_full,type='response')
glm.pred_full =rep("FALSE",200)
glm.pred_full[fitted.results_full>.5]="TRUE"
misClasificError_full = mean(glm.pred_full != test_full$Legendary)
table(glm.pred_full, test_full$Legendary)
print(paste('Misclassification Error is ', misClasificError_full))

# Try Lasso
grid = 10^seq(10,-2,length = 100)
full_lasso = cv.glmnet(x=as.matrix(train_full[,c(1:30)]), y=as.factor(train_full[,c(31)]), alpha=1, family="binomial",lambda=grid)
lasso.bestlam = full_lasso$lambda.min
full_lasso_pred = predict(full_lasso,s=lasso.bestlam,newx=as.matrix(test_full[,c(1:30)]), family="binomial")
glm.pred3 =rep("FALSE",200)
glm.pred3 [full_lasso_pred>0]="TRUE"
misClasificError3 = mean(glm.pred3 != test_full$Legendary)
table(glm.pred3 , test_full$Legendary)
print(paste('Misclassification Error is ', misClasificError3))

# Let's go through forward stepwise selection on the full model by comparing BIC
forward_glm.fits_full = stepAIC(glm.fits_full, k=log(nrow(train_full)), direction = "forward", trace = FALSE)
forward_glm.fits_full$anova
# Foward selection turns out the full model

# Next, we go through backward stepwise selection on the full model by comparing BIC
backward_glm.fits_full = stepAIC(glm.fits_full, k=log(nrow(train_full)), direction = "backward", trace = FALSE)
backward_glm.fits_full$anova
# It turns out the the model we use backward stepwise selection only includes HP, attack, sp. attack, sp. defense and speed

# Let's see how the model with only HP, attack, sp. attack, sp. defense and speed looks like
train_full2 = subset(train,select=c(6,7,9,10,11,13))
test_full2 = subset(test,select=c(6,7,9,10,11,13))
glm.fits3 = glm(Legendary~.,data=train_full2,family=binomial)
fitted.results3 = predict(glm.fits3,newdata=test_full2,type='response')
glm.pred3 =rep("FALSE",200)
glm.pred3 [fitted.results3>.5]="TRUE"
misClasificError3 = mean(glm.pred3 != test_full2$Legendary)
table(glm.pred3 , test_full2$Legendary)
print(paste('Misclassification Error is ', misClasificError3))

