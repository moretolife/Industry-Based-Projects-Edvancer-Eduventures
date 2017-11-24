#' ---
#' title: "Predicting Sales related to counterfeit Medicines: Using Decision Tree and Random Forest Model"
#' author: "Executed by Neha More"
#' date: "Nov 3rd, 2017"
#' ---

#'**PROBLEM STATEMENT**
#'
#'Use dataset "base_data.csv" to build a model.Variable names are self explanatory.Your task here is to 
#'build predictive model for predicting sales figures given other information related to counterfeit medicine 
#'selling operations.
#'
#'**AIM:  Use RandomForest and Decision Tress to build your model on train data and compare their performance on test data. Also get the variable importance plot for the model.**
#'
#'
#'Initial Setup for using decision tree and random forest model:
#'
library(tree)
library(ISLR)
library(randomForest)
library(dplyr)
med=read.csv("base_data.csv") #gives character n num data types.
View(med)
str(med)
glimpse(med)

#'**step 1: Data preparation**
med=med %>%
  na.omit()
glimpse(med)

#'divide data into test n train in ration 30:70%
set.seed(3)
s=sample(1:nrow(med),0.7*nrow(med))
med_train=med[s,]
med_test=med[-s,]
#'here response variable is Counterfeit_Sales which is continues numeric variable type.
#'hence problem is of regression type.
#'
#'**STEP 2: Lets build tree model on train data**
med.tree=tree(Counterfeit_Sales~.-Medicine_ID,data=med_train,na.action = na.exclude)
summary(med.tree)
#'terminal nodes:6
#'mean deviance:1188000
plot(med.tree)
text(med.tree,pretty = 0)

#'**step 3:Predict for test model and find error rmse**
med.pred=predict(med.tree,newdata = med_test)

#'*finding error:RMSE(done for regression variable)*
sum((med_test$Counterfeit_Sales-med.pred)**2) %>%
  sqrt()
#'**error is :49925.79**
#'--------------------------------------------------------------------
#'**Step 4: Lets prune the tree**
#'
set.seed(3)
cv.med=cv.tree(med.tree)

#'**step 5: lets plot the pruned tree **
plot(cv.med$size,cv.med$dev,type="b")
#'deviance is smallest for 6 terminal nodes...so no need to create new model
#'
#'------------------------------------------------------------------------
#'
#'**LETS DO RANDOM FOREST MODEL**
med.rf=randomForest(Counterfeit_Sales~.-Medicine_ID,data=med_train,na.action = na.exclude)#can use do.trace=T
med.rf
#'ntree=500
#'no of variable at each split:3
#'Mean of squared residuals: 1193459
#'% Var explained: 49.83
#'lets predict for test data
med.rf.pred=predict(med.rf,newdata = med_test)
#'lets find rmse
sum((med_test$Counterfeit_Sales-med.rf.pred)**2) %>% 
  sqrt()
#'**rmse:49057.98**

importance(med.rf)
varImpPlot(med.rf)
#'**the mrp and availability rating and counterfeit weight ate the most imp variables contributing towards building of model while side effect level and area city type are least imp variables contributing to build model.**
#'
#'
#'
#'**CONCLUSION:RMSE by decision tree is 49925 and that by random forest is 49057. Hence random forest is little better model.**