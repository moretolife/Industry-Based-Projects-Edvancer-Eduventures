#' ---
#' title: "Predicting whether customers will succesfully repay the loans or No: Using Decision Tree and Random Forest Model"
#' author: "Executed by Neha More"
#' date: "Nov 3rd, 2017"
#' ---

#'**PROBLEM STATEMENT**
#'
#'Payday Loans are high risk short term lending financial products 
#'and its very important to asses risk of payment default. 
#'Use dataset "paydayloan_collections.csv" to build a model whether repayment will
#'be successful or not.
#'
#'**AIM: Break the dataset into test and train data.Use RandomForest and DecisionTress to build your model on train data and compare their performance on test data.**
#'
#'_____
#'Initial Setup for using decision tree and random forest model:
#'
library(tree)
library(ISLR)
library(randomForest)
library(dplyr)


loan=read.csv("paydayloan_collections.csv")
View(loan)
str(loan) #VERY IMP TO NOTE THAT PAYMENT IS OF FACTOR TYPE...WHILE REST OTHERS ARE ALSO OF FACTOR N NUM TYPE.
glimpse(loan)
table(loan$payment)
#'the reponse variable is of classification type.ie.payment=denied/sucess

#'lets make decision tree first and then random forest method
#'
#'**Step 1:divide the data into train and test(50:50%)**
set.seed(2)
s=sample(1:nrow(loan),15000)
loan_train=loan[s,]
loan_test=loan[-s,]

#'**step 2: making tree model for train data**
tree.loan=tree(payment~.,data=loan_train)

#'lets plot it
plot(tree.loan) #model with 7 nodes.
text(tree.loan,pretty=0)
tree.loan #text format output
summary(tree.loan)
#'output:for train dataset having 7 nodes:
#'residual mean dev=0.88
#'error rate=0.1718
#'
#'

#'**step 3:predict for test dataset**
tree.pred=predict(tree.loan,newdata = loan_test,type ="class")

#'**step 4:check the predictions made on test data and calculate error.**
table(tree.pred,loan_test$payment)
(1252+1373)/15000
#'error on test data=0.175
#'

#'lets find error on train data:
tree.pred.train=predict(tree.loan,newdata = loan_train,type ="class")
table(tree.pred.train,loan_train$payment)
(1304+1273)/15000

#'**CONCLUSION:ERROR FOUND ON TRAIN DATA IS 0.1718 WHILE ON TEST DATA 0.175.(GOOD MODEL)**
#'
#'--------------------------------------------------------------------------------
#'**Step 5: STILL LETS TRY AND DO PRUNING FOR OPTIMUM MODEL OF TREE.**
#'**WE HERE USE CROSS VALIDATION FUNCTION AND PRUNE FUNCTION**

set.seed(2)
cv.loan=cv.tree(tree.loan,FUN = prune.misclass)

#'lets plot the pruned tree
plot(cv.loan$size,cv.loan$dev,type="b")
#'plot shows that error is minimum on 7 nodes ONLY where deviance is minimum
#'
#'**Hence the tree itself is best size with 7 nodes which already exist.**
#'
#'----------------------------------------------------------------------
#'
#'**Lets do random forest method**
#'
#'*Here response is clasification type(payment=success/denied)*
#'
#'**step 1:build rf model on train dataset.**
rf.loan=randomForest(payment~.,data = loan_train)#U CAN USE ARGUMENT do.trace=T
rf.loan
#'
#'
#'*output:
#'interpretatn:ntree=500
#'no. of variables tried at each split=5
#'OOB estimate of error rate=13.43%
#'i.e.(1125+890)/15000=13.43%*
#'
#'**step 2:predicting rf model created on test dataset and find error**
#'
#'
rf.loan.pred=predict(rf.loan,newdata = loan_test)
t=table(loan_test$payment,rf.loan.pred)
t

#'interpretation:
(1149+832)/15000
#'error=13.20% by random forest model.
#'
#'
#'**step 3:Plot variablesImpPlot(i.e. variableswhich are important):**
#'
#'
importance(rf.loan)
varImpPlot(rf.loan)
#'
#'
#'variables 25,17,16,23 are most imp variables contributing to the output.
#'
#'**Conclusion:error generated on PREDICTING TEST data by random forest is 13.20% as against 17.5% by decision tree model.**
#'
#'**Hence random forest is better than decision tree model .**
#'
#'