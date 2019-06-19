# library packages
library(ISLR)
library(caret)
library(e1071)
library(ROCR)
library(tree)
library(randomForest)
# get the dataset
data(OJ)
# split the data to a training set (70%) and a test set (30%)
set.seed(100)
train.index <- createDataPartition(OJ[, 1], p = 0.7, list = FALSE)
train <- OJ[train.index, ]
test <- OJ[-train.index, ]
#################################################
############## (1) ##############################
#################################################
# fit a SVM model with linear kernel and tuning cost using training data
set.seed(100)
model1_train <- tune(svm, Purchase~., data = train, ranges = list(cost = c(0.01, 0.1, 1, 10)),
                     kernel = "linear")
# view the best model
model1_train$best.model
# predict the model
pred1 = predict(model1_train$best.model, test, decision.values = TRUE)
# get the error rate
err1 <- mean(pred1 != test[, 1])
err1

#################################################
############## (2) ##############################
#################################################
# fit a SVM model with radial kernel and tuning cost using training data
set.seed(100)
model2_train <- tune(svm, Purchase~., data = train, ranges = list(cost = c(0.01, 0.1, 1, 10)),
                     kernel = "radial")
# view the best model
model2_train$best.model
# predict the model
pred2 = predict(model2_train$best.model, test, decision.values = TRUE)
# get the error rate
err2 <- mean(pred2 != test[, 1])
err2

#################################################
############## (3) ##############################
#################################################
# fit a SVM model with polynomial kernel, degree = 2 and tuning cost using training data
set.seed(100)
model3_train <- tune(svm, Purchase~., data = train, ranges = list(cost = c(0.01, 0.1, 1, 10)),
                     kernel = "polynomial", degree = 2)
# view the best model
model3_train$best.model
# predict the model
pred3 = predict(model3_train$best.model, test, decision.values = TRUE)
# get the error rate
err3 <- mean(pred3 != test[, 1])
err3

#################################################
############## (4) ##############################
#################################################
# get the performances of the ROC curve of the 3 SVM models
# get the decision values
dv1 = attributes(pred1)$decision.values
# use decision values to predict the result
pred.model1.ROCR <- prediction(dv1, test$Purchase, label.ordering = c("MM", "CH"))
# get the performance of the ROC curve
roc.model1.curve <- performance(pred.model1.ROCR,"tpr","fpr")
dv2 = attributes(pred2)$decision.values
pred.model2.ROCR <- prediction(dv2, test$Purchase, label.ordering = c("MM", "CH"))
roc.model2.curve <- performance(pred.model2.ROCR,"tpr","fpr")
dv3 = attributes(pred3)$decision.values
pred.model3.ROCR <- prediction(dv3, test$Purchase, label.ordering = c("MM", "CH"))
roc.model3.curve <- performance(pred.model3.ROCR,"tpr","fpr")
# plot the ROC curve of the 3 SVM models on the same plot
plot(roc.model1.curve,lwd = 2, cex.lab = 1.5, cex.axis = 1.5, font.lab = 2,
     xlab = "False positive rate (1-specificity)", ylab = "True positive rate (sensitivity)",
     xlim = c(0,1), ylim = c(0,1))
plot(roc.model2.curve, lwd = 2, col = "blue", lty = 3, add = TRUE)
plot(roc.model3.curve, lwd = 2, col = "coral", lty = 5, add = TRUE)
# add a legend to the plot
legend("bottomright", col = c("black", "blue", "coral"),lty = c(1, 3, 5), cex = 0.8, text.font = 1,
       legend = c("SVM with linear kernal", "SVM model with radial kernal", 
                  "SVM with polynomial kernal(degree = 2)"))

# compute the AUC values 
auc.tmp.1 <- performance(pred.model1.ROCR,"auc"); auc.1 <- as.numeric(auc.tmp.1@y.values)
auc.tmp.2 <- performance(pred.model2.ROCR,"auc"); auc.2 <- as.numeric(auc.tmp.2@y.values)
auc.tmp.3 <- performance(pred.model3.ROCR,"auc"); auc.3 <- as.numeric(auc.tmp.3@y.values)
auc.1;auc.2;auc.3

#################################################
############## (5) ##############################
#################################################
# grow a decision tree
OJ.tree <- tree(Purchase ~. , train)
# use cross-validation to find the best tree 
set.seed(100)
OJ.cv <- cv.tree(OJ.tree, FUN = prune.misclass)
# see the result
OJ.cv
# prune the tree with the best size (4 terminal nodes) 
OJ.prune <- prune.misclass(OJ.tree, best = 4)
# plot the tuned tree
plot(OJ.prune)
text(OJ.prune, pretty = 0)
# predict the test instances
pred.OJ.prune <- predict(OJ.prune, test[, -1], type = "class")
# get the error rate
OJ.prune.err <- mean(pred.OJ.prune != test[, 1])
OJ.prune.err

#################################################
############## (6) ##############################
#################################################
# set the values of mtry
OJ.mtry <- c(1, 2, 3, 4, 5, 6)
# create an empty vector
OJ.rf.err <- vector("numeric", length(OJ.mtry))
# create a for loop to calculate the error rate of the random forest results with defined mtry
for (i in OJ.mtry) {
  # fit a random forest model
  set.seed(100)
  OJ.rf <- randomForest(Purchase ~., data = train, mtry = OJ.mtry[i], importance = TRUE)
  # predict the test instances
  OJ.rf.pred <- predict(OJ.rf, newdata = test[, -1])
  # calculate the error rate
  OJ.rf.err[i] <- mean(OJ.rf.pred != test[, 1])
}
# get the result of the error rate using defined mtry
OJ.rf.err <- rbind(OJ.mtry, OJ.rf.err)
rownames(OJ.rf.err) <- c("mtry", "error rate")
OJ.rf.err
# fit a random forest model with the best test error
set.seed(100)
OJ.rf <- randomForest(Purchase ~., data = train, mtry = OJ.rf.err[1, which.min(OJ.rf.err[2, ])], 
                      importance = TRUE)
# plot the importance of variables
varImpPlot(OJ.rf, main = "Importance of Variables")

#################################################
############## (7) ##############################
#################################################
# get the performances of the ROC curve of the decision tree
# get the probability
prob.pred.OJ.prune <- predict(OJ.prune, test[, -1], type = "vector")
# get the prediction of the result
pred.prune.ROCR <- prediction(prob.pred.OJ.prune[ , 2], test$Purchase)
# get the performance of the ROC curve
roc.prune.curve <- performance(pred.prune.ROCR,"tpr","fpr")
# get the performances of the ROC curve of the random forest
prob.OJ.rf.pred <- predict(OJ.rf, test[, -1], type = "prob")
pred.rf.ROCR <- prediction(prob.OJ.rf.pred[ , 2], test$Purchase)
roc.rf.curve <- performance(pred.rf.ROCR,"tpr","fpr")
# plot the ROC curve of the decision tree and the random forest on the same plot
plot(roc.prune.curve,lwd = 2, cex.lab = 1.5, cex.axis = 1.5, font.lab = 2, col = "blue", lty = 2,
     xlab = "False positive rate (1-specificity)", ylab = "True positive rate (sensitivity)",
     xlim = c(0,1), ylim = c(0,1))
plot(roc.rf.curve, lwd = 2, col = "brown2", lty = 3, add = TRUE)
# add a legend to the plot
legend("bottomright", col = c("blue", "brown2"),lty = c(2, 3), cex = 1, text.font = 2,
       legend = c("Decision tree", "Random forest"))

# compute the AUC values
auc.tmp.rf <- performance(pred.rf.ROCR,"auc"); auc.rf <- as.numeric(auc.tmp.rf@y.values)
auc.tmp.dt <- performance(pred.prune.ROCR,"auc"); auc.dt <- as.numeric(auc.tmp.dt@y.values)
auc.rf;auc.dt