#setup the session
library(foreign)
library(MASS)
#################################################
############## Q1 ###############################
#################################################
# import the data
lawnmower <- read.dta("lawnmower.dta")
# create a scatter plot of the lawnmower data
pchs <- c(17, 22)
cols <- c("seagreen3", "orangered1")
plot(x = lawnmower$lotsize, y = lawnmower$income, pch = pchs[lawnmower$owner],
     col = cols[lawnmower$owner], xlab = "Lotsize", ylab = "Income",
     main = "Scatter Plot for Lawnmower")
# enble to add a legend outside the plot
par(xpd = TRUE)
# add a legend to the plot
legend("bottomright", legend = c("Non-owner", "Owner"), col = cols, pch = pchs)

#################################################
############## Q2 ###############################
#################################################
# fit a lda model
lda.fit <- lda(owner ~., data = lawnmower)
lda.fit
# predict the lda model
lda.pred <- predict(lda.fit)
# create a confusion matrix of the predicted lda model
table(lda.pred$class, lawnmower$owner)
# get the accuracy rate of the prediction
mean(lda.pred$class == lawnmower$owner)

#################################################
############## Q3 ###############################
#################################################
# randomly select the index of 20 observations as the training data
set.seed(328)
train.index <- sample(nrow(lawnmower), 20)
# get the testing data
test <- lawnmower[-train.index, ]
# fit a lda model
lda.fit.subset <- lda(owner ~., data = lawnmower, subset = train.index)
lda.fit.subset
# predict the testing data using the lda model
lda.pred.subset <- predict(lda.fit.subset, test)
# create a confusion matrix of the predicted results
table(lda.pred.subset$class, test$owner)
# get the accuracy rate of the prediction
mean(lda.pred.subset$class == test$owner)

#################################################
############## Q4 ###############################
#################################################
# fit a qda model
qda.fit.subset <- qda(owner ~., data = lawnmower, subset = train.index)
qda.fit.subset
# predict the testing data using the lda model
qda.pred.subset <- predict(qda.fit.subset, test)
# create a confusion matrix of the predicted results
table(qda.pred.subset$class, test$owner)
# get the accuracy rate of the prediction
mean(qda.pred.subset$class == test$owner)