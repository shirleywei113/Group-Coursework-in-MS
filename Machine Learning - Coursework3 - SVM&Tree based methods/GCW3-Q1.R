# create a sequence x as pj1 ranging from 0 to 1
x = seq(0,1,by=0.01)
# create the formula for entropy
entropy = -(x*log(x)+(1-x)*log(1-x))
# create the formula for Gini index
gini = 2*x*(1-x)
# create the formula for classification error rate
classification.error = 1-pmax(x,1-x)
# plot entropy,Gini index and classification error rate as three lines
plot(x,entropy,col="steelblue1",type="l",xlab="pj1",ylab="Values of impurity measures")
lines(x,gini,col="red",type="l")
lines(x,classification.error,col="seagreen3")
# add legend
legend("topright",legend=c("Entropy","Gini index","Classification error rate"),
       col=c("steelblue1","red","seagreen3"),pch = c(1,1,1))
