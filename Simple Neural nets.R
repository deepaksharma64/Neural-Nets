# Neural nets
library(MASS)
library(neuralnet)
dim(Boston)  #506x14

DataFrame<-Boston
str(DataFrame)
hist(DataFrame$medv) #Left Skew
head(DataFrame,3)
apply(DataFrame,2,range) #check the range of each column.
#seems like scale of each variable is not same.
#Normalizing the data in interval [0,1]
#scale function will give mean =0 and std=1 for each variable

maxValue<-apply(DataFrame,2,max) # 2 id for column,max is for each of the column
minValue<-apply(DataFrame,2,min)
DataFrame<-as.data.frame(scale(DataFrame,center=minValue,scale=maxValue-minValue))
#Scale function converts it in matrix form,so need to convert it to dataframe.
head(DataFrame,3)
ind<-sample(1:nrow(DataFrame),400)
trainDF<-DataFrame[ind,]
testDF<-DataFrame[-ind,]
#Lets take some configuration for neural network
#say 13-4-2-1
#so number of hidden layers=2
#input layer had 10 units

#We need this as formula
#medv~crim+zn+indus+chas+max+rm+age+dis+rad+tax+ptratio+black+lstat

allVars<-colnames(DataFrame)
predictorVars<-allVars[!allVars%in%"medv"]
predictorVars<-paste(predictorVars,collapse='+')
form=as.formula(paste("medv~",predictorVars,collapse = "+"))

neuralModel<-neuralnet(formula=form,hidden=c(4,2),linear.output=T
             ,data=trainDF) #Neural net is {13,4,2,1}
#plot neural network
plot(neuralModel)
##Prdeiction for test data set
predictions<-compute(neuralModel,testDF[,1:13])
str(predictions)

predictions<- predictions$net.result*(max(testDF$medv)-min(testDF$medv))+min(testDF$medv) #sacledvalue*unscalingmethod
actualValues<-(testDF$medv)*(max(testDF$medv)-min(testDF$medv))+min(testDF$medv) #sacledvalue*unscalingmethod

MSE<-sum((predictions-actualValues)^2)/nrow(testDF)
MSE  #0.010

plot(testDF$medv,predictions,col='blue',main='Real vs Predicted',pch=1,cex=0.9,type="p",xlab='Acutal',ylab='Predicted')
abline(0,1,col="black")

###use cross validation to pick the right model####







