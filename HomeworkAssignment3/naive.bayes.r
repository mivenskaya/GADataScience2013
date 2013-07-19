#defines a function that trains a naive bayes classifier on the iris dataset given the number of folds for cross
#validation and returns the average error rate of all the folds. 
iris_bayes_nfold<-function(n)
{
#get the data
install.packages("e1071", lib = "Rpackages")
library("e1071")
data(iris)
#set the seed 
set.seed(1)
#randomize data
iris<-iris[sample(nrow(iris)),]
#get number of rows in data
nrecords<-nrow(iris)
#the length of each subset/fold will be the number of rows in data divided by number of folds
len.of.subsets<-nrecords/n
#we want to train on all but one of the subsets in each fold
train.test.split<-len.of.subsets*(n-1)
#pre-allocate a vector for the error rates
error.list<-numeric()
#train on each subset
for (i in 1:n)
{
  #take a train subset 
  train.sub<-iris[1:train.test.split,]
  #take a test subset
  test.sub<-iris[(train.test.split+1):nrecords,]
  #train the classifier on the train subset
  classifier<-naiveBayes(train.sub[,1:4], train.sub[,5])
  #print the confusion matrix, based on test subset
  print(table(predict(classifier, test.sub[,-5]), test.sub[,5], dnn = list('predicted', 'actual')))
  #calculate the error rate for this fold
  this.err<-sum(test.sub[,5]!= predict(classifier, test.sub[,-5]))/length(test.sub[,5])
  #add this error to the error list
  error.list<-c(error.list, this.err)     
  #rotate the training and test sets
  iris<-rbind(test.sub, train.sub)
  
}
print (error.list)
print (mean(error.list))
return (mean(error.list))
}

#call the function
iris_bayes_nfold(3)