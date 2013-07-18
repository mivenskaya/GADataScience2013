# defines a function knn_nfold, given n (number of folds), maxk (maximum k value), data, 
#and the column of the label trained for.

knn_nfold<- function(n, maxk, dataset, column)
{
library(class)
data<-dataset
#randomize the data
data<-data[sample(nrow(data)),]
#pull labels from data
labels<-data[,column]
data[,column]<-NULL
#set the seed 
set.seed(1)
#pre-allocate a vector for minimum errors in each fold
average.errors<-numeric()
#get number of records
nrecords<-nrow(data)
#the length of each subset/fold will be number of records divided by number of folds
len.of.subsets<-nrecords/n
#we want to train on all but one of the subsets
train.test.split <-len.of.subsets*(n-1)

for (i in 1:n)
  {
  #take a train subset 
  train.sub<-data[1:train.test.split,]
  #the rest is test subset
  test.sub<-data[(train.test.split+1):nrecords,]
  print ("new train and test sets taken")
  #take the train labels
  train.labels<-as.factor(as.matrix(labels)[1:train.test.split,])
  #the rest are test labels
  test.labels<-as.factor(as.matrix(labels)[(train.test.split+1):nrecords,])
  #initialize data frame for error rates
  err.rates<-data.frame()
  

  #train on this fold
  for (k in 1:maxk)
    
  {
    
    knn.fit<-knn(train=train.sub, 
                 test = test.sub, 
                 cl = train.labels, 
                 k=k)
    cat('\n', 'k= ', k, ', fold =', i, '\n', sep = '')
    #print confusion matrix
    print(table(test.labels, knn.fit))
    #calculate error for this k
    this.err<-sum(as.character(test.labels) != as.character(knn.fit))/length(test.labels)
    
    #add this error to the errors dataframe
    err.rates <- rbind(err.rates, this.err)  

  }
  average.errors <- c(average.errors, mean(err.rates[,1]))

  #rotate the train and test data
  data<-rbind(test.sub, train.sub)

  #rotate the train and test labels
  labels<-c(test.labels, train.labels)


  }
print ('average error rates over all folds:')
print (mean(average.errors, na.rm=TRUE))
#create and print plot of error rates in relation to k
library(ggplot2)
results<-data.frame(1:maxk, err.rates)

names(results)<-c('k', 'err.rate')
results.plot<-ggplot(results, aes(x=k, y=err.rate))+geom_smooth()
print(results.plot)
}
knn_nfold(3, 100, iris, 5)