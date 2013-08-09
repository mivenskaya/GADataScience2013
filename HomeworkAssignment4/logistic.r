#load the necessary library
library('hydroGOF')

#load the data
x <- read.csv("contraception.csv")

#look at the structure of data
print (str(x))

#change some of the variables to factors
x$religion<-as.factor(x$religion)
x$work<-as.factor(x$work)
x$occupation<-as.factor(x$occupation)
x$media<-as.factor(x$media)
x$method<-as.factor(x$method)

#make the y binary (no contraception vs. some contraception)
x$has.contraception<-1
x$has.contraception[which(x$method == '1')]<-0

#initiate data frame for error rates
error.rates<-vector()

#create a function that takes n and the dataset and runs an n-fold logistic regression, 
#averaging error rates over the folds. 

logistic_nfold<- function(n, dataset)
{

#set the seed 
set.seed(1)
data<-dataset

#randomize the data
data<-data[sample(nrow(data)),]

#get number of records
nrecords<-nrow(data)

#the length of each subset/fold will be number of records divided by number of folds
len.of.subsets<-nrecords/n

#we want to train on all but one of the subsets
train.test.split <-len.of.subsets*(n-1)

#train on each subset
for (i in 1:n)
  {
  #take a train subset 
  train.sub<-data[1:train.test.split,]

  #the rest is test subset
  test.sub<-data[(train.test.split+1):nrecords,]
  
  print ("new train and test sets taken")
  
  #train on this fold
  #for the first try, let's train on every variable
   logit.fit<- glm(has.contraception ~ 0+.-method, data = train.sub)
   #the average error rate for this fit is .46. 
   print (summary(logit.fit))
   #It looks like husband's education and husband's occupation are the least predictive variables.  
   logit.fit2<- glm(has.contraception ~ 0+.-method -husbeduc -occupation, data = train.sub)
   #the average error has gone down a by less than .1 - not very significant. Let's
   #try getting rid of another variable that's not very predictive - whether or not the woman works:
   logit.fit3<- glm(has.contraception ~ 0+.-method -husbeduc -occupation -work, data = train.sub)
   print (summary(logit.fit3))
   #the error rates has gone down a litte - to .458
   #getting rid of any other variable made the error rate go up - it looks like .458 
   #is as good as it's going to get. 
   
   #test on test set
   prediction <- predict(logit.fit3, newdata=test.sub, type='response')
   
   #print the confusion matrix
   print(table(prediction>.5, test.sub$has.contraception))

    
    #calculate the error for this fold 
    this.error<-rmse(prediction, test.sub$has.contraception)
    
    #attach it to the vector of errors
    error.rates<-rbind(error.rates, this.error)
    
    #rotate training and test sets
    data<-rbind(test.sub, train.sub)


}
#look at the error rate
print (error.rates)
print (mean(error.rates))
}
#call the function
logistic_nfold(3, x)