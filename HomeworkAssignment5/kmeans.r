#For this exercise, I looked at data about breakfast cereals.  I think this dataset is good for clustering analysis- 
# while there are no "true" labels, it will be interesting to see if the clusters allign with some logical groups -
#my hypothesis is that healthy, fiber-rich cereals will fall into one cluster, sugary cereals will fall into another, 
#cereals with nuts will likely fall into a cluster becuase of their high protein content, etc. 

#load necessary library
library(cluster)
#load data
data<-read.csv('cereals.csv')
#look at the structure of data
str(data)
#i only want to look at columns 4 to 16 (the first 3 columns are name, manufacturer, and type (cold vs. hot) - I don't want to inlude these)
mydata<-data[,4:16]
#figure out the number of k by performing the elbow test:
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss) 
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 
#it looks like the "elbow" is around 4-6 clusters - i will try each and look at the results
fit<-kmeans(mydata, 4)
aggregate(mydata,by=list(fit$cluster),FUN=mean)
mydata1 <- data.frame(data, fit$cluster) 
#plot the clusters
clusplot(mydata1, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
write.table(mydata1, file = 'cereal.clusters.1.txt', sep = '\t')

#try with 5 clusters:
fit<-kmeans(mydata, 5)
aggregate(mydata,by=list(fit$cluster),FUN=mean)
mydata2 <- data.frame(data, fit$cluster) 
clusplot(mydata2, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
write.table(mydata2, file = 'cereal.clusters.2.txt', sep = '\t')

#try with 6 clusters:
fit<-kmeans(mydata, 6)
aggregate(mydata,by=list(fit$cluster),FUN=mean)
mydata3 <- data.frame(data, fit$cluster) 
clusplot(mydata3, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
write.table(mydata3, file = 'cereal.clusters.3.txt', sep = '\t')

#comparring the three charts and the three files, it looks like 5 clusters is the best solution. 

#the breakdown of the cererals looks as follows:

#1. moderately sugary, very standard cerals - captain crunch, cornflakes, special k
#2. mostly nutty and raisiny cerals, bran cereals: muesli, raisin bran, fruitful bran, all bran, etc. These tend to be high protein and somewhat high fiber
#3. mostly wheat cereals: shredded_wheat, puffed_wheat, shredded_wheat_n'Bran, frosted_mini_wheats
#4. not clear what this grouping is about: cheerios, rice krispies, golden_grahams.  Very similar to cluster 1 (in fact different types of Cheerios appear in both)
#5. mostly fruity, sweet cereals - froot loops, fruity pebbles, trix, grape nuts flakes, apple jacks. 

# some of the clusters are very clear (the wheat group, the bran/nutty group), others are less clear. 
# out of curiosity i tried to run kmeans with 3 clusters. 
fit<-kmeans(mydata, 3)
aggregate(mydata,by=list(fit$cluster),FUN=mean)
mydata4 <- data.frame(data, fit$cluster) 
clusplot(mydata4, fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
write.table(mydata4, file = 'cereal.clusters.4.txt', sep = '\t')

# the results are just like I expected: 2 small groups (one for wheat cereals, one for bran cereals), and one large group with 
# everything else.  