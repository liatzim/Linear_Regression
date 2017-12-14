## Encoding categorical data 
dataset$State=factor(dataset$State,
                     levels=c('New York','California','Florida'),
                     labels = =c(1,2,3))


## Splitting the dataset into the Training set and Test set
install.packages("caTools")
library(caTools)
set.seed(123) 
#The major advantage of setting a seed is that you 
#can get the same sequence of random numbers whenever you 
#supply the same seed in the random number generator

# Read the data and split to training and testing sets 
dataset=read.csv("Position_Salaries.csv")
split=sample.split(dataset$Profit,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)


# Another way 
ind=sample(2,nrow(iris),replace=T,prob=c(0.67,0.33))


# Training/testing

ind=sample(2,nrow(iris),replace=T,prob=c(0.67,0.33))
iris.training=iris[ind==1,1:4]
iris.test=iris[ind==2,1:4]
iris.trainLabels <- iris[ind==1,5]
iris.testLabels <- iris[ind==2, 5]
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
irisTestLabels <- data.frame(iris.testLabels)
merge <- data.frame(iris_pred, iris.testLabels)
names(merge) <- c("Predicted Species", "Observed Species")
merge

## Normalization

z=utilities[,-c(1,1)] ## dropping the first column in a datasets
m=apply(z,2,mean) ## computing the mean of the data, when inserting "2" means computing by columns
s=apply(z,2,sd) ## same calculation for standard deviation
z=scale(z,m,s)

## Calculating Euclidian distance

distance=dist(z)
print(distance,digits=3)

## Cluster Dendogram with Complete Linkage

hc.c=hclust(distance)
plot(hc.c)
plot(hc.c,labels=utilities$Company)
plot(hc.c,hand=-1)


## K means clustering in R - Udemy

# Discovering groups 

## step 1: choose the number K of clusters , lets say k=2

## Then assign 2 random centroids on the scatterplot, and divide the data into 2, 
## Draw a line in between the two imaginary points, and a line exactly vertical and in the middle to that line
## Once data is divided, find the real center of each cluster 
## Reassign the centroids 


## K means ++ algorithm to avoid the random initialization trap 

## Choosing the right number of clusters : 
## The WCSS will decrease from a substantial value down to zero as we increase the number of clusters 
## Use the elbow method, wherever the decrease stops being substantial we stop 



## Using the Elbow Method to find the optimal number of clusters 
  dataset=read.csv("Mall_Customers.csv")
  x=dataset[4:5]
  set.seed(6)
  wcss=vector()
  for(i in 1:10) wcss[i]=sum(kmeans(x,i)$withinss)
  plot(1:10,wcss,type="b",main=paste('Clusters of clients'),xlab="number of clusters",ylab="wcss")
  
       
## Applying k-means to the mall dataset
set.seed(6) # random number
kmeans=kmeans(x,5,iter.max=300,nstart=10)


# Visualizing 

library(cluster)
clusplot(x,
         kmeans$cluster,
         lines=0,
         shade=T,
         color=T,
         labels=2,
         plotchar=F,
         span=T,
         main=paste('Clusters of clients'),
         xlab="Annual Income",
         ylab="Spending Score")



## REPLACING mISSING DATA: FACTUAL ANALYSIS

fin[is.na(fin$State),]
fin[is.na(fin$State) & fin$City=="New York",] ## wherever there is a missing state but the city is NY , we will automatically add the name 
fin[is.na(fin$State) & fin$City=="New York","State"]="NY"


## Handling Date-Times 
util$PosixTime=as.POSIXct((util$Timestamp,format="%d%m%Y %H:%M"))

## How to rearrange columns in a df 

util=util[,c(4,1,2,3)] ## move the forth column to be the first 



--------------------------------------------------------------------------------                
  
## Regression Trees 

install.packages("rpart")
library(rpart)
regressor=rpart(formula = salary~.,
                data=dataset,data(control=rpart.control(minsplit = =1))
                
## Decision Tree Regression 
                
position=read.csv("Position_Salaries.csv")
  position=position[2:3]
   regressor=rpart(formula=Salary~.,
             data=dataset,
             control=rpart.control(minsplit = 1))
                y_pred=predict(regressor,data.frame(Level=6.5))
                
                x_grid=seq(min(dataset$Level),max(dataset$Level),00.1)
                ggplot()+
                  geom_point(aes(x=dataset$Level,y=dataset$Salary),
                             color='red')
                geom_line(aes(x=x_grid,y=predict(regressor,newdata=data.frame(Level))),
                          color='blue')
                ggtitle('Truth or Bluff(Decision Tree Regression)')
                xlab('Level')
                ylab('Salary')                             
--------------------------------------------------------------------------------
                
## Build your own normalize function

nurmalize= function(x){
  num=x-min(x)
  denom=max(x)-min(x)
  return(num/denom)}

iris_norm=(iris[1:4],normalize)
--------------------------------------------------------------------------------

# Random Forest Regression

library(randomForest)
set.seed(1234)
regressor=randomForest(x = dataset[1],
                       y=dataset$Salary,
                       ntree=10)

## More steps of steps than what we had with one decision tree. Each straight horizontal line is one interval
## Calculating many different averages, adding too many trees the more the average is converting to the same average 
## A higher number of trees can only improve the location of each step 




## R^2 = 1-(SS(residual)/SS(total)) parameter of the model describing the goodness of fit
---------------------------------------------------------------------------------

## SVM
## How to separate boundries between segments? sometimes it is possible to draw a linear line
## The point is to find the line that separates the points with maximum but equal distance, 
# the line called "maximum margin" and the lines that are parallel to the closest points called support vectors
## Why popular?
## imagine distinguishing between an apple and an orange - so look at the most apply apples and orangy oranges - 
  # in our case, SVM looks at the apples that are very much like oranges, and oranges that look like apples.
## Looks at the very extreme case, the boundry, to construct the analysis, at times perform much better

dataset=read.csv("Social_Network_Ads.csv")
dataset=dataset[3:5]

# Split

set.seed(123)
n=nrow(dataset)
trainIndex=sample(1:n,size=round(0.75*n),replace=FALSE)
training_set=dataset[trainIndex,]
test_set=dataset[-trainIndex,]


split=sample.split(dataset$Purchased,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

# Feature scaling

training_set[-3]=scale(training_set[-3])
test_set[-3]=scale(test_set[-3])
attach(dataset)
attach(training_set)
library(e1071)

classifier=svm(formula=Purchased~., 
               data=training_set,
               type='C-classification',
               kernal='linear')

# Predict

y_pred=predict(classifier,newdata=test_set[-3])

# Confusion matrix - see how many predictions we got right 

cm=table(test_set[,3],y_pred)


# Visualizing training_sets

library(ElemStatLearn)
set=training_set
x1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
x2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(x1,x2)
colnames(grid_set)=c('Age','EstimatedSalary')
y_grid=predict(classifier,newdata=grid_set)

plot(set[,-3],
     main='SVM (Training set)',
     xlab='Age',ylab='Estimated Salary',
     xlim=range(x1),ylim=range(x2))
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add=TRUE)

points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen','red'))
points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3'))

-------------------------------------------------------------------------------

## Kernel SVM Intuition 
  
# Whenever data cannot be separated linearly, cannot even draw one single decision boundry
 
  
  classifier=svm(formula=Purchased~., 
                 data=training_set,
                 type='C-classification',
                 kernal='radial')

# Predict

y_pred=predict(classifier,newdata=test_set[-3])
y_pred
# Confusion matrix

cm=table(test_set[,3],y_pred)
cm

# Visualizing training_sets

library(ElemStatLearn)
set=training_set
x1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
x2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(x1,x2)
colnames(grid_set)=c('Age','EstimatedSalary')
y_grid=predict(classifier,newdata=grid_set)

plot(set[,-3],
     main='Kernel SVM (Training set)',
     xlab='Age',ylab='Estimated Salary',
     xlim=range(x1),ylim=range(x2))
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add=TRUE)

points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3'))

-----------------------------------------------------------------------------
  
## Decision Tress Intuition
  
## Line splits to separate the categories and minimize information entropy 

  
library(rpart)
test_set$Purchased=factor(test_set$Purchased)

classifier=rpart(formula=Purchased~.,
                 data=training_set)
y_pred=predict(classifier,newdata=test_set[-3],type='class')

library(ElemStatLearn)
set= training_set
x1=seq(min(set[,1])-1,max(set[,1])+1,by=0.01)
x2=seq(min(set[,2])-1,max(set[,2])+1,by=0.01)
grid_set=expand.grid(x1,x2)
colnames(grid_set)=c('Age','EstimatedSalary')
y_grid=predict(classifier,newdata =grid_set,type= 'class')

plot(set[,-3],
     main='Decision Tree (Training set)',
     xlab='Age',ylab='Estimated Salary',
     xlim=range(x1),ylim=range(x2))
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add=TRUE)
points(grid_set,pch='.',col=ifelse(y_grid==1,'springgreen','tomato'))
points(set,pch=21,bg=ifelse(set[,3]==1,'green4','red3'))

## Plotting the decision tree

plot(classifier)
text(classifier)

 

  
