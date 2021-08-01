#' My hello World Function
#'
#' @param x The name of person to say hi to
#'
#' @return The output from \code{\link{print}}
#' @export
#'
#' @examples
#' hello("Nishant")
#' \dontrun{
#' hello("Steve")
#' }
hello <- function(x)
  {
  print(paste("Hello ",x, " world!"))
  }

#' @title Best K value
#' @description Generates a plot implementing elbow method to choose the best value of K for K means Clustering
#' @param x
#' @return
#' @export
#' @
#' @examples
#' kval(x)
kval=function(x){
  k.max=10
  wss=rep(NA,k.max)
  nClust=list()
  for(i in 1:k.max)
  {
    classes=kmeans(x,i)
    wss[i]=classes$tot.withinss
    nClust[[i]]=classes$size
  }
  plot(1:k.max,wss,type='b',pch=19,xlab="Number of clusters k",ylab="total within sum of squares(wss)")
}

#' @title K means Clustering
#' @description Performs K means clustering algorithm and also develops stepwise plots of how clusters are being formed
#'
#' @param x The Variable
#'
#' @param k Number of Clusters
#'
#' @return
#' @import animation
#' @export
#'
#' @examples
#' kmeansClust(x,k)
kmeansClust=function(x,k)
{
  Cluster=kmeans(x,k)
  print(str(Cluster))
  kmeans.ani(x,k)
}


#' @title K nearest Neighbours
#' @description Performs knn classification on a target variable and a given dataset and predicts the accuracy by building a confusion matrix
#' The training and testing data is splitted in the ration of 80:20 respectively
#'
#' @param t Target Variable
#'
#' @param d Dataset
#'
#' @note Pass the target value with '$' .
#' Ex- dataset$target(d$t)
#'
#' @return
#' @import class
#' @export
#'
#' @examples
#' knearestNeighbour(t,d)
knearestNeighbour=function(t,d)
{
  kdata=d
  normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
  normalized_data=as.data.frame(lapply(kdata, normalize))
  temp=sample(1:nrow(normalized_data),size=nrow(normalized_data)*0.8,
              replace=FALSE)
  training=kdata[temp,]
  testing=kdata[-temp,]
  knn=knn(training,testing,training$t,k=sqrt(NROW(training)))
  print(knn)
  print(table(knn,testing$t))

}

#' @title Hierarchical Clustering
#' @description Performs Hierarchical Clustering algorithm on target variable and given dataset
#' Also constructs a dendogram plot depicting the merging of clusters
#'
#' @param t The Target Variable
#'
#' @param d The Dataset
#'
#' @note Pass the target value with '$' .
#' Ex- dataset$target(d$t)
#'
#' @return
#' @export
#'
#' @examples
#' hierarchicalClust(t,d)
hierarchicalClust=function(t,d)
{
  normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
  z=as.data.frame(lapply(d, normalize))
  #Ecd distance
  dist=dist(z)
  #print(dist,digits = 3)

  hc=hclust(dist)
  plot(hc,labels = t)
}


#' @title Decison Tree
#' @description Generates a Decision tree plot with two branches at each value to predict the output on the basis of given dataset
#'
#' @param t Target Variable
#'
#' @param d Dataset
#'
#' @note Pass the target value with '$' .
#' Ex- dataset$target(d$t)
#'
#' @return
#' @import rpart.plot rpart
#' @export
#'
#' @examples
#' decisionTree(t,d)
decisionTree=function(t,d)
{
  data=d
  tree=rpart(t~.,data)
  rpart.plot(tree)
}

#' @title Random Forest
#' @description Performs Random Forest Classification algorithm for the target variable on the basis of given dataset
#' Splits the given dataset into training and testing in the ratio of 80:20 respectively
#' Also generates a confusion matrix toe predict the error of the model
#'
#' @param t Target Variable
#'
#' @param d Dataset
#'
#' @note Pass the target value with '$' .
#' Ex- dataset$target(d$t)
#'
#' @return
#' @import randomForest
#' @export
#'
#' @examples
#' randomForest(t,d)
randForest=function(t,d)
{

  mydata=d
  t=as.factor(t)
  index=sample(1:nrow(mydata),size=nrow(mydata)*0.8,replace=FALSE)
  training=mydata[index,]
  a=data.frame(training)
  testing=mydata[-index,]
  b=data.frame(testing)
  tv=training$t
  rf=randomForest(tv~.,data=a,mtry=4,ntree=2001,importance=TRUE)
  print(rf)
}

#' @title Linear Regression Algorithm
#' @description Performs Linear on the two input variables and
#' plots the line of best fit between them
#'
#' @param y
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' linearRegression(y,x)
linearRegression=function(y,x){
  LR=lm(y~x)
  print(LR)
  plot(y~x)
  abline(LR)
}

#' @title Multiple Regression Algortihm
#' @description Performs the Multiple linear Regression between the input y value and all other columns variables in the given dataset
#'
#' @param t Target Variable
#'
#' @param d Dataset
#'
#' @note Pass the target value with '$' .
#' Ex- dataset$target(d$t)
#'
#' @return
#' @import car
#' @export
#'
#' @examples
#' multipleRegression(y,d)
multipleRegression=function(t,d)
{
  MR=lm(t~.,data=d)
  print(MR)
  avPlots(MR,main="Plots")
}

#' @title Logistic Regression algorithm for classification
#' @description Predicts the probability of the target variable on the basis of input dataset by performing Logistic Regression Algorithm and
#' also Plots a curve between 0 and 1 in the y axis
#'
#' @param t Target Variable
#'
#'
#'
#' @param d Dataset
#'
#'  @note Pass the target value with '$' .
#' Ex- dataset$target(d$t)
#'
#' @return
#' @export
#'
#' @examples
#' logisticRegression(t,d)
logisticRegression=function(t,d){
  index=sample(1:nrow(d),size=nrow(d)*0.8,replace=FALSE)
  lrtrain=d[index,]
  lrtest=d[-index,]
  tv=lrtrain$t
  lrmodel=glm(tv~.,data=lrtrain,family = 'binomial')
  print(summary(lrmodel))
  res=predict(lrmodel,lrtest,type='response')
  print(res)
  lrconf=table(Actual_value=sd,predicted_val=res >0.5)
  print(lrconf)
  plot(sd~.,data=lrtrain)
  curve(predict(lrmodel,data.frame(tex=x),type = "resp"),add=TRUE)
}

#' @title Central Tendency
#' @description Calculates the central tendency of a given variable i.e., Mean and median
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' centralTend(x)
centralTend=function(x)
{

  print("Mean of x is : ",mean(x))
  print("Median of x is : ",mean(x))
}

#' @title Variability
#' @description Calculates the variability for a given
#' variable i.e., Minimum , Maximum , Range , Standard Deviation
#' Also develops a Barplot to visualize the variability of the variable
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' variability(x)
variability=function(x)
{
  print(paste0("Minimum value of x is : ",min(x)))

  print(paste0("Maximum value of x is : ",max(x)))

  barplot(x)

  print(paste0("Range of x is : ",range(x)))

  plot(x,type="o")

  print(paste0("Standard deviation of x is : ",sd(x)))
}

#' @title Quartiles
#' @description Calculates the percentage quartile density of the variable and
#' Interquartile range of the given Variable
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' quartiles(x)
quartiles=function(x)
{
  print("Quartile density of x is : ")
  print(quantile(x))
  print(paste0("Interquartile range x is : ",IQR(x)))
}




