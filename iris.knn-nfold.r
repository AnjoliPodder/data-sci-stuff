#----------------------------------------------------------------#
#load the library that includes the knn function
library(class)
    
#define function that takes the following arguments
    #n=number of folds
    #k=number of nearest neighbours
    #data = data set
    #labels=label that match the data set
knn.nfold <- function(n, k, data, labels) {
  #randomise data set
  num.rows<-nrow(data)
  set.seed(1) 
  
  #scramble the indices. Will use this instead of the base data set to perform the splits (in case the base data is ordered)
  rand.index <- sample(1:num.rows, num.rows) 
  
  #calculate the number of rows for the test set
  test.rows=floor(num.rows/n)
  
  #initialise error sum variable (used in loop)
  error.sum <- 0
  
  #iterate for each fold
  for (x in 0:n-1) {
    #split the nth partition and create an index
    test.start<-x*test.rows+1
    test.end<-test.start+test.rows-1     
    test.index <- rand.index[test.start:test.end]
    
    test.data <- data[test.index, ]       # perform train/test split
    train.data <- data[-test.index, ]

    test.labels <- labels[test.index] 
    train.labels <- labels[-test.index]  
    
    #run the knn analysis on the current split
    knn.fit <- knn(train = train.data,          # training set
                   test = test.data,           # test set
                   cl = train.labels,          # true labels
                   k = k                       # number of NN to poll
    )
    
    #add the errors in each iteration
    this.err <- sum(test.labels != knn.fit) / length(test.labels) 
    error.sum <- error.sum+this.err
  }
  
  #calculate the average error for n fold cross classification and return it
  error.avg <- error.sum/n
  return(error.avg)
}

# a function to find the value of k with the lowest error rate (as calculated via knn with n-fold classification)
# this function takes the following arguments:
#  n = number of folds
#  max.k is the maximum value of k you want to check for. We will check values 1 to k
#  data = data set
#  labels=label that match the data set
find.optimum.k<- function(n, max.k, data, labels) {
  avg_list <- c();
  for (x in 1:max.k) {
    gen.err.avg = knn.nfold(n,x,data=data,labels=labels)
    avg_list<-rbind(avg_list, gen.err.avg)
  }
# find the index of the smallest average generalized error - this gives us the optimum k
  min = min(avg_list)
  lowest = match(min, avg_list)
  return(lowest)
}

#make some test data to use with the function
data <- iris                # create copy of iris dataframe
labels <- data$Species      # store labels
data$Species <- NULL

#find the optimum value of k that's lower than 14, with 10 fold cross validation on the iris data set
optimum_k <- find.optimum.k(10,14,data=data,labels=labels)
optimum_k