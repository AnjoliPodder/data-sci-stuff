define Mean Absolute Error
mae <- function(one, other) {
  return(mean(abs(one - other)))
} # Make a note about R scoping!

# Check a simple case:
mae(5, 2)
# This is a simple form of testing:
stopifnot(mae(5, 2)==3)

alltrain <- read.csv("train.csv")
#Remove redundant column X
alltrain$X=NULL
alltrain$SalaryRaw=NULL


#Set up the train and test folds
set.seed(42)
alltrain$fold <- sample(1:10, nrow(alltrain), replace=TRUE)

train <- subset(alltrain, fold != 3)
test  <- subset(alltrain, fold == 3)

#Plot Everything
hist(alltrain$SalaryNormalized)
#this is not a normal distribution, so use log
hist(log(alltrain$SalaryNormalized))
#much more normal now!

plot(alltrain$ContractType, log(alltrain$SalaryNormalized))
plot(alltrain$Category, log(alltrain$SalaryNormalized))
plot(alltrain$SourceName, log(alltrain$SalaryNormalized))
plot(alltrain$Company, log(alltrain$SalaryNormalized))

#From manually looking at the data, have decided not to use the following for a linear model:
#  Company + LocationNormalized + FullDescription + Title as there are too many variables without obvious groupings
#  LocationRaw as this has been normalised in LocationNormalized

#first model, using the remaining fields
model <- lm(log(SalaryNormalized) ~ LocationNormalized+ContractType+ContractTime+Category+SourceName, data=train)
summary(model)

#calculate MAE for the training set
mae(exp(fitted(model)), train$SalaryNormalized)

#calculate MAE for the test set
mae(exp(predict(model, test)), test$SalaryNormalized)
#This errors out as there are Locations in the test set that are not in the train set

#This is also potentially going to be an issue if some Locations are not in the final test set. Adding these on to our data frame
alltest <- read.csv("test.csv")
alltest$X=NULL
#Add dummy salary values for the test data (set to mean of train data)
alltest$SalaryNormalized=mean(alltrain$SalaryNormalized)
alltest$fold <- sample(1:10, nrow(alltest), replace=TRUE)
alldata<-rbind(alltrain,alltest)

#Use the Locations list to categorise the LocationNormalized values
locations <- read.csv("Location_Tree2.csv")
#remove the extraneous columns (including the smallest subdivision of Location)
locations$X=NULL
locations$X.1=NULL
locations$X.2=NULL
#Extract the unique rows (there are many dupes after removing the last column)
locations<-unique(locations)
write.csv(locations, file="locations3.csv")
#I did some manual data manipulation as follows:
# Where Location was blank, I copied over Area
# Where area was blank I copied over City
# Where city was blank, I copied over Country
# For example, for the entry where there was UK in the Country column and blank elsewhere, I put "UK" into all columns
locations2 <- read.csv("locations3.csv")
locations2<-unique(locations2)
alldatawithloc <- merge(alldata,locations2,by.x = "LocationNormalized", by.y = "Location")

train <- subset(alldatawithloc, fold != 3)
test  <- subset(alldatawithloc, fold == 3)

#Will use the new Area bucket instead of LocationNormalized
#Make sure that all values of Area are in the train set

levels(alldatawithloc$Area)
#301 levels

levels(train$Area)
#301 levels

model2 <- lm(log(SalaryNormalized) ~ Area+ContractType+ContractTime+Category+SourceName, data=train)

#calculate MAE for the training set
mae(exp(fitted(model2)), train$SalaryNormalized)
#6989.408

#calculate MAE for the test set
mae(exp(predict(model2, test)), test$SalaryNormalized)
#6890.019

library(DAAG)

#find the cross validation error
crossval <- cv.lm(alldatawithloc, model2, m=10, plotit=FALSE, seed=78)
#killing my laptop...may come back to this later

library('glmnet')

model.glm <- glm(log(SalaryNormalized) ~ Area+ContractType+ContractTime+Category+SourceName, data=train, na.action=na.omit)
mae(exp(fitted(model.glm)), train$SalaryNormalized)
#6989.408

#calculate MAE for the test set
mae(exp(predict(model.glm, test)), test$SalaryNormalized)
#6890.019

library(DAAG)
#trying glmnet

#set up a matrix for the x value
glmnet.xval<-cbind(model.matrix(~train$ContractType),
                   model.matrix(~train$Area),
                   model.matrix(~train$ContractTime),
                   model.matrix(~train$Category),
                   model.matrix(~train$SourceName))

crossval <- cv.glmnet( glmnet.xval, as.matrix(log(train['SalaryNormalized'])), nfolds=10 )
lambda.min<-crossval$lambda.min
#0.001259727

model.glmnet <- glmnet(glmnet.xval, as.matrix(log(train['SalaryNormalized'])))
mae(exp(predict(model.glmnet,newx=glmnet.xval, s=lambda.min)), train$SalaryNormalized)
#6958.72

glmnet.xval.test<-cbind(model.matrix(~test$ContractType),
                   model.matrix(~test$Area),
                   model.matrix(~test$ContractTime),
                   model.matrix(~test$Category),
                   model.matrix(~test$SourceName))
mae(exp(predict(model.glmnet,newx=glmnet.xval.test, s=lambda.min)), test$SalaryNormalized)
#6806.378

# Train our final model with all the training data
glmnet.xval.final<-cbind(model.matrix(~alldatawithloc$ContractType),
                        model.matrix(~alldatawithloc$Area),
                        model.matrix(~alldatawithloc$ContractTime),
                        model.matrix(~alldatawithloc$Category),
                        model.matrix(~alldatawithloc$SourceName))
finalmodel <- glmnet(glmnet.xval.final, as.matrix(log(alldatawithloc['SalaryNormalized'])))

#MAE for final model
mae(exp(predict(model.glmnet,newx=glmnet.xval.final, s=lambda.min)), alldatawithloc$SalaryNormalized)
#6942.735

#predict salaries for test set
realtest 
#make a matrix out of the test variables
realtest <- read.csv("test.csv")
realtest$X=NULL
realtestwithloc<-merge(alltest,locations2,by.x = "LocationNormalized", by.y = "Location")

xval.finalmodel<-cbind(model.matrix(~realtestwithloc$ContractType),
                         model.matrix(~realtestwithloc$Area),
                         model.matrix(~realtestwithloc$ContractTime),
                         model.matrix(~realtestwithloc$Category),
                         model.matrix(~realtestwithloc$SourceName))
predict(finalmodel,newx=xval.finalmodel)
#I got an error doing this...not sure what to do here, so submitting with a previous model

predict(model.glm, test)
predictions<-exp(predict(model.glm,realtestwithloc))
submission <- data.frame(Id=realtestwithloc$Id,
                         Salary=predictions)
#something wonky has gone on with my merge so I am missing some IDs and have some dupes (I caught this too late to figure it out and fix it)
#I wasn't quite sure how to include the Locations from the test set without actuallyt training the model with the dummy data, so I suspect it has skewed my results

