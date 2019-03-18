
### Decision Trees in R ###

## Classification Trees
gc()
rm(list = ls(all = TRUE))

packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

packages(caret)
packages(caTools)
packages(tree)
packages(ISLR)
packages(rpart)
packages(rpart.plot)
packages(randomForest)
#library(caret,quietly = TRUE)
# check which are the data sets available in the ISLR package

#data(package="ISLR")
carseats<-Carseats

names(Carseats)

# checking the structure of the data
str(carseats)


hist(Carseats$Sales)

# Observe that Sales is a quantitative variable. You want to demonstrate it using trees with a binary response.
#To do so, you turn Sales into a binary variable, which will be called High.
# If the sales is less than 8, it will be not high. Otherwise, it will be high.
# Then you can put that new variable High back into the dataframe.

High = ifelse(carseats$Sales <= 8, "No", "Yes")

carseats = data.frame(carseats, High)

head(carseats)

#Checking the missing values in the data
nrow(carseats) - sum(complete.cases(carseats))


# Now let's fill a model using decision trees. 
# Of course, you can't have the Sales variable here because your response variable High was created from Sales.
# Thus, let's exclude it and fit the tree.


#tree.carseats = tree(High ~. -Sales, data = carseats)


#summary(tree.carseats)


#plot(tree.carseats)
#text(tree.carseats, pretty = 2)

#tree.carseats

set.seed(1234) 

sample = sample.split(carseats$High, SplitRatio = 3/4)

train = subset(carseats, sample == TRUE)
test  = subset(carseats, sample == FALSE)

High.test = test$High
head(High.test)
# Simple algorithm to gauge variable importance
# In order to have an undertanding for the importance of the 21 different features in identifying the right category (edible or poisonous) we are going to:
#   

table(carseats$High, carseats$ShelveLoc)

#penalty.matrix = matrix(c(0,1,10,0), byrow = T, nrow = 2)

Dec.tree = rpart(High ~. -Sales, data = train,
             method = "class")

summary(Dec.tree)

plot(Dec.tree)
text(Dec.tree, pretty = 1)

# Visualizing the tree using prp() function
prp(Dec.tree)

# Lets predict the model on the test data set

pred = predict(object = Dec.tree, newdata = test, type = 'class')

t = table(High.test, pred)

confusionMatrix(pred, High.test)

#-------- Using tree() function
# This is not a good option
set.seed(1234)
train.1 = sample(2:nrow(carseats), replace = T, prob = 0.8)

  fit = tree(High ~.-Sales, carseats, subset = train.1)

summary(fit)

plot(fit)
text(fit, pretty = 1)

pred1 = predict(fit, newdata = test,type = 'class')
confusionMatrix(pred1, High.test)




#----- Optimizing the decision tree to improve the accuracy-----

# Pruning the tree using Cross Validation

cv.fit = cv.tree(object = Dec.tree, FUN = prune.misclass)

summary(cv.fit)

# as per the summary the K value is 8, lets consider the same for fitting the model

prune.fit = prune.misclass(fit, best = 8)

summary(prune.fit)

pred.prune = predict(prune.fit, newdata = test, type = 'class')

t2 = table(pred.prune, High.test)

confusionMatrix(pred.prune, High.test)

prp(prune.fit)



# splitting sample using some other function
#set.seed(3)
d = sample(2,nrow(carseats), prob = c(0.7, 0.30), replace = T)

TrainData = carseats[d==1,]
TestData = carseats[d==2,]

dim(TrainData)
dim(TestData)

m1 = rpart(High ~. -Sales, data = TrainData,
                 method = "class")
summary(m1)
prp(m1)



data(package = 'MASS')




#----------------------- Using CTG Data-------------#
# Source - https://www.youtube.com/watch?v=dJclNIN-TPo

data = read.csv("C:/Users/Abdul_Yunus/Desktop/Yunus_Personal/Learning/Decision Tree/CTG.csv", header = T)
str(data)

# NSP is the target variable and it is in the numeric form, lets convert it into the factor

data$NSP = as.factor(data$NSP)

# Check how many records (Data points) we have for each category
table(data$NSP)


# Data Partitioning - Splitting the data into train and test
set.seed(123)

split = sample.split(data$NSP, SplitRatio = 0.7)

Train_data = subset(data, split == "TRUE")
Test_data = subset(data, split == "FALSE" )
#id = sample(2,nrow(data),replace = T, prob = c(0.7,0.3))
#Train_data = data[id ==1,]
#Test_data = data[id ==2,]

nsp.test = Test_data$NSP

# Lets build a tree first

m1 = rpart(NSP ~. , data = Train_data, method = "class")

summary(m1)

plot(m1)
text(m1, pretty = 1)

# Visulaize with prp function
prp(m1)

# Predicting the results on test data sets

m1.pred = predict(m1,newdata = Test_data, type = 'class' )


# Lets prepare the confusion matrix to check the accuracy of the model prediction
confusionMatrix(m1.pred, nsp.test)


# Lets build the Random forest to check how we can improve the model
# We will use the library 'randomForest' for this

rf = randomForest(NSP ~., data = Train_data)

print(rf)
