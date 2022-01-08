#########################################################################
#PART 1 ----
#########################################################################
library(tidyverse)
library(caret)

#Read the MLDATASET_PartiallyCleaned.csv and store output in variable called dat.
dat <- read.csv("MLDATASET_PartiallyCleaned.csv",
                na.strings="", stringsAsFactors=TRUE)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Replacing outlier in How Many Times File Seen NA values----
#Dhecking outliers with bloxplot
boxplot(dat$How.Many.Times.File.Seen)
#checking outliers with outliers package.
#library(outliers)
#Grubbs's test
outliers.check.1 <- grubbs.test(dat$How.Many.Times.File.Seen)
outliers.check.1
#Rosner’s test
#library(EnvStats)
outliers.check.2 <- rosnerTest(dat$How.Many.Times.File.Seen, 
                               #number of suspected outliers
                               #Default this is 3
                               k=1)
outliers.check.2
#Replacing the outlier with a NA values
mydata <- dat %>%
  mutate(How.Many.Times.File.Seen=ifelse(How.Many.Times.File.Seen==65535,NA,How.Many.Times.File.Seen))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Covert Threads Started column into factor.----
#Replacing all values greater than 5 with 5.
mydata.1 <- mydata %>%
  mutate(Threads.Started=ifelse(Threads.Started>5,5,Threads.Started))
#checking all the unique values.
unique(mydata.1$Threads.Started)
#labeling values in the factor.
mydata.1$Threads.Started <- factor(mydata.1$Threads.Started,labels=c(1,2,3,4,5))
#Or either this way can be used in order to level the data in the factor.
##levels(mydata.1$Thread.Started) <- c(1,2,3,4,5)
#checking the levels of the Threads Started factor.
levels(mydata.1$Threads.Started)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#log-transform characters In URL using log function.
mydata.2 <- mydata.1  %>%
  mutate(Characters.in.URL=log(Characters.in.URL))
#create histogram to check the original distribution of Characters in URL
hist(mydata.1$Characters.in.URL, col='coral2', main='Original')
#Create a histogram to check the log transformed distribution
hist(mydata.2$Characters.in.URL, col='coral2', main='Log-transformed')

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#check if any na values in the data set.
any(is.na(mydata.2))
#Counting all the na values in the given data set
sum(is.na(mydata.2))
#select only complete cases by removing NA values using na.omit() function.
MLDATASET.cleaned <- mydata.2 %>%
  na.omit(num.mydata)
#check if any na values in the MLDATASET.cleaned data set.
any(is.na(MLDATASET.cleaned))
#View the Clean data set
view(MLDATASET.cleaned)
#Write that data set into a csv file
write.csv(MLDATASET.cleaned, "MLDATASET.cleaned.csv")

#Set a seed that way createDataPartition can randomly generate a data set
set.seed(20313)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
TrainRowNum <- createDataPartition(MLDATASET.cleaned$Actually.Malicious, 
                                   #Split data in to 30/70 training and testing data sets
                                   p=0.3, 
                                   #Not to store the result in a list
                                   list=FALSE);
#Get the training data set
TrainData <- MLDATASET.cleaned[TrainRowNum,]
write.csv(TrainData, "Train.csv")
#Get the testing data set
TestData <- MLDATASET.cleaned[-TrainRowNum,]
write.csv(TrainData, "Test.csv")
#check the percentage of the Training data set from full data set.
nrow(TrainData) * 100 / nrow(MLDATASET.cleaned)

#checking the balance of the dataset
table(MLDATASET.cleaned$Actually.Malicious)
table(MLDATASET.cleaned$Actually.Malicious) %>% prop.table() * 100

#########################################################################
#PART2----
#########################################################################

library(tidyverse)
library(glmnet)
library(caret)

set.seed(20313)
models.list1 <- c("Logistic Ridge Regression",
                  "Logistic LASSO Regression",
                  "Logistic Elastic-Net Regression")
models.list2 <- c("Classification Tree",
                  "Bagging Tree",
                  "Random Forest")
myModels <- c("Binary Logistic Regression",
              sample(models.list1,size=1),
              sample(models.list2,size=1))
myModels %>% data.frame 
#The models that was selected were:
#1      Binary Logistic Regression
#2 Logistic Elastic-Net Regression
#3                   Random Forest

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Binary Logistic Regression----
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Please use following command to get TrainData, TestData data set if variable is not on you memory
##dat <- read.csv("Train.csv",
##na.strings="", stringsAsFactors=TRUE)
##dat <- read.csv("Test.csv",
##na.strings="", stringsAsFactors=TRUE)

#check the structure of the TrainData set basically this make sure that Factors are there as it was
#The reason behind this code is to recheck that all the Factors and other features has been classified as the should be.
str(TrainData)

#Remove Sample.ID and Initial.Statistical.Analysis from the TrainData data frame
#These featurs do not contributes to ML algorithms. Keeping them may mislead the algorithms.
TrainData <- TrainData[ ! names(TrainData) %in% c("Sample.ID", "Initial.Statistical.Analysis")]

#Perform the logistic regression modeling
TOBORROM.lg.md <- glm(Actually.Malicious~., family="binomial", #telling th algorith that this is Logistic ML
                      data=TrainData);
#Get eh summary of mod.
summary(TOBORROM.lg.md)
plot(TOBORROM.lg.md)

#calculating the exponentials.
exp(1.17);exp(0.0031)

#Test the model against TestData. Basically this is the probability prediction.
pred.prob <- predict(TOBORROM.lg.md, new=TestData, type="response")

#calculating R^2 value for the model.
ll.null <- TOBORROM.lg.md$null.deviance/-2
ll.proposed <- TOBORROM.lg.md$deviance/-2
(ll.null - ll.proposed) / ll.null

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#ROC----
library(pROC)
#Checking the best fitting values
TOBORROM.lg.md$fitted.values
#create a plot of best fitting values and Actually.Malicious 
lines(TrainData$Actually.Malicious, TOBORROM.lg.md$fitted.values)

#To remove padding in the both side of the plot
par(pty = "s")
#Create ROC plot
roc.info <- roc(TrainData$Actually.Malicious, TOBORROM.lg.md$fitted.values, 
                plot=TRUE, #enabling plot options
                print.thres="best", #printing  the best threshold on the plot
                percent=TRUE, #enabling percentage 
                print.auc=TRUE, #This print AUC in the plot
                legacy.axes=TRUE, col="#ff98c2", lwd=4)
par(pty="m")

#creating data frame specificity and senstivity in it.
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100,
  fpp=(1 - roc.info$specificities)*100,
  thresholds=roc.info$thresholds
)

#getting the best threshold between 60 to 80.
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

#Test the model against TestData. Basically this is the probability prediction.
pred.prob <- predict(TOBORROM.lg.md, new=TestData, type="response")

#Classifies the threshold, This time 0.5 has been taken as the threshold.
pred_class <- ifelse(pred.prob>0.5,"YES","NO")

#Creating the plot for the BLR model
predicted.data <- data.frame(probability.of.Malicious=TOBORROM.lg.md$fitted.values,
                             Malicious=TrainData$Actually.Malicious)

#Sort the data frame from low probabilities to high probabilities
predicted.data <- predicted.data[
  order(predicted.data$probability.of.Malicious, decreasing=FALSE),]
#adding the column to the data frame that has rank of each sample
predicted.data$rank <- 1:nrow(predicted.data)

#creating the plot using ggplot2 and cowplot.
#NOTE:if cowplot isn't installed. use following code
#install.packages(c("ggplot2","cowplot"))
library(ggplot2)
library(cowplot)
ggplot(data=predicted.data, aes(x=probability.of.Malicious, y=rank)) +
  geom_point(aes(color=Malicious), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")

#Creating the Confusion Matrix.
#Change the reference level NO from YES. Therefore, Malicious files will be the reference level.
Confusion_Metrix_LG <- table(pred_class %>% as.factor %>% relevel(ref="YES"),
                             TestData$Actually.Malicious %>% as.factor %>% relevel(ref="YES"));

prop <- prop.table(Confusion_Metrix_LG,2); prop %>% round(digit=3) #Proportions by columns

library(e1071)
#Summary of confusion matrix
confusionMatrix(Confusion_Metrix_LG)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Elastic-Net Logistic Regression----
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Defining hyper parameters. 
#Defing lambda values
lambdas <- 10^seq(-3,3,length=100) #A sequence 100 lambda values
#Defing alpha values
alphas <- seq(0.1,0.9,by=0.1)
alphas  #A sequence of alpha values to test

#Training elastic-net algorithm. 
mod.TOBORROM.elnet <- train(Actually.Malicious ~., #Formula
                            data = TrainData, #Training data
                            method = "glmnet",  #Penalized regression modeling
                            #Set preProcess to c("center", "scale") to standards data
                            preProcess = NULL,
                            #Perform 10-fold CV, 5 times over.
                            trControl = trainControl("repeatedcv",
                                                     number = 10,
                                                     repeats = 3),
                            tuneGrid = expand.grid(alpha = alphas,
                                                   lambda = lambdas)
)

#Optimal lambda value
mod.TOBORROM.elnet$bestTune

plot(mod.TOBORROM.elnet)
#getting coefficients. 
coef(mod.TOBORROM.elnet$finalModel, mod.TOBORROM.elnet$bestTune$lambda)

#predicted probability of Actually Malicious on the test data
pred.class.elnet1 <- predict(mod.TOBORROM.elnet,new=TestData)

#Confusion matrix with re-ordering of "Yes" and "No" responses
cf.elnet1 <- table(pred.class.elnet1 %>% as.factor %>% relevel(ref="YES"), 
                   TestData$Actually.Malicious %>% as.factor %>% relevel(ref="YES"));  

prop <- prop.table(cf.elnet1,2); prop %>% round(digit=3) #Proportions by columns

#Summary of confusion matrix
confusionMatrix(cf.elnet1)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Random Forest----
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#install.packages("ranger")
library(ranger)

#=========================================================================================================
#use minimum number of tuned parameters to run Random Forest.
Random.Forest.simple.model <- ranger(Actually.Malicious~.,
                                     data = TrainData,
                                     num.trees = 500, #Default is 500
                                     respect.unordered.factors = TRUE,
                                     min.node.size=10,
                                     mtry = 4,
                                     replace = TRUE,
                                     sample.fraction = 0.7,
                                     seed = 1,
                                     importance="impurity"); Random.Forest.simple.model

#Predicted ozone values on the test set.R
rf.simple.pred <- predict(Random.Forest.simple.model,data=TestData)$predictions
rf.simple.cm <- confusionMatrix(rf.simple.pred %>% relevel(ref="YES"),
                                TestData$Actually.Malicious %>% relevel(ref="YES")); rf.simple.cm
#Get the confusion matrix for the random forest
Random.Forest.simple.model$confusion.matrix
#check the prediction error for the Random Forest
Random.Forest.simple.model$prediction.error
#Create the confusion matrix.
Random.Forest.simple.cm <- confusionMatrix(rf.simple.pred %>% relevel(ref="YES"),
                                           TestData$Actually.Malicious %>% relevel(ref="YES"));Random.Forest.simple.cm
#========================================================================================================

#Defining hyperparameters in a dataframe. 
Random.Forest.grid <- expand.grid(num.trees = c(300,400,500), #Number of trees
                                  mtry = c(2,3,4), #Split rule
                                  min.node.size = seq(2,10,2), #Tree complexity
                                  replace = c(TRUE, FALSE), #Sampling with or without replacement
                                  sample.fraction = c(0.5,0.7,1), #Sampling fraction
                                  OOB.Err = NA, #Column to store the OOB RMSE
                                  test.sen = NA, #Column to store the test Sensitivity
                                  test.spe = NA, #Column to store the test Specificity
                                  test.acc = NA) #Column to store the test Accuracy

#Check the dimension
dim(Random.Forest.grid) 
#View the search grid
View(Random.Forest.grid) 


#Train the Random Forest model by tuning its hyperparameters
#WARNING: THIS COULD TAKE UP TO 1 HOUR DEPANDING YOUR COMPUTER RESOURCES.
library(ggplot2)
library(cowplot)
ggplot(data=Random.Forest.grid, aes(x=test.acc, y=OOB.Err)) +
  geom_line(aes(color=replace))

library(ranger)

#Sort the results by the OOB misclassification error and view the top 5
for (I in 1:nrow(Random.Forest.grid))
{
  RandomForest.Tuned.para <- ranger(Actually.Malicious~.,
                                    data=TrainData, #specify the data set
                                    num.trees=Random.Forest.grid$num.trees[I], #specify the num.trees
                                    mtry=Random.Forest.grid$mtry[I], #specify the split rule
                                    min.node.size=Random.Forest.grid$min.node.size[I], #minimum number of nodes
                                    replace=Random.Forest.grid$replace[I],
                                    sample.fraction=Random.Forest.grid$sample.fraction[I],
                                    seed=1,
                                    respect.unordered.factors="order")
  Random.Forest.grid$OOB.Err[I] <- RandomForest.Tuned.para$prediction.error %>% round(5)*100
  #Test classification
  para.tuned.pred <- predict(RandomForest.Tuned.para, data=TestData)$predictions; #Predicted classes
  #Summary of confusion matrix
  para.tuned.cm <- confusionMatrix(para.tuned.pred %>% relevel(ref="YES"),
                                   TestData$Actually.Malicious %>% relevel(ref="YES"));
  prop.cf <- para.tuned.cm$table %>% prop.table(2)
  Random.Forest.grid$test.sen[I] <- prop.cf[1,1] %>% round(5)*100 #Sensitivity
  Random.Forest.grid$test.spe[I] <- prop.cf[2,2] %>% round(5)*100 #Specificity
  Random.Forest.grid$test.acc[I] <- para.tuned.cm$overall[1] %>% round(5)*100 #Accuracy
}

#Write the grid dataframe
andom.Forest.grid <- write.csv( "RFOutput.csv")

#check the top 5 results of Random.Forest.grid
Random.Forest.grid[order(Random.Forest.grid$OOB.Err,decreasing=FALSE)[1:5],]

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Question e.)
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Random Forest vs Initial Analysis.
ISA <- c(85.17, 64.59, 74.91)
RF <- c(84.59, 90.45, 87.51)
Info <- c("specificity","sensitivity","accuracy")
ISA.vs.RF <- data.frame(ISA, RF, Info)
ISA.vs.RF$Info <- factor(ISA.vs.RF$Info,levels=c("specificity", "sensitivity","accuracy"))
#Creating the two plots
ISA.plot <- ggplot(ISA.vs.RF,aes(x=Info,y=ISA,fill=Info))+
  geom_bar(stat="identity",alpha=0.5)  
FR.plot <- ggplot(ISA.vs.RF,aes(x=Info,y=RF,fill=Info))+
  geom_bar(stat="identity",alpha=0.5) 

#NOTE: Please install gridExtra package to view two plots side by side
#install.packages("gridExtra")
library(gridExtra)
grid.arrange(ISA.plot, FR.plot)

#################################THE END###################################################################
