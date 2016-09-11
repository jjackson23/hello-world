
#setting a working directory and import datafiles
setwd("C:/Users/jedso/Downloads")
> train <- read.csv("C:/Users/jedso/Downloads/train.csv")
>   View(train)

test <- read.csv("C:/Users/jedso/Downloads/test.csv")
str(train)

#Access single column put $ in front
train$Survived
#Most basic summary statstic in R run through vector
#gives you simply the count 
table(train$Survived)
#proportion 

prop.table(table(train$Survived))

test$Survived <- rep(0,418)


submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)


summary(train$Sex)
prop.table(table(train$Sex, train$Survived))

prop.table(table(train$Sex, train$Survived),1)
prop.table(table(train$Sex, train$Survived),2)

#everyone dies adding all zeroes
test$Survived <- 0 
#alter column if sex is female with a one square brackets creates a
#subset of the total dataframe
test$Survived[test$Sex == 'female'] <- 1 

summary(train$Age)

train$Child <- 0 
train$Child[train$Age < 18] <- 1 

aggregate(Survived ~ Child + Sex, data = train, FUN = sum)

aggregate(Survived ~ Child + Sex, data = train, FUN = length)

aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {sum(x)/length(x)} )

train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20]<- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10]<- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Sex+ Pclass, data = train, FUN = function(x){sum(x)/length(x)})

test$Survived <-0
test$Survived[test$Sex == 'female']<- 1 
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare>= 20] <- 0 

library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = 'class')

plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
#predict the survival function now 

Prediction <- predict(fit, test, type = 'class')
submit<- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file ='myfirstdecisiontree.csv', row.names = FALSE)

train$Name[1]


test$Survived <- NA
combi <- rbind(train1,test)

#string are automatically imported as factors in R in convert them to 
#back into text string using as.character 
combi$Name <- as.character(combi$Name)
combi$Name[1]

#break apart a string 
strsplit(combi$Name[1], split='[,.]')

strsplit(combi$Name[1],split='[,.]')[[1]]

strsplit(combi$Name[1],split='[,.]')[[1]][2]
#apply this transformation to the whole list 

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
#strip off those spaces from beginning fo the titles
combi$Title <- sub('','',combi$Title)

table(combi$Title)
#combining mademoiselle and Madame 
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
#combing capt, don, major and sir 
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
#combing dona lady, countness 
combi$Title[combi$Title %in% c('Dona', 'Lady','the Countess', 'Jonkheer')] <- 'Lady'

combi$Title <- factor(combi$Title)

#create familysize variable 
combi$FamilySize <- combi$SibSp + combi$Parch + 1

combi$Surname <- sapply(combi$Name, FUN= function(x) {strsplit(x,split = '[,.]')[[1]][1]})
# paste brings two strings together 
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep ="")

combi$FamilyID[combi$FamilySize <= 2]<- 'Small'

table(combi$FamilyID)
#Store the table as a dataframe 
famIDs <- data.frame(table(combi$FamilyID))
#Find which families have 2 or less family members
famIDs <- famIDs[famIDs$Freq <= 2, ]
#overwrite familyIDs for group not correctly id and convert to factor
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)
#break apart the train dataset (, mean take all columns w/ subsets and stroe it ot assigned dataframe )
train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, 
             data = train, method = "class")

fancyRpartPlot(fit)

summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], 
                method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi$Age)
summary(combi)
summary(combi$Embarked)
#Find out who are the passengers that have 2 blank (gives us the indexes of the blank fields)
which(combi$Embarked == "")
#replace those two and encode them as factor
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)
#find out which one it is and replace it with a median fare
which(is.na(combi$Fare))

combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
#random forest can only take 32 so we need to reduce the number of levels 
#copy FamilyID into a new column FamilyID2 and convert them from factor to character w/(as.character)
#I increase a cut off to be small family from 2 to 3 ppl 
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

install.packages('randomForest')
library(randomForest)
#set the random seed b/c of two sources of randomness 
#if not my results will not by reproducible next time i load the code
#the number inside the set.seed just need it be the same to replicate results
set.seed(415)
#run the random forest 
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data = train,
                    importance = TRUE, 
                    ntree = 2000)

# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

