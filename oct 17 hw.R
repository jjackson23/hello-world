#download dataset 
data(Colleged)

#split the data (did not work)
Wage$id <- 1:nrow(Wage)
train <- Wage %>% dplyr::sample_n(.75)
test <- dplyr::anti_join(Wage, train, by = 'id')


#Run a regression 
#all variables on y on the full dataset 
fullreg <- lm(Apps~ Private+ Accept+ Enroll + Top10perc+ Top25perc + F.Undergrad+ P.Undergrad+ Outstate+ Room.Board+ Books+ Personal + PhD + Terminal+ S.F.Ratio+ perc.alumni + Expend+ Grad.Rate,data = College)
#call
fullreg
summary(fullreg)


#split the data (another way)

#75% of sample size
smp_size <- floor(.75*nrow(College))
##Set teh seed to make your partition reproductible 
set.seed(123)
train_ind <- sample(seq_len(nrow(College)), size = smp_size)

college_train <- College[train_ind, ]
college_test <- College[-train_ind, ]

# Run a regression of the training dataset 
#Full regression 
fullreg_train <- lm(Apps~ Private+ Accept+ Enroll + Top10perc+ Top25perc + F.Undergrad+ P.Undergrad+ Outstate+ Room.Board+ Books+ Personal + PhD + Terminal+ S.F.Ratio+ perc.alumni + Expend+ Grad.Rate,data = college_train)
summary(fullreg_train)

s_train <- lm(Apps~ Private+ Accept+ Enroll + Top10perc+ Top25perc + F.Undergrad+ Outstate+ Room.Board+ PhD + S.F.Ratio+ perc.alumni + Expend+ Grad.Rate,data = college_train)
summary(s_train)

s_train1 <- lm(Apps~ Private+ Accept+ Enroll + Top10perc+ Top25perc + F.Undergrad+ Outstate+ Room.Board+ PhD + S.F.Ratio+ Expend+ Grad.Rate,data = college_train)
summary(s_train1)

s_test <- lm(Apps~ Private+ Accept+ Enroll + Top10perc+ Top25perc + F.Undergrad+ Outstate+ Room.Board+ PhD + S.F.Ratio+ Expend+ Grad.Rate,data = college_test)
summary(s_test)

#Ridge Regression
library(glmnet)

#Load data
data(College)
College$Private1 <- College$Private
x <- as.matrix(College[,3:18])
y <- as.matrix(College[,2])

#Fit model
fit <- glmnet(x, y, family= "gaussian", alpha = 0)
#summarize the fit
summary(fit)

predictions <- (predict(fit, x)
rsme <- mean((y - predictions)^ 2)
print(rsme)