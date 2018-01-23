



## 1. Setting the working directory and clearing the R environment



rm(list=ls(all=T))




## 2. Loading the required libraries 



library(RColorBrewer)
library(rattle)
library(ipred)
library(ROSE)
library(ada)
library(rpart.plot)
library(rpart)
library(randomForest)
library(C50)
library(factoextra)
library(xgboost)
library(glmnet)
library(mice)
library(dplyr)
library(ROCR)
library(DMwR)
library(car)
library(MASS)
library(vegan)
library(dummies)
library(infotheo)
library(caTools)
library(caret)
library(e1071)
library(corrplot)
library(ggplot2)



## 3. Reading the data in R 



pay_tr = read.csv("train (1).csv", header= T, sep= ",")
pay_te = read.csv("test (1).csv", header= T, sep= ",")

View(pay_tr)
View(pay_te)



## 4. Basic Data Understanding

dim(pay_tr)
# 26415 x 34
dim(pay_te)
# 10875 x 33

pay_te$Pay_in_INR= 0

combi= rbind( pay_tr, pay_te)

sum(is.na(combi))
combi$School.Board.in.Tenth[combi$School.Board.in.Tenth=="0"]=NA
combi$Board.in.Twelth[combi$Board.in.Twelth=="0"]= NA
combi$Score.in.Domain[combi$Score.in.Domain==-1]= NA

sum(is.na(combi))
# 15581 missing values

dim(combi)
paste("Percentage of missing values in the data is" ,(15581/(37290*34))*100)

# ~1.22% of the data is missing 

colSums(is.na(combi))

str(pay_tr)
summary(pay_tr)

str(pay_te)
summary(pay_te)

colnames(pay_tr)
# Combining the data because the pre-processing done has to be common for both the 
# datasets 


## 5. Data Exploration, Variable Understanding and Data PreProcessing 
# I will also make the required changes in the data as required while exploring


str(combi)
nums <- sapply(combi, is.numeric)
nums_data= combi[,nums]
str(nums_data)
dim(nums_data)

# only using the first 1460 rows - training data
correlations <- cor(nums_data)
# only want the columns that show strong correlations with SalePrice
corr.SalePrice <- as.matrix(sort(correlations[,'Pay_in_INR'], decreasing = TRUE))

corr.idx <- names(which(apply(corr.SalePrice, 1, function(x) (x > 0.025 | x < -0.025))))

corrplot(as.matrix(correlations[corr.idx,corr.idx]), type = 'upper', method='color', addCoef.col = 'black', tl.cex = .7,cl.cex = .7, number.cex=.7)






dim(combi[,colSums(is.na(combi))>0])
# So, 8 columns have missing values. Let us try to understand the behaviour of every 
# variable 

# 1. Candidate.Id: A unique ID to identify a candidate

length(unique(combi$Candidate.ID))
# All unique entries here 
trainid= pay_tr$Candidate.ID
testid= pay_te$Candidate.ID
# Saving the IDs in a vector for easier submission in the end 
combi= combi[,-1]


# 2. Pay_in_INR: Annual CTC offered to the candidate (in INR)
# Numeric variable 

str(pay_tr)
summary(pay_tr)
# Pay ranges from 35000 to 4000000

ggplot(pay_tr,aes(x=Pay_in_INR))+
  geom_histogram(bins = 10)+
  labs(y="Count",
       title="Distribution of pay")


ggplot(pay_tr,aes(x=log(Pay_in_INR)))+
  geom_histogram(bins = 10)+
  labs(y="Count",
       title="Distribution of pay")

# A log transformation of the target variable seems to be helping in making the dis-
# tribution more normal 


# 3.Gender: Candidate's gender

table(combi$Gender)
# Very few females 
ggplot(combi,aes(y=Pay_in_INR,x=Gender))+
  
  geom_boxplot()+
  labs(y="Pay_in_INR",
       title="Pay vs Gender")   
# You see that the pay for males is on the higher side 


ggplot(combi,aes(fill=Gender,x=log(Pay_in_INR)))+
  
  geom_histogram(bins = 10)+
  labs(y="Pay count",
       title="Pay count by gender ")

# Women earn the median income 


# 4. Date Of Birth: Date of birth of the candidate
# Date Time class

sum(is.na(combi$Date.Of.Birth))
# 0 missing values 

# We will extract the age of the candidate using this column. The Age might be 
# more useful than the Date of Birth on its own


# 5. Score in Tenth: Performance in Tenth: range 0 to 100 

sum(is.na(combi$Score.in.Tenth))

ggplot(combi,aes(x=(Score.in.Tenth), fill= Pay_in_INR))+
  
  geom_histogram(bins = 20)+
  labs(y="Scor in tenth",
       title="10th Score ")
plot(combi$Score.in.Tenth, combi$Pay_in_INR)

cor(log(combi$Pay_in_INR+1), combi$Score.in.Tenth)

# You see that there is almost no correlation between the pay of a person 
# and his tenth score 

# 6. School Board in Tenth:School board in Tenth

sum(is.na(combi$School.Board.in.Tenth))
# 7283 missing values 
table(as.numeric((combi$School.Board.in.Tenth))>1)
# We see that there are many school boards. We will convert them to icse, cbse or state
# after NA imputation 
ggplot(combi,aes(x=School.Board.in.Tenth))+
  geom_bar()+
  labs(y="Count",
       title="School Board")



# 7. Year Of Twelth Completion: Numeric 
sum(is.na(combi$Year.Of.Twelth.Completion))
# No NAs here 


ggplot(combi,aes(x=(Year.Of.Twelth.Completion), fill= Pay_in_INR))+
  
  geom_histogram(bins = 20)+
  labs(y="12th completion",
       title="Year ")
plot(combi$Score.in.Tenth, combi$Pay_in_INR)

cor(log(combi$Pay_in_INR+1), combi$Score.in.Tenth)
# Little or no correlation with pay 


# 8. Score in Twelth: Performnace in twelth 
sum(is.na(combi$Score.in.Twelth))
# No NAs

ggplot(combi,aes(x=(Score.in.Twelth), fill= Pay_in_INR))+
  
  geom_histogram(bins = 20)+
  labs(y="Score in 12th",
       title="12th Score ")
plot(combi$Score.in.Twelth, combi$Pay_in_INR)

cor((combi$Pay_in_INR+1), combi$Score.in.Twelth)
# Again we see that the 12th score does not have much impact on pay 

# 9. Board in Twelth

sum(is.na(combi$Board.in.Twelth))
# Many missing values 
table(combi$Board.in.Twelth)
ggplot(combi,aes(x=Board.in.Twelth))+
  geom_bar()+
  labs(y="Count",
       title="School Board")

# Again we see a similar distribution. CBSE majority, state board or ICSE/ ISC

# 10. CollegeCode: College from which graduation has been done 

sum(is.na(combi$CollegeCode))
length(unique(combi$CollegeCode))
ggplot(combi,aes(x=CollegeCode))+
  geom_bar()+
  labs(y="Count",
       title="CollegeCode")
table(combi$CollegeCode)

# Does not seem to be very useful. Too many levels 

# 11. CollegeTier: 1 or 2 (binary) - based on the scores the students got from these institutes

sum(is.na(combi$CollegeTier))
table(combi$CollegeTier)

class(combi$CollegeTier)
combi$CollegeTier= as.factor(combi$CollegeTier)
ggplot(combi,aes(y=Pay_in_INR,x=CollegeTier))+
  
  geom_boxplot()+
  labs(y="Pay_in_INR",
       title="Pay vs College Tier")   

ggplot(combi,aes(x=Pay_in_INR, fill= CollegeTier))+
  geom_histogram(bins=20)+
  labs(y="Count",
       title="CollegeTier")
table(combi$CollegeCode)


# Right! It does intuitively seem that tier 2 students get more extreme pays. This is a 
# strong predictor 


# 12. Graduation: Degree Pursued
sum(is.na(combi$Graduation))
table(combi$Graduation)
ggplot(combi,aes(y=Pay_in_INR,x=Graduation))+
  
  geom_boxplot()+
  labs(y="Pay_in_INR",
       title="Pay vs Graduation")   

ggplot(combi,aes(x=Pay_in_INR, fill= Graduation))+
  geom_histogram(bins=20)+
  labs(y="Count",
       title="Graduation ")


# Many students with only a Btech ear well 

# 13. Discipline: Or specialization 


class(combi$Discipline)
table(combi$Discipline)
length(unique(combi$Discipline))

# 46 levels 

ggplot(combi,aes(x=Discipline))+
  geom_bar()+
  labs(y="Count")


# 14. GPA Score in Graduation

plot(combi$Pay_in_INR, combi$GPA.Score.in.Graduation)
cor(combi$Pay_in_INR, combi$GPA.Score.in.Graduation)
# Low correlation between score and earning. Marks don't matter! 

# 15. CityCode

class(combi$CityCode)
table(combi$CityCode)
length(unique(combi$CityCode))

# 1350 levels. Will remove this later 

# 16. CityTier

class(combi$CityTier)
table(combi$CityTier)
length(unique(combi$CityTier))
combi$CityTier= as.factor(combi$CityTier)
sum(is.na(combi$CityTier))

ggplot(combi,aes(y=Pay_in_INR,x=CityTier))+
  
  geom_boxplot()+
  labs(y="Pay_in_INR",
       title="Pay vs City Tier")   

ggplot(combi,aes(x=Pay_in_INR, fill= CityTier))+
  geom_histogram(bins=20)+
  labs(y="Count",
       title="CityTier")


# Right then! Tier 0 candidates earn more in general 


# 17. State

class(combi$State)
table(combi$State)
length(unique(combi$State))
sum(is.na(combi$State))
ggplot(combi,aes(y=Pay_in_INR,x=State))+
  
  geom_boxplot()+
  labs(y="Pay_in_INR",
       title="Pay vs State")   

# There is quite a lot of variation across states. This seems like an important 
# predictor 

# 18. Year of Graduation Completion

sum(is.na(combi$Year.of.Graduation.Completion))
# No NAs 
str(combi)
table(combi$Year.of.Graduation.Completion)
ggplot(combi,aes(y=Pay_in_INR,x= as.factor(Year.of.Graduation.Completion)))+
  
  geom_boxplot()+
  labs(y="Pay_in_INR",
       title="Pay vs Year of completion")   

# Students who have graduated recently earn less. Seems like an important predictor 


# 19. Score in English language

summary(combi)
cor(combi$Score.in.English.language, log(combi$Pay_in_INR+1))
# 0.028
plot(combi$Score.in.English.language, combi$Pay_in_INR)

# 20. Score in Logic 

plot(combi$Score.in.Logical.skill, combi$Pay_in_INR)

cor(combi$Score.in.Logical.skill, combi$Pay_in_INR)
# 0.03

# 21. Score in Quantitative ability

plot(combi$Score.in.Quantitative.ability , combi$Pay_in_INR)

cor(combi$Score.in.Quantitative.ability , combi$Pay_in_INR)
# 0.020 

# Logical reasoning ability is most crucial

# 22. Scores in domain 

sum(is.na(combi$Score.in.Domain))
# 1000 missing values 

plot(combi$Score.in.Domain , combi$Pay_in_INR)

cor(combi$Score.in.Domain , combi$Pay_in_INR)
# Unable to calculate due to missing values 

?cor

# 23. Score in ComputerProgramming

class(combi$Score.in.ComputerProgramming)
summary(combi)
sum(is.na(combi$Score.in.ComputerProgramming))
combi$CP= ifelse(combi$Score.in.ComputerProgramming==-100,0,1)
table(combi$CP)

# Making a new column. 0 indicates not attempted. 1 indicates attempted

# 24. Score in ElectronicsAndSemicon

class(combi$Score.in.ElectronicsAndSemicon)
summary(combi)
sum(is.na(combi$Score.in.ElectronicsAndSemicon))
combi$EaS= ifelse(combi$Score.in.ElectronicsAndSemicon==-100,0,1)
table(combi$EaS)

# 25. Score in ComputerScience

class(combi$Score.in.ComputerScience)
summary(combi)
sum(is.na(combi$Score.in.ComputerScience))
combi$CS= ifelse(combi$Score.in.ComputerScience==-100,0,1)
table(combi$CS)
cor(combi$Score.in.ComputerScience, combi$Pay_in_INR)


# 26. Score in MechanicalEngg

class(combi$Score.in.MechanicalEngg)
summary(combi)
sum(is.na(combi$Score.in.MechanicalEngg))
combi$ME= ifelse(combi$Score.in.MechanicalEngg==-100,0,1)
table(combi$ME)
cor(combi$Score.in.MechanicalEngg, combi$Pay_in_INR)


# 27. Score in ElectricalEngg

class(combi$Score.in.ElectricalEngg)
summary(combi)
sum(is.na(combi$Score.in.ElectricalEngg))
combi$EE= ifelse(combi$Score.in.ElectricalEngg==-100,0,1)
table(combi$EE)
cor(combi$Score.in.MechanicalEngg, combi$Pay_in_INR)


# 28. Score in TelecomEngg


class(combi$Score.in.TelecomEngg)
summary(combi)
sum(is.na(combi$Score.in.TelecomEngg))
combi$TE= ifelse(combi$Score.in.TelecomEngg==-100,0,1)
table(combi$TE)
cor(combi$Score.in.TelecomEngg, combi$Pay_in_INR)


# 29. Score in CivilEngg


class(combi$Score.in.CivilEngg)
summary(combi)
sum(is.na(combi$Score.in.CivilEngg))
combi$CE= ifelse(combi$Score.in.CivilEngg==-100,0,1)
table(combi$CE)
cor(combi$Score.in.CivilEngg, combi$Pay_in_INR)


# 30. Score in conscientiousness

summary(combi)
class(combi$Score.in.conscientiousness)

plot(combi$Pay_in_INR, combi$Score.in.conscientiousness)
cor(combi$Pay_in_INR, combi$Score.in.conscientiousness)

ggplot(combi,aes(x=Score.in.conscientiousness))+
  geom_histogram(bins=20)+
  labs(y="Count",
       title="Score ")

# 31. Score in agreeableness


summary(combi)
class(combi$Score.in.agreeableness)

plot(combi$Pay_in_INR, combi$Score.in.agreeableness)
cor(combi$Pay_in_INR, combi$Score.in.agreeableness)

ggplot(combi,aes(x=Score.in.agreeableness))+
  geom_histogram(bins=20)+
  labs(y="Count",
       title="Score ")


# 32. Score in extraversion

summary(combi)
class(combi$Score.in.extraversion)

plot(combi$Pay_in_INR, combi$Score.in.extraversion)
cor(combi$Pay_in_INR, combi$Score.in.extraversion)

ggplot(combi,aes(x=Score.in.extraversion))+
  geom_histogram(bins=20)+
  labs(y="Count",
       title="Score ")


# 33. Score in nueroticism

summary(combi)
class(combi$Score.in.nueroticism)

plot(combi$Pay_in_INR, combi$Score.in.nueroticism)
cor(combi$Pay_in_INR, combi$Score.in.nueroticism)

ggplot(combi,aes(x=Score.in.nueroticism))+
  geom_histogram(bins=20)+
  labs(y="Count",
       title="Score ")

# 34. Score in openess_to_experience

summary(combi)
class(combi$Score.in.openess_to_experience)

plot(combi$Pay_in_INR, combi$openess_to_experience)
cor(combi$Pay_in_INR, combi$openess_to_experience)

ggplot(combi,aes(x=Score.in.openess_to_experience))+
  geom_histogram(bins=20)+
  labs(y="Count",
       title="Score ")


##  6. Data PreProcessing and Feature Engineering

## Type Conversions 

# First, I will convert the required variables to factors 

dim(combi)
str(combi)
for(i in c(40,39,38,37,36,35,34)){
  combi[,i]= as.factor(combi[,i])
}

## Missing value Imputation 

colSums(is.na(combi[,colSums(is.na(combi))>0]))

# City Tier has most values missing. Thus, I am going to drop this. 

# Dropping Columns
combi$CollegeCode= NULL
combi$CityCode= NULL

md.pattern(combi)
dim(combi)

paste("Percentage of missing data in the columns is around",(7283/37920)*100)

# That is a lot. I am going to use these variables and try to remove them and see 
# results 
class(combi$Board.in.Twelth)
class(combi$School.Board.in.Tenth)

combi$School.Board.in.Tenth-> tenth
combi$Board.in.Twelth-> twelth
length(tenth)
length(twelth)
colSums(is.na(combi[,colSums(is.na(combi))>0]))

domain<- rpart(Score.in.Domain ~., data=combi[!is.na(combi$Score.in.Domain),], method="anova")
combi$Score.in.Domain[is.na(combi$Score.in.Domain)] <- predict(domain, combi[is.na(combi$Score.in.Domain),])

combi= centralImputation(combi)

sum(is.na(combi))

## All missing values dealt with


# I will convert the date of birth into age, and remove the date of birth column 
class(combi$Date.Of.Birth)
sum(is.na(combi$Date.Of.Birth))

sum(is.na(combi))
class(combi$Date.Of.Birth)
combi$year= format(as.Date(combi$Date.Of.Birth), "%Y")
class(combi$year)
sum(is.na(combi$year))
View(combi)
combi$year= as.numeric(as.character(combi$year))
combi$age= sapply(combi$year, function(x){2017-x})

sum(is.na(combi))

str(combi)
combi$Date.Of.Birth= NULL
combi$year= NULL
class(combi$age)
age<- rpart(age ~., data=combi[!is.na(combi$age),], method="anova")
combi$age[is.na(combi$age)] <- predict(age, combi[is.na(combi$age),])

sum(is.na(combi))

## Great! We have converted date of birth to age. What next? 

str(combi)

cor(combi$Year.Of.Twelth.Completion, combi$Year.of.Graduation.Completion)
# 0.8285. Very strong correlation between both. Thus, will remove 1 of the variables

cor(log(combi$Pay_in_INR+1), combi$Year.Of.Twelth.Completion)
cor(log(combi$Pay_in_INR+1), combi$Year.of.Graduation.Completion)

combi$Year.of.Graduation.Completion-> gradyear
combi$Year.of.Graduation.Completion= NULL


# Removing this and not year of 12th completion because this has a greater 
# correlation with the target variable 

combi$Years.since.twelth=sapply(combi$Year.Of.Twelth.Completion, function(x){2017-x})
sum(is.na(combi$Years.since.twelth))

combi$Year.Of.Twelth.Completion= NULL
new= read.csv("train (1).csv")
new2= read.csv("test (1).csv")
new2$Pay_in_INR= 0
new3= rbind(new,new2)
dim(new3)
gap= new3$Year.of.Graduation.Completion- new3$Year.Of.Twelth.Completion
table(gap<4)
table(gap>4)
dim(combi)
class(new$Year.Of.Twelth.Completion)
View(new)
gap= ifelse(gap<4, 0, 
            ifelse(gap>4,2,1 ))
table(gap)
combi$gap= gap

dim(combi)
length(gap)
class(combi$gap)
combi$gap= as.factor(combi$gap)
table(combi$gap)


table(combi$Graduation)
table(combi$Discipline)

combi$disc= ifelse(combi$Discipline=="computer science & engineering"|combi$Discipline=="computer engineering"|combi$Discipline=="computer application"|combi$Discipline=="computer networking",0,
                   ifelse(combi$Discipline=="information technology",1,
                          ifelse(combi$Discipline=="electronics and communication engineering"|combi$Discipline=="electronics and electrical engineering "|combi$Discipline=="electronics & telecommunications"|combi$Discipline=="electronics and instrumentation engineering "|combi$Discipline=="electronics & instrumentation eng",2,
                                 ifelse(combi$Discipline=="mechanical engineering"|combi$Discipline=="mechanical and automation",3,4))))
table(combi$disc)

# I have divided the discipline column (which had 46 levels) to 5 levels

combi$Discipline-> discc
combi$Discipline= NULL
str(combi)




## 7. Data Preparation 



dim(pay_tr)
dim(pay_te)
dim(combi)

train= combi[1:26415,]
test= combi[26416:37290,]

dim(train)
dim(test)

pre1<-preProcess(train[,setdiff(colnames(train),"Pay_in_INR")])
train_scale<-predict(pre1,train[,setdiff(colnames(train),"Pay_in_INR")])
test_scale<-predict(pre1,test[,setdiff(colnames(test),"Pay_in_INR")])


# Great, we have retrieved the original data! 

# Since, we are allowed to make only 4 submissions, I want to divide the train data
# further into a train and test set. 


train$School.Board.in.Tenth-> tenth
train$Board.in.Twelth-> twelth
train$School.Board.in.Tenth= NULL
train$Board.in.Twelth= NULL

set.seed(123)
rows= sample.split(train$Pay_in_INR, 0.8)
ntrain= train[rows==T,] 
ntest= train[rows==F,] 
ntrain2= train_scale[rows==T,]
ntest2=train_scale[rows==F,]
dim(ntrain)
dim(ntest)
ntrain2$Pay_in_INR= ntrain$Pay_in_INR
ntest2$Pay_in_INR= ntest$Pay_in_INR

# 21279+5136
# ntrain is new train. We are finally ready to build models! 


View(ntrain)
View(ntest)

View(ntrain2)


## 8. LINEAR REGRESSION 

# Model 1: Linear Regression directly

class(ntest$State)
ntest2= ntest2[-which(ntest2$State=="Goa"),]
dim(ntest2)
table(train$State)

model1= lm(Pay_in_INR~., data=ntrain)
summary(model1)
# Adjusted R Squared of 0.09982 
pred1= predict(model1, newdata= ntest)
error1= regr.eval(pred1, ntest$Pay_in_INR)
error1
# mape of 24%
pred1_train= predict(model1)
error1_train= regr.eval(pred1_train, ntrain$Pay_in_INR)
error1_train
# train rmse of 29%
par(mfrow=c(2,2))
plot(model1)

# Model 2: Log transformation of target


model2= lm(log(Pay_in_INR)~., data=ntrain)
summary(model2)
# Adjusted R Squared of 0.1533 
pred2= predict(model2, newdata= ntest)
pred2= exp(pred2)
error2= regr.eval(pred2, ntest$Pay_in_INR)
error2
# mape of 24.69%
pred2_train= predict(model2)
pred2_train= exp(pred2_train)
error2_train= regr.eval(pred2_train, ntrain$Pay_in_INR)
error2_train
# train rmse of 30.84%
par(mfrow=c(2,2))
plot(model1)

# Model 3: Step AIC on model 1 

model3= stepAIC(model1)
summary(model3)
# Adjusted R Squared of 0.09985 
pred3= predict(model3, newdata= ntest)

error3= regr.eval(pred3, ntest$Pay_in_INR)
error3
# mape of 24.91%
pred3_train= predict(model3)

error3_train= regr.eval(pred3_train, ntrain$Pay_in_INR)
error3_train
# train rmse of 29.33%
par(mfrow=c(2,2))
plot(model3)

vif(model3)


# Model 4: Interaction terms 

model4= lm(Pay_in_INR~.*., data= ntrain)
summary(model4)
# Adjusted R Squared of 0.4755  
pred4= predict(model4, newdata= ntest)
error4= regr.eval(pred4, ntest$Pay_in_INR)
error4
# mape of 0.39
par(mfrow=c(2,2))
plot(model4)
# Time for some cutting and chopping 



## The Linear Regression models have not done very well. The data does not seem to 
## be linearly separable. 


which(duplicated(train))

## Decision Tree 

control <- trainControl(method="cv", number=10)
grid <- expand.grid(cp=seq(0.0001,0.005,0.0005))
control
class(control)
class(grid)
View(grid)
class(train2)
DT_rpart_Reg<-caret::train(Pay_in_INR~.,data=ntrain,method="rpart",trControl=control,tuneGrid= grid)
print(DT_rpart_Reg)
class(DT_rpart_Reg)

predCartTest=predict(DT_rpart_Reg, newdata=ntest)
length(predCartTest)
error= regr.eval(predCartTest, ntest$Pay_in_INR)
error

## 10. Random Forest 

dim(ntrain)
?randomForest
model_rf = randomForest(Pay_in_INR ~ . , ntrain, ntree = 50,mtry = 6)
summary(model_rf)
varImpPlot(model_rf)
par(mfrow=c(1,1))
rf_train_pred = predict(model_rf)
preds_rf_test <- predict(model_rf, ntest)
error= regr.eval(preds_rf_test, ntest$Pay_in_INR)
error
plot(model_rf)

# mape falls down drastically to 14.91%

?randomForest
control <- trainControl(method="cv", number=5, search="grid")
grid <- expand.grid(.mtry=5:9)
control
class(control)
class(grid)
View(grid)
class(train2)

library(doParallel)
registerDoParallel(cores= 4)
RF<-caret::train(Pay_in_INR~.,data=ntrain,method="rf",trControl=control,tuneGrid=grid, ntree= 30)
print(RF)
plot(RF)
preds_RF_test <- predict(RF, ntest)
error= regr.eval(preds_RF_test, ntest$Pay_in_INR)
error

# 15% Mape. Maybe due to the ntree? Will try increasing the ntree and the nodesize 

RF_final= predict(model_rf, newdata= test)
length(RF_final)
predictions= data.frame("ID"= testid, "Salary"= RF_final)
write.csv(predictions, "predictions.csv", row.names = F)
dim(predictions)

## 11. XGBoost 

# For XGBoost, we need to convert all columns to numeric data. 

str(train)

dim(train)
dim(test)
test$School.Board.in.Tenth-> tenth2
test$Board.in.Twelth-> twelth2
test$School.Board.in.Tenth= NULL
test$Board.in.Twelth= NULL

combi2= rbind(train, test)
dim(combi2)
str(combi2)
disc1= dummy(combi2$disc)
dim(disc1)

dim(combi2)

for(i in c(32,31,30,29,28,27,26,5)){
  combi2[,i]= as.numeric(as.character(combi2[,i]))
}
str(combi2)

combi2$disc= NULL
combi2=cbind(combi2, disc1)
dim(combi2)
state2= dummy(combi2$State)
dim(state2)
combi2$State= NULL
combi2= cbind(combi2, state2)
dim(combi2)
str(combi2)
grad2= dummy(combi2$Graduation)
dim(grad2)
combi2$Graduation= NULL
combi2= cbind(combi2, grad2)
gend= dummy(combi2$Gender)
combi2$Gender= NULL
combi2= cbind(combi2, gend)
str(combi2)
city= dummy(combi2$CityTier)
combi2$CityTier= NULL
combi2= cbind(combi2, city)
dim(combi2)
which(duplicated(train))
train2= combi2[1:26415,]
test2= combi2[26416:37290,]
dim(train2)
dim(test2)

# Time to build the model, finally


xgb.ctrl <- trainControl(method = "cv", number = 5,
                         search='random')

set.seed(123)
xgb.tune <-train(Pay_in_INR~.,
                 data = train2,
                 method="xgbLinear",
                 trControl=xgb.ctrl,
                 tuneLength=30)

print(xgb.tune)
plot(xgb.tune)



XgB_final= predict(xgb.tune, newdata= test2)
length(XgB_final)
predictions= data.frame("ID"= testid, "Salary"= XgB_final)
write.csv(predictions, "predictions.csv", row.names = F)
dim(predictions)




results= resamples(list(a= xgb.tune, b= RF, c= DT_rpart_Reg))
summary(results)
dotplot(results)




