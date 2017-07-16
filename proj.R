#titanic

trainset=read.csv("C:/Users/priya/Desktop/titanic/train.csv")
testset=read.csv("C:/Users/priya/Desktop/titanic/test.csv")
str(trainset)
trainset$Sex=as.factor(trainset$Sex)
testset$Sex=as.factor(testset$Sex)
table(trainset$Survived)
prop.table(table(trainset$Survived))
testset$Survived=rep(0,418)
submit=data.frame(PassengerId=testset$PassengerId, Survived=testset$Survived)
write.csv(submit,file="C:/Users/priya/Desktop/titanic/theyallperish.csv",row.names = F)

#GenderClassModel
table(trainset$Sex)
summary(trainset$Sex)
prop.table(table(trainset$Sex,trainset$Survived),1)

testset$Survived=0
# lets say that all females in test set survived.
testset$Survived[testset$Sex=="female"]=1
submit=data.frame(PassengerId=testset$PassengerId, Survived=testset$Survived)
write.csv(submit,file="C:/Users/priya/Desktop/titanic/theyallperish.csv",row.names = F)
summary(trainset$Age)
trainset$Age[which(is.na(trainset$Age))]=mean(trainset$Age, na.rm=T)
trainset$Child=0
trainset$Child[trainset$Age<18]=1
aggregate(Survived ~ Child + Sex, data=trainset, FUN=function (x) sum(x)/length(x))
table(trainset$Child,trainset$Sex)
trainset$Fare2= '30+'
trainset$Fare2[which(trainset$Fare< 30 & trainset$Fare >20)]= '20-30'
trainset$Fare2[which(trainset$Fare< 20 & trainset$Fare >10)]= '10-20'
trainset$Fare2[which(trainset$Fare<10)]='< 10'
aggregate(Survived ~ Child + Sex +Pclass + Fare2 , data=trainset, FUN=function (x) sum(x)/length(x))
range(trainset$Fare[trainset$Pclass==3])
testset$Survived=0
testset$Survived[testset$Sex=="female"]=1
testset$Survived[testset$Sex=="female" & testset$Pclass==3 & testset$Fare>=20]=0
names(trainset)
library(rpart)
model=rpart(Survived ~  Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
            data=trainset, 
            method='class')
plot(model)
text(model)
y_pred=predict(model,newdata=testset, type='class')
y_pred2=predict(model,newdata=trainset, type='class')
sumbit=data.frame(PassengerID=testset$PassengerId, Survived=y_pred)
write.csv(submit,file="C:/Users/priya/Desktop/titanic/myfirstdtree.csv",row.names = F )
?rpart.control

sumbit1=data.frame(PassengerID=trainset$PassengerId, Actual_Survived=trainset$Survived, Predicted_Survived=y_pred2)
write.csv(sumbit1,file="C:/Users/priya/Desktop/titanic/check.csv",row.names = F )
library(caret)
# conf mattrix n accuracy

cm=confusionMatrix(trainset$Survived,y_pred2)
cm2
cm2=table(trainset$Survived,y_pred2)
acc=sum(diag(cm2))/sum(cm2)
acc
model2=rpart(Survived ~  Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
            data=trainset, 
            method='class',
            control = rpart.control(minsplit=2, cp=0.02))
plot(model2)
text(model2)
accura

# feature engineering
trainset$Name[1]
testset$Survived=NA
trainset1=trainset[,-c(13,14)]
full=rbind(trainset1,testset)
str(full)
full$Name=as.character(full$Name)
strsplit(full$Name[1], split='[,.]')[[1]][2]
full$title=sapply(full$Name, FUN= function(x) strsplit(x, split='[,.]')[[1]][2])
class(full$title)
typeof(full$title)
# to replace first occ of space with '' (nothing)
full$title=sub(" ","",full$title)
full$title[which(full$title=="Mlle" | full$title=="Mme")]='Mlle'
unique(full$title)
full$title[full$title %in% c('Don','Rev','Major','Sir','Col','Capt')]='Sir'
full$title[full$title %in% c('Lady','','theCountess','Jonkheer','Dona')]='Lady'
full$title=as.factor(full$title)
# creating familysize variable
full$FamilySize=full$Parch+full$SibSp+1
#get the surname
full$Name[1]
full$surname=sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
full$familyID=paste(as.character(full$FamilySize),full$surname,sep="")
str(full)
full$familyID[full$FamilySize<=2]='small'
famID_df=data.frame(table(full$familyID))
famID_df=famID_df[famID_df$Freq<=2,]
full$familyID[which(full$familyID %in% famID_df$Var1)]='small'
full$familyID=as.factor(full$familyID)
train=full[1:891,]
test=full[892:1309,]
model3=rpart(Survived ~ Pclass + Sex + Age + SibSp+ Parch+ Embarked + title + FamilySize+familyID,
             data=train, method="class")
plot(model3)
text(model3)
y_pred=predict(model3, newdata = test, type='class')
submit3=data.frame(PassengerID=test$PassengerId, Survived=y_pred)
write.csv(submit3, file="C:/Users/priya/Desktop/titanic/testoutputwithFE.csv",row.names = F)
# check the accuracy with traindata by splitting train data into test and train 
nrow(train)
library(caret)
intrain=createDataPartition(train$Survived,p=0.8,list=F)
trainsample=train[intrain,]
testsample=train[-intrain,]
modelsample=rpart(Survived ~ Pclass + Sex + Age +FamilySize+familyID,
                  data=trainsample, method='class')
plot(modelsample)
y_predsample=predict(modelsample,newdata=testsample, type='class')
cmsample=confusionMatrix(testsample$Survived, y_predsample)
cmsample
# random forest classifier
# data processing for building RF. 
# imputing missing age value
agefit=rpart(Age ~ Pclass + Sex + Parch + Fare + Embarked + title + FamilySize,
             data=full[!is.na(full$Age),], method="anova")
full$Age[is.na(full$Age)]=predict(agefit, newdata=full[is.na(full$Age),])

see=full[is.na(full$Age),]
see
summary(full)
# missing value in Fare and 
table(full$Embarked)
full$Embarked[which(full$Embarked=="")]='S'
table(full$Embarked)
summary(full$Fare)
which(is.na(full$Fare))
full$Fare[1044]=median(full$Fare,na.rm=T)

# reducing the number of levels of FamilyID
#full=full[-17]
full$familyID2=full$familyID
str(full)
full$familyID2=as.character(full$familyID2)
full$familyID2[full$FamilySize<=3]="small"
full$familyID2=as.factor(full$familyID2)
levels(full$familyID2)
rftrain=full[1:891,]
rftest=full[892:1309,]
library(randomForest)
set.seed(100)
rffit=randomForest(as.factor(Survived) ~ Pclass + Sex +Age + SibSp + Parch + Fare +Embarked + title + FamilySize + familyID2,
                   data=rftrain, importance=T, ntree=200)
varImpPlot(rffit)
yrf_pred=predict(rffit, newdata=rftest)
submitrf=data.frame(PassengerID=rftest$PassengerId, Survived=yrf_pred)
write.csv(submitrf,file="C:/Users/priya/Desktop/titanic/randforestOutput.csv",row.names = F)

#lets test the accuracy by splitting the train set
rftrainsample=createDataPartition(rftrain$Survived,p=0.8, list=F)
rf_samptrain=rftrain[rftrainsample,]
rf_sampltest=rftrain[-rftrainsample,]

rffit2=randomForest(as.factor(Survived) ~ Pclass + Sex +Age + SibSp + Parch + Fare +Embarked + title + FamilySize + familyID2,
                   data=rf_samptrain, importance=T, ntree=200)
varImpPlot(rffit2)
yrf_pred=predict(rffit2, newdata=rf_sampltest)
cm=confusionMatrix(rf_sampltest$Survived,yrf_pred)
cm
# loading party library
library(party)
set.seed(415)
fit=cforest(as.factor(Survived) ~ Pclass + Sex +Age + SibSp + Parch + Fare +Embarked + title + FamilySize + familyID2,
            data=rftrain,
            controls = cforest_unbiased(ntree=2000, mtry=3))
y_predd=predict(fit, rftest, OOB=T, type='response')
submitrf1=data.frame(PassengerID=rftest$PassengerId, Survived=y_predd)
write.csv(submitrf1,file="C:/Users/priya/Desktop/titanic/cforestOutput.csv",row.names = F)
rftrainsample1=createDataPartition(rftrain$Survived,p=0.8, list=F)
rf_samptrain1=rftrain[rftrainsample1,]
rf_sampltest1=rftrain[-rftrainsample1,]

rffit3=cforest(as.factor(Survived) ~ Pclass + Sex +Age + SibSp + Parch + Fare +Embarked + title + FamilySize + familyID2,
               data=rf_samptrain1,
               controls = cforest_unbiased(ntree=2000, mtry=3))
varImpPlot(rffit2)
yrf_pred1=predict(rffit3, newdata=rf_sampltest1)
cm=confusionMatrix(rf_sampltest1$Survived,yrf_pred1)
