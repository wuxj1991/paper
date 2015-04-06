setwd('d:/python/Kaggle-Give Me Some Credit')
library(arm);library(cluster)
library(DMwR)
library(caret)
library(gbm)
library(randomForest)
library(caTools)
library(foreach)
library(doMC)
library(e1071)
library(Daim)
#Get data
getData <- function() {
  training1 <- read.csv("cs-training.csv")
  training <- training1[c(1:120000),]
  test <- training1[c(120001:150000),]
  list(training=training,testing=test)
}
xform_data1 <- function(x) {
  x$NumberOfDependents[is.na(x$NumberOfDependents)] <- 0
  x$MonthlyIncome[is.na(x$MonthlyIncome)]<-median(x$MonthlyIncome,na.rm=T)
  x
}

outtrain<-training1[-train$X,]
test1<-getResample(outtrain,pctDeadbeat=0.2,pct=6000/nrow(outtrain))
test1<-xform_data1(test1)
save(test1,file="test1.rda");load("test1.rda")
gettestsample<-function(data){
  
}
trained<-data.frame()
trained<-rbind(train,test1)
outtrain<-training1[-trained$X,]
test2<-getResample(outtrain,pctDeadbeat=0.2,pct=6000/nrow(outtrain))

trained<-rbind(trained,test2)
outtrain<-training1[-trained$X,]
test3<-getResample(outtrain,pctDeadbeat=0.2,pct=6000/nrow(outtrain))

trained<-rbind(trained,test3)
outtrain<-training1[-trained$X,]
test4<-getResample(outtrain,pctDeadbeat=0.2,pct=6000/nrow(outtrain))

trained<-rbind(trained,test4)
outtrain<-training1[-trained$X,]
test5<-getResample(outtrain,pctDeadbeat=0.2,pct=6000/nrow(outtrain))

trained<-rbind(trained,test5)
outtrain<-training1[-trained$X,]
test6<-getResample(outtrain,pctDeadbeat=0.2,pct=6000/nrow(outtrain))
save(test2,file="test2.rda")
save(test2,file="test2.rda")
save(test3,file="test3.rda")
save(test4,file="test4.rda")
save(test5,file="test5.rda")
save(test6,file="test6.rda")

getResample <- function(training, pctDeadbeat,pct) {
  deadbeat.size <- floor(pct*pctDeadbeat*nrow(training))
  responsible.size <- ceiling(pct*nrow(training)*(1-pctDeadbeat))
  
  training.deadbeat <- training[training$SeriousDlqin2yrs == 1,]
  training.responsible <- training[training$SeriousDlqin2yrs == 0,]
  
  training <- rbind(training.deadbeat[sample(1:nrow(training.deadbeat),
                                             deadbeat.size
  ),],
  training.responsible[sample(1:nrow(training.responsible),
                              responsible.size
  ),])
  training<-training[sample(1:nrow(training),nrow(training)),]
  training
}
train<-getResample(training,pctDeadbeat=0.2,pct=0.05)
train<-xform_data1(train) 
write.csv(train,file = "train.csv", row.names = FALSE)
train<-read.csv("train.csv");train<-train[sample(1:nrow(train),nrow(train)),]
train.pca<-princomp(train[,c(-1,-2)],cor=TRUE)
summary(train.pca,loadings=TRUE)
screeplot(train.pca,type="lines")
e=eigen(cor(train[,c(-1,-2)]))
#calVIF
vif(lm(SeriousDlqin2yrs~.,data=train))
#monthly cost
getcomp<-function(data){
  data$MonthlyCost<-data$DebtRatio*data$MonthlyIncome
  data$DebtRatio<-NULL
  data<-data[,c(1:5,12,6:11)]
  pca<-princomp(data[,c(-1,-2)],cor=TRUE)
  pcalog<-data.frame(cbind(pca$scores[,1:6],data$SeriousDlqin2yrs))
  return(pcalog)
}
predd<-function(train,testdata){
  pca_response<-getcomp(train)
  test_pca_response<-getcomp(xform_data1(testdata))
  colnames(pca_response)[7]<-c("SeriousDlqin2yrs")
  colnames(test_pca_response)[7]<-c("SeriousDlqin2yrs")
  logmodel<-glm(SeriousDlqin2yrs~.,family=binomial(link='logit'),data=pca_response)
  pred<-predict(step(logmodel,direction=c("backward")),type ="response")
  test_pred<-predict(step(logmodel,direction=c("backward")),newdata=test_pca_response[,-7])
  train_preddata<-cbind(pca_response,pred)
  test_preddata<-cbind(test_pca_response,test_pred)
  return(list(trainpred=pred,testpred=test_pred,train_preddata=train_preddata,
              test_preddata=test_preddata,model=step(logmodel,direction=c("backward"))))
}

predd_test1<-predd(train=train,testdata=test1)
plot(roc(predd_test1$train_preddata$pred,
         predd_test1$train_preddata$SeriousDlqin2yrs,"1"))
plot(roc(predd_test1$test_preddata$test_pred,
         predd_test1$test_preddata$SeriousDlqin2yrs,"1"))
genmcost<-function(dataset){
  tt<-dataset
  tt$MonthlyCost<-tt$DebtRatio*tt$MonthlyIncome
  tt$DebtRatio<-NULL
  tt<-tt[,c(1:5,12,6:11)]
  return(tt)
}
tt<-genmcost(train)
#logistic
library(pROC)
log.pred<-function(traindata,testdata){
  reg<-glm(SeriousDlqin2yrs~.,family=binomial(link='logit'),data=traindata[,c(-1)])
  reg1<-step(reg,direction=c("backward"))
  reg_pred<-predict(reg1,type ="response")
  reg_test_pred<-predict(reg1,type ="response",newdata=testdata)
  #plot(roc(reg_pred, tt$SeriousDlqin2yrs,"1"))
  log.roc<-roc(testdata$SeriousDlqin2yrs,reg_test_pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)
  AUC<-log.roc$auc  
  return(list(model=reg1,AUC=AUC,reg_pred=reg_pred,reg_test_pred=reg_test_pred))
}
test1.log.pred<-log.pred(train,test1)
log.pred(genmcost(train),genmcost(test1))

pcalog<-data.frame(cbind(tt.pca$scores[,1:6],tt$SeriousDlqin2yrs))
colnames(pcalog)[7]<-c("SeriousDlqin2yrs")
logmodel<-glm(SeriousDlqin2yrs~.,family=binomial(link='logit'),data=pcalog)
step(logmodel,direction=c("backward"))
logisticmodel<-glm(SeriousDlqin2yrs~.,
                   family=binomial(link='logit'),data=pcalog)
log.pred<-predict(step(logmodel,direction=c("backward")),type ="response")
log.test_pred<-predict(step(reg1,direction=c("backward")),newdata=test1)
train_AUC <- colAUC(pred, tt$SeriousDlqin2yrs,plotROC=T)
library(pROC)
modelroc <- roc(tt$SeriousDlqin2yrs,pred)
plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="skyblue", print.thres=TRUE)

plot(pcalog[,1:2],col=as.integer(pcalog[,7]+2),
     xlab="1st Principal Component",ylab="2nd Principal Component")
#calAUC

plot(roc(pred, tt$SeriousDlqin2yrs,"1"))


e=eigen(cor(tt[,c(-1,-2)]))
tt.kpca <- kpca(~.,data=tt[,c(-1,-2)],kernel="rbfdot",
                kpar=list(sigma=0.2))
#name
ttname<-tt[,c(-1,-2)]
colnames(ttname)<-paste('var',1:10,sep='')
ttname.pca<-princomp(ttname,cor=TRUE)
summary(ttname.pca,loadings=TRUE)
#kpca
library(kernlab)
kpc <- kpca(~.,data=train[,c(-1,-2)],kernel="rbfdot",
            kpar=list(sigma=0.2),features=2)
tt.kpc <- kpca(~.,data=tt[,c(-1,-2)],kernel="rbfdot",
               kpar=list(sigma=0.2),features=2)
plot(rotated(tt.kpc),col=as.integer(tt[,2]+2),
     xlab="1st Principal Component",ylab="2nd Principal Component")
#print the principal component vectors
pcv(kpc)
plot(rotated(kpc),col=as.integer(training[1:4000,2]),
     xlab="1st Principal Component",ylab="2nd Principal Component")


#randomforest
classwt <- c((1-pctDeadbeat)/sum(training.SeriousDlqin2yrs == 0),
             pctDeadbeat/sum(training.SeriousDlqin2yrs == 1)) *
  nrow(training)
train[,c(-1,-2)]
data<-train
colnames(data)[c(3,5,8:12)]<-c('balance','30-59DaysDue','Credits.No','90DaysDue','RealEstateLoans.No',
                              '60-89DaysDue','Dependents.No')
testtt<-xform_data1(test1)
colnames(testtt)[c(3,5,8:12)]<-c('balance','30-59DaysDue','Credits.No','90DaysDue','RealEstateLoans.No',
                               '60-89DaysDue','Dependents.No')

#data<-cbgmadata
rf.pred<-function(data,test,ntree,mtry){
  set.seed(71)
  RF<-randomForest(data[,c(-1,-2)],
                   data$SeriousDlqin2yrs,
                   ntree=ntree,mtry=mtry,
                   strata=factor(data$SeriousDlqin2yrs),
                   do.trace=TRUE, importance=TRUE, forest=TRUE,
                   replace=TRUE,proximity=T)
#   RFout<-randomForest(data[,c(-1,-2)],
#                   factor(data$SeriousDlqin2yrs),
#                   ntree=ntree,mtry=mtry,
#                   strata=factor(data$SeriousDlqin2yrs),
#                   do.trace=TRUE, importance=TRUE, forest=TRUE,
#                   replace=TRUE,proximity=T)
#   outlier(RFout)
#   plot(outlier(RFout),type='h',col=c("red", "green")[as.numeric(data$SeriousDlqin2yrs+1)])
  RF.pred<-predict(RF,type ="response")
  RF.test_pred<-predict(RF,newdata=test[c(-1,-2)],type ="response") 
#   if the response variable is a factor vector
#   RF.test_pred<-predict(RF,newdata=test[c(-1,-2)],type ="prob") 
  return(list(RFmodel=RF,RFpred=RF.pred,RFtest_pred=RF.test_pred))
}
# data=train;test=xform_data1(test1);ntree=400;mtry=2;RFout.test_pred<-predict(RFout)
# RFpred<-rf.pred(data=data,test=testtt,ntree=400,mtry=2)
RFpred<-rf.pred(data=train,test=xform_data1(test1),ntree=400,mtry=2)
#detect outlier
#plot(outlier(RFpred$RFmodel),type='h',col=c("red", "blue")[as.numeric(train$SeriousDlqin2yrs)+1])
test1.rf.pred<-RFpred$RFtest_pred
train.rf.pred<-RFpred$RFpred
#plot(roc(test1.rf.pred, test1$SeriousDlqin2yrs,"1"))#conbine gma auc=0.849
#plot(roc(train.rf.pred, train$SeriousDlqin2yrs,"1"))
train_AUC <- colAUC(train.rf.pred, train$SeriousDlqin2yrs)
test_AUC <- colAUC(test1.rf.pred, test1$SeriousDlqin2yrs)
train_AUC;test_AUC
varImpPlot(RFpred$RFmodel)
library(pROC)
roc(test1$SeriousDlqin2yrs,RFpred$RFtest_pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)

#GAM
library(mgcv)
RFpred<-rf.pred(train,xform_data1(test1),100)
test1.rf.pred<-RFpred$RFtest_pred
train.rf.pred<-RFpred$RFpred
RF.cbdata<-cbind(train,train.rf.pred)
test11=xform_data1(test1)[,c(-1,-2)]
RF.cbtestdata<-cbind(test11,test1.rf.pred)
head(cbind(test1,RF.test_pred),2)
#gamdata<-RF.cbdata[,-1]
#gamdata<-train[,-1];colnames(gamdata)[2:11]<-paste('var',1:10,sep='')
gam.RFcbdata<-RF.cbdata[,-1]
colnames(RF.cbtestdata)[1:11]<-paste('var',1:11,sep='')
colnames(gam.RFcbdata)[2:12]<-paste('var',1:11,sep='')
gam2<-gam(SeriousDlqin2yrs~s(var1,k=5)+s(var2,k=5)+s(var3,k=5)
          +s(var5,k=5)+s(var6,k=5)+s(var7,k=5)+s(var8,k=5)+s(var9,k=5)+s(var10,k=5)+s(var11,k=5),
          family=binomial(link = "logit"),data=gam.RFcbdata,method="REML")
gam3<-gam(SeriousDlqin2yrs~s(var2,k=5)+s(var3,k=5)
          +s(var6,k=5)+s(var7,k=5)+s(var11,k=5),
          family=binomial(link = "logit"),data=gam.RFcbdata,method="REML")
summary(gam3)
#summary(gam2)
#gam.pred<-predict(gam1,newdata=test1[,c(-1,-2)],type ="response")
rfgam.test1.pred<-predict(gam3,newdata=RF.cbtestdata,type ="response")
rfgam.train.pred<-predict(gam3,type ="response")
colAUC(gam.test1.pred,test1$SeriousDlqin2yrs)
plot(roc(as.integer(rfgam.test1.pred), test1$SeriousDlqin2yrs,"1"))
roc(test1$SeriousDlqin2yrs,rfgam.test1.pred,plot=TRUE, print.thres=T,print.auc=TRUE)
colAUC(rfgam.test1.pred, test1$SeriousDlqin2yrs)
plot(roc(as.integer(ifelse(rfgam.train.pred>0.5,1,0)), train$SeriousDlqin2yrs,"1"))
roc(train$SeriousDlqin2yrs,rfgam.train.pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)
#GAM model
gamdata<-train[,-1];colnames(gamdata)[2:11]<-paste('var',1:10,sep='')
testdata<-test1[,c(-1,-2)];colnames(testdata)<-paste('var',1:10,sep='')
gam1<-gam(SeriousDlqin2yrs~s(var2,k=5)+s(var3,k=5)
          +s(var5,k=5)+s(var6,k=5)+s(var7,k=5)+s(var8,k=5)+s(var9,k=5)+s(var10,k=5),
          family=binomial(link = "logit"),data=gamdata,method="REML")
summary(gam1)
gam.test1.pred<-predict(gam1,newdata=testdata,type ="response")
gam.train.pred<-predict(gam1,type ="response")
plot(roc(as.integer(ifelse(gam.test1.pred<0.5,0,1)), test1$SeriousDlqin2yrs,"1"))
roc(test1$SeriousDlqin2yrs,gam.test1.pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)
#roc(test1$SeriousDlqin2yrs,0.3*gam.test1.pred+0.7*test1.rf.pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)
plot(roc(as.integer(ifelse(gam.train.pred>0.5,1,0)), train$SeriousDlqin2yrs,"1"))
roc(train$SeriousDlqin2yrs,gam.train.pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)
colAUC(gam.test1.pred,test1$SeriousDlqin2yrs)


#GAM+Tree
colnames(gamdata)[2:11]<-paste('var',1:10,sep='')
test.gamtree<-test1[,-1]
colnames(test.gamtree)[2:11]<-paste('var',1:10,sep='')
gamtree.tree<-gam(SeriousDlqin2yrs~s(var2,k=5)+s(var3,k=5)
          +s(var5,k=5)+s(var6,k=5)+s(var7,k=5)+s(var8,k=5)+s(var9,k=5)+s(var10,k=5)+s(var7,var1,k=10)
          +s(var9,var3,k=10)+s(var3,var5,k=10)+s(var1,var7,var3,var5,var9),
          family=binomial(link = "logit"),data=gamdata,method="REML")
gamtree<-gam(SeriousDlqin2yrs~s(var2,k=5)+
               s(var6,k=5)+s(var8,k=5)+s(var9,k=5)+s(var10,k=5)+s(var7,var1,k=5)
          +s(var3,var5,k=5)+s(var1,var7,var3),
          family=binomial(link = "logit"),data=gamdata,method="REML")
summary(gamtree)
gamtree.test1.pred<-predict(gamtree,newdata=test.gamtree,type ="response")
#gamtree.train.pred<-predict(gamtree,type ="response")
roc(test.gamtree$SeriousDlqin2yrs,gamtree.test1.pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)

dtree<-rpart(SeriousDlqin2yrs~.,data=gamdata,method = "class",
             parms = list(split = 'gini'),control = rpart.control(minsplit=30, minbucket=10, cp=0.005))
rpart.plot(dtree,type=2,faclen=T)
#plot(dtree);text(dtree, use.n = TRUE, fancy=T,cex = 0.8)
gam.pred<-function(traindata,testdata){
  gamdata<-traindata[,-1];colnames(gamdata)[2:11]<-paste('var',1:10,sep='')
  gamtest<-testdata[,c(-1,-2)];colnames(gamtest)<-paste('var',1:10,sep='')
  gamtree<-gam(SeriousDlqin2yrs~s(var2,k=5)+s(var6,k=5)+s(var8,k=3)+s(var9,k=3)
               +s(var10,k=2)+s(var7,var1,k=5)+s(var3,var5,k=5)+s(var1,var7,var3),
               family=binomial(link = "logit"),data=gamdata,method="REML")
  gamtree.test.pred<-predict(gamtree,newdata=gamtest,type ="response")
  gam.roc<-roc(testdata$SeriousDlqin2yrs,gamtree.test.pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)
  AUC<-gam.roc$auc  
  return(list(model=gamtree,AUC=AUC,gamtree.test.pred=gamtree.test.pred))
}
gam.pred(train,test1)
#combine
cbdata<-cbind(gamdata,pred)
cbtestdata<-cbind(test1[,-1],gam.pred)
cbgmadata<-cbind(train,pred)
cb.logmodel<-glm(SeriousDlqin2yrs~.,family=binomial(link='logit'),data=cbdata)
cb.reg<-step(cb.logmodel,direction=c("backward"))#var2+pred
cbpred<-predict(cb.reg,type ="response")
cbtestpred1<-predict(cb.reg,data=test1)
plot(roc(as.integer(cbtestpred1), test1$SeriousDlqin2yrs,"1"))#AUC=0.7892


plot(roc(as.integer(gam.pred), test1$SeriousDlqin2yrs,"1"))
gam.pred2<-predict(gam2,data=test1)
plot(roc(as.integer(gam.pred2), test1$SeriousDlqin2yrs,"1"))

#cluster
cl<-kmeans(scale(train[,2:12]),3)
plot(train, col = cl$cluster)
#decision tree
par(mfrow = c(1,1), xpd = NA)
tree<-train[,-1]
colnames(tree)[c(2,4,6,7:11)]<-c('balance','30-59Days','Income',
                                 'Credits.No','90Days','RealEstateLoans.No',
                               '60-89Days','Dependents.No')
#colnames(tree)[2:11]<-paste('var',1:10,sep='')
dtree<-rpart(SeriousDlqin2yrs~.,data=tree,method = "class",
             parms = list(split = 'gini'),control = rpart.control(minsplit=30, minbucket=10, cp=0.005))
rpart.plot(dtree,type=2,faclen=T)
#plot(dtree);text(dtree, use.n = TRUE, fancy=T,cex = 0.8)
dtree1<-rpart(SeriousDlqin2yrs~.,data=train[,-1],method = "class",parms = list(split = 'information'))
#plot(dtree1);text(dtree1, use.n = TRUE, cex = 0.8)

library(GAMBoost)
gamboost1<-gamboost(SeriousDlqin2yrs~.,data=gamdata, dfbase = 4,
         control = boost_control(mstop = 100))
gamboost1.pred<-predict(gamboost1,data=test1)
plot(roc(as.integer(gamboost1.pred), test1$SeriousDlqin2yrs,"1"))
gamdata<-train[,-1]
library(GAMBoost)
gb1 <- GAMBoost(as.matrix(gamdata[,2:11]),gamdata[,1],penalty=500,stepno=100,
                family=binomial(link = "logit"),trace=TRUE) 
gb1.pred<-predict(gb1,data=test1)
plot(roc(as.integer(gb1.pred), test1$SeriousDlqin2yrs,"1"))
