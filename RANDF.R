#cluster
train.cluster<-getResample(train,0.2,pct=2000/6000)
train.cluster<-scale(train.cluster[,c(-1,-2)],center = TRUE, scale = TRUE)
kmeans <- kmeans(train.cluster,centers=2)
kmeans$centers

data=train[1:600,]
function(data=,k=){
  data<-data[,c(-1,-2)]
  #data<-data[,c(-1)]
  model <- randomForest(data)
  # 将相似矩阵做为距离来绘制多维标度
  MDSplot(model,data[,1])
  # 根据相似矩阵来计算距离矩阵
  dist <- 1-model$proximity
  # 聚类分析
  clust <- pam(dist,k=k) 
  data$cluster<-clust$clustering
  
}
eD<-function(x,y){
  #Euclidean Distance
  #For multiple vectors in an array 
  #rows contain the k vectors
  #columns the n coordinates in the n-space
  #str(x) ==  matrix [1:k, 1:n]
  
  square <- (x-y)^2
  if(is.null(dim(square))){
    dim(square) <- c(1, length(square))
  }
  return(sqrt(rowSums(square)))
}


wyvalidityIndex<-function(dataset,k,clustMethod){
  data<-data.frame(scale(dataset[,c(-1,-2)], center = TRUE, scale = TRUE))
  if(clustMethod=='pam'){
  model <- randomForest(data)
  dist <- 1-model$proximity
  clust <- pam(dist,k)
  data$cluster<-clust$clustering
  }
  if(clustMethod=='kmeans'){
    clust <- kmeans(data,k)
    data$cluster<-clust$cluster
    clust$centers<-clust$centers
  } 
  intra<-0;intersum<-0;
  internum<-0;
  interlist<-list()
  for(i in 1:k){
    data.i<-data[data$cluster==i,];data.i$cluster<-NULL
    val<-data;val$cluster<-NULL
    eDist<-apply(data.i, 1, FUN=eD,y=colMeans(data.i))
    intra<-intra+(sum(eDist)/max(eDist))
    intersum<-intersum+eD(colMeans(data.i),colMeans(val))
    for(j in (i+1):k){
      if(j<k+1){
        data.j<-data[data$cluster==j,];data.j$cluster<-NULL
        ceD<-eD(colMeans(data.i),colMeans(data.j))
        interlist<-list(interlist,ceD)
        internum<-internum+ceD 
      }
    }
  }
  inter<-exp((-2/(k-1))*internum/intersum)
  intra<-intra/nrow(data)
  return(list(VI=inter*intra,intra=intra,inter=inter,cluster=clust))
}
wyvalidityIndex(train[1:200,] , k=3)$inter
maxc<-9
validity <- rep(NA, maxc)
all.cluster.rer <- list()
for(centers in 2:maxc){
  rfclu<-wyvalidityIndex(train[1:200,] , k=centers)
  validity[centers] <- rfclu$VI
  all.cluster.rer[[centers]] <-rfclu$cluster
}

plot(validity, type="l")

maxc <- 10
#FCM
validity <- rep(NA, maxc)
all.cluster.rer <- list()
for(centers in 2:maxc){
  cluster.rer<-cmeans(x=data, centers=centers, method="cmeans", m=2)
  validity[centers] <- validityIndex(cluster.rer , data)
  all.cluster.rer[[centers]] <- cluster.rer
}

plot(validity, type="l")#centers=3
fcm.clust<-cmeans(x=data, centers=3, method="cmeans", m=2)


clust.pred<-function(dataset,test,k,method){
  data<-data.frame(scale(dataset[,c(-1,-2)], center = TRUE, scale = TRUE))
  clust <- kmeans(data,k)
  data$cluster<-clust$cluster 
  test.dist0<-data.frame(matrix(rep(0,nrow(test)),nrow=nrow(test)))
  test.scale<-scale(test[,c(-1,-2)], center = TRUE, scale = TRUE)
  for(j in 1:k){
    test.dist0<-cbind(test.dist0,dist=data.frame(eD(test.scale,clust$centers[j,])))
  }
  test.dist<-test.dist0[,-1]
  colnames(test.dist)<-paste('dist',1:k,sep='')
  test.weight<-test.dist
  for(i in 1:nrow(test.dist)){
    test.weight[i,]<-test.dist[i,]/sum(test.dist[i,])
  }  
  clus.pred0<-data.frame(matrix(rep(0,nrow(test),nrow=nrow(test))))
  for(i in 1:k){
    data.i<-dataset[data$cluster==i,];data.i$cluster<-NULL
    if(method=='logistic'){
      test.log.pred<-log.pred(data.i,test)
      clus.pred0<-cbind(clus.pred0,data.frame(pred=test.log.pred$reg_test_pred))
    }
    if(method=='gam'){
      test.gam.pred<-gam.pred(data.i,test)
      clus.pred0<-cbind(clus.pred0,data.frame(pred=test.gam.pred$gamtree.test.pred))
    }
  }
  clus.pred<-clus.pred0[,-1]
  pred<-apply(test.weight*clus.pred,1,sum)
  roc(test$SeriousDlqin2yrs,pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)
}
dataset=train;test=test1;k=3;method='gam';
clust.pred(dataset=train,test=test1,k=3,method='logistic')
clust.pred(dataset=train,test=test1,k=3,method='gam')

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
gam.pred(data.i,test1)

clust.pred<-function(dataset,test,k,method,weight){
  data<-data.frame(scale(dataset[,c(-1,-2)], center = TRUE, scale = TRUE))
  clust <- kmeans(data,k)
  data$cluster<-clust$cluster 
  test.dist0<-data.frame(matrix(rep(0,nrow(test)),nrow=nrow(test)))
  test.scale<-scale(test[,c(-1,-2)], center = TRUE, scale = TRUE)
  for(j in 1:k){
    test.dist0<-cbind(test.dist0,dist=data.frame(eD(test.scale,clust$centers[j,])))
  }
  test.dist<-test.dist0[,-1]
  colnames(test.dist)<-paste('dist',1:k,sep='')
  test.weight<-test.dist
  for(i in 1:nrow(test.dist)){
    if(weight!=1){
      test.weight[i,]<-as.integer(test.dist[i,]==test.dist[i,which.max(test.dist[i,])])
    } else {
      test.weight[i,]<-test.dist[i,]/sum(test.dist[i,])
    }
  }  
  clus.pred0<-data.frame(matrix(rep(0,nrow(test),nrow=nrow(test))))
  for(i in 1:k){
    data.i<-dataset[data$cluster==i,];data.i$cluster<-NULL
    if(method=='logistic'){
      test.log.pred<-log.pred(data.i,test)
      clus.pred0<-cbind(clus.pred0,data.frame(pred=test.log.pred$reg_test_pred))
    }
    if(method=='gam'){
      test.gam.pred<-gam.pred(data.i,test)
      clus.pred0<-cbind(clus.pred0,data.frame(pred=test.gam.pred$gamtree.test.pred))
    }
  }
  clus.pred<-clus.pred0[,-1]
  pred<-apply(test.weight*clus.pred,1,sum)
  roc(test$SeriousDlqin2yrs,pred,plot=TRUE, print.thres=TRUE, print.auc=TRUE)
}
dataset=train;test=test1;k=3;method='gam';weight=1
clust.pred(dataset=train,test=test1,k=3,method='logistic',weight=1)
clust.pred(dataset=train,test=test1,k=3,method='logistic',weight=0)
clust.pred(dataset=train,test=test1,k=3,method='gam',weight=1)
clust.pred(dataset=train,test=test1,k=3,method='gam',weight=0)

