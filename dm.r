source("~/git/TeleSales/chenxuyuan/connection")
date<-format(Sys.Date()-1,format="%m-%d")
wd<-"~/git//TeleSales/dm_fenpei/"
setwd(wd)
sink("sink_log.txt")
log1<-try(source(paste(wd,"sqlofr.r",sep=""),encoding="utf-8"),silent=F)

connectbx(2)
connectbx(4)

checkbxdw<-function(){
  a<-dbGetQuery(connDW,"select * from dds.dim_city limit 10")
  b<-dbGetQuery(conn,"select * from ods.dim_city limit 10")
  if(nrow(a)>3){
    print("pgsql连接上了")
  }else{print("pgsql连接失败")}
  if(nrow(b)>3){
    print("hive连接上了")
  }else{print("hive连接失败")}
}
checkbxdw()

#simpledata通常是一行sql直接跑出来的,比如dim
setClass("simpledata",
         slots=list(
           sql="character",
           data="data.frame",
           description="character")
)

##hivedata是需要用hive跑出来的sql,prototype中的需要与cxy_daily_filter中的sql一致
setClass("hivedata",
         contains="simpledata",
         slots=list(
           dbtable="character",
           day="character",
           sql_raw="character"),
         prototype=list(dbtable = "hivetemp.cxy_dm_data")
)

#bxdata通常是需要输入对象数据库名称和日期参数的
setClass("bxdata",
         contains="simpledata",
         slots=list(
           dbtable="character",
           day="numeric",
           sql_raw="character"),
         prototype=list(dbtable = "pgtemp.cxy_dm_data") #测试的时候改成pgtemp.cxy_test
)
#castdata指的是需要用reshape包cast一下的
setClass("castdata",
         contains="bxdata",
         slots=list(
           cast_type="character"),
         prototype=list(cast_type="id~type") #本项目id代表每个电话
)

#hivecastdata指的是需要用reshape包cast一下的
setClass("hivecastdata",
         contains="hivedata",
         slots=list(
           cast_type="character"),
         prototype=list(cast_type="id~type") #本项目id代表每个电话
)


#设定运行sql泛函接口
setGeneric("runsql",function(obj,...) standardGeneric("runsql"))
#设定现实
setMethod("runsql","simpledata",function(obj,...){
  connectbx(2,jar_locate=jar_locate)
  time<-system.time(obj@data<-dbGetQuery(connDW,obj@sql))
  print(paste(Sys.time(),obj@description,"运行完毕"," 运行时间：",round(time[3],2)))
  save.image()
  return (obj)
  #print(dbGetQuery(connDW,obj@sql))
}
)
#bxdata的现实
setMethod("runsql","bxdata",function(obj,...){
  if(length(obj@sql)==0){ #如果sql不能拼接，支持直接写进来,也可以换成simpledata
    n=length(obj@day) 
    temp_day<-array(c(1:(6-n))) #最长支持六个日期
    obj@day<-c(obj@day,temp_day)
    #通过一个很2的方式把日期合并进来
    obj@sql<-sprintf(obj@sql_raw,obj@dbtable,obj@day[1],obj@day[2],obj@day[3],obj@day[4],obj@day[5],obj@day[6])       
  }
  connectbx(2,jar_locate=jar_locate)
  time<-system.time(obj@data<-dbGetQuery(connDW,obj@sql))
  print(paste(Sys.time(),obj@description,"运行完毕"," 运行时间：",round(time[3],2)))
  save.image()
  return (obj)
  #print(dbGetQuery(connDW,obj@sql))
  
}
)
#castdata的现实
setMethod("runsql","castdata",function(obj,...){
  if(length(obj@sql)==0){ #如果sql不能拼接，支持直接写进来,也可以换成simpledata
    n=length(obj@day) 
    temp_day<-array(c(1:(6-n))) 
    obj@day<-c(obj@day,temp_day)
    #通过一个很2的方式把日期合并进来
    obj@sql<-sprintf(obj@sql_raw,obj@dbtable,obj@day[1],obj@day[2],obj@day[3],obj@day[4],obj@day[5],obj@day[6])       
  }
  connectbx(2,jar_locate=jar_locate)
  time<-system.time(temp_data<-dbGetQuery(connDW,obj@sql))
  obj@data<-as.data.frame(meltcast(temp_data))
  print(paste(Sys.time(),obj@description,"运行完毕"," 运行时间：",round(time[3],2)))
  save.image()
  return (obj)
  #print(dbGetQuery(connDW,obj@sql))
}
)

#hivedata的现实
setMethod("runsql","hivedata",function(obj,...){
  if(length(obj@sql)==0){ #如果sql不能拼接，支持直接写进来,也可以换成simpledata
    n=length(obj@day) 
    temp_day<-array(c(1:(6-n))) #最长支持六个日期
    obj@day<-c(obj@day,temp_day)
    #通过一个很2的方式把日期合并进来
    obj@sql<-sprintf(obj@sql_raw,obj@dbtable,obj@day[1],obj@day[2],obj@day[3],obj@day[4],obj@day[5],obj@day[6])       
  }
  connectbx(4)
  time<-system.time(obj@data<-dbGetQuery(conn,obj@sql))
  print(paste(Sys.time(),obj@description,"运行完毕"," 运行时间：",round(time[3],2)))
  save.image()
  return (obj)
}
)

#hivecastdata的现实
setMethod("runsql","hivecastdata",function(obj,...){
  if(length(obj@sql)==0){ #如果sql不能拼接，支持直接写进来,也可以换成simpledata
    n=length(obj@day) 
    temp_day<-array(c(1:(6-n))) 
    obj@day<-c(obj@day,temp_day)
    #通过一个很2的方式把日期合并进来
    obj@sql<-sprintf(obj@sql_raw,obj@dbtable,obj@day[1],obj@day[2],obj@day[3],obj@day[4],obj@day[5],obj@day[6])       
  }
  connectbx(4)
  time<-system.time(temp_data<-dbGetQuery(conn,obj@sql))
  obj@data<-as.data.frame(meltcast(temp_data))
  print(paste(Sys.time(),obj@description,"运行完毕","运行时间：",round(time[3],2)))
  save.image()
  return (obj)
  #print(dbGetQuery(connDW,obj@sql))
}
)

meltcast<-function(data){
  id_name<-names(data)[1]
  col_name<-names(data)[2]
  data[col_name]<-paste(col_name, c(data[col_name])[[1]],sep="_")
  data2<-melt(data,id=c(id_name,col_name))
  expr<-sprintf("cast(data2,%s~%s+variable)",id_name,col_name)
  eval(parse(text=expr))
}

#设定输出description泛函接口
setGeneric("des",function(obj,...) standardGeneric("des"))
#设定现实
setMethod("des","simpledata",function(obj,...){
  print(obj@description)
  if(nrow(obj@data)>1){ #前两行和最后两行
    print(rbind(head(obj@data,2),tail(obj@data,2)))
  }else{print("无数据，可能是runsql尚未运行")}
}
)


library(reshape)
#开始创建对象啦
#确定本次运行的对象
setwd("/home/chenxuyuan/杂/")

duixiang<-new("bxdata",
              sql_raw=sql.duixiang,
              description="本次运行的对象")
duixiang<-runsql(duixiang)
#des(duixiang)
#把对象放到hivetemp中
#dbWriteTable(conn,"hivetemp.cxy_dm_data",duixiang)

call_length<-new("bxdata",
                 sql_raw=sql.call_length,
                 description="电话总时长和总通话量"
)
call_length<-runsql(call_length)

gezi_match<-new("hivedata",
                sql_raw=sql.gezi_match,
                day=c('20140501'),
                description="把hivetemp中的filter+callinfo inner join ad表，拉上来做的用户格子匹配，这张表的用户是偏多的"
)
gezi_match<-runsql(gezi_match)

#发帖城市与类目数
ad_city_category_num<-new("hivedata",
                          sql_raw=sql.city_category_num,
                          description="这些用户发帖的城市数和类目数"
)
ad_city_category_num<-runsql(ad_city_category_num)

ad_type<-new("hivecastdata",
             sql_raw=sql.ad_type,
             description="ad表,包含posttype的信息"
)
ad_type<-runsql(ad_type)


order_service<-new("hivecastdata",
                   sql_raw=sql.order_service,
                   description="order表，把service_type_id作为列"
)
order_service<-runsql(order_service)

order_service_30<-new("hivecastdata",
                      sql_raw=sql.order_service_30,
                      description="30天order表，把service_type_id作为列"
)
order_service_30<-runsql(order_service_30)

order_service_all<-new("hivecastdata",
                       sql_raw=sql.order_service_all,
                       description="所有天数order表，把service_type_id作为列"
)
order_service_all<-runsql(order_service_all)


quan_created<-new("bxdata",
                  sql_raw=sql.quan_created,
                  description="得到券的数量和金额"
)
quan_created<-runsql(quan_created)


quan_used<-new("bxdata",
               sql_raw=sql.quan_used,
               description="用掉的券的数量和金额"
)
quan_used<-runsql(quan_used)


ticket<-new("bxdata",
            sql_raw=sql.ticket,
            description="充值的数量和金额"
)
ticket<-runsql(ticket)

card<-new("bxdata",
          sql_raw=sql.card,
          description="购买一卡通的数量和金额"
)
card<-runsql(card)

yue<-new("hivedata",
         sql_raw=sql.yue,
         description="前一天的余额"
)
yue<-runsql(yue)


card_yue<-new("hivedata",
              sql_raw=sql.card_yue,
              description="前一天的一卡通余额"
)
card_yue<-runsql(card_yue)


ticket_fail<-new("bxdata",
                 sql_raw=sql.ticket_fail,
                 description="充值失败的次数"
)
ticket_fail<-runsql(ticket_fail)

banjia<-new("hivedata",
            sql_raw=sql.banjia,
            description="搬家大师的数据")
banjia<-runsql(banjia)

xinxilianmeng<-new("hivedata",
                   sql_raw=sql.xinxilianmeng,
                   description="信息联盟的数据")
xinxilianmeng<-runsql(xinxilianmeng)

#设置时间范围
date_monthago<-format(Sys.Date()-120,format="%Y-%m-%d")
crawler_58<-new("hivedata",
                sql_raw=sql.crawler_58,
                day=c(date_monthago,date_monthago),
                description="58爬虫的数据"
)
crawler_58<-runsql(crawler_58)

crawler_ganji<-new("hivedata",
                   sql_raw=sql.crawler_ganji,
                   day=c(date_monthago,date_monthago),
                   description="ganji爬虫的数据"
)
crawler_ganji<-runsql(crawler_ganji)

user_created_date<-new("hivedata",
                       sql_raw=sql.user_created_date,
                       description="用户注册的时间"
)
user_created_date<-runsql(user_created_date)


user_first_pay_date<-new("hivedata",
                         sql_raw=sql.user_first_pay_date,
                         description="用户第一次消费的时间"
)
user_first_pay_date<-runsql(user_first_pay_date)

ever_success<-new("bxdata",
                  sql_raw=sql.ever_success,
                  description="客户类型：成多次oldcustomer,成单一次first_success,未成单neversuccess")
ever_success<-runsql(ever_success)

success<-new("bxdata",
             sql_raw=sql.success,
             description="是否成功"
)
success<-runsql(success)



#把众多数据都merge起来
duixiang1<-duixiang@data
j<-1
for(i in ls()){
  
  if(class(get(i))[1] %in% c("bxdata","castdata","hivedata","hivecastdata") & i!=as.character(quote(duixiang))){
    if( nrow(get(i)@data)>1 ){
      if(colnames(get(i)@data)[1]=="id"){
        initial_data<-get(paste("duixiang",j,sep=""))
        mergedata<-get(i)@data
        mymerge<-merge(get(paste("duixiang",j,sep=""),),mergedata,by="id",all.x=T)
        j+1
        assign(paste("duixiang",j,sep=""),mymerge)
      }
    } 
  }
}
duixiang_final<-get(paste("duixiang",j,sep=""))

#格子的信息比较特殊，不能按照id来merge
connectbx(2,jar_locate=jar_locate)
gezi_info<-dbGetQuery(connDW,sql.gezi_info)

#化为factor
gezi_info$city_size_2014<-factor(gezi_info$city_size_2014,levels=c("很大","大","中等","小","很小"),ordered =T)
gezi_info$top_category_name_en<-factor(gezi_info$top_category_name_en,ordered =F)
#gezi_info<-gezi_info[-(ncol(gezi_info))] #估计是因为city后面才出的问题，先不要了
#gezi_info<-gezi_info[-(ncol(gezi_info))]

duixiang_final2<-merge(duixiang_final,gezi_info,by=c("city_name_en","category_name_en"),all.x=T)

#把匹配不上格子的用户删掉（为什么匹配不上呢，之后查一下）
duixiang_final2<-subset(duixiang_final2,is.na(gezi_rev)==F)

#存档
setwd("/home/ftpusers/ftpbxcrm/bxin/dailyadd")
save(duixiang_final2,file="duixiang_final2.rda")
#load("duixiang_final2.rda")

print(Sys.time())
print("数据存好了")

dataclean<-duixiang_final2
dataclean[is.na(dataclean)]<-0

#call的日期做下限制
julian_7days<-format(Sys.Date()-7,format="%Y%m%d")
julian_90days<-format(Sys.Date()-90,format="%Y%m%d")

#为了发到网页版
mydata<-subset(dataclean,id<0 & julian_date<julian_7days &julian_date>julian_90days)
mydata$user_id<-mydata$user_id-5438
save(mydata,file="mydata.rda")
write.csv(mydata,file="mydata.csv",fileEncoding="GBK",row.names=F,quote=F)


call_sub<-subset(dataclean,id<0 & julian_date<julian_7days &julian_date>julian_90days &ever_sucess=="never_success")
#call_sub<-subset(dataclean,id<0 & julian_date<julian_7days &julian_date>julian_90days)
#call_sub<-subset(dataclean,id<0 & julian_date<julian_7days &julian_date>julian_90days)
filter_sub<-subset(dataclean,id>0)
call<-call_sub[-c(1,2,3,4,5)]
filter<-filter_sub[-c(1,2,3,4,5)]

#该删除的变量都删除
a<-which(colnames(call) %in% c("success_money","post_type_NA_ad_cnt","user_created_date","user_first_pay_date","ever_sucess"))
#a<-which(colnames(call) %in% c("top_category_name_en","city_size_2014","ever_sucess"))
call<-call[-a]
filter<-filter[-a]


library(caTools)
set.seed(88)
split <- sample.split(call$success, SplitRatio = 0.75)
call.train <- subset(call, split == TRUE)
call.test <- subset(call, split == FALSE)

# #拟合与预测 glm
# trainglm <- glm(success ~ ., data = call.train, family=binomial)
# call.testglm <- predict(trainglm, type="response", newdata=call.test)
# table(call.test$success, call.testglm > 0.5)
library(randomForest)

#一般周六不需要分数据，周六跑吧
if(weekdays(Sys.Date())=="Saturday"){
  trainrf <- randomForest(success~.,data=call.train)
  save(trainrf,file="trainrf.rda")
  print("trainrf.rda保存完毕")
}else{
  load("trainrf.rda")
  print("trainrf.rda读取完毕")
}

summary(trainrf)
call.testrf <- predict(trainrf,newdata=call.test)
table(call.test$success, call.testrf > 0.05)
filterrf <- predict(trainrf, newdata=filter)
filterall<- data.frame(id=filter_sub$id,success=filterrf)
save(filterall,file="filterall.rda")

##############success_money
if(1){
  call<-call_sub[-c(1,2,3,4,5)]
  filter<-filter_sub[-c(1,2,3,4,5)]
  
  a<-which(colnames(call) %in% c("success","post_type_NA_ad_cnt","user_created_date","user_first_pay_date","ever_sucess"))
  #a<-which(colnames(call) %in% c("top_category_name_en","city_size_2014","ever_sucess"))
  call<-call[-a]
  filter<-filter[-a]
  
  set.seed(88)
  split <- sample.split(call$success_money>0, SplitRatio = 0.75)
  call.train <- subset(call, split == TRUE)
  call.test <- subset(call, split == FALSE)
  
  trainlm_money <- lm(success_money ~ ., data = call.train)
  summary(trainlm_money)
  #预测未来的充值
  filterlm <- predict(trainlm_money, newdata=filter)
  filterall_money<- data.frame(id=filter_sub$id,success_money=filterlm)
  save(filterall_money,file="filterall_money.rda")
}


print(Sys.time())
print("第二个文件运行完毕")
sink()
