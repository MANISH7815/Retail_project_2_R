#######correct


train=read.csv("store_train.csv", stringsAsFactors = F)
test=read.csv("store_test.csv", stringsAsFactors = F)

test$store=NA                   

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

train$data='train'
test$data='test'

all=rbind(train,test)

glimpse(all)

table(all$storecode)

lapply(all,function(x) length(unique(x)))

names(all)[sapply(all,function(x) is.character(x))]


cat_cols=c("country","State","CouSub","store_Type")

for(cat in cat_cols){
  all=CreateDummies(all,cat,50)
}


glimpse(all)

## ------------------------------------------------------------------------
all=all %>%
  select(-Areaname,-countytownname,-Id,-state_alpha,-storecode,-countyname)

sum(sapply(all,function(x) is.character(x)))
# that tells us that there are no character columns remaining [ 1 comes for column 'data']

lapply(all,function(x) sum(is.na(x)))
for(col in names(all)){
  
  if(sum(is.na(all[,col]))>0 & !(col %in% c("data","store"))){
    
    all[is.na(all[,col]),col]=mean(all[all$data=='train',col],na.rm=T)
  }
} 

lapply(all,function(x) sum(is.na(x)))



glimpse(all)

train=all %>% filter(data=='train') %>% select(-data)
test=all %>% filter(data=='test') %>% select (-data,-store)
## ------------------------------------------------------------------------
set.seed(2)
s=sample(1:nrow(train),0.8*nrow(train))
train1=all[s,]
train2=all[-s,]



library(car)

for_vif=lm(store~.-sales0,data=train1)

sort(vif(for_vif),decreasing = T)[1:3]
#######################################################################################################################
summary(for_vif)

train$store=as.factor(train$store)

library(randomForest)
log_fit2=randomForest(store~.-sales0-sales2-sales3,data=train1)
test.score2=predict(log_fit2,newdata = train2,type='prob')[,2]
test.score2

roc(train2$store,test.score2)$auc

rf.fit=randomForest(store~.-sales0-sales2-sales3,data=train,na.action = na.roughfix)
tree.pred=predict(rf.fit,data=test,type = "prob")[,2]

write.csv(tree.pred,"NAMITA_GARHWAL_P2_part2.csv",row.names = F)