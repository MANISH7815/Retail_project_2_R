getwd()

setwd("C:\\Users\\satis\\OneDrive\\Desktop\\Edvancer\\R prog\\Retail_project_2")

st_train=read.csv("store_train.csv",stringsAsFactors = F)
st_test=read.csv("store_test.csv",stringsAsFactors = F)


library(tidyr)
library(car)
library(tree)
library(dplyr)
library(randomForest)
library(ISLR)
library(cvTools)

glimpse(st_train)

st_test$store=NA

st_train$data='train'
st_test$data='test'

#binding column ---------------------------------------------------
store_all=rbind(st_train,st_test)

glimpse(store_all)

#now creating dummies-----------------------------------------------------------------

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

#now finding the var which contain character init ----------------------------------------------

names(store_all)[sapply(store_all,function(x) is.character(x))]
##-----------------------------------------------------------------------------

cat_cols=c("countyname","storecode","Areaname","countytownname","state_alpha","store_Type")



for(cat in cat_cols){
  store_all=CreateDummies(store_all,cat,100)
}

glimpse(store_all)

#we have created dummy variable  

for(col in names(store_all)){
  if(sum(is.na(store_all[,col]))>0 & !(col %in% c("data","store"))){
    store_all[is.na(store_all[,col]),col]=mean(store_all[store_all$data=='train',col],na.rm=T)
  }
}

#store_all$store=as.factor(store_all$store)

## separate train and test

st_train=store_all %>% 
  filter(data=='train') %>% 
  select(-data)

st_test=store_all %>% 
  filter(data=='test') %>% 
  select(-data,-store)

##------------------------------------------------------------------------------------------



#now modelling of data 


set.seed(2)
s=sample(1:nrow(st_train),0.8*nrow(st_train))
st_train1=st_train[s,]
st_train2=st_train[-s,]

library(car)
library(tree)

for_vif=lm(store~.-Id,data=st_train1)

sort(vif(for_vif),decreasing = T)[1:3]
##----------------------------------------------------------------------------------

log_fit=glm(store~.-Id,data=st_train1,family = "binomial")

##---------------------------------------------------------------------------------

log_fit=step(log_fit)



summary(log_fit)

##-------------------------------------------------------------------------------------

store_all$store=as.factor(store_all$store)
st_train1$store=as.factor(st_train1$store)
st_train2$store=as.factor(st_train2$store)

##----------------------------------------------------------------------------
library(randomForest)

log_fit_rf=randomForest(store ~.-Id,data=st_train1)


test.score2=predict(log_fit_rf,newdata =st_train2,type='prob')[,2]
test.score2

pROC::roc(st_train2$store,test.score2)$auc

#random forest modelling----------------------------------------------------------------------

param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)
mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

subset_paras=function(full_list_para,n=10){
  all_comb=expand.grid(full_list_para)
  s=sample(1:nrow(all_comb),n)
  subset_para=all_comb[s,]
  return(subset_para)
}


num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myerror=9999999

myauc=0

for(i in 1:num_trials){
  print(paste('starting iteration :',i))
  params=my_params[i,]
  
  k=cvTuning(randomForest,store~.-Id,
             data =st_train,
             tuning =params,
             folds = cvFolds(nrow(st_train), K=10, type ="random"),
             cost =mycost_auc, seed =2
             #predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    
    myauc=score.this
    print(myauc)
    
    best_params=params
  }
  
  print('DONE')
 
}

myauc

#mtry  ntree maxnodes nodesize
#5      500      100        10


st.rf.tuned.model=randomForest(store~.-Id,
                            data = st_train,
                            ntree=500,
                            mtry=5,
                            maxnodes=100,
                            nodesize=10,
                            do.trace=T)


test.score=predict(st.rf.tuned.model,newdata =st_test,type='prob')[,2]

test.score=predict(st.rf.tuned.model,newdata =st_test)


write.csv(test.score,'firstName_LastName12_P2_part2.csv',row.names = F)

#-----------------------------------------------


prob_3=read.csv("firstName_LastName_P2_part2.csv",stringsAsFactors = F)

library(dplyr)
library(tidyr)

prob_3=prob_3 %>% 
  mutate(prob=ifelse(store>=0.5,1,0))

table(prob_3$prob)

table(prob_1)

