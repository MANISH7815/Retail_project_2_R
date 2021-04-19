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

for_vif=lm(store~.-Id-sales0-sales2-sales3,data=st_train1)

for_vif=lm(store~.-Id-sales0-sales2,data=st_train1)

sort(vif(for_vif),decreasing = T)[1:3]
##----------------------------------------------------------------------------------

log_fit=glm(store~.-Id-sales0-sales2-sales3,data=st_train1,family = "binomial")

##---------------------------------------------------------------------------------

log_fit=step(log_fit)

formula(log_fit)

log_fit=glm(store ~  + sales4 + State  + population + countyname_WorcesterCounty + 
              countyname_PenobscotCounty + storecode_METRO14460MM1120 + 
              state_alpha_WV + state_alpha_LA + state_alpha_AL + state_alpha_AR + 
              + state_alpha_PR + 
              + state_alpha_IN + state_alpha_TN  + 
              state_alpha_IL + state_alpha_MO + state_alpha_KY + 
              state_alpha_GA + state_alpha_CT + state_alpha_VT + state_alpha_NH + 
              state_alpha_MA,data=st_train1,family='binomial')


summary(log_fit)

##-------------------------------------------------------------------------------------

store_all$store=as.factor(store_all$store)
st_train1$store=as.factor(st_train1$store)
st_train2$store=as.factor(st_train2$store)

library(randomForest)

log_fit_rf=randomForest(store ~.-Id-sales0-sales2,data=st_train1)


test.score2=predict(log_fit_rf,newdata =st_train2,type='prob')[,2]
test.score2

roc(st_train2$store,test.score2)$auc