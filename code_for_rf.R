library(dplyr)

library(tidyr)

library(randomForest)

library(extraTrees)

library(pROC)

library(car)

library(cvTools)





setwd("/Users/HP/Downloads/Data/Data")





s.train = read.csv("store_train.csv" , stringsAsFactors = F)

s.test = read.csv("store_test.csv" , stringsAsFactors = F)



s.test$store = NA



s.train$data = "train"

s.test$data = "test"



s_all = rbind(s.train , s.test )



CreateDummies=function(data,var,freq_cutoff=100){
  
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
    
    name=gsub(">","GT_",name)
    
    name=gsub("=","EQ_",name)
    
    name=gsub(",","",name)
    
    name=gsub("/","_",name)
    
    data[,name]=as.numeric(data[,var]==cat)
    
  }
  
  
  
  data[,var]=NULL
  
  return(data)
  
}



s_all=s_all%>% 
  
  select(-countyname,-storecode,-Areaname,-countytownname,-Id,-state_alpha)





glimpse(s_all)



names(s_all)[sapply(s_all,is.character)]



for_dummy_vars=c('country','State','CouSub','store_Type')



for(var in for_dummy_vars){
  
  s_all=CreateDummies(s_all,var,50)
  
}



for(col in names(s_all)){
  
  
  
  if(sum(is.na(s_all[,col]))>0 & !(col %in% c("data","store"))){
    
    
    
    s_all[is.na(s_all[,col]),col]=mean(s_all[s_all$data=='train',col],na.rm=T)
    
  }
  
  
  
}



lapply(s_all,function(x) sum(is.na(x)))



s_train=s_all %>% filter(data=='train') %>% select(-data)

s_test=s_all %>% filter(data=='test') %>% select(-data,-store)



for_vif=lm(store~.,data=s_train)

sort(vif(for_vif),decreasing = T)[1:3]



for_vif=lm(store~.-sales0,data=s_train)

sort(vif(for_vif),decreasing = T)[1:3]



for_vif=lm(store~.-sales0-sales2,data=s_train)

sort(vif(for_vif),decreasing = T)[1:3]



for_vif=lm(store~.-sales0-sales2-sales3,data=s_train)

sort(vif(for_vif),decreasing = T)[1:3]





s_train$store=as.factor(s_train$store)



rf.model=randomForest(store~.-sales0-sales2-sales3,data = s_train,do.trace=T)



rf.tuned.model=randomForest(store~.-sales0-sales2-sales3,
                            data = s_train,
                            ntree=400,
                            mtry=15,
                            maxnodes=70,
                            nodesize=10,
                            do.trace=T)

