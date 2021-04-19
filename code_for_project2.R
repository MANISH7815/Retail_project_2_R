getwd()

setwd("C:\\Users\\satis\\OneDrive\\Desktop\\Edvancer\\R prog\\Retail_project_2")

st_train=read.csv("store_train.csv",stringsAsFactors = F)
st_test=read.csv("store_test.csv",stringsAsFactors = F)

table(st_train$Areaname)


table(st_train$Areaname)
library(dplyr)
library(tidyr)
glimpse(st_train)

sum_of_sale=st_train %>% 
  filter(st_train$store_Type=='Supermarket Type1',st_train$Areaname=='Kennebec County, ME') %>% 
  mutate(sum=sales0+sales1+sales2+sales3+sales4)


a=data.frame(unique(st_train$Areaname))

##-------------------------------------------------------------------------------------------
response_rate=st_train %>% 
  filter(st_train$store_Type=="Grocery Store",st_train$store==1)


response_rate_T=st_train %>% 
  filter(st_train$store_Type=="Grocery Store",st_train$store==0)

unique(st_train$store_Type)
##---------------------------------------------------------------------------------------------
table(st_train$store_Type)

sum_of_sale_store1=st_train %>% 
  filter(st_train$store_Type=='Supermarket Type1') %>% 
  mutate(sum=sales0+sales1+sales2+sales3+sales4)
##----------------------------------------------------------------------------------------------

table(st_train$state_alpha)

unique(st_train$state_alpha)

library(tidyr)
library(ggplot2)
y=st_train$sales4
ggplot(st_train,aes(x=sales4))+geom_histogram()+geom_density()

mean(st_train$sales4)

total_sale=st_train %>% 
  mutate(total=sales0+sales1+sales2+sales3+sales4)

sum(total_sale$total)      #14377231

median(total_sale$total)

##------------------------------------------------------------------------------
library(psych)
mean(total_sale$total)
median(total_sale$total)
min(total_sale$total)
max(total_sale$total)
var(total_sale$total)
sd(total_sale$total)
IQR(total_sale$total)

summary((total_sale$total))

Q1=3422
Q3=4969
iqr=1546.75

out1=Q1-(1.5*iqr)
#1101.875
out3=Q3+(1.5*iqr)
#7289.125
out1+out3

table((total_sale$store_Type))

tapply(total_sale$total,total_sale$store_Type=="Grocery Store",var)

tapply(total_sale$total,total_sale$store_Type=="Supermarket Type1",var)

tapply(total_sale$total,total_sale$store_Type=="Supermarket Type2",var)

tapply(total_sale$total,total_sale$store_Type=="Supermarket Type3",var)


table(st_train$state_alpha)




