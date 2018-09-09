
library(data.table)
library(tidyverse)
library(ggthemr)
ggthemr('solarized')


#####描述性函数
mystats<-function(x,na.omit=FALSE){
  y=sum(is.na(x))
  if(na.omit)
    x<-x[!is.na(x)]
  l<-length(x)
  m<-mean(x)
  n<-sd(x)
  o<-median(x)
  p<-quantile(x,0.25)
  q<-quantile(x,0.75)
  r<-min(x)
  s<-max(x)
  return(c(n=l,na=y,mean=m,sd=n,median=o,median1=p,median3=q,min=r,max=s))
}

############

#1
fa<-fread("fasj.csv")

#人数
fa.rs=fa%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()


table(fa.rs$year)
table(fa.rs$MENGTYPE)

#靶向治疗
fa.bx=fa%>%
  filter(MENGTYPE=="肺癌靶向治疗")

table1=table(fa.bx$MED)%>%
  as.data.frame()


#普通门诊
fa.bx=fa%>%
  filter(MENGTYPE=="普通门诊")

table1=table(fa.bx$MED)%>%
  as.data.frame()

sum(table1[,"Var1"]=="克唑替尼胶囊")


table2=table(fa$MED)%>%
  as.data.frame()











