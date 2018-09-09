

library(tidyverse)
library(ggthemr)
ggthemr('solarized')
library(haven)

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


#导入数据
tnb.jz <- read_sas("tnb.sas7bdat")
tnb <- read_sas("mz2013.sas7bdat")
data.tnb100<-head(tnb,100)


#13年人数
#总人数
tnb.rs=tnb%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

#分别普通门诊和门慢的人数
tnb.rs1=tnb%>%
  group_by(MENGTYPE,COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

tnb.rs2=tnb.rs1%>%
  group_by(MENGTYPE)%>%
  summarise(人数=n())

#同时在普通门诊和糖尿病门慢就诊的人数
tnb.rs3=tnb.rs1%>%
  filter(MENGMAN==1)%>%
  select(COMPID)%>%
  inner_join(tnb.rs1%>%
               filter(MENGMAN==0),by="COMPID")


#合计tnb性别分布
tnb.rs.gender=tnb.rs%>%
  group_by(GENDER)%>%
  summarise(人数=n())%>%
  mutate(比例=round(人数/sum(人数),4))

#门慢性别分布
tnb.rs.gender1=tnb.rs1%>%
  filter(MENGMAN==1)%>%
  group_by(GENDER)%>%
  summarise(人数=n())%>%
  mutate(比例=round(人数/sum(人数),4))

#非门慢性别分布
tnb.rs.gender2=tnb.rs1%>%
  anti_join(tnb.rs1%>%filter(MENGMAN==1),by="COMPID")%>%
  group_by(GENDER)%>%
  summarise(人数=n())%>%
  mutate(比例=round(人数/sum(人数),4))
    

#合计tnb年龄
tnb.rs.age=tnb.rs%>%
  select(age)

mystats(tnb.rs.age$age,na.omit = T)
#门慢tnb年龄
tnb.rs.age1=tnb.rs1%>%
  filter(MENGMAN==1)%>%
  select(age)
mystats(tnb.rs.age1$age,na.omit = T)
#非门慢tnb年龄
tnb.rs.age2=tnb.rs1%>%
  anti_join(tnb.rs1%>%
              filter(MENGMAN==1),by="COMPID")%>%
  select(age)

mystats(tnb.rs.age2$age,na.omit = T)

#tnb用药情况
tnb.yy=tnb.rs%>%
  select(COMPID)%>%
  inner_join(tnb.jz%>%
               filter(year==2013),by="COMPID")

table.tnb=tnb.yy%>%
  group_by(MENGTYPE,MED)%>%
  summarise(开药次数=n())

tt1=table.tnb[grep("他汀",as.vector(as.matrix(as.data.frame(table.tnb[,"MED"])))),]

for(i in 1:nrow(table.tnb)){
  if(grepl("他汀",table.tnb[i,"MED"])) table.tnb[i,"药品类别"]<-"他汀类"
  if((grepl("双胍",table.tnb[i,"MED"]))|(grepl("脲",table.tnb[i,"MED"]))|(grepl("吡嗪",table.tnb[i,"MED"]))|(grepl("列奈",table.tnb[i,"MED"]))|(grepl("酮",table.tnb[i,"MED"]))|(grepl("波糖",table.tnb[i,"MED"]))|(grepl("列汀",table.tnb[i,"MED"]))) table.tnb[i,"药品类别"]="口服降糖药"
  if((grepl("艾塞那肽",table.tnb[i,"MED"]))) table.tnb[i,"药品类别"]="GLP-1受体激动剂"
  if(grepl("胰岛素",table.tnb[i,"MED"])) table.tnb[i,"药品类别"]="胰岛素类"
  if((grepl("匹林",table.tnb[i,"MED"]))|(grepl("格雷",table.tnb[i,"MED"]))) table.tnb[i,"药品类别"]="抗血小板类"
}


table.tnb1=table.tnb%>%
  group_by(MENGTYPE,药品类别)%>%
  summarise(开药总次数=sum(开药次数))

table.tnb2=na.omit(table.tnb1)
table.tnb2$MENGTYPE[table.tnb2$MENGTYPE=="糖尿病门诊药物治疗"]<-"门慢糖尿病"
ggplot(table.tnb2,aes(reorder(药品类别,开药总次数),y=开药总次数,fill=MENGTYPE))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  ggtitle("糖尿病患者门慢与普通门诊的处方情况比较")+
  guides(fill=guide_legend(title="MENGTYPE"))+
  facet_grid(.~MENGTYPE)+
  coord_flip()+
  ylab("处方量")+
  xlab("")+
  theme(axis.title = element_text(),legend.position='none')+ 
  geom_text(aes(label = 开药总次数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.95), size = 4)

#不区分门慢和普通门诊
table.tnb3=table.tnb2%>%
  group_by(药品类别)%>%
  summarise(开药总次数=sum(开药总次数))


#########################################
#糖尿病门慢患者用药
tnb.mm.yy=tnb.rs3%>%
  select(COMPID)%>%
  inner_join(tnb.jz%>%
               filter(year==2013),"COMPID")

tnb.mm.yy1=tnb.mm.yy%>%
  group_by(MENGTYPE,MED)%>%
  summarise(开药次数=n())


for(i in 1:nrow(tnb.mm.yy1)){
  if(grepl("他汀",tnb.mm.yy1[i,"MED"])) tnb.mm.yy1[i,"药品类别"]<-"他汀类"
  if((grepl("双胍",tnb.mm.yy1[i,"MED"]))|(grepl("脲",tnb.mm.yy1[i,"MED"]))|(grepl("吡嗪",tnb.mm.yy1[i,"MED"]))|(grepl("列奈",tnb.mm.yy1[i,"MED"]))|(grepl("酮",tnb.mm.yy1[i,"MED"]))|(grepl("波糖",tnb.mm.yy1[i,"MED"]))|(grepl("列汀",tnb.mm.yy1[i,"MED"]))) tnb.mm.yy1[i,"药品类别"]="口服降糖药"
  if((grepl("艾塞那肽",tnb.mm.yy1[i,"MED"]))) tnb.mm.yy1[i,"药品类别"]="GLP-1受体激动剂"
  if(grepl("胰岛素",tnb.mm.yy1[i,"MED"])) tnb.mm.yy1[i,"药品类别"]="胰岛素类"
  if((grepl("匹林",tnb.mm.yy1[i,"MED"]))|(grepl("格雷",tnb.mm.yy1[i,"MED"]))) tnb.mm.yy1[i,"药品类别"]="抗血小板类"
}


tnb.mm.yy2=tnb.mm.yy1%>%
  group_by(MENGTYPE,药品类别)%>%
  summarise(开药总次数=sum(开药次数))

tnb.mm.yy3=na.omit(tnb.mm.yy2)
tnb.mm.yy3$MENGTYPE[tnb.mm.yy3$MENGTYPE=="糖尿病门诊药物治疗"]<-"门慢糖尿病"

ggplot(tnb.mm.yy3,aes(reorder(药品类别,开药总次数),y=开药总次数,fill=MENGTYPE))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  ggtitle("糖尿病门慢患者门慢与普通门诊处方情况比较")+
  guides(fill=guide_legend(title="MENGTYPE"))+
  facet_grid(.~MENGTYPE)+
  coord_flip()+
  ylab("处方量")+
  xlab("")+
  theme(axis.title = element_text(),legend.position='none')+ 
  geom_text(aes(label = 开药总次数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.95), size = 4)



#######################################################
#用药人数的统计
tnb.tt=tnb.rs%>%
  select(COMPID)%>%
  left_join(tnb.jz%>%
              filter(year==2013),by="COMPID")


tnb.tt.rs=tnb.tt[grep("他汀",as.vector(as.matrix(as.data.frame(tnb.tt[,"MED"])))),]

tnb.tt.rs1=tnb.tt.rs%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

tnb.tt.rs2=tnb.tt.rs%>%
  group_by(COMPID)%>%
  summarise(使用他汀类次数=n())

tnb.tt.rs3=table(tnb.tt.rs2$使用他汀类次数)%>%
  as.data.frame()%>%
  rename(开药次数=Var1,人数=Freq)


tnb.tt.rs3$开药次数=as.numeric(as.character(tnb.tt.rs3$开药次数))
for(i in 1:nrow(tnb.tt.rs3)){
  if(tnb.tt.rs3[i,1]<=2) tnb.tt.rs3[i,"开药次数1"]<-"1-2次"
  if((tnb.tt.rs3[i,1]<=5)&(tnb.tt.rs3[i,1]>2)) tnb.tt.rs3[i,"开药次数1"]<-"3-5次"
  if((tnb.tt.rs3[i,1]<10)&(tnb.tt.rs3[i,1]>5)) tnb.tt.rs3[i,"开药次数1"]<-"6-9次"
  if(tnb.tt.rs3[i,1]>=10) tnb.tt.rs3[i,"开药次数1"]<-"10次以上"
}

tnb.tt.rs4=tnb.tt.rs3%>%
  group_by(开药次数1)%>%
  summarise(人数1=sum(人数))

myLabel=as.vector(tnb.tt.rs4$开药次数1)
myLabel = paste(tnb.tt.rs4$人数1,"(", round(tnb.tt.rs4$人数1 / sum(tnb.tt.rs4$人数1) * 100, 2), "%", ")",sep = "")   


ggplot(tnb.tt.rs4,aes(reorder(开药次数1,-人数1),y=人数1))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = myLabel), vjust = -0.2,hjust=.4, colour = "black", position = position_dodge(.95), size = 4)


hh4=data.frame(x=c("他汀类开药人数","其他人数"),y=c(12172,141046),z=c(round(12172/153218,4),round(141046/153218,4)))
myLabel=as.vector(hh4$x)
myLabel = paste( round(hh4$y / sum(hh4$y) * 100, 2), "%", sep = "")   

#饼图1
ggplot(hh4,aes(x='',y=y,fill=x))+
  geom_bar(stat="identity",width = 1)+
  coord_polar(theta = 'y')+
  labs(x='',y='',title='')+
  theme(axis.ticks = element_blank(),axis.line = element_blank())+
  theme(legend.title = element_blank(), legend.position = "top",panel.background = element_blank())+
  theme(axis.text.x = element_blank())+
  geom_text(aes(y = hh4$y/2 + c(0, cumsum(hh4$y)[-length(hh4$y)]), x = sum(hh4$y)/130000, label = myLabel))






########################################
#门慢患者使用他汀类药物人数

tnb.mm.tt=tnb.rs1%>%
  filter(MENGMAN==1)%>%
  select(COMPID)%>%
  left_join(tnb.jz%>%
              filter(year==2013),by="COMPID")

tnb.mm.tt.rs=tnb.mm.tt[grep("他汀",as.vector(as.matrix(as.data.frame(tnb.mm.tt[,"MED"])))),]

tnb.mm.tt.rs1=tnb.mm.tt.rs%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

tnb.mm.tt.rs2=tnb.mm.tt.rs%>%
  group_by(COMPID)%>%
  summarise(使用他汀类次数=n())

sum(tnb.mm.tt.rs2$使用他汀类次数)
mean(tnb.mm.tt.rs2$使用他汀类次数)


tnb.mm.tt.rs3=table(tnb.mm.tt.rs2$使用他汀类次数)%>%
  as.data.frame()%>%
  rename(开药次数=Var1,人数=Freq)



tnb.mm.tt.rs3$开药次数=as.numeric(as.character(tnb.mm.tt.rs3$开药次数))
for(i in 1:nrow(tnb.mm.tt.rs3)){
  if(tnb.mm.tt.rs3[i,1]<=2) tnb.mm.tt.rs3[i,"开药次数1"]<-"1-2次"
  if((tnb.mm.tt.rs3[i,1]<=5)&(tnb.mm.tt.rs3[i,1]>2)) tnb.mm.tt.rs3[i,"开药次数1"]<-"3-5次"
  if((tnb.mm.tt.rs3[i,1]<10)&(tnb.mm.tt.rs3[i,1]>5)) tnb.mm.tt.rs3[i,"开药次数1"]<-"6-9次"
  if(tnb.mm.tt.rs3[i,1]>=10) tnb.mm.tt.rs3[i,"开药次数1"]<-"10次以上"
}

tnb.mm.tt.rs4=tnb.mm.tt.rs3%>%
  group_by(开药次数1)%>%
  summarise(人数1=sum(人数))

myLabel=as.vector(tnb.mm.tt.rs4$开药次数1)
myLabel = paste(tnb.mm.tt.rs4$人数1,"(", round(tnb.mm.tt.rs4$人数1 / sum(tnb.mm.tt.rs4$人数1) * 100, 2), "%", ")",sep = "")   


ggplot(tnb.mm.tt.rs4,aes(reorder(开药次数1,-人数1),y=人数1))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = myLabel), vjust = -0.2,hjust=.4, colour = "black", position = position_dodge(.95), size = 4)


hh3=data.frame(x=c("他汀类开药人数","其他人数"),y=c(9832,114071),z=c(round(9832/123903,4),round(114071/123903,4)))
myLabel=as.vector(hh3$x)
myLabel = paste( round(hh3$y / sum(hh3$y) * 100, 2), "%", sep = "")   

#饼图1
ggplot(hh3,aes(x='',y=y,fill=x))+
  geom_bar(stat="identity",width = 1)+
  coord_polar(theta = 'y')+
  labs(x='',y='',title='')+
  theme(axis.ticks = element_blank(),axis.line = element_blank())+
  theme(legend.title = element_blank(), legend.position = "top",panel.background = element_blank())+
  theme(axis.text.x = element_blank())+
  geom_text(aes(y = hh3$y/2 + c(0, cumsum(hh3$y)[-length(hh3$y)]), x = sum(hh3$y)/100000, label = myLabel))


#................................
#糖尿病患者他汀类花费
sum(tnb.tt.rs$TOTAMOUN)
sum(tnb.tt.rs$TOTAMOUN)/sum(!duplicated(tnb.tt.rs$COMPID))
sum(tnb.tt.rs$TOTAMOUN)/nrow(tnb.tt.rs)


#糖尿病门慢患者他汀类花费
sum(tnb.mm.tt.rs$TOTAMOUN)
sum(tnb.mm.tt.rs$TOTAMOUN)/sum(!duplicated(tnb.mm.tt.rs$COMPID))
sum(tnb.mm.tt.rs$TOTAMOUN)/nrow(tnb.mm.tt.rs)











