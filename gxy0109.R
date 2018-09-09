
library(data.table)
library(tidyverse)
library(ggthemr)
ggthemr('solarized')

#16年普通门诊与门慢患者各自人数
data1=fread("16年高血压普通门诊与门慢各自的人次数.csv",sep = ",")
#16年同时普通门诊与门慢就诊的高血压人数
data2=fread("16年高血压同时在门慢与普通门诊人数记录.csv",sep=",")
#16年高血压患者普通门诊与门慢的就诊记录
data3=fread("16年高血压患者普通门诊与门慢的就诊记录.csv",sep=",")
#16年高血压患者同时在普通门诊与门慢就诊的就诊记录
data4=fread("16年高血压患者同时在门慢与普通门诊的就诊记录.csv",sep = ",")
#16年门诊全数据
data5=fread("gxy.csv",sep = ",")

#..........................
#16年人数统计
#总人数
data1.rs=data1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

#分别在普通门诊和门慢的人数
data1.rs1=data1%>%
  group_by(MENGTYPE,COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

data1.rs2=data1.rs1%>%
  group_by(MENGTYPE)%>%
  summarise(人数=n())

#同时在普通门诊和门慢就诊的人数
data1.rs3=data1.rs1%>%
  filter(MENGMAN==1)%>%
  select(COMPID)%>%
  inner_join(data1.rs1%>%
               filter(MENGMAN==0),by="COMPID")



#合计性别分布
data1.rs.gender=data1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  group_by(GENDER)%>%
  summarise(人数=n())%>%
  mutate(比例=round(人数/sum(人数),4))

data1.rs.gender1=data1%>%
  filter(MENGMAN==1)%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  group_by(GENDER)%>%
  summarise(人数=n())%>%
  mutate(比例=round(人数/sum(人数),4))


data1.rs.gender2=data1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  anti_join(data1%>%filter(MENGMAN==1),by="COMPID")%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  group_by(GENDER)%>%
  summarise(人数=n())%>%
  mutate(比例=round(人数/sum(人数),4))





#年龄
data1.rs.age=data1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  select(age)

mystats(data1.rs.age$age,na.omit = T)

data1.rs.age1=data1%>%
  filter(MENGMAN==1)%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  select(age)

mystats(data1.rs.age1$age,na.omit = T)

data1.rs.age2=data1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  anti_join(data1%>%filter(MENGMAN==1),by="COMPID")%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  select(age)

mystats(data1.rs.age2$age,na.omit = T)




#门慢与非门慢数据
data.fman=data1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  anti_join(data1%>%filter(MENGMAN==1),by="COMPID")%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

data.man=data1%>%
  filter(MENGMAN==1)%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()



#16年同时在门慢与普通门诊的人数统计
data2.rs1=data2%>%
  group_by(COMPID,MENGTYPE)%>%
  filter(row_number()==1)%>%
  ungroup()
  

data2.rs=data2.rs1%>%
  group_by(MENGTYPE)%>%
  summarise(人数=n())

data2.rs.gender=data2.rs1%>%
  group_by(MENGTYPE,GENDER)%>%
  summarise(人数=n())

############################
data5.1=data1.rs%>%
  select(COMPID)%>%
  inner_join(data5,by="COMPID")


data.hz.yy1=data5.1%>%
  group_by(MENGTYPE,MED)%>%
  summarise(开药次数=n())


hh1=data.hz.yy1[grep("他汀",as.vector(as.matrix(as.data.frame(data.hz.yy1[,"MED"])))),]



for(i in 1:nrow(data.hz.yy1)){
  if(grepl("他汀",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]<-"他汀类"
  if((grepl("地平",data.hz.yy1[i,"MED"]))|(grepl("帕米",data.hz.yy1[i,"MED"]))|(grepl("硫卓",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="钙离子阻断剂"
  if((grepl("洛尔",data.hz.yy1[i,"MED"]))|(grepl("唑嗪",data.hz.yy1[i,"MED"]))|(grepl("地洛",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="阻滞剂类"
  if(grepl("普利",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]="ACEI类"
  if((grepl("匹林",data.hz.yy1[i,"MED"]))|(grepl("格雷",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="抗血小板类"
  if((grepl("噻嗪",data.hz.yy1[i,"MED"]))|(grepl("呋塞",data.hz.yy1[i,"MED"]))|(grepl("螺内酯",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="利尿药"  
  if(grepl("沙坦",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]="血管紧张素受体拮抗剂"
}




table.gxy.1=data.hz.yy1%>%
  group_by(MENGTYPE,药品类别)%>%
  summarise(开药总次数=sum(开药次数))

table.gxy.2=na.omit(table.gxy.1)


ggplot(table.gxy.2,aes(reorder(药品类别,开药总次数),y=开药总次数,fill=MENGTYPE))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  ggtitle("高血压患者门慢与普通门诊的处方情况比较")+
  guides(fill=guide_legend(title="MENGTYPE"))+
  facet_grid(.~MENGTYPE)+
  coord_flip()+
  ylab("处方量")+
  xlab("")+
  theme(axis.title = element_text(),legend.position='none')+ 
  geom_text(aes(label = 开药总次数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.95), size = 4)

table.gxy.2.1=table.gxy.2%>%
  group_by(药品类别)%>%
  summarise(开药总次数=sum(开药总次数))



#############################
data5$COMPID=as.numeric(data5$COMPID)
data4.1=data1.rs3%>%
  select(COMPID)%>%
  inner_join(data5,by="COMPID")


data.hz.yy1=data4.1%>%
  group_by(MENGTYPE,MED)%>%
  summarise(开药次数=n())



for(i in 1:nrow(data.hz.yy1)){
  if(grepl("他汀",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]<-"他汀类"
  if((grepl("地平",data.hz.yy1[i,"MED"]))|(grepl("帕米",data.hz.yy1[i,"MED"]))|(grepl("硫卓",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="钙离子阻断剂"
  if((grepl("洛尔",data.hz.yy1[i,"MED"]))|(grepl("唑嗪",data.hz.yy1[i,"MED"]))|(grepl("地洛",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="阻滞剂类"
  if(grepl("普利",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]="ACEI类"
  if((grepl("匹林",data.hz.yy1[i,"MED"]))|(grepl("格雷",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="抗血小板类"
  if((grepl("噻嗪",data.hz.yy1[i,"MED"]))|(grepl("呋塞",data.hz.yy1[i,"MED"]))|(grepl("螺内酯",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="利尿药"  
  if(grepl("沙坦",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]="血管紧张素受体拮抗剂"
}



table.gxy.1=data.hz.yy1%>%
  group_by(MENGTYPE,药品类别)%>%
  summarise(开药总次数=sum(开药次数))

table.gxy.3=na.omit(table.gxy.1)


ggplot(table.gxy.3,aes(reorder(药品类别,开药总次数),y=开药总次数,fill=MENGTYPE))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  ggtitle("高血压门慢患者门慢与普通门诊处方情况比较")+
  guides(fill=guide_legend(title="MENGTYPE"))+
  facet_grid(.~MENGTYPE)+
  coord_flip()+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = 开药总次数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.95), size = 4)


############################################################

data.hz.id=data1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()%>%
  select(COMPID)%>%
  left_join(data3.1,by="COMPID")

data.hz.id=na.omit(data.hz.id)

data.mm.rs1=data.hz.id[grep("他汀",as.vector(as.matrix(as.data.frame(data.hz.id[,"MED"])))),]

data.mm.rs2=data.mm.rs1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

data.mm.rs3=data.mm.rs1%>%
  group_by(COMPID)%>%
  summarise(使用他汀类次数=n())

table.tt.1=table(data.mm.rs3$使用他汀类次数)%>%as.data.frame()%>%
  rename(开药次数=Var1,人数=Freq)






#使用药物的人数
#1、50万门慢患者有多少人拿了他汀类药物
#这些人这一年期间拿了多少次他汀类药物

#16年普通门诊与门慢患者各自人数
#在门慢中他汀类药物的处方量

#门慢患者的所有就诊记录
data.mm.id=data1%>%
  filter(MENGTYPE=="高血压病")%>%
  select(COMPID)%>%
  left_join(data3.1,by="COMPID")

data.mm.id=na.omit(data.mm.id)

data.mm.rs1=data.mm.id[grep("他汀",as.vector(as.matrix(as.data.frame(data.mm.id[,"MED"])))),]

data.mm.rs2=data.mm.rs1%>%
  group_by(COMPID)%>%
  filter(row_number()==1)%>%
  ungroup()

data.mm.rs3=data.mm.rs1%>%
  group_by(COMPID)%>%
  summarise(使用他汀类次数=n())

table.tt.1=table(data.mm.rs3$使用他汀类次数)%>%as.data.frame()%>%
  rename(开药次数=Var1,人数=Freq)




ggplot(table.tt.1,aes(reorder(开药次数,-人数),y=人数))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = 人数), vjust = -0.2,hjust=.4, colour = "black", position = position_dodge(.95), size = 4)


table.tt.1$开药次数=as.numeric(as.character(table.tt.1$开药次数))
for(i in 1:nrow(table.tt.1)){
  if(table.tt.1[i,1]<=2) table.tt.1[i,"开药次数1"]<-"1-2次"
  if((table.tt.1[i,1]<=5)&(table.tt.1[i,1]>2)) table.tt.1[i,"开药次数1"]<-"3-5次"
  if((table.tt.1[i,1]<10)&(table.tt.1[i,1]>5)) table.tt.1[i,"开药次数1"]<-"6-9次"
  if(table.tt.1[i,1]>=10) table.tt.1[i,"开药次数1"]<-"10次以上"
}


table.tt.2=table.tt.1%>%group_by(开药次数1)%>%summarise(人数1=sum(人数))


ggplot(table.tt.2,aes(reorder(开药次数1,-人数1),y=人数1))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = 人数1), vjust = -0.2,hjust=.4, colour = "black", position = position_dodge(.95), size = 4)




table.tt.2=table.tt.1%>%group_by(开药次数1)%>%summarise(人数1=sum(人数))

hh2=data.frame(x=c("他汀类需求人数"),y=c(62875),h=c("他汀类需求比例"),z=0.121)
hh2$z=round(hh2$y/sum(hh2$y),3)


p1=ggplot(hh2,aes(reorder(x,y),y))+
  geom_bar(stat="identity",position="dodge",width = 0.1)+
  theme(axis.ticks.length=unit(.2,'cm'))+
  theme(axis.title = element_blank(),legend.position='none')+
  geom_text(aes(label = y), vjust = -0.2,hjust=.4, colour = "black", position = position_dodge(.95), size = 4)

p2=ggplot(hh2,aes(reorder(h,z),z))+
  geom_bar(stat="identity",position="dodge",width = 0.1)+
  theme(axis.ticks.length=unit(.2,'cm'))+
  theme(axis.title = element_blank(),legend.position='none')+
  geom_text(aes(label = z), vjust = -0.2,hjust=.4, colour = "black", position = position_dodge(.95), size = 4)

ggplot2.multiplot(p1, p2,cols=2)

hh3=data.frame(x=c("他汀类开药人数","其他人数"),y=c(62875,445527),z=c(round(62875/508402,4),round(445527/508402,4)))
myLabel=as.vector(hh3$x)
myLabel = paste( round(hh3$y / sum(hh3$y) * 100, 2), "%", sep = "")   

hh4=data.frame(x=c("他汀类开药人数","其他人数"),y=c(83098,663239),z=c(round(83098/746337,4),round(663239/746337,4)))
myLabel=as.vector(hh4$x)
myLabel = paste( round(hh4$y / sum(hh4$y) * 100, 2), "%", sep = "")   


#饼图
ggplot(hh4,aes(x='',y=y,fill=x))+
  geom_bar(stat="identity",width = 1)+
  coord_polar(theta = 'y')+
  labs(x='',y='',title='')+
  theme(axis.ticks = element_blank(),axis.line = element_blank())+
  theme(legend.title = element_blank(), legend.position = "top",panel.background = element_blank())+
  theme(axis.text.x = element_blank())+
  geom_text(aes(y = hh4$y/2 + c(0, cumsum(hh4$y)[-length(hh4$y)]), x = sum(hh4$y)/600000, label = myLabel))

######################################################################
费用计算


sum(data.mm.rs1$TOTAMOUN)         #总费用
sum(data.mm.rs1$TOTAMOUN)/sum(!duplicated(data.mm.rs1$COMPID))       #人均他汀类药费
sum(data.mm.rs1$TOTAMOUN)/nrow(data.mm.rs1)             #次均他汀类药费


########################################################################
######0127

gxy.yy=read.table("用药0126.csv",sep = ",")

library(data.table)
gxy.yy=fread("用药0126.csv")




table.gxy.1=gxy.yy%>%
  filter(MENGTYPE=="高血压病"|MENGTYPE=="普通门诊")%>%
  group_by(MENGTYPE,药品类别)%>%
  summarise(开药总次数=sum(开药次数))

table.gxy.3=na.omit(table.gxy.1)


ggplot(table.gxy.3,aes(reorder(药品类别,开药总次数),y=开药总次数,fill=MENGTYPE))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  ggtitle("高血压患者门慢与普通门诊处方情况比较")+
  guides(fill=guide_legend(title="MENGTYPE"))+
  facet_grid(.~MENGTYPE)+
  coord_flip()+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = 开药总次数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.95), size = 4)










