

data.yy1=read.csv("e:/project项目文件/pfizer_project_特殊病种遴选/20171114进展/病种数据统计结果/高血压用药.csv")
data.hz.yy1=read.csv('e:/project项目文件/pfizer_project_特殊病种遴选/20171114进展/病种数据统计结果/高血压各个门诊用药统计.csv')


for(i in 1:nrow(data.yy1)){
  if(grepl("他汀",data.yy1[i,"MED"])) data.yy1[i,"药品类别"]<-"他汀类"
  if(grepl("地平",data.yy1[i,"MED"])) data.yy1[i,"药品类别"]="二氢吡啶类"
  if((grepl("洛尔",data.yy1[i,"MED"]))|(grepl("唑嗪",data.yy1[i,"MED"]))|(grepl("地洛",data.yy1[i,"MED"]))) data.yy1[i,"药品类别"]="阻滞剂类"
  if(grepl("普利",data.yy1[i,"MED"])) data.yy1[i,"药品类别"]="ACEI类"
  if((grepl("匹林",data.yy1[i,"MED"]))|(grepl("格雷",data.yy1[i,"MED"]))) data.yy1[i,"药品类别"]="抗血小板类"
  if((grepl("噻嗪",data.yy1[i,"MED"]))|(grepl("呋塞",data.yy1[i,"MED"]))|(grepl("螺内酯",data.yy1[i,"MED"]))) data.yy1[i,"药品类别"]="利尿药"
  if((grepl("定",data.yy1[i,"MED"]))|(grepl("血平",data.yy1[i,"MED"]))|(grepl("甲基多巴",data.yy1[i,"MED"]))) data.yy1[i,"药品类别"]="中枢作用类"
  if(grepl("沙坦",data.yy1[i,"MED"])) data.yy1[i,"药品类别"]="血管紧张素受体拮抗剂"
  if((grepl("帕米",data.yy1[i,"MED"]))|(grepl("硫卓",data.yy1[i,"MED"]))) data.yy1[i,"药品类别"]="非二氢吡啶类"
  }

data.yy2=na.omit(data.yy1)

table.yy1=data.yy2%>%
  group_by(year,药品类别)%>%
  summarise(开药总次数=sum(开药次数))


table.yy2=table.yy1%>%
  group_by(year)%>%
  summarise(每年开药总次数=sum(开药总次数))%>%
  left_join(table.yy1,by="year")%>%
  mutate(prop=开药总次数/每年开药总次数)
table.yy2$prop=round(table.yy2$prop,4)



ggplot(table.yy1%>%filter(year>=13),aes(x=药品类别,y=开药总次数,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title="year"))+
  ggtitle("13-16年高血压门慢用药的比较")+
  facet_grid(.~year)+
  coord_flip()+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = 开药总次数), vjust = 1.5, colour = "white", position = position_dodge(.95), size = 3)


ggplot(table.yy2%>%filter(year>=13),aes(reorder(药品类别,-prop),y=prop,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title="year"))+
  ggtitle("13-16年高血压门慢用药的比较")+
  facet_grid(.~year)+
  coord_flip()+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = prop), vjust = 1.5, colour = "black", position = position_dodge(.95), size = 3)

##############################

data.hz.yy1=read.csv('e:/project项目文件/pfizer_project_特殊病种遴选/20171114进展/病种数据统计结果/高血压各个门诊用药统计.csv')


for(i in 1:nrow(data.hz.yy1)){
  if(grepl("他汀",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]<-"他汀类"
  if(grepl("地平",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]="二氢吡啶类"
  if((grepl("洛尔",data.hz.yy1[i,"MED"]))|(grepl("唑嗪",data.hz.yy1[i,"MED"]))|(grepl("地洛",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="阻滞剂类"
  if(grepl("普利",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]="ACEI类"
  if((grepl("匹林",data.hz.yy1[i,"MED"]))|(grepl("格雷",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="抗血小板类"
  if((grepl("噻嗪",data.hz.yy1[i,"MED"]))|(grepl("呋塞",data.hz.yy1[i,"MED"]))|(grepl("螺内酯",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="利尿药"  
  if(grepl("沙坦",data.hz.yy1[i,"MED"])) data.hz.yy1[i,"药品类别"]="血管紧张素受体拮抗剂"
  if((grepl("帕米",data.hz.yy1[i,"MED"]))|(grepl("硫卓",data.hz.yy1[i,"MED"]))) data.hz.yy1[i,"药品类别"]="非二氢吡啶类"
}

data.hz.yy2=na.omit(data.hz.yy1)

table.hz.yy1=data.hz.yy2%>%
  group_by(year,药品类别)%>%
  summarise(开药总次数=sum(开药次数))


table.hz.yy2=table.hz.yy1%>%
  group_by(year)%>%
  summarise(每年开药总次数=sum(开药总次数))%>%
  left_join(table.hz.yy1,by="year")%>%
  mutate(prop=开药总次数/每年开药总次数)
table.hz.yy2$prop=round(table.hz.yy2$prop,3)



ggplot(table.hz.yy1%>%filter(year>=13),aes(x=药品类别,y=开药总次数,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title="year"))+
  ggtitle("13-16年高血压总就诊用药的比较")+
  facet_grid(.~year)+
  coord_flip()+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = 开药总次数), vjust = 1.5, colour = "white", position = position_dodge(.95), size = 3)

  
  ggplot(table.hz.yy2%>%filter(year>=13),aes(reorder(药品类别,-prop),y=prop,fill=year))+
    geom_bar(stat="identity",position="dodge")+
    theme_wsj()+
    theme(axis.ticks.length=unit(0.5,'cm'))+
    guides(fill=guide_legend(title="year"))+
    ggtitle("13-16年高血压总就诊用药的比较")+
    facet_grid(.~year)+
    
    coord_flip()+
    theme(axis.title = element_blank(),legend.position='none')+ 
    geom_text(aes(label = prop), vjust = 1.5,hjust=.1, colour = "black", position = position_dodge(.9), size = 4)


#################################################

table.yy2=ddply(table.yy1,"year",function(x){x=(table.yy1$开药总次数),4)})










