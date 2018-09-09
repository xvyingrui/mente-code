


data.gxy.hf1$HOSLEVEL[data.gxy.hf1$HOSLEVEL=="相当三级"]<-"三级"
data.gxy.hf1$HOSLEVEL[data.gxy.hf1$HOSLEVEL=="相当一级"]<-"一级"
data.gxy.hf1$HOSLEVEL=as.character(data.gxy.hf1$HOSLEVEL)
data.gxy.hf1$HOSLEVEL[data.gxy.hf1$HOSLEVEL=="三级"]<-"3级"
data.gxy.hf1$HOSLEVEL[data.gxy.hf1$HOSLEVEL=="二级"]<-"2级"
data.gxy.hf1$HOSLEVEL[data.gxy.hf1$HOSLEVEL=="一级"]<-"1级"
data.gxy.hf1$HOSLEVEL[data.gxy.hf1$HOSLEVEL=="相当二级"]<-"2级"

data.gxy.hf2=data.gxy.hf1%>%
  group_by(HOSLEVEL,year,MENGMAN,MENGTYPE)%>%
  summarise(每年总花费=sum(每年总花费),例数=sum(例数),人数=sum(人数))%>%
  mutate(人均年花费=每年总花费/人数)%>%
  arrange(year,desc(HOSLEVEL))

data.gxy.hf2$人均年花费=round(data.gxy.hf2$人均年花费,2)
data.gxy.hf2$year=as.character(data.gxy.hf2$year)


data.gxy.hf3=data.gxy.hf2%>%
  group_by(HOSLEVEL,year)%>%
  summarise(每年总花费=sum(每年总花费),例数=sum(例数),人数=sum(人数))%>%
  mutate(人均年花费=每年总花费/人数)



#.................................................................................

data.gxy.yp1=read.delim("clipboard")




for(i in 1:nrow(data.gxy.yp1)){
  if(grepl("他汀",data.gxy.yp1[i,"MED"])) data.gxy.yp1[i,"药品类别"]<-"他汀类"
  if(grepl("地平",data.gxy.yp1[i,"MED"])) data.gxy.yp1[i,"药品类别"]="二氢吡啶类"
  if((grepl("洛尔",data.gxy.yp1[i,"MED"]))|(grepl("唑嗪",data.gxy.yp1[i,"MED"]))|(grepl("地洛",data.gxy.yp1[i,"MED"]))) data.gxy.yp1[i,"药品类别"]="阻滞剂类"
  if(grepl("普利",data.gxy.yp1[i,"MED"])) data.gxy.yp1[i,"药品类别"]="ACEI类"
  if((grepl("噻嗪",data.gxy.yp1[i,"MED"]))|(grepl("呋塞",data.gxy.yp1[i,"MED"]))|(grepl("螺内酯",data.gxy.yp1[i,"MED"]))) data.gxy.yp1[i,"药品类别"]="利尿药"  
  if(grepl("沙坦",data.gxy.yp1[i,"MED"])) data.gxy.yp1[i,"药品类别"]="血管紧张素受体拮抗剂"
  if((grepl("帕米",data.gxy.yp1[i,"MED"]))|(grepl("硫卓",data.gxy.yp1[i,"MED"]))) data.gxy.yp1[i,"药品类别"]="非二氢吡啶类"
}

data.gxy.yp2=na.omit(data.gxy.yp1)

table.hz.yy1=data.gxy.yp2%>%
  group_by(year,药品类别,MENGTYPE)%>%
  summarise(开药总次数=sum(开药次数))


table.hz.yy2=table.hz.yy1%>%
  group_by(year)%>%
  summarise(每年开药总次数=sum(开药总次数))%>%
  left_join(table.hz.yy1,by="year")%>%
  mutate(prop=开药总次数/每年开药总次数)
table.hz.yy2$prop=round(table.hz.yy2$prop,3)

ggplot(table.hz.yy1%>%filter(year==16),aes(x=药品类别,y=开药总次数,fill=MENGTYPE))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title="year"))+
  ggtitle("13-16年高血压总就诊用药的比较")+
  facet_grid(.~MENGTYPE)+
  coord_flip()+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = 开药总次数), vjust = 1.5, colour = "white", position = position_dodge(.95), size = 3)



data.gxy.yp3=read.delim("clipboard")
data.gxy.yp3$比率=round(data.gxy.yp3$比率,4)

ggplot(data.gxy.yp3%>%filter(MENGTYPE=="高血压病"),aes(reorder(药品类别,比率),y=比率,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title="门诊类型"))+
  ggtitle("13-16门慢用药比较")+
  facet_grid(.~year)+
  coord_flip()+
  theme(axis.title = element_blank(),legend.position='none')+ 
  geom_text(aes(label = 比率), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.95), size = 3)





#。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。

data.fa=read.delim("clipboard")

data.fa$HOSLEVEL=as.character(data.fa$HOSLEVEL)

data.fa$HOSLEVEL[data.fa$HOSLEVEL=="二级"]<-"2级"
data.fa$HOSLEVEL[data.fa$HOSLEVEL=="三级"]<-"3级"
data.fa$HOSLEVEL[data.fa$HOSLEVEL=="一级"]<-"1级"
data.fa$HOSLEVEL[data.fa$HOSLEVEL=="相当三级"]<-"3级"
data.fa$HOSLEVEL[data.fa$HOSLEVEL=="相当一级"]<-"1级"
data.fa$HOSLEVEL[data.fa$HOSLEVEL=="相当二级"]<-"2级"


data.fa1=data.fa%>%
  group_by(HOSLEVEL,year,MENGMAN,MENGTYPE)%>%
  summarise(每年总花费=sum(每年总花费),例数=sum(例数),人数=sum(人数))%>%
  mutate(人均年花费=每年总花费/人数)%>%
  arrange(year,desc(HOSLEVEL))

data.fa1$人均年花费=round(data.fa1$人均年花费,1)
data.fa1$year=as.character(data.fa1$year)



ggplot(data.fa1%>%filter(year>=2013&MENGTYPE!="恶性肿瘤化疗"),aes(reorder(MENGTYPE,HOSLEVEL),y=人数,fill=HOSLEVEL))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title='HOSLEVEL'))+
  ggtitle("13-16年各级医院不同项目肺癌治疗人数比较")+
  theme(axis.title = element_blank(),legend.position='bottom')+ 
  facet_grid(.~year)+
  coord_flip()+
  geom_text(aes(label = 人数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.9), size = 3)



data.fa2=data.fa1%>%
  group_by(year,MENGTYPE)%>%
  summarise(总人数=sum(人数))

ggplot(data.fa2%>%filter(year>=2013),aes(reorder(MENGTYPE,总人数),y=总人数,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.title = element_blank(),legend.position='none')+ 
  facet_grid(.~year)+
  ggtitle("13-16年各级医院不同待遇人数变化比较")+
  coord_flip()+
  geom_text(aes(label = 总人数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.85), size = 3.5)



data.fa3=data.fa1%>%
  group_by(year,MENGTYPE)%>%
  summarise(人均总花费=mean(人均年花费))
data.fa3$人均总花费=round(data.fa3$人均总花费,0)

ggplot(data.fa3%>%filter(year>=2013&MENGTYPE!="恶性肿瘤化疗"),aes(reorder(MENGTYPE,人均总花费),y=人均总花费,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title='HOSLEVEL'))+
  ggtitle("13-16年各级医院不同项目肺癌花费的比较")+
  theme(axis.title = element_blank(),legend.position='none')+ 
  facet_grid(.~year)+
  coord_flip()+
  geom_text(aes(label = 人均总花费), vjust = .6,hjust=.9, colour = "black", position = position_dodge(.9), size = 3)


