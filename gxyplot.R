


library(tidyverse)
library(ggthemr)



data1$HOSLEVEL[data1$HOSLEVEL=="相当三级"]<-"三级"
data1$HOSLEVEL[data1$HOSLEVEL=="相当一级"]<-"一级"
data1$HOSLEVEL=as.character(data1$HOSLEVEL)
data1$HOSLEVEL[data1$HOSLEVEL=="三级"]<-"3级"
data1$HOSLEVEL[data1$HOSLEVEL=="二级"]<-"2级"
data1$HOSLEVEL[data1$HOSLEVEL=="一级"]<-"1级"
data1$HOSLEVEL[data1$HOSLEVEL=="无"]<-"社区"



data2=data1%>%
  group_by(HOSLEVEL,year,MENGMAN,MENGTYPE)%>%
  summarise(每年总花费=sum(每年总花费),例数=sum(例数),人数=sum(人数))%>%
  mutate(人均年花费=每年总花费/人数)%>%
  arrange(year,desc(HOSLEVEL))

data2$人均年花费=round(data2$人均年花费,2)
data2$year=as.character(data2$year)

ggplot(data2%>%filter(year>=13),aes(x=year,y=人均年花费,fill=HOSLEVEL))+
  geom_bar(position = "dodge",stat = "identity")+
  ggtitle("13-16年各级医院费用变化比较")+
  theme_economist()+   #主题
  scale_fill_economist()+
  geom_text(aes(label = 人均年花费), vjust = 1.5, colour = "black", position = position_dodge(.9), size = 4)

ggplot(data2%>%filter(year>=13),aes(x=year,y=人均年花费,fill=HOSLEVEL))+
  geom_bar(position = "dodge",stat = "identity")+
  ggtitle("13-16年各级医院费用变化比较")+
  theme_wsj()+   #主题
  scale_fill_wsj()+
  geom_text(aes(label = 人均年花费), vjust = 1.5, colour = "white", position = position_dodge(.9), size = 4)


ggplot(data2%>%filter(year>=13),aes(x=HOSLEVEL,y=人均年花费,fill=year))+
    geom_bar(stat="identity",position="dodge")+
    guides(fill=guide_legend(title=NULL))+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  theme(axis.title = element_blank(),legend.position='top')+ 
    theme(legend.position='none',title=element_text(size = 12,family = 'myFont',face = 'italic',hjust =2))+ 
    facet_grid(.~year)+
  scale_y_continuous()+
  geom_text(aes(label = 人均年花费), vjust = 1.5, colour = "white", position = position_dodge(.9), size = 4)


#.............................................................................

  geom_text(data = data2%>%filter(year>=13),aes(x=year,y=人均年花费,label=人均年花费),posiion="identity")+
    coord_flip()  #柱形图变条形图
  
#,............................................................................
  
  
  data3$HOSLEVEL=as.character(data3$HOSLEVEL)
  
  data3$HOSLEVEL[data3$HOSLEVEL=="二级"]<-"2级"
  data3$HOSLEVEL[data3$HOSLEVEL=="三级"]<-"3级"
  data3$HOSLEVEL[data3$HOSLEVEL=="一级"]<-"1级"
  data3$HOSLEVEL[data3$HOSLEVEL=="相当三级"]<-"3级"
  data3$HOSLEVEL[data3$HOSLEVEL=="相当一级"]<-"1级"
  data3$HOSLEVEL[data3$HOSLEVEL=="无"]<-"社区"
  data3$HOSLEVEL[data3$HOSLEVEL=="相当二级"]<-"2级"
  
  
  data4=data3%>%
    group_by(HOSLEVEL,year,MENGMAN,MENGTYPE)%>%
    summarise(每年总花费=sum(每年总花费),例数=sum(例数),人数=sum(人数))%>%
    mutate(人均年花费=每年总花费/人数)%>%
    arrange(year,desc(HOSLEVEL))
  
  data4$人均年花费=round(data4$人均年花费,1)
  data4$year=as.character(data4$year)
  
  
  
  
  
  ggplot(data4%>%filter(year>=13&MENGTYPE=="普通门诊"),aes(reorder(HOSLEVEL,year),y=人均年花费,fill=year))+
    geom_bar(stat="identity",position="dodge")+
    theme_wsj()+scale_fill_wsj("rgby", "")+
    theme(axis.ticks.length=unit(0.5,'cm'))+
    guides(fill=guide_legend(title=NULL))+
    ggtitle("13-16年各级医院高血压治疗费用变化比较")+
    theme(axis.title = element_blank(),legend.position='top')+ 
    facet_grid(year~.)+
    geom_text(aes(label = 人均年花费), vjust = 1.5, colour = "white", position = position_dodge(.9), size = 4)
  
  
  ggplot(data4%>%filter(year==16),aes(x=MENGTYPE,y=人均年花费,fill=HOSLEVEL))+
    geom_bar(position = "dodge",stat = "identity")+
    ggtitle("13-16年各级医院费用变化比较")+
    theme_wsj()+   #主题
    scale_fill_wsj()+
    geom_text(aes(label = 人均年花费), vjust = 1.5, colour = "white", position = position_dodge(.9), size = 4)
  
  ggthemr('solarized')
  
    ggplot(data4%>%filter(year>=13),aes(reorder(MENGTYPE,人数),y=人数,fill=HOSLEVEL))+
      geom_bar(stat="identity",position="dodge")+
      theme(axis.ticks.length=unit(0.5,'cm'))+
      guides(fill=guide_legend(title="HOSLEVEL"))+
      theme(axis.title = element_blank(),legend.position='bottom')+ 
      facet_grid(.~year)+
      ggtitle("13-16年各级医院不同待遇人数变化比较")+
      coord_flip()+
      geom_text(aes(label = 人数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.85), size = 3)
    
    

  
data5=data4%>%
  group_by(year,MENGTYPE)%>%
  summarise(总人数=sum(人数))
    
    
    

ggplot(data5%>%filter(year>=13),aes(reorder(MENGTYPE,总人数),y=总人数,fill=year))+
  geom_bar(stat="identity",position="dodge")+
  theme(axis.ticks.length=unit(0.5,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.title = element_blank(),legend.position='none')+ 
  facet_grid(.~year)+
  ggtitle("13-16年各级医院不同待遇人数变化比较")+
  coord_flip()+
  geom_text(aes(label = 总人数), vjust = .6,hjust=.6, colour = "black", position = position_dodge(.85), size = 3.5)


    
    
    
    
    
    
  
  
  