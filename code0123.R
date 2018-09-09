
library(readxl)
DATA1=read_excel("DATA2.xlsx")

DATA2=DATA1
DATA2[,c(4:9)]=lapply(DATA2[,c(4:9)],function(x){as.factor(x)})
table(DATA2$Y)

DATA2=subset(DATA2,Y!=0)


#πÈ“ªªØ
DATA2$PRE=scale(DATA2$PRE)
DATA2$COST=scale(DATA2$COST)

library(e1071)
model=naiveBayes(Y~PRE+COST+clinic+social+rare+PRE1+GROUP1+GROUP2+GROUP3,data = DATA2)
predict(model,DATA2[1:20,],type = "raw")

pred=predict(model,DATA2)
table(pred,DATA2$Y)


library(rpart)
library(rpart.plot)
library(rattle)

a=rpart(Y~PRE+COST+clinic+social+rare+PRE1+GROUP1+GROUP2+GROUP3,data = DATA2)

asRules(a)
fancyRpartPlot(a)




#R in action
set.seed(1234)
train<-sample(nrow(DATA2),0.8*nrow(DATA2))
df.train<-DATA2[train,]
df.test<-DATA2[-train,]


b=rpart(Y~PRE+COST+clinic+social+rare+PRE1+GROUP1+GROUP2+GROUP3,
        data = df.train,method = "class",
        parms = list(split="information"))

b$cptable

plotcp(b)
b.1<-prune(b,cp=.0125)
library(rpart.plot)
prp(b.1,type = 2,extra = 104,
    fallen.leaves = TRUE,main="Decision Tree")
b.pred=predict(b.1,df.test,type = "class")
b.perf<-table(df.test$Y,b.pred,dnn = c("actual","predicted"))
b.perf


#logistic
set.seed(1234)
train<-sample(nrow(DATA2),0.8*nrow(DATA2))
df.train<-DATA2[train,]
df.test<-DATA2[-train,]

c<-glm(Y~PRE+COST+clinic+social+rare+PRE1+GROUP1+GROUP2+GROUP3,
       data = df.train,family = binomial())

summary(c)

prob<-predict(c,df.test,type = "response")
logit.pred<-factor(prob>.5,levels = c(FALSE,TRUE),
                   labels = c("0","1"))
logit.perf<-table(df.test$Y,logit.pred,
                  dnn = c("actual","predicted"))

logit.perf

logit.fit.reduced<-step(c)

summary(logit.fit.reduced)




