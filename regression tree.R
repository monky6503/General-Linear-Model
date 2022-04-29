
Data<-read.csv("~/Desktop/研究所/數理統計/Titanic_train.csv",header=T)
mean_age<-mean(Data$Age,na.rm = T)
na.impute<-is.na(Data$Age)
Data[na.impute,6]<-round(mean_age,0)


library(rpart)

set.seed(22)
train.index <- sample(x=1:nrow(Data), size=ceiling(nrow(Data) ))
train <- Data[train.index, ]

cart.model<- rpart(Survived ~ Sex+Pclass+Age,data=train)

library(rpart.plot) 

prp(cart.model,         
    faclen=0,           
    fallen.leaves=TRUE, 
    shadow.col="gray",  
    # number of correct classifications / number of observations in that node
    extra=1
)