# General-Linear-Model
---
title: "Untitled"
author: "Chun Ming,Liao"
date: "5/18/2022"
output: pdf_document
---




```{r cars}
titanic<-read.csv("~/Desktop/數理統計/titanic.csv",header=T,sep=",")
mean_fare<-mean(titanic$fare,na.rm=T) ##missing value replace by mean
na.cols.1<-is.na(titanic$fare)
titanic[na.cols.1,5]<-round(mean_fare,0)
mean_age<-mean(titanic$age,na.rm=T)
na.cols.2<-is.na(titanic$age)
titanic[na.cols.2,3]<-round(mean_age,0)

##Use package
model<-glm(survival~fare,data=titanic,family=binomial(link="logit"))
summary(model)

##Use Newton Method
x1<-titanic$fare
y1<-titanic$survival
n<-length(y1)
x1<-matrix(cbind(rep(1,n),x1),n,2)
beta<-rep(0,2)


lambda <- function(x,beta)
{
  lambda <- 1/(1+exp(-x%*%beta) )
  return(lambda)
}

likelihood <- function(x,y,beta)
{
  result <- rep(0,n)
  for (i in 1:n)
  {
    result[i] <- y[i]*log(lambda(x[i,],beta))+(1-y[i])*log(1-lambda(x[i,],beta))
  }
  return(sum(result))
}


hessian <- function(x,beta)
{
  hess<-array(0,dim=c(2,2,n))
  for (i in 1:n)
  {
    hess[,,i]<- -(x[i,]%*%t(x[i,]))*as.numeric(lambda(x[i,],beta)*(1-lambda(x[i,],beta)))
  }
  a<-matrix(0,2,2)
  for(i in 1:n)
  {
  a<-a+hess[,,i]
  }
  return(a)
}

score <- function(x,y,beta)
{
  score<-array(0, c(n, 2))
  for (i in 1:n)
  {
    score[i,]<-(y[i]-lambda(x[i,],beta))%*%x[i,]
  }
  return(matrix(c(sum(score[,1]),sum(score[,2])),1,2))
}

Newton<-function(x,y)
{
  Noldbeta <-rep(0,2)
  iter <-0
  repeat{
    Nnewbeta <- Noldbeta - solve(hessian(x,Noldbeta))%*%t(score(x,y,Noldbeta))
    converge <- (abs(Nnewbeta-Noldbeta))
    Noldbeta <- Nnewbeta
    iter <- iter+1
    if (converge[1]< 10^(-5) & converge[2]< 10^(-5)) break
  }
  return(c(Nnewbeta,iter))
}

beta_hat<-Newton(x1,y1)[1:2]
beta_hat

```

