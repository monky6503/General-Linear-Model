# General-Linear-Model

## Logistic Regression simulation

```{r cars}
titanic<-read.csv("~/Desktop/數理統計/titanic.csv",header=T,sep=",")
mean_fare<-mean(titanic$fare,na.rm=T) ##missing value replace by mean
na.cols.1<-is.na(titanic$fare)
titanic[na.cols.1,5]<-round(mean_fare,0)
mean_age<-mean(titanic$age,na.rm=T)
na.cols.2<-is.na(titanic$age)
titanic[na.cols.2,3]<-round(mean_age,0)
```
### Use package
```{r cars}
model<-glm(survival~fare,data=titanic,family=binomial(link="logit"))
summary(model)
```

![截圖 2022-05-18 14 13 06](https://user-images.githubusercontent.com/97944035/168969962-18ca85ab-802a-492c-83db-8e10189f1358.png)


### Use Newton Method
```{r cars}
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
![截圖 2022-05-18 14 13 46](https://user-images.githubusercontent.com/97944035/168970026-97ae871d-bc9c-4a30-833d-8459277077d2.png)


## Poission Regression
```{r cars}
crabs<-read.table("~/Desktop/研究所//數理統計/Crabs.txt",header=TRUE)
```
###  Use package
```{r cars}
model2<-glm(Sa~weight+W,data=crabs,family=poisson(link="log"))
summary(model2)
```

![截圖 2022-05-18 14 20 46](https://user-images.githubusercontent.com/97944035/168970983-73a2a0af-a252-442d-bf17-5f8be511a4af.png)



### Use Newton Method
```{r cars}
x1<-crabs$weight
x2<-crabs$W
y<-crabs$Sa
n<-length(y)
x<-matrix(cbind(rep(1,n),x1,x2),n,3)
theta<-rep(0,3)

mu<-function(x,theta)
{
    mu<-exp(x%*%theta)
  return(mu)
}

score<-function(x,y,theta)
{
  score<-array(0,dim=c(1,3,n))
  for(i in 1:n)
  {
  score[,,i]<-y[i]*x[i,]-mu(x[i,],theta)%*%x[i,]
  }
  b<-matrix(0,1,3)
  for(i in 1:n)
  {
    b<-b+score[,,i]
  }
  return(b)
}

hessian<-function(x,theta)
{
  hess<-array(0,dim=c(3,3,n))
  for(i in 1:n)
  {
  hess[,,i]<- -as.numeric(mu(x[i,],theta))*x[i,]%*%t(x[i,])
  }
  a<-matrix(0,3,3)
  for(i in 1:n)
  {
    a<-a+hess[,,i]
  }
  return(a)
}

Newton<-function(x,y)
{
  Noldbeta <-rep(0,3)
  iter <-0
  repeat{
    Nnewbeta <- Noldbeta - solve(hessian(x,Noldbeta))%*%t(score(x,y,Noldbeta))
    converge <- (abs(Nnewbeta-Noldbeta))
    Noldbeta <- Nnewbeta
    iter <- iter+1
    if (converge[1]< 10^(-5) & converge[2]< 10^(-5) & converge[3]< 10^(-5) ) break
  }
  return(c(Nnewbeta,iter))
}

theta_hat<-Newton(x,y)[1:3]
theta_hat
```
![截圖 2022-05-18 14 21 07](https://user-images.githubusercontent.com/97944035/168970990-3b516223-f3e5-42bb-83a8-6379516e6081.png)

## Rgression Tree
```{r cars}

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
```
![tree](https://user-images.githubusercontent.com/97944035/168971884-630deeae-82a0-4332-9c3d-6cf084171b7d.png)
