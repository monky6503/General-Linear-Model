crabs<-read.table("~/Desktop/數理統計/Crabs.txt",header=TRUE)



##Use package
model2<-glm(Sa~weight+W,data=crabs,family=poisson(link="log"))
summary(model2)


##Use Newton Method
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
