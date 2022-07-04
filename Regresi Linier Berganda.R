regresi_terboboti<-function(respon,prediktor1,prediktor2,prediktor3,prediktor4)
{
  data<-cbind(respon,prediktor1,prediktor2,prediktor3,prediktor4)
  y<-data[,1]
  x1<-data[,2]
  x2<-data[,3]
  x3<-data[,4]
  x4<-data[,5]
  beta<-rep(0,5)
  n<-length(y)
  ytopi<-rep(0,n)
  error<-rep(0,n)
  X<-matrix(0,n,5)
  V<-matrix(0,n,n)
  for (i in 1:n)
  {
    X[i,1]<-1
    X[i,2]<-x1[i]
    X[i,3]<-x2[i]
    X[i,4]<-x3[i]
    X[i,5]<-x4[i]
    
  }
  for (i in 1:n)
  {
    V[i,i]<-(1/(3*n))^2
  }
  cat("Matrik X = \n")
  print(X)
  beta<-solve(t(X)%*%solve(V)%*%X)%*%t(X)%*%solve(V)%*%y
  for (i in 1:5)
    cat("beta",i-1, "=", beta[i],"\n")
  ytopi<-X%*%beta
  error<-ytopi-y
  cat("\n y \t\t ytopi \t\t error \t\t \n")
  cat("-------------------------------------------------\n")
  for(i in 1:n)
  {
    cat(y[i], "\t\t", ytopi[i], "\t\t", error[i], "\n")
    
  }
  MSE=(sum((y[i]-ytopi[i])^2)/n)
  cat("MSE= ", MSE, "\n")
  
}
UAS <-c(64,87,98,54,65,74,68,98,45,81)
MINAT <- c(5,7,9,4,5,8,6,7,6,5)
BELAJAR <-c(8,6,8,7,6,4,3,8,6,8)
HADIR <- c(90,100,100,90,95,100,95,94,98,95)
UTS<-c(55,77,84,54,62,89,84,87,65,76)


regresi_terboboti(UAS,MINAT,BELAJAR,HADIR,UTS)

