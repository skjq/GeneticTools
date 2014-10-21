# Experimental parts on time series clustering and the corresponding distances measures

calcDistances <- function(X,method=c("euclidean","splines"),df=NULL){
  method <- match.arg(method)
  res <- c()
  if(method=="euclidean") res <- calcDistances.C(X)
  if(method=="splines") res <- calcDistances.Splines(X,nodes=nodes)
  res
} 

calcDistances.Splines <- function(X,df){
  NC <- ncol(X)
  NR <- nrow(X) 
  for(i in 1:NR)
  {
    temp[[i]] <- smooth.spline(1:NC, X[i,], df=df, all.knots=TRUE)
    deriv0 <- predict(temp,seq(1,NC,0.1),deriv=0)$y
    #deriv1 <- predict(temp,seq(1,NC,0.1),deriv=1)$y
  }
}

X <- matrix(sample(1000,200),ncol=20)
calcDistances(X, method="sp", df=18)
