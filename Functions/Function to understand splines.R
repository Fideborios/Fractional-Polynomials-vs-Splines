### Functions to understand the 



library(faraway)

y <- corrosion$loss
x <- corrosion$Fe

dks <- function(x=NULL , knots=NULL , intercept =FALSE ){
  dk <- function(x, i, z){
    (z[i+1]+z[i]-1)*(z[i+1]-z[i])*((x -0.5)^2 -1/12)/4 +
      ((abs(x-z[i+1]) -0.5)^2+(abs(x-z[i]) -0.5)^2 -0.5)*
      (abs(x-z[i+1]) -abs(x-z[i])+(z[i+1]-z[i])*(2*x-z[i+1]-z[i]))/24}
  xcol <- if( intercept ) 2 else 1
  q <- length(knots )+ xcol
  ## Form spline basis matrix
  n <- length(x)
  X <- matrix(1,n,q)
  X[,xcol] <- 0.02*x
  knots <- c(0, knots)
  if(q>xcol) X[, (xcol +1):q] <-
    outer(x ,1:(q-xcol),
          FUN=function(x, i)dk(x, i, knots ))
  X
}

dkp <- function(knots ){
  dk <- function(x, i, z){
    (z[i+1]+z[i]-1)*(z[i+1]-z[i])*((x -0.5)^2 -1/12)/4 +
      ((abs(x-z[i+1]) -0.5)^2+(abs(x-z[i]) -0.5)^2 -0.5)*
      (abs(x-z[i+1]) -abs(x-z[i])+(z[i+1]-z[i])*(2*x-z[i+1]-z[i]))/24}
  q <- length(knots )+2
  ## Form penalty matrix
  S <- matrix(0,q,q)
  knots <- c(0, knots)
  S[3:q,3:q] <- outer(1:(q-2) ,1:(q-2), FUN=function(i,j)
    dk(knots[i+1], j, knots) -
      dk(knots[i], j, knots ))
  d <- eigen(S, symmetric =TRUE)
  d$vectors%*%diag(d$values ^0.5)%*%t(d$vectors)
}



smfit <-
  function(x, y, knots=NULL , lambda ){
    ran <- range(x)
    x <- (x-ran [1])/diff(ran)
    if(is.null(knots )){
      xk <- sort(unique(x))
      knots <- xk[-c(1,length(xk ))]
    }
    q <- length(knots) + 2
    n <- length(x)
    X <- model.matrix(~dks(x, knots ))
    S <- dkp(knots)
    y[(n+1):(n+q)] <- 0
    obj <- list(knots=knots , range=ran , lambda=lambda ,
                coef=matrix(0, nrow=dim(X)[2] , ncol=length(lambda )))
    i <- 0
    for(lam in lambda ){
      i <- i+1
      Xa <- rbind(X, sqrt(lam)*S)
      obj$coef[,i] <- coef(lm(y ~Xa -1))
    }
    obj
  }



mod.2 <- smfit(x, y, knots=NULL ,lambda=c(5 , 0.01 , 0.00001))
b <- mod.2$coef
knots <- mod.2$knots
ran <- mod.2$range
lambda <- mod.2$lambda
xp <- (0:500)/500
Xp <- model.matrix(~dks(xp , knots ))
hat <- Xp%*%b
dhat <- model.matrix(~dks ((x-ran [1])/diff(ran), knots ))%*%b
rss <- apply(dhat , 2, function(z)sum((y-z)^2))
plot(y ~ x)
for(i in 1:length(lambda )){
  lines(ran[1]+xp*diff(ran), hat[,i], lty=i)
}
