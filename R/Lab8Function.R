#' @title mycltu
#'
#' @param n number of trials in each sample
#' @param iter number of samples
#' @param a lower bound for the uniform distribution
#' @param b upper bound for the uniform distribution
#'
#'@importFrom graphics hist lines
#'@importFrom stats density dunif runif
#'
#' @return returns a histogram of sample means from a uniform distribution. Shows theoretical distribution and actual distribution
#' @export
#'
#' @examples
#' mycltu(n=10, iter=1000, a=5, b=10)
mycltu=function(n,iter,a=0,b=10){
  y=runif(n*iter,a,b)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  w=apply(data,2,mean)
  param=hist(w,plot=FALSE)
  ymax=max(param$density)
  ymax=1.1*ymax
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean", "\n", "sample size= ",n,sep=""),
       xlab="Sample mean")
  lines(density(w),col="Blue",lwd=3)
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3)
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
