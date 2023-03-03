#' Title
#'
#' @param a quantile
#' @param mu mean of normal distribution
#' @param sigma standard deviation of normal distribution
#'
#'@importFrom graphics curve polygon text
#'@importFrom stats dnorm pnorm
#'
#' @return
#' @export
#'
#' @examples
#' myncurve(4, 5, .6)
myncurve = function(a, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  area <- pnorm(a,mu,sigma)
  area <- round(area,4)
  print(area)
  xcurve <- seq(mu-3*sigma-1,a,length = 10000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma,xcurve,a), c(0,ycurve,0), col = "Purple")
  text(mu,.5*dnorm(mu, mu, sigma),paste0("Area= ", area))
}
