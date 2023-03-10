#' Sum of Squares
#'
#' @param lm linear model of the data
#' @param x a vector consisting of the dependent variable measurements in the linear model
#'
#'@importFrom stats fitted
#'
#' @return First the RSS, then the MSS, and the last value is the TSS
#' @export
#'
#' @examples
#'\dontrun{SS(spruce.lm, spruce$Height)}
#'
SS <- function(lm, x) {
  yhat=fitted(lm)
  RSS=sum((x-yhat)^2)
  MSS=sum((yhat-mean(x))^2)
  TSS=sum((x-mean(x))^2)
  print(RSS)
  print(MSS)
  print(TSS)
}
